use crate::parser::{AssignSource, BinOp, Condition, Expr, FileOp, NumOrVar, Operand};
use anyhow::{anyhow, bail};
use rand::Rng;
use std::iter::repeat;

mod parser;

// HAX
fn rand_label_id() -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    const PASSWORD_LEN: usize = 5;
    let mut rng = rand::thread_rng();

    (0..PASSWORD_LEN)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect()
}

fn to_reg_name(operand: &parser::Operand) -> anyhow::Result<String> {
    Ok(match operand {
        Operand::File => "F".to_owned(),
        Operand::GlobalLink(_) => "M".to_owned(),
        Operand::SpecialRegister(name) => format!("#{}", name),
        Operand::TVarName(_) => "T".to_owned(),
        Operand::XVarName(_) => format!("X"),
        Operand::LiteralNum(n) => n.to_string(),
        Operand::HostID => bail!("host_id requires special assignment"),
    })
}

fn to_binop_instruction(binop: parser::BinOp) -> &'static str {
    match binop {
        BinOp::Add => "ADDI",
        BinOp::Sub => "SUBI",
        BinOp::Div => "DIVI",
        BinOp::Mul => "MULI",
        BinOp::Swizzle => "SWIZ",
    }
}

fn assign_expr(assignment: &parser::Assignment) -> anyhow::Result<Vec<String>> {
    let dest = to_reg_name(&assignment.dest)?;
    Ok(vec![match &assignment.src {
        AssignSource::Operand(src_operand) => {
            if let Operand::HostID = src_operand {
                format!("HOST {}", dest)
            } else {
                let src = to_reg_name(src_operand)?;
                format!("COPY {} {}", src, dest)
            }
        }
        AssignSource::BinOp(lhs, binop, rhs) => {
            let instruction = to_binop_instruction(*binop);
            let lhs = to_reg_name(lhs)?;
            let rhs = to_reg_name(rhs)?;
            format!("{} {} {} {}", instruction, lhs, rhs, dest)
        }
    }])
}

fn cond_op(expr: &Expr) -> anyhow::Result<String> {
    match expr {
        Expr::FileOp(parser::FileOp {
            op_name,
            arg: None,
            binding: _,
        }) if op_name == "read" => Ok("F".to_owned()),
        Expr::XVarRef(_) => Ok("X".to_owned()),
        Expr::TVarRef(_) => Ok("T".to_owned()),
        Expr::SpecialReg(name) => Ok(format!("#{}", name)),
        Expr::GlobalLink(_) => Ok("M".to_owned()),
        Expr::LiteralNum(n) => Ok(n.to_string()),
        Expr::OpenFileBlock(_)
        | Expr::Repeat(_, _)
        | Expr::ChannelToggle
        | Expr::ChannelWait
        | Expr::CreateFileBlock(_)
        | Expr::Assignment(_)
        | Expr::Link(_)
        | Expr::Wait(_)
        | Expr::Halt
        | Expr::Spawn(_)
        | Expr::Kill
        | Expr::FileOp(_)
        | Expr::If(_)
        | Expr::While(_)
        | Expr::Continue
        | Expr::Loop(_) => {
            bail!("conditions not supported for {:?}", expr)
        }
    }
}

struct TestExpr {
    test_statement: String,
    negate: bool,
}

struct CompileContext {
    loop_start_labels: Vec<String>,
}

impl CompileContext {
    fn new() -> Self {
        Self {
            loop_start_labels: vec![],
        }
    }

    fn compile_exa(&mut self, exa: &parser::Exa) -> anyhow::Result<Vec<String>> {
        self.compile_block(&exa.block)
    }

    fn compile_block(&mut self, block: &parser::Block) -> anyhow::Result<Vec<String>> {
        block
            .exprs
            .iter()
            .map(|expr| match expr {
                Expr::Repeat(n, block) => {
                    let compiled_block = self.compile_block(block)?;
                    Ok(repeat(compiled_block).take(*n).flatten().collect())
                }
                Expr::OpenFileBlock(open_file_block) => {
                    let mut v = vec![format!("GRAB {}", to_reg_name(&open_file_block.file_id)?)];
                    v.extend(self.compile_block(&open_file_block.block)?);
                    v.push("DROP".to_owned());
                    Ok(v)
                }
                Expr::CreateFileBlock(block) => {
                    let mut v = vec!["MAKE".to_owned()];
                    v.extend(self.compile_block(block)?);
                    v.push("DROP".to_owned());
                    Ok(v)
                }
                Expr::Assignment(assignment) => assign_expr(assignment),
                Expr::ChannelToggle => Ok(vec!["MODE".to_string()]),
                Expr::ChannelWait => Ok(vec!["VOID M".to_string()]),
                Expr::Halt => Ok(vec!["HALT".to_string()]),
                Expr::Kill => Ok(vec!["KILL".to_string()]),
                Expr::Wait(n) => Ok(repeat("NOOP".to_string()).take(*n as _).collect()),
                Expr::Link(link) => Ok(link
                    .dests
                    .iter()
                    .map(|dest| format!("LINK {}", to_arg(dest)))
                    .collect()),
                Expr::Continue => {
                    let prev_start_label = self
                        .loop_start_labels
                        .last()
                        .ok_or_else(|| anyhow!("continue used outside of loop"))?;
                    Ok(vec![format!("JUMP {}", prev_start_label)])
                }
                Expr::While(r#while) => self.compile_while(r#while),
                Expr::Spawn(block) => self.compile_spawn(block),
                Expr::Loop(block) => self.compile_loop(block),
                Expr::If(r#if) => self.compile_if(r#if),
                Expr::FileOp(file_op) => self.compile_file_op(file_op),
                Expr::LiteralNum(_)
                | Expr::XVarRef(_)
                | Expr::TVarRef(_)
                | Expr::SpecialReg(_)
                | Expr::GlobalLink(_) => {
                    bail!("{:?} on bare level not supported", expr)
                }
            })
            .collect::<anyhow::Result<Vec<_>>>()
            .map(|v| v.into_iter().flatten().collect())
    }

    fn compile_condition(&mut self, cond: &Condition) -> anyhow::Result<TestExpr> {
        match cond {
            Condition::Equals(lhs, rhs) => Ok(TestExpr {
                test_statement: format!("TEST {} = {}", cond_op(lhs)?, cond_op(rhs)?),
                negate: false,
            }),
            Condition::NotEquals(lhs, rhs) => Ok(TestExpr {
                test_statement: format!("TEST {} = {}", cond_op(lhs)?, cond_op(rhs)?),
                negate: true,
            }),
            Condition::LessThan(lhs, rhs) => Ok(TestExpr {
                test_statement: format!("TEST {} < {}", cond_op(lhs)?, cond_op(rhs)?),
                negate: false,
            }),
            Condition::GreaterThan(lhs, rhs) => Ok(TestExpr {
                test_statement: format!("TEST {} > {}", cond_op(lhs)?, cond_op(rhs)?),
                negate: false,
            }),
            Condition::Not(inner_cond) => {
                let mut inner_test_expr = self.compile_condition(inner_cond)?;
                inner_test_expr.negate = !inner_test_expr.negate;
                Ok(inner_test_expr)
            }
            Condition::EOF => Ok(TestExpr {
                test_statement: "TEST EOF".to_string(),
                negate: false,
            }),
            Condition::ChannelReady => Ok(TestExpr {
                test_statement: "TEST MRD".to_string(),
                negate: false,
            }),
        }
    }

    fn compile_while(&mut self, r#while: &parser::While) -> anyhow::Result<Vec<String>> {
        let while_id = rand_label_id();
        let start_label = format!("WH_ST_{}", while_id);
        let end_label = format!("WH_EN_{}", while_id);

        let TestExpr {
            test_statement,
            negate: needs_negation,
        } = self.compile_condition(&r#while.cond)?;
        let mut v = vec![format!("MARK {}", start_label), test_statement];
        let jmp_instruction = if needs_negation { "TJMP" } else { "FJMP" };
        v.push(format!("{} {}", jmp_instruction, end_label));

        self.push_loop_start_label(start_label.clone());
        v.extend(self.compile_block(&r#while.block)?);
        self.pop_loop_start_label();

        v.push(format!("JUMP {}", start_label));
        v.push(format!("MARK {}", end_label));
        Ok(v)
    }

    fn compile_loop(&mut self, block: &parser::Block) -> anyhow::Result<Vec<String>> {
        let loop_id = rand_label_id();
        let start_of_loop_label = format!("LO_ST_{}", loop_id);

        let mut v = vec![];
        v.push(format!("MARK {}", start_of_loop_label));

        self.push_loop_start_label(start_of_loop_label.clone());
        v.extend(self.compile_block(&block)?);
        self.pop_loop_start_label();

        v.push(format!("JUMP {}", start_of_loop_label));
        Ok(v)
    }

    fn compile_spawn(&mut self, block: &parser::Block) -> anyhow::Result<Vec<String>> {
        let spawn_id = rand_label_id();
        let start_of_spawn_label = format!("SP_ST_{}", spawn_id);
        let end_of_spawn_label = format!("SP_EN_{}", spawn_id);

        let mut v = vec![];
        v.push(format!("REPL {}", start_of_spawn_label));
        v.push(format!("JUMP {}", end_of_spawn_label));
        v.push(format!("MARK {}", start_of_spawn_label));
        v.extend(self.compile_block(&block)?);
        v.push(format!("MARK {}", end_of_spawn_label));
        Ok(v)
    }

    fn compile_if(&mut self, r#if: &parser::If) -> anyhow::Result<Vec<String>> {
        let if_id = rand_label_id();
        let end_label = format!("IF_EN_{}", if_id);
        let else_label = format!("IF_EL_{}", if_id);

        let TestExpr {
            test_statement,
            negate: needs_negation,
        } = self.compile_condition(&r#if.cond)?;
        let jmp_instruction = if needs_negation { "TJMP" } else { "FJMP" };

        let mut v = vec![test_statement];
        if r#if.else_block.is_none() {
            v.push(format!("{} {}", jmp_instruction, end_label));
        } else {
            v.push(format!("{} {}", jmp_instruction, else_label));
        }
        v.extend(self.compile_block(&r#if.block)?);
        if let Some(else_block) = &r#if.else_block {
            v.push(format!("JUMP {}", end_label));
            v.push(format!("MARK {}", else_label));
            v.extend(self.compile_block(else_block)?);
        }
        v.push(format!("MARK {}", end_label));
        Ok(v)
    }

    fn compile_file_op(&mut self, file_op: &FileOp) -> anyhow::Result<Vec<String>> {
        match file_op.op_name.as_str() {
            "seek" => {
                let arg = file_op
                    .arg
                    .as_ref()
                    .ok_or(anyhow!("seek must have an argument"))?;
                Ok(vec![format!("SEEK {}", to_arg(&arg))])
            }
            "wipe" => Ok(vec!["WIPE".to_owned()]),
            _ => bail!("file operation {:?} unsupported at bare level", file_op),
        }
    }

    fn push_loop_start_label(&mut self, start_label: String) {
        self.loop_start_labels.push(start_label);
    }

    fn pop_loop_start_label(&mut self) {
        self.loop_start_labels.pop();
    }
}

fn to_arg(num_or_var: &NumOrVar) -> String {
    match num_or_var {
        NumOrVar::Var(_) => "X".to_owned(),
        NumOrVar::Int(i) => i.to_string(),
    }
}

fn remove_comments(txt: &str) -> String {
    txt.lines()
        .map(|line| {
            if line.trim_start().starts_with("//") {
                // replacing newlines with empty lines will preserve line numbers
                ""
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn main() -> anyhow::Result<()> {
    let filename = std::env::args()
        .nth(1)
        .ok_or(anyhow!("usage: exc <filename>"))?;
    let program_txt = remove_comments(&std::fs::read_to_string(filename)?);
    let program = parser::parser::program(&program_txt)?;
    let mut compiler = CompileContext::new();
    //dbg!(p);
    for exa in program.exas {
        // println!("-- exa {} --", exa.name);
        println!("{}", compiler.compile_exa(&exa)?.join("\n"));
        // println!();
        // println!();
    }
    Ok(())
}
