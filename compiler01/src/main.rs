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

fn to_reg_name(operand: &parser::Operand) -> String {
    match operand {
        Operand::FileRead => "F".to_owned(),
        Operand::GlobalLink(_) => "M".to_owned(),
        Operand::SpecialRegister(name) => format!("#{}", name),
        Operand::XVarName(_) => format!("X"),
        Operand::LiteralNum(n) => n.to_string(),
    }
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
    let dest = to_reg_name(&assignment.dest);
    Ok(vec![match &assignment.src {
        AssignSource::Operand(src_operand) => {
            let src = to_reg_name(src_operand);
            format!("COPY {} {}", src, dest)
        }
        AssignSource::BinOp(lhs, binop, rhs) => {
            let instruction = to_binop_instruction(*binop);
            let lhs = to_reg_name(lhs);
            let rhs = to_reg_name(rhs);
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
        Expr::VarRef(_) => Ok("X".to_owned()),
        Expr::SpecialReg(name) => Ok(format!("#{}", name)),
        Expr::GlobalLink(_) => Ok("M".to_owned()),
        Expr::LiteralNum(n) => Ok(n.to_string()),
        Expr::OpenFileBlock(_)
        | Expr::Assignment(_)
        | Expr::Link(_)
        | Expr::Wait(_)
        | Expr::Halt
        | Expr::Kill
        | Expr::FileOp(_)
        | Expr::If(_)
        | Expr::While(_) => {
            bail!("conditions not supported for {:?}", expr)
        }
    }
}

struct TestExpr {
    test_statement: String,
    negate: bool,
}

fn compile_condition(cond: &Condition) -> anyhow::Result<TestExpr> {
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
            let mut inner_test_expr = compile_condition(inner_cond)?;
            inner_test_expr.negate = !inner_test_expr.negate;
            Ok(inner_test_expr)
        }
        Condition::EOF => Ok(TestExpr {
            test_statement: "TEST EOF".to_string(),
            negate: false,
        }),
    }
}

fn compile_while(r#while: &parser::While) -> anyhow::Result<Vec<String>> {
    let while_id = rand_label_id();
    let start_label = format!("WH_ST_{}", while_id);
    let end_label = format!("WH_EN_{}", while_id);

    let TestExpr {
        test_statement,
        negate: needs_negation,
    } = compile_condition(&r#while.cond)?;
    let mut v = vec![format!("MARK {}", start_label), test_statement];
    let jmp_instruction = if needs_negation { "TJMP" } else { "FJMP" };
    v.push(format!("{} {}", jmp_instruction, end_label));
    v.extend(compile_block(&r#while.block)?);
    v.push(format!("JUMP {}", start_label));
    v.push(format!("MARK {}", end_label));
    Ok(v)
}

fn compile_if(r#if: &parser::If) -> anyhow::Result<Vec<String>> {
    let if_id = rand_label_id();
    let end_label = format!("IF_EN_{}", if_id);
    let else_label = format!("IF_EL_{}", if_id);

    let TestExpr {
        test_statement,
        negate: needs_negation,
    } = compile_condition(&r#if.cond)?;
    let jmp_instruction = if needs_negation { "TJMP" } else { "FJMP" };

    let mut v = vec![test_statement];
    if r#if.else_block.is_none() {
        v.push(format!("{} {}", jmp_instruction, end_label));
    } else {
        v.push(format!("{} {}", jmp_instruction, else_label));
    }
    v.extend(compile_block(&r#if.block)?);
    if let Some(else_block) = &r#if.else_block {
        v.push(format!("JUMP {}", end_label));
        v.push(format!("MARK {}", else_label));
        v.extend(compile_block(else_block)?);
    }
    v.push(format!("MARK {}", end_label));
    Ok(v)
}

fn compile_file_op(file_op: &FileOp) -> anyhow::Result<Vec<String>> {
    match file_op.op_name.as_str() {
        "seek" => {
            let arg = file_op
                .arg
                .as_ref()
                .ok_or(anyhow!("seek must have an argument"))?;
            Ok(vec![format!("SEEK {}", to_arg(&arg))])
        }
        "write" => {
            let arg = file_op
                .arg
                .as_ref()
                .ok_or(anyhow!("write must have an argument"))?;
            Ok(vec![format!("COPY {} F", to_arg(&arg))])
        }
        _ => bail!("file operation {:?} unsupported at bare level", file_op),
    }
}

fn compile_block(block: &parser::Block) -> anyhow::Result<Vec<String>> {
    block
        .exprs
        .iter()
        .map(|expr| match expr {
            Expr::OpenFileBlock(open_file_block) => {
                let mut v = vec![format!("GRAB {}", to_arg(&open_file_block.file_id))];
                v.extend(compile_block(&open_file_block.block)?);
                v.push("DROP".to_owned());
                Ok(v)
            }
            Expr::Assignment(assignment) => assign_expr(assignment),
            Expr::Halt => Ok(vec!["HALT".to_string()]),
            Expr::Kill => Ok(vec!["KILL".to_string()]),
            Expr::Wait(n) => Ok(repeat("NOOP".to_string()).take(*n as _).collect()),
            Expr::Link(link) => Ok(link
                .dests
                .iter()
                .map(|dest| format!("LINK {}", to_arg(dest)))
                .collect()),
            Expr::While(r#while) => compile_while(r#while),
            Expr::If(r#if) => compile_if(r#if),
            Expr::FileOp(file_op) => compile_file_op(file_op),
            Expr::LiteralNum(_) | Expr::VarRef(_) | Expr::SpecialReg(_) | Expr::GlobalLink(_) => {
                bail!("{:?} on bare level not supported", expr)
            }
        })
        .collect::<anyhow::Result<Vec<_>>>()
        .map(|v| v.into_iter().flatten().collect())
}

fn compile_exa(exa: &parser::Exa) -> anyhow::Result<Vec<String>> {
    compile_block(&exa.block)
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
    //dbg!(p);
    for exa in program.exas {
        println!("-- exa {} --", exa.name);
        println!("{}", compile_exa(&exa)?.join("\n"));
        println!();
        println!();
    }
    Ok(())
}
