use crate::parser::{Condition, Expr, FileOp, NumOrVar};
use anyhow::{anyhow, bail};
use rand::Rng;

mod parser;

const EXAMPLE_PROGRAM: &str = r#"
smt {
    open(300) -> file {
        username = file.read()
    }
    
    link 800
    
    open(199) -> file {
        while (username != file.read()) {
            file.seek(2)
        }
        file.seek(1)
        user_file_id = file.read()
    }
    
    link 799
    open(user_file_id) -> file {
        file.seek(2)
        sum = 0
        while (!feof) {
            sum += file.read()
        }
    }
}
"#;

// HAX
fn rand_label_id() -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    const PASSWORD_LEN: usize = 10;
    let mut rng = rand::thread_rng();

    (0..PASSWORD_LEN)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect()
}

fn assign_expr(assignment: &parser::Assignment) -> anyhow::Result<Vec<String>> {
    match &assignment.expr {
        Expr::FileOp(parser::FileOp {
            op_name,
            arg: None,
            binding: _,
        }) if op_name == "read" => Ok(vec!["COPY F X".to_owned()]),
        Expr::LiteralNum(n) => Ok(vec![format!("COPY {} X", n)]),
        Expr::OpenFileBlock(_)
        | Expr::Assignment(_)
        | Expr::PlusAssignment(_)
        | Expr::Link(_)
        | Expr::FileOp(_)
        | Expr::While(_)
        | Expr::VarRef(_) => {
            bail!("assignment not supported for {:?}", assignment.expr)
        }
    }
}

fn plus_assign_expr(assignment: &parser::Assignment) -> anyhow::Result<Vec<String>> {
    match &assignment.expr {
        Expr::FileOp(parser::FileOp {
            op_name,
            arg: None,
            binding: _,
        }) if op_name == "read" => Ok(vec!["ADDI F X X".to_owned()]),
        Expr::LiteralNum(n) => Ok(vec![format!("ADDI {} X X", n)]),
        Expr::OpenFileBlock(_)
        | Expr::Assignment(_)
        | Expr::PlusAssignment(_)
        | Expr::Link(_)
        | Expr::FileOp(_)
        | Expr::While(_)
        | Expr::VarRef(_) => {
            bail!("plus assignment not supported for {:?}", assignment.expr)
        }
    }
}

fn cond_op(expr: &Expr) -> anyhow::Result<String> {
    match expr {
        Expr::FileOp(parser::FileOp {
            op_name,
            arg: None,
            binding: _,
        }) if op_name == "read" => Ok("F".to_owned()),
        Expr::VarRef(_) => Ok("X".to_owned()),
        Expr::LiteralNum(n) => Ok(n.to_string()),
        Expr::OpenFileBlock(_)
        | Expr::PlusAssignment(_)
        | Expr::Assignment(_)
        | Expr::Link(_)
        | Expr::FileOp(_)
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
        Condition::NotEquals(lhs, rhs) => Ok(TestExpr {
            test_statement: format!("TEST {} = {}", cond_op(lhs)?, cond_op(rhs)?),
            negate: true,
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
    let start_label = format!("WHILE_START_{}", while_id);
    let end_label = format!("WHILE_END_{}", while_id);

    let TestExpr {
        test_statement,
        negate: needs_negation,
    } = compile_condition(&r#while.cond)?;
    let mut v = vec![format!("MARK {}", start_label), test_statement];
    let jmp_instruction = if needs_negation { "TJMP" } else { "FJMP" };
    v.push(format!("{} {}", jmp_instruction, end_label));
    v.extend(compile_block(&r#while.block)?);
    v.push(format!("JMP {}", start_label));
    v.push(format!("MARK {}", end_label));
    Ok(v)
}

fn compile_file_op(file_op: &FileOp) -> anyhow::Result<Vec<String>> {
    if file_op.op_name != "seek" {
        bail!(
            "files can only be seeked on the bare level, not {:?}",
            file_op
        )
    }
    let arg = file_op
        .arg
        .as_ref()
        .ok_or(anyhow!("seek must have an argument"))?;
    Ok(vec![format!("SEEK {}", to_arg(&arg))])
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
            Expr::PlusAssignment(assignment) => plus_assign_expr(assignment),
            Expr::Link(link) => Ok(vec![format!("LINK {}", to_arg(&link.dest))]),
            Expr::While(r#while) => compile_while(r#while),
            Expr::FileOp(file_op) => compile_file_op(file_op),
            Expr::LiteralNum(_) | Expr::VarRef(_) => {
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

fn main() -> anyhow::Result<()> {
    //dbg!(EXAMPLE_PROGRAM);
    let program = parser::parser::program(EXAMPLE_PROGRAM)?;
    //dbg!(p);
    for exa in program.exas {
        println!("-- exa {} --", exa.name);
        println!("{}", compile_exa(&exa)?.join("\n"));
    }
    Ok(())
}
