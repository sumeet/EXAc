use crate::parser::{Expr, NumOrVar};
use anyhow::bail;

mod parser;

const EXAMPLE_PROGRAM: &str = r#"
smt {
    open(300) -> file {
        username = file.read()
    }
    
    link 800
}
"#;

fn assign_expr(assignment: &parser::Assignment) -> anyhow::Result<Vec<String>> {
    let read = "read".to_owned();
    match &assignment.expr {
        Expr::FileOp(parser::FileOp {
            op_name: read,
            arg: None,
            binding: _,
        }) => Ok(vec!["COPY F X".to_owned()]),
        Expr::OpenFileBlock(_) | Expr::Assignment(_) | Expr::Link(_) | Expr::FileOp(_) => {
            bail!("assignment not supported for {:?}", assignment.expr)
        }
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
            Expr::FileOp(_) => Ok(vec![]),
            Expr::Link(_) => Ok(vec![]),
        })
        .collect::<anyhow::Result<Vec<_>>>()
        .map(|v| v.into_iter().flatten().collect())
}

fn compile_exa(exa: &parser::Exa) -> anyhow::Result<Vec<String>> {
    compile_block(&exa.block)
}

fn to_arg(num_or_var: &NumOrVar) -> String {
    match num_or_var {
        NumOrVar::Var(_) => "x".to_owned(),
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
