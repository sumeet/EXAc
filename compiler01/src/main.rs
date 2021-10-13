mod parser;

const EXAMPLE_PROGRAM: &str = r#"
smt {
    open(300) -> file {
        username = file.read()
    }
    
    link 800
}
"#;

fn main() -> anyhow::Result<()> {
    dbg!(EXAMPLE_PROGRAM);
    let p = parser::parser::program(EXAMPLE_PROGRAM)?;
    dbg!(p);
    Ok(())
}
