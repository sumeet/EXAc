const EXAMPLE_PROGRAM: &str = r#"
smt {
    open(300) -> awfp {
    }
}
"#;

fn main() -> anyhow::Result<()> {
    dbg!(EXAMPLE_PROGRAM);
    let p = parser::program(EXAMPLE_PROGRAM)?;
    dbg!(p);
    Ok(())
}

#[derive(Debug)]
pub struct Program {
    exas: Vec<Exa>,
}

#[derive(Debug)]
pub struct Exa {
    name: String,
    block: Block,
}

#[derive(Debug)]
pub struct Block {
    els: Vec<BlockEl>,
}

#[derive(Debug)]
pub enum BlockEl {
    OpenFileBlock(OpenFileBlock),
}

#[derive(Debug)]
pub struct OpenFileBlock {
    file_id: NumOrVar,
    binding: String,
    block: Block,
}

#[derive(Debug)]
pub enum NumOrVar {
    Var(String),
    Int(u32),
}

peg::parser! {
    pub grammar parser() for str {
        pub rule program() -> Program
            = exas:(exa()+) { Program { exas } }

        rule exa() -> Exa
            = _? name:ident() _? block:block() _? { Exa { name: name.into(), block } }

        rule block() -> Block
            = empty_block() / block_with_exprs()
        rule empty_block() -> Block
            = "{" _? "}" { Block { els: vec![] }}
        rule block_with_exprs() -> Block
            = "{" els:(block_el_with_whitespace()*) "}" { Block { els } }

        rule block_el_with_whitespace() -> BlockEl
            = _* block_el:block_el() _* { block_el }

        rule block_el() -> BlockEl
            = open_file_block()

        rule open_file_block() -> BlockEl
            = "open(" _? file_id:num_or_var() _? ")" _? "->" _? binding:ident() _? block:block() {
                BlockEl::OpenFileBlock(OpenFileBlock { file_id, binding: binding.to_owned(), block })
            }

        rule num_or_var() -> NumOrVar
            = num_or_var_num() / num_or_var_var()
        rule num_or_var_var() -> NumOrVar
            = ident:ident() { NumOrVar::Var(ident.to_owned()) }
        rule num_or_var_num() -> NumOrVar
            = num:num() { NumOrVar::Int(num) }
        rule num() -> u32
            = num:$("0" / "-"? ['1' ..= '9']+ ['0' ..= '9']*) {? num.parse().or(Err("num")) }

        rule ident() -> &'input str = $(ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
        rule ident_start() -> &'input str = $(['a'..='z' | 'A'..='Z' | '_']+)
        rule comma() -> () = _? "," _?
        rule nbspace() = onespace()+
        rule onespace() = [' ' | '\t']
        rule newline() = "\n" / "\r\n"
        rule whitespace() = (nbspace() / newline())+
        rule _() = quiet!{ whitespace() };
    }
}
