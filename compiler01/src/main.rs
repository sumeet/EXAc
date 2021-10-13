const EXAMPLE_PROGRAM: &str = r#"
smt {}
smt2 {}
"#;

fn main() {
    dbg!(EXAMPLE_PROGRAM);
    let p = parser::program(EXAMPLE_PROGRAM).unwrap();
    dbg!(p);
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
    exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Expr {}

peg::parser! {
    pub grammar parser() for str {
        pub rule program() -> Program
            = exas:(exa()+) { Program { exas } }

        rule exa() -> Exa
            = _? name:ident() _? block:block() _? { Exa { name: name.into(), block } }

        rule block() -> Block
            = "{" exprs:(expr_with_newlines()*) "}" { Block { exprs } }

        rule expr_with_newlines() -> Expr
            = newline()* expr:expr() newline()* { expr }

        rule expr() -> Expr
            = ident() { Expr {} }

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
