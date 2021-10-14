#[derive(Debug)]
pub struct Program {
    pub exas: Vec<Exa>,
}

#[derive(Debug)]
pub struct Exa {
    pub name: String,
    pub block: Block,
}

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    OpenFileBlock(OpenFileBlock),
    Assignment(Box<Assignment>),
    PlusAssignment(Box<Assignment>),
    MinusAssignment(Box<Assignment>),
    FileOp(FileOp),
    Halt,
    Link(Link),
    While(Box<While>),
    If(Box<If>),
    VarRef(String),
    LiteralNum(i32),
}

#[derive(Debug)]
pub struct While {
    pub cond: Condition,
    pub block: Block,
}

#[derive(Debug)]
pub struct If {
    pub cond: Condition,
    pub block: Block,
}

#[derive(Debug)]
pub enum Condition {
    NotEquals(Expr, Expr),
    GreaterThan(Expr, Expr),
    Not(Box<Condition>),
    EOF,
}

#[derive(Debug)]
pub struct Link {
    pub dest: NumOrVar,
}

#[derive(Debug)]
pub struct OpenFileBlock {
    pub file_id: NumOrVar,
    binding: String,
    pub block: Block,
}

#[derive(Debug)]
pub struct Assignment {
    binding: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct FileOp {
    pub binding: String,
    pub op_name: String,
    pub arg: Option<NumOrVar>,
}

#[derive(Debug)]
pub enum NumOrVar {
    Var(String),
    Int(i32),
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
            = "{" _? "}" { Block { exprs: vec![] }}
        rule block_with_exprs() -> Block
            = "{" exprs:(expr_with_whitespace()*) "}" { Block { exprs } }

        rule expr_with_whitespace() -> Expr
            = _* expr:expr() _* { expr }

        rule expr() -> Expr
            = (open_file_block() / assignment() / plus_assignment() / minus_assignment() /
               file_op() / link() / halt() / while() / if() / var_ref() / literal_num())

        rule literal_num() -> Expr
            = num:num() { Expr::LiteralNum(num) }

        rule var_ref() -> Expr
            = ident:ident() { Expr::VarRef(ident.to_owned()) }

        rule open_file_block() -> Expr
            = "open(" _? file_id:num_or_var() _? ")" _? "->" _? binding:ident() _? block:block() {
                Expr::OpenFileBlock(OpenFileBlock { file_id, binding: binding.to_owned(), block })
            }
        rule assignment() -> Expr
            = binding:ident() _? "=" _? expr:expr() {
                Expr::Assignment(Box::new(Assignment { binding: binding.to_owned(), expr }))
            }
        rule plus_assignment() -> Expr
            = binding:ident() _? "+=" _? expr:expr() {
                Expr::PlusAssignment(Box::new(Assignment { binding: binding.to_owned(), expr }))
            }
        rule minus_assignment() -> Expr
            = binding:ident() _? "-=" _? expr:expr() {
                Expr::MinusAssignment(Box::new(Assignment { binding: binding.to_owned(), expr }))
            }
        // TODO: this is method call syntax... maybe could be more than fileops later
        rule file_op() -> Expr
            = binding:ident() _? "." _? op_name:ident() _? "(" _? arg:num_or_var()? _? ")" {
                Expr::FileOp(FileOp {
                    binding: binding.to_owned(),
                    op_name: op_name.to_owned(),
                    arg,
                })
            }
        rule link() -> Expr
            = "link" _? dest:num_or_var() { Expr::Link(Link { dest }) }
        rule halt() -> Expr
            = "HALT" { Expr::Halt }
        rule while() -> Expr
            = "while" _? "(" _? cond:condition() _? ")" _? block:block() {
                Expr::While(Box::new(While { cond, block }))
            }
        rule if() -> Expr
            = "if" _? "(" _? cond:condition() _? ")" _? block:block() {
                Expr::If(Box::new(If { cond, block }))
            }

        rule condition() -> Condition
            = not_equals() / greater_than() / not_of_condition() / feof()
        rule not_equals() -> Condition
            = lhs:expr() _? "!=" _? rhs:expr() { Condition::NotEquals(lhs, rhs) }
        rule greater_than() -> Condition
            = lhs:expr() _? ">" _? rhs:expr() { Condition::GreaterThan(lhs, rhs) }
        rule not_of_condition() -> Condition
            = "!" _? cond:condition() { Condition::Not(Box::new(cond)) }
        rule feof() -> Condition = "feof" { Condition::EOF }

        rule num_or_var() -> NumOrVar
            = num_or_var_num() / num_or_var_var()
        rule num_or_var_var() -> NumOrVar
            = ident:ident() { NumOrVar::Var(ident.to_owned()) }
        rule num_or_var_num() -> NumOrVar
            = num:num() { NumOrVar::Int(num) }
        rule num() -> i32
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
