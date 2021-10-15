use anyhow::bail;

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
    Assignment(Assignment),
    FileOp(FileOp),
    Halt,
    Kill,
    Link(Link),
    Wait(u32),
    While(Box<While>),
    If(Box<If>),
    VarRef(String),
    SpecialReg(String),
    GlobalLink(String),
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
    pub else_block: Option<Block>,
}

#[derive(Debug)]
pub enum Condition {
    Equals(Expr, Expr),
    NotEquals(Expr, Expr),
    LessThan(Expr, Expr),
    GreaterThan(Expr, Expr),
    Not(Box<Condition>),
    EOF,
}

#[derive(Debug)]
pub struct Link {
    pub dests: Vec<NumOrVar>,
}

#[derive(Debug)]
pub struct OpenFileBlock {
    pub file_id: NumOrVar,
    binding: String,
    pub block: Block,
}

#[derive(Debug)]
pub struct Assignment {
    pub dest: Operand,
    pub src: AssignSource,
}

impl Assignment {
    pub fn new(dest: Operand, src: AssignSource) -> anyhow::Result<Self> {
        match dest {
            Operand::LiteralNum(_) => {
                bail!("can't assign into a literal number: {:?}", dest)
            }
            Operand::FileRead
            | Operand::GlobalLink(_)
            | Operand::SpecialRegister(_)
            | Operand::XVarName(_) => (),
        }
        Ok(Self { dest, src })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Div,
    Mul,
    Swizzle,
}

#[derive(Debug)]
pub enum AssignSource {
    Operand(Operand),
    BinOp(Operand, BinOp, Operand),
}

#[derive(Debug)]
pub enum Operand {
    FileRead,
    GlobalLink(String),
    SpecialRegister(String),
    XVarName(String),
    LiteralNum(i32),
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
            = (open_file_block() / assignment() / file_op() / link() / wait() / kill() / halt() /
               while() / if() / var_ref() / special_reg_expr() / global_link_expr() / literal_num())

        rule literal_num() -> Expr
            = num:num() { Expr::LiteralNum(num) }

        rule var_ref() -> Expr
            = ident:ident() { Expr::VarRef(ident.to_owned()) }
        rule special_reg_expr() -> Expr
            = "#" ident:ident() { Expr::SpecialReg(ident.to_owned() )}
        rule global_link_expr() -> Expr
            = "$" ident:ident() { Expr::GlobalLink(ident.to_owned() )}

        rule open_file_block() -> Expr
            = "open(" _? file_id:num_or_var() _? ")" _? "->" _? binding:ident() _? block:block() {
                Expr::OpenFileBlock(OpenFileBlock { file_id, binding: binding.to_owned(), block })
            }

        rule binop() -> BinOp
            = add_op() / sub_op() / div_op() / mul_op() / swizzle_op()
        rule add_op() -> BinOp
            = "+" { BinOp::Add }
        rule sub_op() -> BinOp
            = "-" { BinOp::Sub }
        rule div_op() -> BinOp
            = "/" { BinOp::Div }
        rule mul_op() -> BinOp
            = "*" { BinOp::Mul }
        rule swizzle_op() -> BinOp
            = "~" { BinOp::Swizzle }

        rule assignment() -> Expr = regular_assignment() / op_assignment()

        rule regular_assignment() -> Expr
            = dest:operand() _? "=" _? src:assign_source() {
                Expr::Assignment(Assignment::new(dest, src))
            }
        rule op_assignment() -> Expr
            = dest:operand() _? binop() "=" _? src:operand() {
                let src = AssignSource::Operand(src);
                Expr::Assignment(Assignment::new(dest, src))
            }

        rule operand() -> Operand
            = x_var_operand() / special_reg_operand() / global_link_operand()

        rule x_var_operand() -> Operand
            = name:ident() { Operand::XVarName(name.to_owned()) }
        rule special_reg_operand() -> Operand
            = "#" name:ident() { Operand::SpecialRegister(name.to_owned()) }
        rule global_link_operand() -> Operand
            = "$" name:ident() { Operand::GlobalLink(name.to_owned()) }

        rule assign_source() -> AssignSource
            = operand_assign_source() / binop_assign_source()
        rule operand_assign_source() -> AssignSource
            = operand:operand() { AssignSource::Operand(operand) }
        rule binop_assign_source() -> AssignSource
            = lhs:operand() _? binop:binop() _? rhs:operand() {
                AssignSource::BinOp(lhs, binop, rhs)
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
            = "link" _? dests:(num_or_var() ** comma()) { Expr::Link(Link { dests }) }
        rule wait() -> Expr
            = wait_n_times() / wait_no_args()
        rule wait_no_args() -> Expr
            = "wait" { Expr::Wait(1) }
        rule wait_n_times() -> Expr
            = "wait" _? times:num() { Expr::Wait(times as _) }
        rule kill() -> Expr
            = "kill" { Expr::Kill }
        rule halt() -> Expr
            = "HALT" { Expr::Halt }
        rule while() -> Expr
            = "while" _? "(" _? cond:condition() _? ")" _? block:block() {
                Expr::While(Box::new(While { cond, block }))
            }
        rule if() -> Expr
            = if_else() / if_no_else()

        rule if_else() -> Expr
            = "if" _? "(" _? cond:condition() _? ")" _? block:block() _? "else" _? else_block:block() {
                Expr::If(Box::new(If { cond, block, else_block: Some(else_block) }))
            }

        rule if_no_else() -> Expr
            = "if" _? "(" _? cond:condition() _? ")" _? block:block() {
                Expr::If(Box::new(If { cond, block, else_block: None }))
            }

        rule condition() -> Condition
            = equals() / not_equals() / greater_than() / not_of_condition() / feof()
        rule equals() -> Condition
            = lhs:expr() _? "==" _? rhs:expr() { Condition::Equals(lhs, rhs) }
        rule not_equals() -> Condition
            = lhs:expr() _? "!=" _? rhs:expr() { Condition::NotEquals(lhs, rhs) }
        rule less_than() -> Condition
            = lhs:expr() _? ">" _? rhs:expr() { Condition::LessThan(lhs, rhs) }
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
