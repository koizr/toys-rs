/// プログラム全体
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub definitions: Vec<TopLevel>,
}

impl Program {
    pub fn new(definitions: Vec<TopLevel>) -> Self {
        Self { definitions }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<String>,
    pub body: Expression,
}

/// トップレベルに定義できるもの
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopLevel {
    GlobalVariableDefinition {
        name: String,
        expression: Expression,
    },
    FunctionDefinition(FunctionDefinition),
}

/// 式
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Block(Vec<Expression>),
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_clause: Box<Expression>,
        else_clause: Box<Expression>,
    },
    Binary {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Assignment {
        name: String,
        expression: Box<Expression>,
    },
    Identifier(String),
    IntegerLiteral(i32),
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    Println(Box<Expression>),
}

/// 演算子
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    EqualEqual,
    NotEqual,
}

#[allow(dead_code)]
pub fn binary_expression(lhs: Expression, op: Operator, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn subtract(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::Subtract,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn multiply(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::Multiply,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn divide(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::Divide,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn lt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::LessThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn le(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::LessOrEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn gt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::GreaterThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn ge(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::GreaterOrEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn eq(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::EqualEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn neq(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        operator: Operator::NotEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[allow(dead_code)]
pub fn integer(value: i32) -> Expression {
    Expression::IntegerLiteral(value)
}

#[allow(dead_code)]
pub fn assign(name: String, expression: Expression) -> Expression {
    Expression::Assignment {
        name,
        expression: Box::new(expression),
    }
}

#[allow(dead_code)]
pub fn identifier(name: String) -> Expression {
    Expression::Identifier(name)
}

#[allow(dead_code)]
pub fn if_(condition: Expression, then_clause: Expression, else_clause: Expression) -> Expression {
    Expression::If {
        condition: Box::new(condition),
        then_clause: Box::new(then_clause),
        else_clause: Box::new(else_clause),
    }
}

#[allow(dead_code)]
pub fn while_(condition: Expression, body: Expression) -> Expression {
    Expression::While {
        condition: Box::new(condition),
        body: Box::new(body),
    }
}

#[allow(dead_code)]
pub fn block(expressions: Vec<Expression>) -> Expression {
    Expression::Block(expressions)
}

#[allow(dead_code)]
pub fn call(name: String, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall { name, args }
}

#[allow(dead_code)]
pub fn println_(expression: Expression) -> Expression {
    Expression::Println(Box::new(expression))
}

#[allow(dead_code)]
pub fn function(name: String, args: Vec<String>, body: Expression) -> TopLevel {
    TopLevel::FunctionDefinition(FunctionDefinition { name, args, body })
}

#[allow(dead_code)]
pub fn global_variable(name: String, expression: Expression) -> TopLevel {
    TopLevel::GlobalVariableDefinition { name, expression }
}

#[allow(dead_code)]
pub fn program(definitions: Vec<TopLevel>) -> Program {
    Program::new(definitions)
}
