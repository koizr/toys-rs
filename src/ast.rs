/// プログラム全体
#[derive(Debug, Clone)]
pub struct Program {
    pub definitions: Vec<TopLevel>,
}

impl Program {
    pub fn new(definitions: Vec<TopLevel>) -> Self {
        Self { definitions }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<String>,
    pub body: Expression,
}

/// トップレベルに定義できるもの
#[derive(Debug, Clone)]
pub enum TopLevel {
    GlobalVariableDefinition {
        name: String,
        expression: Expression,
    },
    FunctionDefinition(FunctionDefinition),
}

/// 式
#[derive(Debug, Clone)]
pub enum Expression {
    BlockExpression(Vec<Expression>),
    WhileExpression {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    IfExpression {
        condition: Box<Expression>,
        then_clause: Box<Expression>,
        else_clause: Box<Expression>,
    },
    BinaryExpression {
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
}

/// 演算子
#[derive(Debug, Clone)]
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

pub fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn subtract(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Subtract,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn multiply(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Multiply,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn divide(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Divide,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn lt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::LessThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn le(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::LessOrEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn gt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::GreaterThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn ge(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::GreaterOrEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn eq(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::EqualEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn neq(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::NotEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn integer(value: i32) -> Expression {
    Expression::IntegerLiteral(value)
}

pub fn assign(name: String, expression: Expression) -> Expression {
    Expression::Assignment {
        name,
        expression: Box::new(expression),
    }
}

pub fn identifier(name: String) -> Expression {
    Expression::Identifier(name)
}

pub fn if_(condition: Expression, then_clause: Expression, else_clause: Expression) -> Expression {
    Expression::IfExpression {
        condition: Box::new(condition),
        then_clause: Box::new(then_clause),
        else_clause: Box::new(else_clause),
    }
}

pub fn while_(condition: Expression, body: Expression) -> Expression {
    Expression::WhileExpression {
        condition: Box::new(condition),
        body: Box::new(body),
    }
}

pub fn block(expressions: Vec<Expression>) -> Expression {
    Expression::BlockExpression(expressions)
}

pub fn call(name: String, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall { name, args }
}

pub fn function(name: String, args: Vec<String>, body: Expression) -> TopLevel {
    TopLevel::FunctionDefinition(FunctionDefinition { name, args, body })
}

pub fn global_variable(name: String, expression: Expression) -> TopLevel {
    TopLevel::GlobalVariableDefinition { name, expression }
}
