/// 式
pub enum Expression {
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
}

/// 演算子
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
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
