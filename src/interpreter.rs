use crate::ast::Expression;

pub fn interpret(expression: Expression) -> i32 {
    match expression {
        Expression::BinaryExpression { operator, lhs, rhs } => match operator {
            crate::ast::Operator::Add => interpret(*lhs) + interpret(*rhs),
            crate::ast::Operator::Subtract => interpret(*lhs) - interpret(*rhs),
            crate::ast::Operator::Multiply => interpret(*lhs) * interpret(*rhs),
            crate::ast::Operator::Divide => interpret(*lhs) / interpret(*rhs),
        },
        Expression::IntegerLiteral(value) => value,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_10_plus_20_should_be_30() {
        assert_eq!(interpret(add(integer(10), integer(20))), 30);
    }
}
