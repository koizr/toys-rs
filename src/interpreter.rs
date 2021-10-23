use std::collections::HashMap;

use crate::ast::Expression;

type Environment = HashMap<String, i32>;
/// インタープリタ本体
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: HashMap::new(),
        }
    }

    /// 式を評価する
    pub fn interpret(&mut self, expression: Expression) -> i32 {
        match expression {
            Expression::BinaryExpression { operator, lhs, rhs } => match operator {
                crate::ast::Operator::Add => self.interpret(*lhs) + self.interpret(*rhs),
                crate::ast::Operator::Subtract => self.interpret(*lhs) - self.interpret(*rhs),
                crate::ast::Operator::Multiply => self.interpret(*lhs) * self.interpret(*rhs),
                crate::ast::Operator::Divide => self.interpret(*lhs) / self.interpret(*rhs),
            },
            Expression::Assignment { name, expression } => {
                let value = self.interpret(*expression);
                self.environment.insert(name, value);
                value
            }
            Expression::Identifier(name) => self
                .environment
                .get(&name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .to_owned(),
            Expression::IntegerLiteral(value) => value,
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_10_plus_20_should_be_30() {
        let mut interpreter = Interpreter::new();
        assert_eq!(interpreter.interpret(add(integer(10), integer(20))), 30);
    }

    #[test]
    fn test_定義した変数を参照することができる() {
        let mut interpreter = Interpreter::new();
        interpreter.interpret(assign("x".into(), integer(10)));
        assert_eq!(
            interpreter.interpret(add(identifier("x".into()), integer(20))),
            30
        );
    }

    #[test]
    #[should_panic(expected = "Identifier x is not defined")]
    fn test_未定義の変数を参照しようとするとpanicする() {
        let mut interpreter = Interpreter::new();
        interpreter.interpret(add(identifier("x".into()), integer(20)));
    }
}
