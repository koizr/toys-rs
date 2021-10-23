use std::collections::HashMap;

use crate::ast::Expression;

type Bindings = HashMap<String, i32>;

pub struct Environment {
    bindings: Bindings,
    next: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(bindings: Bindings, next: Option<Box<Environment>>) -> Self {
        Self { bindings, next }
    }

    pub fn find_binding(&self, name: &str) -> Option<&Bindings> {
        if self.bindings.contains_key(name) {
            Some(&self.bindings)
        } else {
            match &self.next {
                Some(next) => next.find_binding(name),
                None => None,
            }
        }
    }

    pub fn insert(&mut self, name: String, value: i32) {
        self.bindings.insert(name, value);
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            bindings: Default::default(),
            next: Default::default(),
        }
    }
}

/// インタープリタ本体
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new(environment: Environment) -> Self {
        Self { environment }
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
                .find_binding(&name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .get(&name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .to_owned(),
            Expression::IntegerLiteral(value) => value,
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_10_plus_20_should_be_30() {
        let mut interpreter = Interpreter::default();
        assert_eq!(interpreter.interpret(add(integer(10), integer(20))), 30);
    }

    #[test]
    fn test_定義した変数を参照することができる() {
        let mut interpreter = Interpreter::default();
        interpreter.interpret(assign("x".into(), integer(10)));
        assert_eq!(
            interpreter.interpret(add(identifier("x".into()), integer(20))),
            30
        );
    }

    #[test]
    #[should_panic(expected = "Identifier x is not defined")]
    fn test_未定義の変数を参照しようとするとpanicする() {
        let mut interpreter = Interpreter::default();
        interpreter.interpret(add(identifier("x".into()), integer(20)));
    }
}
