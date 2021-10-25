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
    pub fn interpret(&mut self, expression: &Expression) -> i32 {
        match expression {
            Expression::BlockExpression(expressions) => {
                let mut value = 0;
                for exp in expressions.iter() {
                    value = self.interpret(exp);
                }
                value
            }
            Expression::WhileExpression { condition, body } => {
                loop {
                    if self.interpret(condition) != 0 {
                        self.interpret(body);
                    } else {
                        break;
                    }
                }
                1
            }
            Expression::IfExpression {
                condition,
                then_clause,
                else_clause,
            } => {
                if int_to_bool(self.interpret(condition)) {
                    self.interpret(then_clause)
                } else {
                    self.interpret(else_clause)
                }
            }
            Expression::BinaryExpression { operator, lhs, rhs } => match operator {
                crate::ast::Operator::Add => self.interpret(lhs) + self.interpret(rhs),
                crate::ast::Operator::Subtract => self.interpret(lhs) - self.interpret(rhs),
                crate::ast::Operator::Multiply => self.interpret(lhs) * self.interpret(rhs),
                crate::ast::Operator::Divide => self.interpret(lhs) / self.interpret(rhs),
                crate::ast::Operator::LessThan => {
                    bool_to_int(self.interpret(lhs) < self.interpret(rhs))
                }
                crate::ast::Operator::LessOrEqual => {
                    bool_to_int(self.interpret(lhs) <= self.interpret(rhs))
                }
                crate::ast::Operator::GreaterThan => {
                    bool_to_int(self.interpret(lhs) > self.interpret(rhs))
                }
                crate::ast::Operator::GreaterOrEqual => {
                    bool_to_int(self.interpret(lhs) >= self.interpret(rhs))
                }
                crate::ast::Operator::EqualEqual => {
                    bool_to_int(self.interpret(lhs) == self.interpret(rhs))
                }
                crate::ast::Operator::NotEqual => {
                    bool_to_int(self.interpret(lhs) != self.interpret(rhs))
                }
            },
            Expression::Assignment { name, expression } => {
                let value = self.interpret(expression);
                self.environment.insert(name.clone(), value);
                value
            }
            Expression::Identifier(name) => self
                .environment
                .find_binding(name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .get(name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .to_owned(),
            Expression::IntegerLiteral(value) => value.to_owned(),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

fn bool_to_int(b: bool) -> i32 {
    if b {
        1
    } else {
        0
    }
}

fn int_to_bool(i: i32) -> bool {
    match i {
        1 => true,
        0 => false,
        _ => panic!("{} is not used as condition", i),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_10_plus_20_should_be_30() {
        let mut interpreter = Interpreter::default();
        assert_eq!(interpreter.interpret(&add(integer(10), integer(20))), 30);
    }

    #[test]
    fn 真を評価すると1になる() {
        let mut interpreter = Interpreter::default();

        assert_eq!(interpreter.interpret(&lt(integer(1), integer(2))), 1);
        assert_eq!(interpreter.interpret(&le(integer(1), integer(2))), 1);
        assert_eq!(interpreter.interpret(&le(integer(1), integer(1))), 1);

        assert_eq!(interpreter.interpret(&gt(integer(2), integer(1))), 1);
        assert_eq!(interpreter.interpret(&ge(integer(2), integer(1))), 1);
        assert_eq!(interpreter.interpret(&ge(integer(1), integer(1))), 1);

        assert_eq!(interpreter.interpret(&eq(integer(1), integer(1))), 1);
        assert_eq!(interpreter.interpret(&neq(integer(1), integer(2))), 1);
    }

    #[test]
    fn 偽を評価すると0になる() {
        let mut interpreter = Interpreter::default();

        assert_eq!(interpreter.interpret(&lt(integer(2), integer(1))), 0);
        assert_eq!(interpreter.interpret(&lt(integer(1), integer(1))), 0);
        assert_eq!(interpreter.interpret(&le(integer(2), integer(1))), 0);

        assert_eq!(interpreter.interpret(&gt(integer(1), integer(2))), 0);
        assert_eq!(interpreter.interpret(&gt(integer(1), integer(1))), 0);
        assert_eq!(interpreter.interpret(&ge(integer(1), integer(2))), 0);

        assert_eq!(interpreter.interpret(&eq(integer(1), integer(2))), 0);
        assert_eq!(interpreter.interpret(&neq(integer(1), integer(1))), 0);
    }

    #[test]
    fn 定義した変数を参照することができる() {
        let mut interpreter = Interpreter::default();
        interpreter.interpret(&assign("x".into(), integer(10)));
        assert_eq!(
            interpreter.interpret(&add(identifier("x".into()), integer(20))),
            30
        );
    }

    #[test]
    #[should_panic(expected = "Identifier x is not defined")]
    fn 未定義の変数を参照しようとするとpanicする() {
        let mut interpreter = Interpreter::default();
        interpreter.interpret(&add(identifier("x".into()), integer(20)));
    }

    mod if_expression {
        use super::super::*;
        use crate::ast::*;

        #[test]
        fn conditionが真であればthen節が評価される() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.interpret(&if_(
                    eq(subtract(integer(5), integer(3)), integer(2)),
                    integer(10),
                    integer(20)
                )),
                10
            );
        }

        #[test]
        fn conditionが偽であればelse節が評価される() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.interpret(&if_(
                    eq(gt(integer(5), integer(3)), integer(100)),
                    integer(10),
                    integer(20)
                )),
                20
            );
        }
    }

    mod while_expression {
        use super::super::*;
        use crate::ast::*;

        #[test]
        fn conditionが真の間bodyの評価を繰り返す() {
            let mut interpreter = Interpreter::default();
            interpreter.interpret(&block(vec![
                assign("x".into(), integer(1)),
                assign("i".into(), integer(0)),
                while_(
                    lt(identifier("i".into()), integer(5)),
                    block(vec![
                        assign("x".into(), multiply(identifier("x".into()), integer(2))),
                        assign("i".into(), add(identifier("i".into()), integer(1))),
                    ]),
                ),
            ]));
            assert_eq!(interpreter.interpret(&identifier("x".into())), 32);
            assert_eq!(interpreter.interpret(&identifier("i".into())), 5);
        }

        #[test]
        fn while自体の評価値は1() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.interpret(&block(vec![
                    assign("i".into(), integer(0)),
                    while_(
                        lt(identifier("i".into()), integer(5)),
                        block(vec![
                            assign("i".into(), add(identifier("i".into()), integer(1))),
                            identifier("i".into()),
                        ]),
                    ),
                ])),
                1
            );
        }
    }

    mod body {
        use super::super::*;
        use crate::ast::*;

        #[test]
        fn 中身が空のときの評価値は0() {
            let mut interpreter = Interpreter::default();
            assert_eq!(interpreter.interpret(&block(Vec::new())), 0);
        }
    }
}
