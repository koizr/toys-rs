use std::{collections::HashMap, panic};

use crate::ast::{Expression, FunctionDefinition, LabeledParameter, Program, TopLevel};

type Bindings = HashMap<String, i32>;

pub struct Environment<'a> {
    bindings: Bindings,
    next: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(bindings: Bindings, next: Option<&'a Environment<'a>>) -> Self {
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

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        Self {
            bindings: Default::default(),
            next: Default::default(),
        }
    }
}

/// インタープリタ本体
pub struct Interpreter<'a> {
    variable_environment: Environment<'a>,
    function_environment: HashMap<String, FunctionDefinition>,
}

impl<'a> Interpreter<'a> {
    pub fn new(
        variable_environment: Environment<'a>,
        function_environment: HashMap<String, FunctionDefinition>,
    ) -> Self {
        Self {
            variable_environment,
            function_environment,
        }
    }

    /// 式を評価する
    pub fn interpret(&mut self, expression: &Expression) -> i32 {
        match expression {
            Expression::Block(expressions) => {
                let mut value = 0;
                for exp in expressions.iter() {
                    value = self.interpret(exp);
                }
                value
            }
            Expression::While { condition, body } => {
                loop {
                    if int_to_bool(self.interpret(condition)) {
                        self.interpret(body);
                    } else {
                        break;
                    }
                }
                1
            }
            Expression::If {
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
            Expression::Binary { operator, lhs, rhs } => match operator {
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
                self.variable_environment.insert(name.clone(), value);
                value
            }
            Expression::Identifier(name) => self
                .variable_environment
                .find_binding(name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .get(name)
                .unwrap_or_else(|| panic!("Identifier {} is not defined", name))
                .to_owned(),
            Expression::IntegerLiteral(value) => value.to_owned(),
            Expression::FunctionCall { name, args } => {
                let function = self.function_environment.get(name).cloned();
                match function {
                    Some(function) => {
                        let values = args.iter().map(|a| self.interpret(a)).collect::<Vec<_>>();
                        let mut inner_interpreter = Interpreter::new(
                            Environment::new(
                                function.args.into_iter().zip(values).collect(),
                                Some(&self.variable_environment),
                            ),
                            self.function_environment.clone(), // TODO: いい方法が思いつかなくて clone したけど本当はしないほうがいい
                        );
                        inner_interpreter.interpret(&function.body)
                    }
                    None => panic!("Function {} is not found", name),
                }
            }
            Expression::Println(expression) => {
                println!("{}", self.interpret(expression));
                0
            }
            Expression::LabeledCall { name, args } => {
                let function = self.function_environment.get(name).cloned();
                match function {
                    Some(function) => {
                        let mut inner_interpreter = Interpreter::new(
                            Environment::new(
                                args.into_iter()
                                    .map(|LabeledParameter { name, parameter }| {
                                        (name.clone(), self.interpret(parameter))
                                    })
                                    .collect(),
                                Some(&self.variable_environment),
                            ),
                            self.function_environment.clone(),
                        );
                        inner_interpreter.interpret(&function.body)
                    }
                    None => panic!("Function {} is not found", name),
                }
            }
        }
    }

    /// メイン関数の呼び出し
    pub fn call_main(&mut self, program: Program) -> i32 {
        for top_level in program.definitions {
            match top_level {
                TopLevel::GlobalVariableDefinition { name, expression } => {
                    let value = self.interpret(&expression);
                    self.variable_environment.insert(name, value);
                }
                TopLevel::FunctionDefinition(func) => {
                    self.function_environment.insert(func.name.clone(), func);
                }
            }
        }

        // main_function が self のフィールドを借用していると、 &mut self が必要な interpret を呼び出せなくなってしまうので
        // 借用せずに済むように cloned() している
        let main_function = self.function_environment.get("main").cloned();
        match main_function {
            Some(m) => self.interpret(&m.body),
            None => panic!("This program doesn't have main() function"),
        }
    }
}

impl<'a> Default for Interpreter<'a> {
    fn default() -> Self {
        Self::new(Default::default(), Default::default())
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

    mod function {
        use super::super::*;
        use crate::ast::*;

        #[test]
        fn main関数を定義して実行できる() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.call_main(Program {
                    definitions: vec![function(
                        "main".into(),
                        Vec::new(),
                        block(vec![
                            assign("x".into(), integer(5)),
                            subtract(identifier("x".into()), integer(10)),
                        ])
                    )]
                }),
                -5
            );
        }

        #[test]
        fn 関数を定義して呼び出せる() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.call_main(Program {
                    definitions: vec![
                        function(
                            "main".into(),
                            Vec::new(),
                            block(vec![
                                assign("x".into(), integer(5)),
                                call("add".into(), vec![identifier("x".into()), integer(20)]),
                            ])
                        ),
                        function(
                            "add".into(),
                            vec!["a".into(), "b".into()],
                            block(vec![
                                assign("x".into(), integer(10)),
                                add(
                                    add(identifier("a".into()), identifier("b".into())),
                                    identifier("x".into())
                                ),
                            ])
                        )
                    ]
                }),
                35
            );
        }

        #[test]
        fn 関数をラベル引数で呼び出せる() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.call_main(Program {
                    definitions: vec![
                        function(
                            "main".into(),
                            Vec::new(),
                            block(vec![
                                assign(
                                    "x".into(),
                                    labeled_call(
                                        "sub".into(),
                                        vec![("a".into(), integer(10)), ("b".into(), integer(5))]
                                    ),
                                ),
                                assign(
                                    "y".into(),
                                    labeled_call(
                                        "sub".into(),
                                        vec![("b".into(), integer(2)), ("a".into(), integer(3))]
                                    ),
                                ),
                                add(identifier("x".into()), identifier("y".into()))
                            ])
                        ),
                        function(
                            "sub".into(),
                            vec!["a".into(), "b".into()],
                            block(vec![subtract(
                                identifier("a".into()),
                                identifier("b".into())
                            )])
                        )
                    ]
                }),
                6
            );
        }
    }

    mod global_variable {
        use super::super::*;
        use crate::ast::*;

        #[test]
        fn グローバル変数を参照できる() {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.call_main(Program {
                    definitions: vec![
                        global_variable("global_x".into(), integer(100)),
                        function(
                            "main".into(),
                            Vec::new(),
                            block(vec![
                                assign("x".into(), integer(5)),
                                add(identifier("x".into()), identifier("global_x".into())),
                            ])
                        )
                    ]
                }),
                105
            )
        }

        #[test]
        fn グローバル変数と同名のローカル変数を定義するとそのスコープではローカル変数が優先される()
        {
            let mut interpreter = Interpreter::default();
            assert_eq!(
                interpreter.call_main(Program {
                    definitions: vec![
                        global_variable("x".into(), integer(100)),
                        function(
                            "main".into(),
                            Vec::new(),
                            block(vec![
                                assign("x".into(), integer(5)),
                                add(identifier("x".into()), identifier("x".into())),
                            ])
                        )
                    ]
                }),
                10
            )
        }
    }

    mod program {
        use super::super::*;
        use crate::ast::*;

        #[test]
        fn 階乗のプログラム() {
            let top_level = vec![
                // define main() {
                //    factorial(5);
                // }
                function(
                    "main".into(),
                    vec![],
                    block(vec![call("factorial".into(), vec![integer(5)])]),
                ),
                // define factorial(n) {
                //     if (n < 2) {
                //         1;
                //     } else {
                //         n * factorial(n - 1);
                //     }
                // }
                function(
                    "factorial".into(),
                    vec!["n".into()],
                    block(vec![if_(
                        lt(identifier("n".into()), integer(2)),
                        integer(1),
                        multiply(
                            identifier("n".into()),
                            call(
                                "factorial".into(),
                                vec![subtract(identifier("n".into()), integer(1))],
                            ),
                        ),
                    )]),
                ),
            ];
            let mut interpreter = Interpreter::default();
            assert_eq!(interpreter.call_main(Program::new(top_level)), 120)
        }
    }
}
