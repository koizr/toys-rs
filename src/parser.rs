use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, multispace1},
    combinator::{map, map_res, recognize},
    multi::{fold_many0, many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult,
};

use crate::ast;
use crate::error::ToysError;

/// プログラムをパースして AST を返す
pub fn parse(input: &str) -> Result<ast::Program, ToysError> {
    match program(input).finish() {
        Ok((remain, program_ast)) => {
            if !remain.is_empty() {
                dbg!(program_ast);
                Err(ToysError::ParseError(format!(
                    "contains invalid characters: {}",
                    remain
                )))
            } else {
                Ok(program_ast)
            }
        }
        Err(err) => Err(ToysError::ParseError(format!("{:?}", err))),
    }
}

/// program <- topLevelDefinition*;
fn program(input: &str) -> IResult<&str, ast::Program> {
    map(
        many0(delimited(multispace0, top_level_definition, multispace0)),
        ast::program,
    )(input)
}

/// topLevelDefinition <- globalVariableDefinition / functionDefinition;
fn top_level_definition(input: &str) -> IResult<&str, ast::TopLevel> {
    alt((global_variable_definition, function_definition))(input)
}

/// functionDefinition <- "define" identifier "(" (identifier ("," identifier)*)? ")" blockExpression;
fn function_definition(input: &str) -> IResult<&str, ast::TopLevel> {
    let (input, _) = tag("define")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    let (input, args) = delimited(
        tag("("),
        delimited(multispace0, identifier_list, multispace0),
        tag(")"),
    )(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = block(input)?;
    Ok((
        input,
        ast::function(
            name.into(),
            args.into_iter().map(String::from).collect(),
            body,
        ),
    ))
}

/// globalVariableDefinition <- "global" identifier "=" expression;
fn global_variable_definition(input: &str) -> IResult<&str, ast::TopLevel> {
    let (input, _) = tag("global")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expression) = expression(input)?;
    Ok((input, ast::global_variable(name.into(), expression)))
}

/// line <- println ";" / forInExpression / whileExpression / ifExpression / assignment / expressionLine / blockExpression;
fn line(input: &str) -> IResult<&str, ast::Expression> {
    alt((
        terminated(println_, tag(";")),
        for_in,
        while_,
        if_,
        assignment,
        expression_line,
        block,
    ))(input)
}

/// println <- "println" "(" expression ")";
fn println_(input: &str) -> IResult<&str, ast::Expression> {
    map(
        preceded(
            tag("println"),
            delimited(
                tag("("),
                delimited(multispace0, expression, multispace0),
                tag(")"),
            ),
        ),
        ast::println_,
    )(input)
}

/// ifExpression <- "if" "(" expression ")" line "else" line;
///
/// 本当はこっち ↓
/// ifExpression <- "if" "(" expression ")" line ("else" line)?;
fn if_(input: &str) -> IResult<&str, ast::Expression> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, condition) = delimited(tag("("), expression, tag(")"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, then_clause) = line(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("else")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, else_clause) = line(input)?;
    Ok((input, ast::if_(condition, then_clause, else_clause)))
}

/// whileExpression <- "while" "(" expression ")" line;
fn while_(input: &str) -> IResult<&str, ast::Expression> {
    let (input, _) = tag("while")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, condition) = delimited(tag("("), expression, tag(")"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = line(input)?;
    Ok((input, ast::while_(condition, body)))
}

/// forInExpression <- "for" "(" identifier "in" integer "to" integer ")" line;
fn for_in(input: &str) -> IResult<&str, ast::Expression> {
    let (input, _) = pair(tag("for"), multispace0)(input)?;
    let (input, (i, from, to)) = delimited(
        tag("("),
        |input| {
            let (input, _) = multispace0(input)?;
            let (input, i) = identifier(input)?;
            let (input, _) = delimited(multispace1, tag("in"), multispace1)(input)?;
            let (input, from) = integer(input)?;
            let (input, _) = delimited(multispace1, tag("to"), multispace1)(input)?;
            let (input, to) = integer(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, (i, from, to)))
        },
        tag(")"),
    )(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = line(input)?;

    Ok((
        input,
        ast::block(vec![
            ast::assign(i.into(), from),
            ast::while_(
                ast::lt(ast::identifier(i.into()), to),
                ast::block(vec![
                    body,
                    ast::assign(
                        i.into(),
                        ast::add(ast::identifier(i.into()), ast::integer(1)),
                    ),
                ]),
            ),
        ]),
    ))
}

/// blockExpression <- "{" line* "}";
fn block(input: &str) -> IResult<&str, ast::Expression> {
    map(
        delimited(
            tag("{"),
            delimited(
                multispace0,
                many0(delimited(multispace0, line, multispace0)),
                multispace0,
            ),
            tag("}"),
        ),
        ast::block,
    )(input)
}

/// 代入式
///
/// assignment <- identifier "=" expression ";";
fn assignment(input: &str) -> IResult<&str, ast::Expression> {
    let (input, name) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expression) = terminated(expression, tag(";"))(input)?;
    Ok((input, ast::assign(name.into(), expression)))
}

/// 式行
///
/// expressionLine <- expression ";";
fn expression_line(input: &str) -> IResult<&str, ast::Expression> {
    map(terminated(expression, tag(";")), |exp| exp)(input)
}

/// 式
///
/// expression <- comparative;
fn expression(input: &str) -> IResult<&str, ast::Expression> {
    let (input, _) = multispace0(input)?;
    let (input, exp) = comparative(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, exp))
}

/// カンマ区切りの式リスト
///
/// expressionList <- ( expression, ("," expression)* )?;
fn expression_list(input: &str) -> IResult<&str, Vec<ast::Expression>> {
    separated_list0(delimited(multispace0, tag(","), multispace0), expression)(input)
}

/// 比較演算と同程度の優先度の演算
///
/// comparative <- additive (("<" / "<=" / ">" / ">=", "==") additive)*;
fn comparative(input: &str) -> IResult<&str, ast::Expression> {
    let lt_op = map(tag("<"), |_| ast::Operator::LessThan);
    let le_op = map(tag("<="), |_| ast::Operator::LessOrEqual);

    let gt_op = map(tag(">"), |_| ast::Operator::GreaterThan);
    let ge_op = map(tag(">="), |_| ast::Operator::GreaterOrEqual);

    let eq_op = map(tag("=="), |_| ast::Operator::EqualEqual);
    let ne_op = map(tag("!="), |_| ast::Operator::NotEqual);

    let (input, head) = additive(input)?;
    fold_many0(
        pair(
            delimited(
                multispace0,
                // 先に <= を読み取るようにしておかないと、 < がマッチしてそのあとの = が無視されるので注意
                alt((le_op, lt_op, ge_op, gt_op, eq_op, ne_op)),
                multispace0,
            ),
            additive,
        ),
        move || head.clone(),
        |lhs, (op, rhs)| ast::binary_expression(lhs, op, rhs),
    )(input)
}

/// 加算と同程度の優先度の演算
///
/// additive <- multitive (("+" / "-") multitive)*;
fn additive(input: &str) -> IResult<&str, ast::Expression> {
    let add_operator = map(tag("+"), |_| ast::Operator::Add);
    let subtract_operator = map(tag("-"), |_| ast::Operator::Subtract);

    let (input, head) = multitive(input)?;
    fold_many0(
        pair(
            delimited(
                multispace0,
                alt((add_operator, subtract_operator)),
                multispace0,
            ),
            multitive,
        ),
        move || head.clone(),
        |lhs, (op, rhs)| ast::binary_expression(lhs, op, rhs),
    )(input)
}

/// 乗算と同程度の優先度の演算
///
/// multitive <- primary (("*" / "/") primary)*;
fn multitive(input: &str) -> IResult<&str, ast::Expression> {
    let multiply_operator = map(tag("*"), |_| ast::Operator::Multiply);
    let divide_operator = map(tag("/"), |_| ast::Operator::Divide);

    let (input, head) = primary(input)?;
    fold_many0(
        pair(
            delimited(
                multispace0,
                alt((multiply_operator, divide_operator)),
                multispace0,
            ),
            primary,
        ),
        move || head.clone(),
        |lhs, (op, rhs)| ast::binary_expression(lhs, op, rhs),
    )(input)
}

/// 最優先の演算
///
/// primary <- "(" expression ")" / integer / functionCall / identifier;
fn primary(input: &str) -> IResult<&str, ast::Expression> {
    alt((
        delimited(
            tag("("),
            delimited(multispace0, expression, multispace0),
            tag(")"),
        ),
        integer,
        function_call,
        labeled_function_call,
        identifier_expression,
    ))(input)
}

/// 整数値
fn integer(input: &str) -> IResult<&str, ast::Expression> {
    map_res(digit1, |n: &str| n.parse().map(ast::integer))(input)
}

/// functionCall <- identifier "("
///     (expression ("," expression)*)?
/// ")";
fn function_call(input: &str) -> IResult<&str, ast::Expression> {
    let (input, name) = identifier(input)?;
    let (input, args) = delimited(tag("("), expression_list, tag(")"))(input)?;
    Ok((input, ast::call(name.into(), args)))
}

/// labeledFunctionCall <- identifier "["
///     (identifier "=" expression ("," identifier "=" expression)*)?
/// "]";
fn labeled_function_call(input: &str) -> IResult<&str, ast::Expression> {
    let (input, name) = identifier(input)?;
    let (input, args) = delimited(
        tag("["),
        delimited(multispace0, labeled_parameters, multispace0),
        tag("]"),
    )(input)?;
    Ok((input, ast::labeled_call(name.into(), args)))
}

fn labeled_parameters(input: &str) -> IResult<&str, Vec<(String, ast::Expression)>> {
    separated_list0(delimited(multispace0, tag(","), multispace0), |input| {
        let (input, name) = identifier(input)?;
        let (input, _) = delimited(multispace0, tag("="), multispace0)(input)?;
        let (input, expression) = expression(input)?;
        Ok((input, (name.into(), expression)))
    })(input)
}

/// identifier <- IDENT;
fn identifier_expression(input: &str) -> IResult<&str, ast::Expression> {
    map(identifier, |name| ast::identifier(name.into()))(input)
}

fn identifier_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(delimited(multispace0, tag(","), multispace0), identifier)(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn 階乗を計算するプログラムをパースする() {
        let code = r"
            define factorial(n) {
                if (n < 2) {
                    1;
                } else {
                    n * factorial(n - 1);
                }
            }

            define main() {
                factorial(5);
            }
        ";

        let expected_ast = ast::program(vec![
            ast::function(
                "factorial".into(),
                vec!["n".into()],
                ast::block(vec![ast::if_(
                    ast::lt(ast::identifier("n".into()), ast::integer(2)),
                    ast::block(vec![ast::integer(1)]),
                    ast::block(vec![ast::multiply(
                        ast::identifier("n".into()),
                        ast::call(
                            "factorial".into(),
                            vec![ast::subtract(ast::identifier("n".into()), ast::integer(1))],
                        ),
                    )]),
                )]),
            ),
            ast::function(
                "main".into(),
                vec![],
                ast::block(vec![ast::call("factorial".into(), vec![ast::integer(5)])]),
            ),
        ]);

        assert_eq!(program(code), Ok(("", expected_ast)));
    }

    #[test]
    fn グローバル変数宣言をパースできる() {
        assert_eq!(
            global_variable_definition("global g = 42"),
            Ok(("", ast::global_variable("g".into(), ast::integer(42))))
        );
    }

    #[test]
    fn 関数定義をパースできる() {
        let code = "define add(a, b) {
                a + b;
            }";

        let expected_ast = ast::function(
            "add".into(),
            vec!["a".into(), "b".into()],
            ast::block(vec![ast::add(
                ast::identifier("a".into()),
                ast::identifier("b".into()),
            )]),
        );

        assert_eq!(function_definition(code), Ok(("", expected_ast)));
    }

    #[test]
    fn printlnをパースできる() {
        assert_eq!(
            println_("println(5 / 7)"),
            Ok((
                "",
                ast::println_(ast::divide(ast::integer(5), ast::integer(7)))
            ))
        );
    }

    #[test]
    fn if式をパースできる() {
        assert_eq!(
            if_("if (condition) 1 + 2; else 3 - 4;"),
            Ok((
                "",
                ast::if_(
                    ast::identifier("condition".into()),
                    ast::add(ast::integer(1), ast::integer(2)),
                    ast::subtract(ast::integer(3), ast::integer(4))
                )
            ))
        );
        assert_eq!(
            if_("if (condition) { 1 + 2; } else { 3 - 4; }"),
            Ok((
                "",
                ast::if_(
                    ast::identifier("condition".into()),
                    ast::block(vec![ast::add(ast::integer(1), ast::integer(2))]),
                    ast::block(vec![ast::subtract(ast::integer(3), ast::integer(4))])
                )
            ))
        );
    }

    #[test]
    fn else_ifをパースできる() {
        let code = "if (condition) {
                1 + 2;
            } else if (flag) {
                3 - 4;
            } else {
                5 * 6;
            }";
        assert_eq!(
            if_(code),
            Ok((
                "",
                ast::if_(
                    ast::identifier("condition".into()),
                    ast::block(vec![ast::add(ast::integer(1), ast::integer(2))]),
                    ast::if_(
                        ast::identifier("flag".into()),
                        ast::block(vec![ast::subtract(ast::integer(3), ast::integer(4))]),
                        ast::block(vec![ast::multiply(ast::integer(5), ast::integer(6))])
                    )
                )
            ))
        );
    }

    #[test]
    fn for_in式をパースできる() {
        assert_eq!(
            for_in("for (i in 0 to 10) { println(i + 10); }"),
            Ok((
                "",
                ast::block(vec![
                    ast::assign("i".into(), ast::integer(0)),
                    ast::while_(
                        ast::lt(ast::identifier("i".into()), ast::integer(10)),
                        ast::block(vec![
                            ast::block(vec![ast::println_(ast::add(
                                ast::identifier("i".into()),
                                ast::integer(10)
                            ))]),
                            ast::assign(
                                "i".into(),
                                ast::add(ast::identifier("i".into()), ast::integer(1))
                            )
                        ])
                    )
                ])
            ))
        )
    }

    #[test]
    fn while式をパースできる() {
        assert_eq!(
            while_("while (value < 100) { value = value + 1; }"),
            Ok((
                "",
                ast::while_(
                    ast::lt(ast::identifier("value".into()), ast::integer(100)),
                    ast::block(vec![ast::assign(
                        "value".into(),
                        ast::add(ast::identifier("value".into()), ast::integer(1))
                    )])
                )
            ))
        );
    }

    #[test]
    fn block式をパースできる() {
        assert_eq!(
            block("{ value = 1; println(value); println(value + 1); }"),
            Ok((
                "",
                ast::block(vec![
                    ast::assign("value".into(), ast::integer(1),),
                    ast::println_(ast::identifier("value".into())),
                    ast::println_(ast::add(ast::identifier("value".into()), ast::integer(1)))
                ])
            ))
        );
    }

    #[test]
    fn 代入式をパースできる() {
        assert_eq!(
            assignment("a = 20;"),
            Ok(("", ast::assign("a".into(), ast::integer(20))))
        );
    }

    #[test]
    fn 整数値をパースできる() {
        assert_eq!(integer("0").unwrap(), ("", ast::integer(0)));
        assert_eq!(integer("1234").unwrap(), ("", ast::integer(1234)));
        assert_eq!(integer("1234foo").unwrap(), ("foo", ast::integer(1234)));
    }

    #[test]
    fn identifierを読み取れる() {
        assert_eq!(
            identifier_expression("foo").unwrap(),
            ("", ast::Expression::Identifier("foo".into()))
        )
    }

    #[test]
    fn identifierを文字列として読み取れる() {
        assert_eq!(identifier("foo").unwrap(), ("", "foo".into()))
    }

    #[test]
    fn 比較演算子() {
        assert_eq!(
            comparative("1 < 2"),
            Ok(("", ast::lt(ast::integer(1), ast::integer(2))))
        );
        assert_eq!(
            comparative("1 <= 2"),
            Ok(("", ast::le(ast::integer(1), ast::integer(2))))
        );
        assert_eq!(
            comparative("1 > 2"),
            Ok(("", ast::gt(ast::integer(1), ast::integer(2))))
        );
        assert_eq!(
            comparative("1 >= 2"),
            Ok(("", ast::ge(ast::integer(1), ast::integer(2))))
        );
        assert_eq!(
            comparative("1 == 2"),
            Ok(("", ast::eq(ast::integer(1), ast::integer(2))))
        );
        assert_eq!(
            comparative("1 != 2"),
            Ok(("", ast::neq(ast::integer(1), ast::integer(2))))
        );
    }

    #[test]
    fn 四則演算の組み合わせを読み取れる() {
        assert_eq!(
            additive("(1 + 2 * (15 - 5)) / 7"),
            Ok((
                "",
                ast::divide(
                    ast::add(
                        ast::integer(1),
                        ast::multiply(
                            ast::integer(2),
                            ast::subtract(ast::integer(15), ast::integer(5))
                        )
                    ),
                    ast::integer(7)
                )
            ))
        );
    }

    #[test]
    fn 乗算を読み取れる() {
        assert_eq!(
            multitive("1 * 2"),
            Ok(("", ast::multiply(ast::integer(1), ast::integer(2))))
        );
    }

    #[test]
    fn 除算を読み取れる() {
        assert_eq!(
            multitive("1 / 2"),
            Ok(("", ast::divide(ast::integer(1), ast::integer(2))))
        );
    }

    #[test]
    fn 加算を読み取れる() {
        assert_eq!(
            additive("1 + 2"),
            Ok(("", ast::add(ast::integer(1), ast::integer(2))))
        );
    }

    #[test]
    fn 減算を読み取れる() {
        assert_eq!(
            additive("1 - 2"),
            Ok(("", ast::subtract(ast::integer(1), ast::integer(2))))
        );
    }

    #[test]
    fn カッコでくくった式を読み取れる() {
        assert_eq!(primary("(1)"), Ok(("", ast::integer(1))));
        assert_eq!(
            primary("(   1 * 10 )"),
            Ok(("", ast::multiply(ast::integer(1), ast::integer(10))))
        );
        assert_eq!(
            primary("(1 + 10)"),
            Ok(("", ast::add(ast::integer(1), ast::integer(10))))
        );
    }

    #[test]
    fn 関数呼び出しを読み取れる() {
        assert_eq!(
            function_call("func()"),
            Ok(("", ast::call("func".into(), vec![])))
        );
        assert_eq!(
            function_call("func(a)"),
            Ok((
                "",
                ast::call("func".into(), vec![ast::identifier("a".into())])
            ))
        );
        assert_eq!(
            function_call("func(a, b)"),
            Ok((
                "",
                ast::call(
                    "func".into(),
                    vec![ast::identifier("a".into()), ast::identifier("b".into())]
                )
            ))
        );
    }

    #[test]
    fn ラベル引数を使った関数呼び出しをパースできる() {
        assert_eq!(
            labeled_function_call("func[a = 1]"),
            Ok((
                "",
                ast::labeled_call("func".into(), vec![("a".into(), ast::integer(1))])
            ))
        );
        assert_eq!(
            labeled_function_call("func[a = 1, b = 2]"),
            Ok((
                "",
                ast::labeled_call(
                    "func".into(),
                    vec![("a".into(), ast::integer(1)), ("b".into(), ast::integer(2))]
                )
            ))
        );
        assert_eq!(
            labeled_function_call("func[a = 1 + 2, b = func_x[c = 8]]"),
            Ok((
                "",
                ast::labeled_call(
                    "func".into(),
                    vec![
                        ("a".into(), ast::add(ast::integer(1), ast::integer(2))),
                        (
                            "b".into(),
                            ast::labeled_call("func_x".into(), vec![("c".into(), ast::integer(8))])
                        ),
                    ]
                )
            ))
        );
    }

    #[test]
    fn identifierのリストを読み取れる() {
        assert_eq!(identifier_list(""), Ok(("", vec![])));
        assert_eq!(identifier_list("foo"), Ok(("", vec!["foo"])));
        assert_eq!(
            identifier_list("foo,bar,baz"),
            Ok(("", vec!["foo", "bar", "baz"]))
        );
        assert_eq!(
            identifier_list("foo, bar ,baz"),
            Ok(("", vec!["foo", "bar", "baz"]))
        );
    }
}
