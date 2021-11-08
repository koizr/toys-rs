use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1, one_of},
    combinator::{map, map_res, recognize},
    multi::{fold_many0, many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use crate::ast;

/// program <- topLevelDefinition*;
pub fn program(input: &str) -> IResult<&str, ast::Program> {
    map(
        many0(delimited(multispace0, top_level_definition, multispace0)),
        ast::program,
    )(input)
}

/// topLevelDefinition <- globalVariableDefinition / functionDefinition;
pub fn top_level_definition(input: &str) -> IResult<&str, ast::TopLevel> {
    alt((global_variable_definition, function_definition))(input)
}

/// functionDefinition <- "define" identifier "(" (identifier ("," identifier)*)? ")" blockExpression;
pub fn function_definition(input: &str) -> IResult<&str, ast::TopLevel> {
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
pub fn global_variable_definition(input: &str) -> IResult<&str, ast::TopLevel> {
    let (input, _) = tag("global")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expression) = expression(input)?;
    Ok((input, ast::global_variable(name.into(), expression)))
}

/// line <- println / whileExpression / ifExpression / assignment / expressionLine / blockExpression;
pub fn line(input: &str) -> IResult<&str, ast::Expression> {
    alt((println_, while_, if_, assignment, expression_line, block))(input)
}

/// println <- "println" "(" expression ")";
pub fn println_(input: &str) -> IResult<&str, ast::Expression> {
    map(
        preceded(tag("println"), delimited(tag("("), expression, tag(")"))),
        ast::println_,
    )(input)
}

/// ifExpression <- "if" "(" expression ")" line "else" line;
///
/// 本当はこっち ↓
/// ifExpression <- "if" "(" expression ")" line ("else" line)?;
pub fn if_(input: &str) -> IResult<&str, ast::Expression> {
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
pub fn while_(input: &str) -> IResult<&str, ast::Expression> {
    let (input, _) = tag("while")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, condition) = delimited(tag("("), expression, tag(")"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = line(input)?;
    Ok((input, ast::while_(condition, body)))
}

/// blockExpression <- "{" line* "}";
pub fn block(input: &str) -> IResult<&str, ast::Expression> {
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
pub fn assignment(input: &str) -> IResult<&str, ast::Expression> {
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
pub fn expression_line(input: &str) -> IResult<&str, ast::Expression> {
    map(terminated(expression, tag(";")), |exp| exp)(input)
}

/// 式
///
/// expression <- comparative;
pub fn expression(input: &str) -> IResult<&str, ast::Expression> {
    let (input, _) = multispace0(input)?;
    let (input, exp) = comparative(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, exp))
}

/// カンマ区切りの式リスト
///
/// expressionList <- ( expression, ("," expression)* )?;
pub fn expression_list(input: &str) -> IResult<&str, Vec<ast::Expression>> {
    separated_list0(delimited(multispace0, tag(","), multispace0), expression)(input)
}

/// 比較演算と同程度の優先度の演算
///
/// comparative <- additive (("<" / "<=" / ">" / ">=", "==") additive)*;
pub fn comparative(input: &str) -> IResult<&str, ast::Expression> {
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
pub fn additive(input: &str) -> IResult<&str, ast::Expression> {
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
pub fn multitive(input: &str) -> IResult<&str, ast::Expression> {
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
pub fn primary(input: &str) -> IResult<&str, ast::Expression> {
    alt((
        delimited(
            tag("("),
            delimited(multispace0, expression, multispace0),
            tag(")"),
        ),
        integer,
        function_call,
        identifier_expression,
    ))(input)
}

/// 整数値
pub fn integer(input: &str) -> IResult<&str, ast::Expression> {
    map_res(
        pair(one_of("123456789"), many0(one_of("0123456789"))),
        |(h, mut t)| {
            let mut ns = vec![h];
            ns.append(&mut t);
            ns.iter()
                .collect::<String>()
                .parse::<i32>()
                .map(ast::integer)
        },
    )(input)
}

/// functionCall <- identifier "("
///     (expression ("," expression)*)?
/// ")";
pub fn function_call(input: &str) -> IResult<&str, ast::Expression> {
    let (input, name) = identifier(input)?;
    let (input, args) = delimited(tag("("), expression_list, tag(")"))(input)?;
    Ok((input, ast::call(name.into(), args)))
}

/// identifier <- IDENT;
pub fn identifier_expression(input: &str) -> IResult<&str, ast::Expression> {
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

pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
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
    fn 整数値をパースできる() {
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
