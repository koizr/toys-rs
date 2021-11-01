use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{alpha1, alphanumeric1, one_of, space0},
        streaming::char,
    },
    combinator::{map, map_res, recognize},
    multi::{fold_many1, many0, separated_list0},
    sequence::{delimited, pair, terminated},
    IResult,
};

use crate::ast;

pub fn program(_input: &str) -> IResult<&str, ast::Program> {
    todo!()
}

pub fn top_level_definition(_input: &str) -> IResult<&str, ast::TopLevel> {
    todo!()
}

/// 代入式
///
/// assignment <- identifier "=" expression ";";
pub fn assignment(input: &str) -> IResult<&str, ast::Expression> {
    map(
        pair(
            identifier,
            pair(char('='), terminated(expression, char(';'))),
        ),
        |(name, (_, value))| ast::assign(name.into(), value),
    )(input)
}

/// 式行
///
/// expressionLine <- expression ";";
pub fn expression_line(input: &str) -> IResult<&str, ast::Expression> {
    map(terminated(expression, char(';')), |exp| exp)(input)
}

/// 式
///
/// expression <- comparative;
pub fn expression(input: &str) -> IResult<&str, ast::Expression> {
    comparative(input)
}

/// カンマ区切りの式リスト
///
/// expressionList <- ( expression, ("," expression)* )?;
pub fn expression_list(input: &str) -> IResult<&str, Vec<ast::Expression>> {
    separated_list0(pair(char(','), space0), expression)(input)
}

/// 比較演算と同程度の優先度の演算
///
/// comparative <- additive (("<" / "<=" / ">" / ">=", "==") additive)*;
pub fn comparative(input: &str) -> IResult<&str, ast::Expression> {
    let lt_op = map(char('<'), |_| ast::Operator::LessThan);
    let le_op = map(tag("<="), |_| ast::Operator::LessOrEqual);

    let gt_op = map(char('>'), |_| ast::Operator::GreaterThan);
    let ge_op = map(tag(">="), |_| ast::Operator::GreaterOrEqual);

    let eq_op = map(tag("=="), |_| ast::Operator::EqualEqual);

    let (input, head) = additive(input)?;
    fold_many1(
        pair(alt((lt_op, le_op, gt_op, ge_op, eq_op)), additive),
        move || head.clone(),
        |lhs, (op, rhs)| ast::binary_expression(lhs, op, rhs),
    )(input)
}

/// 加算と同程度の優先度の演算
///
/// additive <- multitive (("+" / "-") multitive)*;
pub fn additive(input: &str) -> IResult<&str, ast::Expression> {
    let add_operator = map(char('+'), |_| ast::Operator::Add);
    let subtract_operator = map(char('-'), |_| ast::Operator::Subtract);

    let (input, head) = multitive(input)?;
    fold_many1(
        pair(alt((add_operator, subtract_operator)), multitive),
        move || head.clone(),
        |lhs, (op, rhs)| ast::binary_expression(lhs, op, rhs),
    )(input)
}

/// 乗算と同程度の優先度の演算
///
/// multitive <- primary (("*" / "/") primary)*;
pub fn multitive(input: &str) -> IResult<&str, ast::Expression> {
    let multiply_operator = map(char('*'), |_| ast::Operator::Multiply);
    let divide_operator = map(char('/'), |_| ast::Operator::Divide);

    let (input, head) = primary(input)?;
    fold_many1(
        pair(alt((multiply_operator, divide_operator)), primary),
        move || head.clone(),
        |lhs, (op, rhs)| ast::binary_expression(lhs, op, rhs),
    )(input)
}

/// 最優先の演算
///
/// primary <- "(" expression ")" / integer / functionCall / identifier;
pub fn primary(input: &str) -> IResult<&str, ast::Expression> {
    alt((
        delimited(char('('), expression, char(')')),
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
    map(
        pair(identifier, delimited(char('('), expression_list, char(')'))),
        |(name, args)| ast::call(name.into(), args),
    )(input)
}

/// identifier <- IDENT;
pub fn identifier_expression(input: &str) -> IResult<&str, ast::Expression> {
    map(identifier, |name| ast::identifier(name.into()))(input)
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
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
}
