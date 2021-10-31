#[allow(unused_imports)]
use pest::Parser;

// TODO: Pest でも実装してみる
#[derive(Parser)]
#[grammar = "toys.pest"]
pub struct ToysParser;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let result = ToysParser::parse(Rule::field, "123.45").unwrap().as_str();
        assert_eq!(result, "123.45");
    }
}
