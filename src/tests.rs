use crate::interpreter::Interpreter;
use crate::parser::parse;

#[test]
fn コードをパースして評価できる() {
    assert_eq!(
        Interpreter::default().call_main(
            parse(
                "
global g = 1000

define main() {
    x = 1 + 2;
    y = foo(x, 10);
    z = foo[a = 20, b = 30];

    g / (x * y - z);
}

define foo(a, b) {
    (a + 100) * b;
}
    "
            )
            .unwrap()
        ),
        -1
    );
}
