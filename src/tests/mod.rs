use value::ExceptionKind;

use super::*;

#[test]
fn fibonacci_sequence() {
    assert_eq!(eval(include_str!("fib.olang")).unwrap(), Value::Int(6765));
}

#[test]
fn loops() {
    assert_eq!(
        eval(include_str!("continue.olang")).unwrap(),
        Value::Int(35)
    );
    assert_eq!(eval(include_str!("break.olang")).unwrap(), Value::Int(55));
    assert_eq!(
        eval(include_str!("for.olang")).unwrap(),
        Value::Int(87178291200)
    );
}

#[test]
fn throw() {
    match eval("throw \"Custom error message!\"") {
        Err(EvalError::UnhandledException(exception)) => match exception.kind {
            ExceptionKind::Custom(v) => {
                assert_eq!(v, "Custom error message!".to_string())
            }
            _ => {
                panic!("exception is of wrong kind")
            }
        },
        _ => {
            panic!("throw returned wrong value")
        }
    }
}

#[test]
fn strings() {
    assert_eq!(eval("\"arst\"").unwrap(), Value::String("arst".to_string()));
    assert_eq!(
        eval("\"arst\narstarst\"").unwrap(),
        Value::String("arst\narstarst".to_string())
    );
}

#[test]
fn strings_escape_sequences() {
    assert_eq!(
        eval("\"arst\\narstarst\"").unwrap(),
        Value::String("arst\narstarst".to_string()) // newline
    );
    assert_eq!(
        eval("\"hello\\rworld\"").unwrap(),
        Value::String("hello\rworld".to_string()) // carriage return
    );
    assert_eq!(
        eval("\"tab\\tcharacter\"").unwrap(),
        Value::String("tab\tcharacter".to_string()) // tab
    );
    assert_eq!(
        eval("\"backspace\\btest\"").unwrap(),
        Value::String("backspace\x08test".to_string()) // backspace
    );
    assert_eq!(
        eval("\"formfeed\\fpage\"").unwrap(),
        Value::String("formfeed\x0cpage".to_string()) // form feed
    );
    assert_eq!(
        eval("\"vertical\\vtab\"").unwrap(),
        Value::String("vertical\x0btab".to_string()) // vertical tab
    );
    assert_eq!(
        eval("\"backslash\\\\\"").unwrap(),
        Value::String("backslash\\".to_string()) // backslash
    );
    assert_eq!(
        eval("\"single quote\\'\"").unwrap(),
        Value::String("single quote'".to_string()) // single quote
    );
    assert_eq!(
        eval("\"double quote\\\"\"").unwrap(),
        Value::String("double quote\"".to_string()) // double quote
    );
    assert_eq!(
        eval("\"null character\\0\"").unwrap(),
        Value::String("null character\x00".to_string()) // null character
    );

    assert_eq!(
        eval("\"nonexsistant\\e escape sequence\"").unwrap_err(),
        EvalError::Lexer(lexer::LexerError::UnexpectedEscapeSequence {
            location: location::Location { row: 1, col: 15 },
            char: 'e'
        })
    );
}

#[test]
fn comments() {
    assert_eq!(
        eval(include_str!("block-comments.olang")).unwrap(),
        Value::Int(5)
    );
    assert_eq!(eval(include_str!("comments.olang")).unwrap(), Value::Int(4));
}

#[test]
fn int() {
    assert_eq!(eval("1").unwrap(), Value::Int(1));
    assert_eq!(eval("1234").unwrap(), Value::Int(1234));
    // assert_eq!(eval("-1234").unwrap(), Value::Int(-1234));
}

#[test]
fn pemdas() {
    assert_eq!(eval("6/2*(1+2)").unwrap(), Value::Int(9));
    assert_eq!(eval("3+5*2").unwrap(), Value::Int(13));
    assert_eq!(eval("(3+5)*2").unwrap(), Value::Int(16));
    assert_eq!(eval("8/4*2").unwrap(), Value::Int(4));
    assert_eq!(eval("10-2+3").unwrap(), Value::Int(11));
    assert_eq!(eval("((1+2)*(3+4))").unwrap(), Value::Int(21));
    assert_eq!(eval("((5-2)+(3*4))").unwrap(), Value::Int(15));
    assert_eq!(eval("((2+3)*(4-(1+1)))").unwrap(), Value::Int(10));
}

#[test]
fn exponents() {
    assert_eq!(eval("2**3").unwrap(), Value::Int(8));
    assert_eq!(eval("3+2**2").unwrap(), Value::Int(7));
    assert_eq!(eval("5*2**3").unwrap(), Value::Int(40));
    assert_eq!(eval("(2+3)**2").unwrap(), Value::Int(25));
    assert_eq!(eval("10-2**3+1").unwrap(), Value::Int(3));
    assert_eq!(eval("((2+3)**2-4)/3").unwrap(), Value::Int(7));

    // NOTE: is it actually fine that these simple exponents are creating errors? python3 handles them fine
    assert_eq!(
        eval("2**(0-2)").unwrap_err().unwrap_exception(),
        &ExceptionKind::ExponentiationOverflowed
    );
    assert_eq!(
        eval("3**(0-1)").unwrap_err().unwrap_exception(),
        &ExceptionKind::ExponentiationOverflowed
    );
    assert_eq!(
        eval("5**(1-2)").unwrap_err().unwrap_exception(),
        &ExceptionKind::ExponentiationOverflowed
    );
    // assert_eq!(eval("2**-2").unwrap(), Value::Int(0));
    // assert_eq!(eval("3**-1").unwrap(), Value::Int(0));
    // assert_eq!(eval("5**-3").unwrap(), Value::Int(0));

    assert_eq!(eval("0**0").unwrap(), Value::Int(1));
    assert_eq!(eval("0**1").unwrap(), Value::Int(0));
    assert_eq!(eval("1**0").unwrap(), Value::Int(1));
    assert_eq!(
        eval("(0-2)**3").unwrap_err().unwrap_exception(),
        &ExceptionKind::ExponentiationOverflowed
    );
    // assert_eq!(eval("(0-2)**2").unwrap(), Value::Int(4));
    // assert_eq!(eval("(-2)**3").unwrap(), Value::Int(-8));
    // assert_eq!(eval("(-2)**2").unwrap(), Value::Int(4));
}

#[test]
fn assign() {
    assert_eq!(
        eval(include_str!("assign.olang")).unwrap(),
        Value::Int(430912)
    )
}

#[test]
fn pemdas_braces() {
    assert_eq!(eval("6/2*{1+2}").unwrap(), Value::Int(9));
    assert_eq!(eval("{3+5}*2").unwrap(), Value::Int(16));
    assert_eq!(eval("{{1+2}*{3+4}}").unwrap(), Value::Int(21));
    assert_eq!(eval("{{5-2}+{3*4}}").unwrap(), Value::Int(15));
    assert_eq!(eval("{{2+3}*{4-{1+1}}}").unwrap(), Value::Int(10));
}
