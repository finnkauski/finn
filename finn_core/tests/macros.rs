use finn_core::*;

#[test]
fn test_macros_accessible() {
    token!(EOF);
    make_punctuation_kind!(Seperator);
}

#[test]
fn test_token_works() {
    // TODO: this test is weak
    // TODO: could well be doctests
    assert_eq!(token!(EOF), TokenType::EOF);
    assert_eq!(token!(Char 'c'), TokenType::Char('c'));
    assert_eq!(
        token!(Punct '(' (Open 0)),
        TokenType::Punctuation {
            raw: '(',
            kind: make_punctuation_kind!(Open 0),
        },
    );
    assert_eq!(
        token!(Punct ')' (Close 0)),
        TokenType::Punctuation {
            raw: ')',
            kind: make_punctuation_kind!(Close 0),
        },
    );
}
