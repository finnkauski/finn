// TODO: depends on stuff being imported
use finn_core::*;

#[test]
fn check_round_bracket_nesting() {
    // TODO: when iterator is implemented this can be refactors
    let tokens: Vec<_> = Lexer::new("(())").collect();
    assert_eq!(
        vec![
            TokenType::Punctuation {
                raw: '(',
                kind: PunctuationKind::Open(0)
            },
            TokenType::Punctuation {
                raw: '(',
                kind: PunctuationKind::Open(1)
            },
            TokenType::Punctuation {
                raw: ')',
                kind: PunctuationKind::Close(1)
            },
            TokenType::Punctuation {
                raw: ')',
                kind: PunctuationKind::Close(0)
            },
        ],
        tokens
    )
}
#[test]
fn check_square_bracket_nesting() {
    let tokens: Vec<_> = Lexer::new("[[]]").collect();
    assert_eq!(
        vec![
            TokenType::Punctuation {
                raw: '[',
                kind: PunctuationKind::Open(0)
            },
            TokenType::Punctuation {
                raw: '[',
                kind: PunctuationKind::Open(1)
            },
            TokenType::Punctuation {
                raw: ']',
                kind: PunctuationKind::Close(1)
            },
            TokenType::Punctuation {
                raw: ']',
                kind: PunctuationKind::Close(0)
            },
        ],
        tokens
    )
}
#[test]
fn check_curly_bracket_nesting() {
    let tokens: Vec<_> = Lexer::new("{{}}").collect();
    assert_eq!(
        vec![
            TokenType::Punctuation {
                raw: '{',
                kind: PunctuationKind::Open(0)
            },
            TokenType::Punctuation {
                raw: '{',
                kind: PunctuationKind::Open(1)
            },
            TokenType::Punctuation {
                raw: '}',
                kind: PunctuationKind::Close(1)
            },
            TokenType::Punctuation {
                raw: '}',
                kind: PunctuationKind::Close(0)
            },
        ],
        tokens
    )
}
#[test]
fn parse_int() {
    let token = Lexer::new("1234")
        .next_token()
        .expect("Should produce a single int token");
    assert_eq!(
        token,
        TokenType::Numeric {
            raw: "1234".to_string(),
            hint: NumericHint::Int
        }
    )
}
#[test]
fn parse_float() {
    let mut lex = Lexer::new("3.1415 .1415");
    assert_eq!(
        lex.next_token().expect("Should produce a single token"),
        TokenType::Numeric {
            raw: "3.1415".to_string(),
            hint: NumericHint::Float
        }
    );
    assert_eq!(
        lex.next_token().expect("Should produce a single token"),
        TokenType::Numeric {
            raw: ".1415".to_string(),
            hint: NumericHint::Float
        }
    );
}
#[test]
fn parse_float_e() {
    let mut lex = Lexer::new("2.2e92 4.2e+24 4.2e-24");
    assert_eq!(
        lex.next_token().expect("Should produce a single token"),
        TokenType::Numeric {
            raw: "2.2e92".to_string(),
            hint: NumericHint::Float
        }
    );
    assert_eq!(
        lex.next_token().expect("Should produce a single token"),
        TokenType::Numeric {
            raw: "4.2e+24".to_string(),
            hint: NumericHint::Float
        }
    );
    assert_eq!(
        lex.next_token().expect("Should produce a single token"),
        TokenType::Numeric {
            raw: "4.2e-24".to_string(),
            hint: NumericHint::Float
        }
    )
}
