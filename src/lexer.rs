use std::collections::HashMap;
/// Lexer ideas from:
///
/// https://www.youtube.com/watch?v=KZokxZrghCc&t=1079s

/// The different kinds of punctuation
#[derive(Debug, PartialEq)]
pub enum PunctuationKind {
    /// Keeps track of open brackets
    Open(BalancingDepth),
    /// Keeps track of close brackets
    Close(BalancingDepth),
    /// Actual punctuation for seperation purposes
    Seperator,
}
pub type BalancingDepth = u32;
enum BalancingUpdate {
    Push,
    Pop,
}

/// Type hints for the numeric token
#[derive(Debug, PartialEq)]
pub enum NumericHint {
    Int,
    Float,
}

/// The core tokens we can process
#[derive(Debug, PartialEq)]
pub enum Token {
    /// End of token stream and file
    EOF,

    /// Punctuation like brackets, commas etc
    Punctuation { raw: char, kind: PunctuationKind },

    /// Actions you can take on an entity
    /// *, |, ->
    Operator(String),

    /// Identifiers - Sequences of characters
    Identifier(String),

    /// One Character
    Char(char),

    /// Numeric value
    Numeric { raw: String, hint: NumericHint },
}

/// The lexer
///
/// This is the main struct for the Lexer
/// TODO: embed the line and current column updates into the chars
#[derive(Debug)]
pub struct Lexer<'s> {
    /// Human readable number of the line
    /// the lexer is on.
    pub current_line: usize,
    /// Human readable column the lexer is
    /// currently on.
    pub current_col: usize,
    /// Machine readable offset within the
    /// file.
    pub codepoint_offset: usize,

    /// Contents of the code
    chars: std::iter::Peekable<std::str::Chars<'s>>,

    bracket_balancing: HashMap<char, BalancingDepth>,
}

impl<'l> Lexer<'l> {
    /// Initialise a new lexer from a given source string
    pub fn new(chars: &str) -> Lexer {
        Lexer {
            current_line: 1,
            current_col: 1,
            codepoint_offset: 0,

            chars: chars.chars().peekable(),

            bracket_balancing: HashMap::default(),
        }
    }

    fn map_balance(c: &char) -> char {
        match c {
            ')' => '(',
            '(' => ')',
            '}' => '{',
            '{' => '}',
            ']' => '[',
            '[' => ']',
            _ => panic!("Tried balancing a character that doesn't make sense to balance: {c:?}"),
        }
    }

    /// Handles updating the internal state of the lexer to keep track of the state
    /// of the bracket balancing.
    fn update_balancing(&mut self, c: &char, how: BalancingUpdate) -> LResult<BalancingDepth> {
        match how {
            BalancingUpdate::Push => Ok(if let Some(v) = self.bracket_balancing.get_mut(c) {
                *v += 1;
                *v - 1
            } else {
                self.bracket_balancing.insert(*c, 1);
                0
            }),
            BalancingUpdate::Pop => match self.bracket_balancing.get_mut(&Lexer::map_balance(c)) {
                Some(0) => Err(LexerError::MisbalancedSymbol {
                    symbol: *c,
                    open: Lexer::map_balance(c),
                }),
                Some(v) => {
                    *v -= 1;
                    Ok(*v)
                }
                None => Err(LexerError::MisbalancedSymbol {
                    symbol: *c,
                    open: Lexer::map_balance(c),
                }),
            },
        }
    }

    fn get_next_digit(&mut self, num: &str, with_radix: u32) -> LResult<char> {
        match self.chars.peek() {
            None => return Err(LexerError::UnexpectedEndOfContent),
            Some(c) if !c.is_digit(with_radix) => {
                return Err(LexerError::NumericInvalidChar {
                    raw: *c,
                    num: String::from(num),
                })
            }
            Some(c) => Ok(*c),
        }
    }

    /// The number parsing handler
    ///
    /// It supports:
    /// - integers: 42
    /// - floats: 3.1415, .6969
    /// - exp: 1e-10 or 1e+10
    ///
    /// This doesn't deal with negative values.
    fn parse_number(&mut self, start: char) -> LResult<Token> {
        // Initialise these here to keep track of the state
        // Follow along for float
        // example 3.1415
        // start is 3
        let mut seen_dot = false;
        let mut seen_exp = false;
        let mut num = start.to_string();
        let radix = 10;

        // with 3.1415 we don't fall into this
        if start == '.' {
            seen_dot = true;
            // check here if the next value after `.` is a digit
            // or not. If it isn't a digit, we get an error and we
            // pass it up with early return.
            //
            // If it is a digit we also consume it here.
            num.push(self.get_next_digit(&num, radix)?);
            self.consume_char();
        }

        loop {
            // grab the next char which is `.` when we parse 3.1415
            // and start is at `3`
            match self.chars.peek() {
                // this is where we fall in because the next is `.` on the first loop
                Some(c) if *c == '.' && !seen_dot && !seen_exp => {
                    num.push(*c);
                    self.consume_char(); // consume to advance cursor
                    seen_dot = true;
                }
                Some(c) if *c == 'e' || *c == 'E' && !seen_exp => {
                    num.push(*c);
                    self.consume_char();
                    seen_exp = true;

                    let exp_radix = 10;

                    // We now need to handle the cases for `1e+10` and `1e-10`
                    match self.chars.peek() {
                        // TODO: could do an enum and store it in the numeric type
                        // that indicates what kind of value we're getting for the
                        // exponent - positive or negative.
                        Some(c) if *c == '+' || *c == '-' => {
                            num.push(*c);
                            self.consume_char();
                        }
                        _ => {}
                    }
                    num.push(self.get_next_digit(&num, exp_radix)?);
                    self.consume_char();
                }
                // this will handle the radix cases before it drops down to the other
                // checks
                //
                // in the second loop of 3.1415 we land here
                Some(c) if c.is_digit(radix) => {
                    num.push(*c);
                    self.consume_char();
                }
                // is_digit(10) is for the case where radix won't be 10
                // this looks at a case where we don't parse a preset radix
                // and we parse for '0'..='9'
                Some(c) if c.is_ascii_alphabetic() || c.is_digit(10) => {
                    num.push(*c);
                    return Err(LexerError::NumericInvalidChar { raw: *c, num });
                }
                // TODO: can make this the while condition
                _ => {
                    break Ok(Token::Numeric {
                        raw: num,
                        hint: if seen_dot || seen_exp {
                            NumericHint::Float
                        } else {
                            NumericHint::Int
                        },
                    })
                }
            }
        }
    }

    /// Main functio to convert a character to a token type
    fn to_type(&mut self, c: char) -> LResult<Token> {
        match c {
            '(' | '{' | '[' => Ok(Token::Punctuation {
                raw: c,
                kind: PunctuationKind::Open(self.update_balancing(&c, BalancingUpdate::Push)?),
            }),
            ')' | '}' | ']' => Ok(Token::Punctuation {
                raw: c,
                kind: PunctuationKind::Close(self.update_balancing(&c, BalancingUpdate::Pop)?),
            }),
            '0'..='9' | '.' => self.parse_number(c),
            _ => Err(LexerError::UnknownSymbolError { symbol: c }),
        }
    }

    /// Consumes the next character and handles updating the state
    /// of the lexers line and column tracking
    pub fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.current_line += 1;
                }
                self.current_col += 1;
                // NOTE: this is where the other encoding support can come in handy
                self.codepoint_offset += 1;

                Some(c)
            }
            None => None,
        }
    }

    /// Skips all whitespace
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.consume_char();
        }
    }

    pub fn next_token(&mut self) -> LResult<Token> {
        self.skip_whitespace();

        match self.consume_char() {
            Some(c) => self.to_type(c),
            None => Ok(Token::EOF),
        }
    }
}

impl<'l> Iterator for Lexer<'l> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::EOF) => None,
            Ok(token) => Some(token),
            Err(_) => None,
        }
    }
}

/// Lexer related errors
///
/// NOTE: unclear if tying the lifetime of this to the
/// input string of a file is a wise decision or a premature
/// optimisation that would lead to more issues down the
/// line.
#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("Failed to operate on file")]
    IOError(#[from] std::io::Error),

    #[error("Expected {expected:?}, found {found:?}")]
    MissingExpectedSymbol { expected: Token, found: Token },

    #[error("Can't find opening symbol for: {symbol:?}")]
    MisbalancedSymbol { symbol: char, open: char },

    #[error("Can't create a numeric token. Wrong character: {raw:?}")]
    NumericInvalidChar { raw: char, num: String },

    #[error("Unknown token: {symbol:?}")]
    UnknownSymbolError { symbol: char },

    #[error("Expected more content but could not get next char")]
    UnexpectedEndOfContent,
}
pub type LResult<'s, T> = Result<T, LexerError>;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn check_round_bracket_nesting() {
        // TODO: when iterator is implemented this can be refactors
        let tokens: Vec<_> = Lexer::new("(())").collect();
        assert_eq!(
            vec![
                Token::Punctuation {
                    raw: '(',
                    kind: PunctuationKind::Open(0)
                },
                Token::Punctuation {
                    raw: '(',
                    kind: PunctuationKind::Open(1)
                },
                Token::Punctuation {
                    raw: ')',
                    kind: PunctuationKind::Close(1)
                },
                Token::Punctuation {
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
                Token::Punctuation {
                    raw: '[',
                    kind: PunctuationKind::Open(0)
                },
                Token::Punctuation {
                    raw: '[',
                    kind: PunctuationKind::Open(1)
                },
                Token::Punctuation {
                    raw: ']',
                    kind: PunctuationKind::Close(1)
                },
                Token::Punctuation {
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
                Token::Punctuation {
                    raw: '{',
                    kind: PunctuationKind::Open(0)
                },
                Token::Punctuation {
                    raw: '{',
                    kind: PunctuationKind::Open(1)
                },
                Token::Punctuation {
                    raw: '}',
                    kind: PunctuationKind::Close(1)
                },
                Token::Punctuation {
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
            Token::Numeric {
                raw: "1234".to_string(),
                hint: NumericHint::Int
            }
        )
    }
    #[test]
    fn parse_float() {
        let mut lex = Lexer::new("3.1415 .1415 2.2e92 4.2e+24 4.2e-24");
        assert_eq!(
            lex.next_token()
                .expect("Should produce a single float token"),
            Token::Numeric {
                raw: "3.1415".to_string(),
                hint: NumericHint::Float
            }
        );
        assert_eq!(
            lex.next_token().expect("Should produce a single int token"),
            Token::Numeric {
                raw: ".1415".to_string(),
                hint: NumericHint::Float
            }
        );
        assert_eq!(
            lex.next_token().expect("Should produce a single int token"),
            Token::Numeric {
                raw: "2.2e92".to_string(),
                hint: NumericHint::Float
            }
        );
        assert_eq!(
            lex.next_token().expect("Should produce a single int token"),
            Token::Numeric {
                raw: "4.2e+24".to_string(),
                hint: NumericHint::Float
            }
        );
        assert_eq!(
            lex.next_token().expect("Should produce a single int token"),
            Token::Numeric {
                raw: "4.2e-24".to_string(),
                hint: NumericHint::Float
            }
        )
    }
}
