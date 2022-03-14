/// Lexer
///
/// Loads of help from:
/// https://www.youtube.com/watch?v=KZokxZrghCc

/// The different kinds of punctuation
#[derive(Debug, PartialEq)]
pub enum PunctuationKind {
    /// Keeps track of open brackets
    Open(BalancingDepth),
    /// Keeps track of close brackets
    Close(BalancingDepth),
    /// Actual punctuation for seperation purposes
    Separator,
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
    Any,
}

/// The core tokens we can process
#[derive(Debug, PartialEq)]
pub enum TokenType {
    /// End of token stream and file
    EOF,

    /// Punctuation like brackets, commas etc
    Punctuation { raw: char, kind: PunctuationKind },

    /// Identifiers - Sequences of characters
    Identifier(String),

    /// One Character
    Char(char),

    /// Numeric value
    Numeric { raw: String, hint: NumericHint },

    /// String value
    String(String),

    /// Operators like + - as well as assignment operators
    /// like += and /=
    Operator { raw: char, assignment: bool },

    /// Comparison Operators
    /// <, >, >=, <=
    ComparisonOperator { raw: char, equal: bool },

    /// Negation, stand alone `!`
    Negation,

    /// Assignment operation
    Assignment,

    /// For error handling, this is a token that can be anything
    /// for humans to read. This token is to be used only for
    /// Error handling purposes
    Expected(String),
}

pub type Token = TokenType;

/// Lexer module
use std::collections::HashMap;
/// The lexer
///
/// This is the main struct for the Lexer
/// TODO: embed the line and current column updates into the chars
#[derive(Debug, Clone)]
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

// TODO: would this better as a method?
#[macro_export]
macro_rules! try_consume {
    ($self: ident, $($inner:tt),*) => {
        if let Some(c) = $self.chars.peek() {
            if try_consume!(impl c, $($inner),*) {
                let tmp = *c;
                $self.consume_char();
                Some(tmp)
            } else {
                None
            }
        } else {
            None
        }

    };

    (impl , ) => (false);
    (impl $c:tt, $item:tt) => (*$c == $item);
    (impl $c:tt, $item:tt, $($rest:tt),+) => (try_consume!(impl $c, $item) || try_consume!(impl $c, $($rest),*));
}

impl<'l> Lexer<'l> {
    /// Initialise a new lexer from a given source string
    pub fn new(chars: &'l str) -> Self {
        Self {
            current_line: 1,
            current_col: 1,
            codepoint_offset: 0,

            chars: chars.chars().peekable(),

            bracket_balancing: HashMap::default(),
        }
    }

    /// Main function to convert a character to a token type
    fn convert_to_type(&mut self, c: char) -> LResult<TokenType> {
        match c {
            // Opening brackets
            '(' | '{' | '[' => Ok(TokenType::Punctuation {
                raw: c,
                kind: PunctuationKind::Open(self.update_balancing(&c, BalancingUpdate::Push)?),
            }),
            // Closing brackets
            ')' | '}' | ']' => Ok(TokenType::Punctuation {
                raw: c,
                kind: PunctuationKind::Close(self.update_balancing(&c, BalancingUpdate::Pop)?),
            }),

            // Numeric
            '0'..='9' => self.parse_number(c),
            // Numeric - Float if starts with '.' and is followed by
            // a digit
            '.' if self.check_next_with(|&c| c.is_digit(10)) => self.parse_number(c),

            // Strings
            // Any strings starting with '"' as an identifier
            '"' => self.parse_string(),

            // Assignment operator
            ':' if try_consume!(self, '=').is_some() => Ok(TokenType::Assignment),

            // The semicolon, useful seperator
            ';' | ':' => Ok(TokenType::Punctuation {
                raw: c,
                kind: PunctuationKind::Separator,
            }),

            // Operator
            '.' | '+' | '-' | '*' | '/' => self.parse_operator(c),

            // Equality comparison operators
            '!' | '=' if try_consume!(self, '=').is_some() => Ok(TokenType::ComparisonOperator {
                raw: c,
                equal: false,
            }),
            // Comparison Operators
            '<' | '>' => {
                let equal = try_consume!(self, '=').is_some();
                Ok(TokenType::ComparisonOperator { raw: c, equal })
            }

            // Stand alone boolean negation operator
            '!' if try_consume!(self, '=').is_none() => Ok(TokenType::Negation),

            // Identifiers
            c if c.is_ascii_alphabetic() => self.parse_ident(c),

            // Unknown
            _ => Err(LexerError::UnknownSymbolError { symbol: c }),
        }
    }

    /// Checks if the next item is what you expect it to be without
    /// consuming it using a given predicate function.
    fn check_next_with<F>(&mut self, check: F) -> bool
    where
        F: Fn(&char) -> bool,
    {
        self.chars.peek().map(check).unwrap_or(false)
    }

    /// Consumes the next character and handles updating the state
    /// of the lexers line and column tracking
    fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.current_line += 1;
                }
                // XXX: This is a weird thing
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

    pub fn next_token(&mut self) -> LResult<TokenType> {
        self.skip_whitespace();

        match self.consume_char() {
            Some(c) => self.convert_to_type(c),
            None => Ok(TokenType::EOF),
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
            _ => {
                panic!("Tried balancing a character that doesn't make sense to balance: {c:?}")
            }
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

    fn parse_digits(&mut self, radix: u32, allow_empty: bool) -> LResult<String> {
        let mut raw = String::new();

        loop {
            match self.chars.peek() {
                None => {
                    // if we allow empty then we can return empty
                    // but if we ended up in none after some loops and
                    // have pushed vaues on raw, we return that as well.
                    break if allow_empty || raw.len() > 0 {
                        Ok(raw)
                    } else {
                        Err(LexerError::MissingExpectedSymbol {
                            expected: Token::Numeric {
                                raw: "<int>".to_string(),
                                hint: NumericHint::Any,
                            },
                            found: Token::EOF,
                        })
                    };
                }
                // if the next character is a digit, or if the character is an underscore and
                // we already have some characters on the string stack, we should push this next
                // character
                Some(c) if c.is_digit(radix) || (*c == '_' && raw.len() > 0) => {
                    raw.push(*c);
                    self.consume_char();
                }
                // if the next character is not alphabetic AND if the next character is not
                // an underscore, we return what we have
                Some(c) if !c.is_ascii_alphabetic() && *c != '_' => break Ok(raw),

                // Deal with that case that the next value is e, otherwise it fall
                // just drops to the next one and we error out
                Some(c) if *c == 'e' || *c == 'E' => break Ok(raw),

                // if we get any other character, we return
                Some(c) => {
                    break Err(LexerError::NumericInvalidChar { raw, invalid: *c });
                }
            }
        }
    }

    /// The number parsing handler
    fn parse_number(&mut self, start: char) -> LResult<TokenType> {
        let mut raw = start.to_string();
        let radix = 10;
        let mut hint = NumericHint::Int;

        if start == '.' {
            // If we start with a float with .
            // the next things following this should a set of numbers
            raw += &self.parse_digits(radix, false)?;
            hint = NumericHint::Float;
        } else if start.is_digit(radix) {
            // If we start with a digit we should parse it until we
            // face something else. So adds a set of digits in order
            raw += &self.parse_digits(radix, true)?;

            // once thats done, we try to see if the next value is a
            // . and if so we push it and then we parse the rest of the float.
            if let Some(c) = try_consume!(self, '.') {
                raw.push(c);
                raw += &self.parse_digits(radix, false)?;
                hint = NumericHint::Float;
            }
        } else {
            println!("Compiler Bug: We wouldn't expect this to hit");
            return Err(LexerError::NumericInvalidChar {
                raw,
                invalid: start,
            });
        }

        // Once we're don with floats we should deal with any e or E
        // to indicate exponential. This is done after we've parsed the
        // decimal part of the float.
        if let Some(c) = try_consume!(self, 'e', 'E') {
            hint = NumericHint::Float;
            raw.push(c);
            // This makes it optional
            if let Some(c) = try_consume!(self, '+', '-') {
                raw.push(c);
            }
            // Parses the size of the exponent
            raw += &self.parse_digits(radix, false)?;
        }

        Ok(Token::Numeric { raw, hint })
    }

    /// Parse a string that starts with a "
    fn parse_string(&mut self) -> LResult<TokenType> {
        let mut buf = String::new();
        loop {
            match self.chars.next() {
                Some('"') => break Ok(Token::String(buf)),
                Some(c) => buf.push(c),
                None => {
                    break Err(LexerError::MissingExpectedSymbol {
                        expected: Token::Expected("Closing Delimiter".to_string()),
                        found: Token::EOF,
                    })
                }
            }
        }
    }
    /// Parse an identifier
    fn parse_ident(&mut self, start: char) -> LResult<TokenType> {
        let mut buf = start.to_string();
        loop {
            match self.chars.peek() {
                // XXX: this might be an issue
                Some(c) if c.is_ascii_alphanumeric() || *c == '_' => {
                    buf.push(*c);
                    self.consume_char();
                }
                _ => break Ok(TokenType::Identifier(buf)),
            }
        }
    }

    fn parse_operator(&mut self, raw: char) -> LResult<TokenType> {
        let assignment = try_consume!(self, '=').is_some();
        Ok(TokenType::Operator { raw, assignment })
    }
}

impl<'l> Iterator for Lexer<'l> {
    type Item = TokenType;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(TokenType::EOF) => None,
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
    MissingExpectedSymbol {
        expected: TokenType,
        found: TokenType,
    },

    #[error("Can't find opening symbol for: {symbol:?}")]
    MisbalancedSymbol { symbol: char, open: char },

    #[error("Can't create a numeric token. Wrong character: {raw:?}")]
    NumericInvalidChar { raw: String, invalid: char },

    #[error("Unknown token: {symbol:?}")]
    UnknownSymbolError { symbol: char },

    #[error("Expected more content but could not get next char")]
    UnexpectedEndOfContent,
}
pub type LResult<'s, T> = Result<T, LexerError>;
