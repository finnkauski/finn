#[macro_export]
macro_rules! make_punctuation_kind {
    (Seperator) => {
        $crate::PunctuationKind::Separator
    };
    (Open $depth:expr) => {
        $crate::PunctuationKind::Open($depth)
    };
    (Close $depth:expr) => {
        $crate::PunctuationKind::Close($depth)
    };
}

#[macro_export]
macro_rules! token {
        (EOF) => {
            $crate::TokenType::EOF;
        };
        (Char $raw:tt) => {
            $crate::TokenType::Char($raw)
        };
        (Punct $raw:tt ($($inner:tt)+)) => {
            $crate::TokenType::Punctuation { raw: $raw, kind: make_punctuation_kind!($($inner) +) }
        };

}
