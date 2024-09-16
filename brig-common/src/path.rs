use crate::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<Symbol>,
}

#[macro_export]
macro_rules! path {
    () => {
        $crate::Path { segments: vec![] }
    };
    ($($segment:expr),+ $(,)?) => {
        $crate::Path {
            segments: vec![$($segment),+],
        }
    };
}
