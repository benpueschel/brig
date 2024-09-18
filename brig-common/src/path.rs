use std::fmt::Display;

use thin_vec::ThinVec;

use crate::{Span, Symbol};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: ThinVec<Symbol>,
    pub span: Span,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("::")
        )
    }
}

#[macro_export]
macro_rules! path {
    ($span:expr, $($segment:expr),+ $(,)?) => {{
        use ::thin_vec::thin_vec;
        $crate::Path {
            segments: thin_vec![$($segment),+],
            span: $span,
        }}
    };
}
