pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Self {
        Self { line, column }
    }
}

impl From<Position> for (usize, usize) {
    fn from(pos: Position) -> Self {
        (pos.line, pos.column)
    }
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}
