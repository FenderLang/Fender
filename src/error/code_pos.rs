/// A position in code either as an absolute character count
/// from the begging of the file or as a line number and column number
#[derive(Debug, Clone)]
pub enum InnerCodePos {
    LineCol(usize, usize),
    Pos(usize),
}

/// A location in code within file `file_name` at position `pos`
#[derive(Debug, Clone)]
pub struct CodePos {
    pub file_name: Option<String>,
    pub pos: InnerCodePos,
}

impl CodePos {
    pub fn new_rel(file_name: Option<String>, lin: u32, col: u32) -> CodePos {
        CodePos {
            file_name,
            pos: InnerCodePos::LineCol(lin as usize, col as usize),
        }
    }
    pub fn new_abs(file_name: Option<String>, pos: usize) -> CodePos {
        CodePos {
            file_name,
            pos: InnerCodePos::Pos(pos),
        }
    }

    pub fn absolute_to_relative(&mut self, src: &str) {
        let InnerCodePos::Pos(pos) = self.pos else {
            return;
        };

        let src = &src[0..pos].lines().collect::<Vec<_>>();
        self.pos = InnerCodePos::LineCol(src.len(), src[src.len() - 1].len());
    }
}
