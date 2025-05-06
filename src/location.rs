use std::fmt;

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

impl Location {
    pub fn from_index(source: &Vec<char>, index: usize) -> Self {
        let mut location = Location { row: 1, col: 1 };

        let target = if index > source.len() {
            // if the index is out of bounds
            // return the last character in the source
            source.len() - 1
        } else {
            index
        };

        for i in 0..target {
            if source[i] == '\n' {
                location.row += 1;
                location.col = 1;
            } else {
                location.col += 1
            };
        }

        location
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Region {
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.start, self.end)
    }
}
