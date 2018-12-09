use std::fs::File;
use std::io::Read;

pub mod grid;
pub mod count_map;
pub use count_map::*;

pub mod zip_list;

pub fn read_input_lines(filename: &str) -> Vec<String> {
    let mut file = File::open(filename).expect("Could not find input file");
    let mut string = String::new();
    file.read_to_string(&mut string).expect("Could not read file");

    return string.lines()
        .map(|x| x.to_string())
        .collect();
}

pub fn read_single_input_line(filename: &str) -> String {
    let lines = read_input_lines(filename);
    assert_eq!(lines.len(), 1, "The input was supposed to only have a single line");
    return lines.into_iter().nth(0).unwrap();
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct AsciiValue {
    code: u8
}

impl AsciiValue {
    pub fn from_code(code: u8) -> AsciiValue {
        AsciiValue { code }
    }

    pub fn from_char(c: char) -> Option<AsciiValue> {
        let mut buffer: [u8; 4] = [0;4];
        {
            let slice = c.encode_utf8(&mut buffer);
            if slice.len() > 1 {
                return None;
            }
        }

        return Some(AsciiValue::from_code(buffer[0]));
    }

    pub fn uppercase_index(&self) -> Option<usize> {
        if self.code < 65 || self.code > 90 { None }
            else { Some(self.code as usize - 65) }
    }

    pub fn lowercase_index(&self) -> Option<usize> {
        if self.code < 97 || self.code > 122 { None }
        else { Some(self.code as usize - 97) }
    }

    pub fn ascii_code(&self) -> u8 { self.code }

    // pub fn to_char(self) -> char {
    //     let mut buffer: [u8; 4] = [0;4];
    //     buffer[0] = self.code;

    //     let a = char.


    // }
}

mod test {
    use super::*;

    #[test]
    fn test_ascii() {
        let v = AsciiValue::from_char('G').unwrap();

        assert_eq!(v.ascii_code(), 71);
        assert_eq!(v.uppercase_index(), Some(6));
        assert_eq!(v.lowercase_index(), None);
    }
}

