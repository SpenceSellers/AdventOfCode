#[derive(Debug, Clone)]
pub struct ZipList<T> {
    left: Vec<T>,
    current: Option<T>,
    right: Vec<T>
}

impl<T> ZipList<T> {
    pub fn new() -> Self {
        ZipList {
            left: Vec::new(),
            current: None,
            right: Vec::new()
        }
    }

    pub fn current(&self) -> &Option<T> { &self.current }


//    pub fn shift_to_right(&mut self) {
//        let left = self.left.pop().expect("")
//    }
}