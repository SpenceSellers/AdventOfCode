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

    pub fn current(&self) -> Option<&T> { self.current.as_ref() }

    pub fn insert_center_shifting_right(&mut self, val: T) {
        let prev_current = self.current.take();
        self.current = Some(val);
        if let Some(prev_current) = prev_current {
            self.right.push(prev_current);
        }
    }

    pub fn insert_center_shifting_left(&mut self, val: T) {
        let prev_current = self.current.take();
        self.current = Some(val);
        if let Some(prev_current) = prev_current {
            self.left.push(prev_current);
        }
    }

    pub fn insert_to_right(&mut self, val: T) {
        self.right.push(val);
    }

    pub fn insert_to_left(&mut self, val: T) {
        self.left.push(val);
    }

    pub fn shift_right(&mut self) {
        let from_left = self.left.pop().expect("Cannot shift right: At edge");
        let old_center = self.current.take().expect("No center");
        self.right.push(old_center);
        self.current = Some(from_left);
    }

    pub fn shift_left(&mut self) {
        let from_right = self.right.pop().expect("Cannot shift left: At edge");
        let old_center = self.current.take().expect("No center");
        self.right.push(old_center);
        self.current = Some(from_right);
    }

    pub fn remove_filling_from_right(&mut self) -> Option<T> {
        let prev_current = self.current.take();
        self.current = self.right.pop();
        return prev_current;
    }

    pub fn remove_filling_from_left(&mut self) -> Option<T> {
        let prev_current = self.current.take();
        self.current = self.left.pop();
        return prev_current;
    }
}

mod test {
    use super::*;

    #[test]
    fn test_shift() {
        let mut zl: ZipList<i32> = ZipList::new();
        zl.insert_center_shifting_left(10);
        zl.insert_center_shifting_left(20);

        assert_eq!(zl.current(), Some(&20));
        zl.shift_right();
        assert_eq!(zl.current(), Some(&10));
    }

    #[test]
    fn test_empty_ziplist() {
        let mut zl : ZipList<i32> = ZipList::new();
    }
}