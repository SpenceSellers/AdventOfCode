#[derive(Debug, Clone)]
pub struct ZipList<T> {
    left: Vec<T>,
    center: Option<T>,
    right: Vec<T>
}

impl<T> ZipList<T> {
    pub fn new() -> Self {
        ZipList {
            left: Vec::new(),
            center: None,
            right: Vec::new()
        }
    }

    pub fn current(&self) -> Option<&T> { self.center.as_ref() }

    pub fn insert_center_shifting_right(&mut self, val: T) {
        let prev_current = self.center.take();
        self.center = Some(val);
        if let Some(prev_current) = prev_current {
            self.right.push(prev_current);
        }
    }

    pub fn insert_center_shifting_left(&mut self, val: T) {
        let prev_current = self.center.take();
        self.center = Some(val);
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

    pub fn shift_right(&mut self, n: usize) {
        assert!(n <= self.left.len(), "Cannot shift further than edge");
        for _ in 0..n {
            self.right.push(self.center.take().expect("No center"));
            self.center = Some(self.left.pop().unwrap());
        }
    }

    pub fn shift_left(&mut self, n: usize) {
        assert!(n <= self.right.len(), "Cannot shift further than edge");
        for _ in 0..n {
            self.left.push(self.center.take().expect("No center"));
            self.center = Some(self.right.pop().unwrap());
        }
    }

    pub fn remove_filling_from_right(&mut self) -> Option<T> {
        let prev_center = self.center.take();
        self.center = self.right.pop();
        return prev_center;
    }

    pub fn remove_filling_from_left(&mut self) -> Option<T> {
        let prev_center = self.center.take();
        self.center = self.left.pop();
        return prev_center;
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
        zl.shift_right(1);
        assert_eq!(zl.current(), Some(&10));
        zl.shift_left(1);
        assert_eq!(zl.current(), Some(&20));
    }

    #[test]
    fn test_empty_ziplist() {
        let mut zl : ZipList<i32> = ZipList::new();
    }
}