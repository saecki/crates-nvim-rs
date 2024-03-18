#[derive(Clone, Debug)]
pub struct InlineStr {
    ptr: *mut u8,
}

impl InlineStr {
    pub const fn new() -> Self {
        todo!()
    }
}

impl From<&str> for InlineStr {
    fn from(value: &str) -> Self {
        todo!()
    }
}

impl Eq for InlineStr {}

impl PartialEq for InlineStr {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Ord for InlineStr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl PartialOrd for InlineStr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
