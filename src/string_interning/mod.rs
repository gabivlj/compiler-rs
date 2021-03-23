#![allow(unused_variables)]
use std::{collections::HashMap, sync::Mutex};

/// Reference to an internal string.
#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub struct StringId(usize);

impl std::fmt::Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", StringInternal::get_id(*self))
    }
}

///
/// Super simple string interning so you can have references to a single string
/// in your code without getting headaches about lifetimes
///
/// ## Example
/// ```rs
/// let mut string_internal = StringInternal::new();
/// let string_id = string_internal.add("Mr. Smith");
/// assert_eq!(string_internal.get(string_id), "Mr. Smith");
/// ```
///
/// It's really fast, and just a good way overall to have references to strings.
pub struct StringInternal<'a> {
    strings: HashMap<&'a str, StringId>,
    buffer: String,
    buffers: Vec<String>,
    interned: Vec<&'a str>,
}

// static mut GLOBAL_STRING_INTERNAL: Option<StringInternal<'static>> = None;

lazy_static! {
    static ref GLOBAL_STRING_INTERNAL: Mutex<StringInternal<'static>> =
        Mutex::new(StringInternal::new());
}

impl<'a> StringInternal<'a> {
    pub fn add_string<T: AsRef<str>>(string: T) -> StringId {
        let mut t = GLOBAL_STRING_INTERNAL.lock().unwrap();
        t.add(string)
    }

    pub fn get_id(string_id: StringId) -> &'static str {
        let string_internals = GLOBAL_STRING_INTERNAL.lock().unwrap();
        string_internals.get(string_id)
    }

    /// Returns a new internal string structure, the first internal buffer
    /// will be initialized with 100 bytes so maybe it's a better idea to use
    /// `with_capacity` if you know that you are gonna use this with lots of strings
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            buffer: String::with_capacity(100),
            buffers: Vec::new(),
            interned: Vec::with_capacity(4096),
        }
    }

    /// Creates a new string interning structure, initializes the first string buffer
    /// with the specified bytes.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            strings: HashMap::new(),
            buffer: String::with_capacity(capacity),
            buffers: Vec::new(),
            interned: Vec::with_capacity(4096),
        }
    }

    /// Creates a new string interning structure, initializes the first string buffer
    /// with the specified bytes. Also provides a capacity for the number of strings that
    /// are gonna be interned.
    pub fn with_capacity_for_internals(capacity: usize, capacity_internal_strings: usize) -> Self {
        Self {
            strings: HashMap::new(),
            buffer: String::with_capacity(capacity),
            buffers: Vec::new(),
            interned: Vec::with_capacity(capacity_internal_strings),
        }
    }

    /// Adds a new string, keep in mind that it will be cloned if it's a new string.
    pub fn add<T: AsRef<str>>(&mut self, string: T) -> StringId {
        let string = string.as_ref();
        let val = self.strings.get(string);
        if let Some(val) = val {
            *val
        } else {
            let (s, id) = self.intern(string);
            self.strings.insert(s, id);
            id
        }
    }

    /// Gets a previously added string
    /// ### Panics
    /// * Panics on invalid string id
    pub fn get(&self, string_id: StringId) -> &'a str {
        assert!(string_id.0 <= self.interned.len());
        self.interned[string_id.0]
    }

    fn intern<T: AsRef<str>>(&mut self, string: T) -> (&'a str, StringId) {
        let string = string.as_ref();
        let len = string.len();
        // We do this because we know that if we add this string to the buffer
        // it will reallocate and previous string references won't be correct,
        // so what we do here is create a new buffer with a bigger capacity,
        // minimum the size of the string to be added and keep it as a new buffer,
        // and the previous buffer just add it into a vector so we can keep a reference to it.
        if len + self.buffer.len() >= self.buffer.capacity() {
            let new_capacity = self.buffer.capacity().next_power_of_two();
            let to_add_buffer =
                std::mem::replace(&mut self.buffer, String::with_capacity(len + new_capacity));
            self.buffers.push(to_add_buffer);
        }
        let old_len = self.buffer.len();
        let new_len = old_len + string.len();
        self.buffer.push_str(string);
        // This is safe because this string lifetime will be kept alive because of the explanation
        // above
        let totally_safe_str_ref: &'a str =
            unsafe { std::mem::transmute(&self.buffer[old_len..new_len]) };
        self.interned.push(totally_safe_str_ref);
        (totally_safe_str_ref, StringId(self.interned.len() - 1))
    }
}

#[cfg(test)]
mod tests {

    use crate::string_interning::*;

    #[test]
    fn it_works_basic() {
        let mut db = StringInternal::new();
        let string_id = db.add("hello world!");
        assert_eq!(string_id, db.add("hello world!"));
        assert_eq!(db.get(string_id), "hello world!");
    }

    // #[test]
    // fn it_works_advanced() {
    //     let mut db = StringInternal::with_capacity(1000000);
    //     for i in 0..5_000_000 {
    //         db.add(i.to_string());
    //     }
    //     for i in 0..5_000_000 {
    //         let a = i.to_string();
    //         assert_eq!(db.add(&a).0, i);
    //         assert_eq!(db.get(StringId(i)), a);
    //     }
    // }

    #[test]
    fn example_works() {
        let mut string_internal = StringInternal::new();
        let string_id = string_internal.add("Mr. Smith");
        assert_eq!(string_internal.get(string_id), "Mr. Smith");
    }
}
