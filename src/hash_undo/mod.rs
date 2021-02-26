use std::borrow::BorrowMut;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::{borrow::Borrow, mem::MaybeUninit};
use std::{borrow::Cow, cmp::PartialEq};

struct Node<'a, T, S>
where
    S: 'static + ToOwned,
{
    head: T,
    key: Cow<'a, S>,
    next: Option<Box<Node<'a, T, S>>>,
}

type NodePtr<'a, S, T> = Option<Box<Node<'a, S, T>>>;

pub struct HashUndo<'a, T, S>
where
    T: std::hash::Hash + 'static + ToOwned,
{
    map: [NodePtr<'a, S, T>; 501],
    undo_stack: Vec<usize>,
}

impl<'hash, T, S> HashUndo<'hash, T, S>
where
    T: std::hash::Hash + 'static + ToOwned + PartialEq,
    S: PartialEq,
{
    pub fn new() -> Self {
        let mut uninit_arr: [MaybeUninit<NodePtr<'hash, S, T>>; 501] = MaybeUninit::uninit_array();
        for element in uninit_arr.iter_mut() {
            *element = MaybeUninit::new(None);
        }
        Self {
            // Safety: definitely safe because we initialized the array before
            map: unsafe { std::mem::transmute(uninit_arr) },
            undo_stack: Vec::with_capacity(50),
        }
    }

    pub fn add_ref(&mut self, key_val: &'hash T, to_add: S) {
        let mut hasher = DefaultHasher::new();
        key_val.hash(&mut hasher);
        let key = (hasher.finish() as usize) % self.map.len();
        self.undo_stack.push(key);
        let new_node = Box::new(Node {
            head: to_add,
            key: Cow::Borrowed(key_val),
            next: None,
        });
        let replaced = std::mem::replace(&mut self.map[key], Some(new_node));
        if let Some(node) = replaced {
            self.map[key].as_mut().expect("shouldn't be null").next = Some(node);
        }
    }

    pub fn add(&mut self, key_val: T, to_add: S) {
        let mut hasher = DefaultHasher::new();
        key_val.hash(&mut hasher);
        let key = (hasher.finish() as usize) % self.map.len();
        self.undo_stack.push(key);
        let new_node = Box::new(Node {
            head: to_add,
            key: Cow::Owned(key_val.to_owned()),
            next: None,
        });
        let replaced = std::mem::replace(&mut self.map[key], Some(new_node));
        if let Some(node) = replaced {
            self.map[key].as_mut().expect("shouldn't be null").next = Some(node);
        }
    }

    pub fn get<'a>(&'a self, key: &T) -> Option<&'a S> {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let key_n = (hasher.finish() as usize) % self.map.len();
        let mut value = self.map[key_n].as_ref();
        while value.is_some() && value.unwrap().key.as_ref() != key {
            value = value.unwrap().next.as_ref();
        }
        if value.is_some() {
            Some(&value.as_ref().unwrap().head)
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Option<(S, Cow<'hash, T>)> {
        let key = self.undo_stack.pop()?;
        let node = self.map[key].take();
        if let Some(mut node) = node {
            self.map[key] = node.next.take();
            Some((node.head, node.key))
        } else {
            None
        }
    }
}

mod test {
    #[test]
    fn test_add_and_get_hashmap() {
        use super::HashUndo;
        let mut hashmap = HashUndo::<String, String>::new();
        let s = "sss".to_string();
        let ss = "sss".to_string();
        hashmap.add_ref(&s, "aaa".to_string());
        hashmap.add_ref(&ss, "ccc".to_string());
        hashmap.add_ref(&ss, "qqq".to_string());
        hashmap.add("www".to_string(), "qqqeee".to_string());
        assert_eq!(
            hashmap.get(&ss).expect("expect this to have something"),
            "qqq"
        );
        let z = "www".to_string();
        assert_eq!(
            hashmap.get(&z).expect("expect this to have something"),
            "qqqeee"
        );
        // undo action
        let (val, key) = hashmap.pop().expect("to work");
        assert!(key.as_ref() == "www");
        assert!(val == "qqqeee");
        let (val, key) = hashmap.pop().expect("to work");
        assert!(key.as_ref() == "sss");
        assert!(val == "qqq");
        let (val, key) = hashmap.pop().expect("to work");
        assert!(key.as_ref() == "sss");
        assert!(val == "ccc");
        let (val, key) = hashmap.pop().expect("to work");
        assert!(key.as_ref() == "sss");
        assert!(val == "aaa");
    }
}
