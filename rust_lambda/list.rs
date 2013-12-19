use std::{container, iter};

pub struct List<'a, A> {
  priv node: ListNode<'a, A>
}

enum ListNode<'a, A> {
  Empty(),
  Cons(A, &'a ListNode<'a, A>),
}

impl<'a, A> List<'a, A> {
  pub fn new_empty() -> List<'a, A> {
    List { node: Empty }
  }

  pub fn cons(&'a self, x: A) -> List<'a, A> {
    List { node: Cons(x, &self.node) }
  }

  pub fn iter(&'a self) -> ListIterator<'a, A> {
    ListIterator { node: &self.node }
  }

  pub fn index(&'a self, idx: uint) -> &'a A {
    let mut i = 0;
    for x in self.iter() {
      if i == idx {
        return x;
      } else {
        i = i + 1;
      }
    }
    fail!("List.index: Index out of bounds")
  }
}

pub struct ListIterator<'a, A> {
  priv node: &'a ListNode<'a, A>
}

impl<'a, A> iter::Iterator<&'a A> for ListIterator<'a, A> {
  fn next(&mut self) -> Option<&'a A> {
    match *self.node {
      Empty() => None,
      Cons(ref hd, rest) => {
        self.node = rest;
        Some(hd)
      }
    }
  }
}

impl<'a, A> container::Container for List<'a, A> {
  fn len(&self) -> uint {
    let mut len = 0;
    for _ in self.iter() {
      len = len + 1;
    }
    len
  }

  fn is_empty(&self) -> bool {
    match self.node {
      Empty() => true,
      Cons(_, _) => false,
    }
  }
}
