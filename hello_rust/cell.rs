#![allow(dead_code)]

use std::cell::{Cell, OnceCell, RefCell};

// Cell/RefCell/OnceCell 特性: 无须 mut, 也可以修改内部值

// copy or move 内部值
// 没有运行时 borrow check
#[test]
fn test_cell() {
    let c = Cell::new("foo".to_owned());

    // set: own 新值, drop 内部旧值
    c.set("bar".to_owned());

    // replace: own 新值，return owned 旧值
    let old_val = c.replace("baz".to_owned());
    assert!(&old_val == "bar");
}

// borrow or borrow_mut 内部值
// 运行时 borrow check
#[test]
fn test_refcell() {
    let ref_cell = RefCell::new("foo".to_owned());
    // borrow_mut
    *ref_cell.borrow_mut() = "bar".to_owned();
    // borrow
    assert!(ref_cell.borrow().as_str() == "bar");
}

// 只能 set 一次内部值
// 没有运行时 borrow check
#[test]
fn test_oncecell() {
    let once_cell = OnceCell::new();

    let _ = once_cell.set("foo".to_owned());
    assert!(once_cell.get().unwrap().as_str() == "foo");

    // 第二次 set 无效， 返回 Error("foo".to_owned())
    assert!(once_cell.set("bar".to_owned()).unwrap_err().as_str() == "bar");
}
