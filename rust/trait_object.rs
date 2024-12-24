#![allow(dead_code)]
use std::{
    fmt::{Debug, Display},
    rc::Rc,
    sync::Arc,
};

/// trait object 是 ?size (DST), 所以基本使用其引用（e.g. Box<dyn trait> or & dyn trait ...）
///
/// trait object 的引用是双指针大小的（包括 data ptr 和 vtable ptr (用于动态委派)）
///
/// trait object 限制：
/// object safety -> https://doc.rust-lang.org/stable/reference/items/traits.html#object-safety

#[derive(Debug, Default)]
struct User {
    name: String,
    age: u32,
}

trait Printable {
    fn print(&self) {
        println!("{} print", std::any::type_name::<Self>())
    }
}

impl Printable for User {}

#[test]
fn test_size_of_trait_object_ref() {
    let u = User::default();
    let raw_ref: &User = &u;
    let trait_object_ref: &dyn Printable = &u;
    assert_eq!(std::mem::size_of_val(&raw_ref), 8); // data ptr
    assert_eq!(std::mem::size_of_val(&trait_object_ref), 16); // data ptr + vtable ptr

    assert_eq!(std::mem::size_of::<Box<User>>(), 8);
    assert_eq!(std::mem::size_of::<Box<dyn Printable>>(), 16);

    assert_eq!(std::mem::size_of::<Rc<User>>(), 8);
    assert_eq!(std::mem::size_of::<Rc<dyn Printable>>(), 16);

    assert_eq!(std::mem::size_of::<Arc<User>>(), 8);
    assert_eq!(std::mem::size_of::<Arc<dyn Printable>>(), 16);
}

unsafe fn get_data_and_vtable(v: &Box<dyn Printable>) -> (*const (), *const ()) {
    std::mem::transmute_copy::<Box<_>, (*const (), *const ())>(v)
}

#[test]
fn test_trait_object_vtable() {
    let a: Box<dyn Printable> = Box::new(User::default());
    let b: Box<dyn Printable> = Box::new(User::default());
    unsafe {
        let (a_data, a_vtable) = get_data_and_vtable(&a);
        let (b_data, b_vtable) = get_data_and_vtable(&b);

        assert!(a_data != b_data);
        assert!(a_vtable == b_vtable);
    }
}

#[test]
fn test_trait_object_vtable2() {
    let s1 = String::from("hello world!");
    let s2 = String::from("goodbye world!");
    // Display / Debug trait object for s
    let w1: &dyn Display = &s1;
    let w2: &dyn Debug = &s1;
    // Display / Debug trait object for s1
    let w3: &dyn Display = &s2;
    let w4: &dyn Debug = &s2;

    let (addr1, vtable1): (usize, usize) = unsafe { std::mem::transmute(w1) };
    let (addr2, vtable2): (usize, usize) = unsafe { std::mem::transmute(w2) };
    let (addr3, vtable3): (usize, usize) = unsafe { std::mem::transmute(w3) };
    let (addr4, vtable4): (usize, usize) = unsafe { std::mem::transmute(w4) };

    // 指向同一个数据的 trait object 其 ptr 地址相同
    assert_eq!(addr1, addr2);
    assert_eq!(addr3, addr4);
    // 指向同一种类型的同一个 trait 的 vtable 地址相同
    // 这里都是 String + Display
    assert_eq!(vtable1, vtable3);
    // 这里都是 String + Debug
    assert_eq!(vtable2, vtable4)
}
