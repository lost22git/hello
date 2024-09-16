#![allow(dead_code)]

use std::{
    collections::HashMap,
    fmt::Debug,
    mem::{align_of, align_of_val, size_of, size_of_val},
    num::NonZeroU32,
    rc::Rc,
    sync::Arc,
    usize,
};

///
///  see: https://cheats.rs/#memory-layout
///
///  RFC:
///  - https://github.com/rust-lang/rfcs/blob/master/text/1358-repr-align.md
///  - https://github.com/rust-lang/rfcs/blob/master/text/1399-repr-pack.md
///  - https://github.com/rust-lang/rfcs/blob/master/text/1758-repr-transparent.md
///

#[test]
fn test_primitive() {
    assert_eq!(1, size_of::<u8>());
    assert_eq!(1, align_of::<u8>());

    assert_eq!(4, size_of::<char>());
    assert_eq!(4, align_of::<char>());

    assert_eq!(4, size_of::<u32>());
    assert_eq!(4, align_of::<u32>());

    assert_eq!(8, size_of::<u64>());
    assert_eq!(8, align_of::<u64>());

    assert_eq!(8, size_of::<usize>());
    assert_eq!(8, align_of::<usize>());

    assert_eq!(4, size_of::<NonZeroU32>());
    assert_eq!(4, align_of::<NonZeroU32>());
}

#[test]
fn test_array() {
    assert_eq!(4 * 8, size_of::<[usize; 4]>());
    assert_eq!(8, align_of::<[usize; 4]>());
}

#[test]
fn test_tuple() {
    assert_eq!(8 + 24, size_of::<(usize, String)>());
    assert_eq!(8, align_of::<(usize, String)>());
}

#[test]
fn test_string() {
    assert_eq!(24, size_of::<String>()); // ptr + cap + len
    assert_eq!(8, align_of::<String>());
}

#[test]
fn test_vec() {
    assert_eq!(24, size_of::<Vec<String>>()); // ptr + cap + len
    assert_eq!(8, align_of::<Vec<String>>());
}

#[test]
fn test_slice() {
    assert_eq!(16, size_of::<&str>()); // ptr + len
    assert_eq!(8, align_of::<&str>());

    assert_eq!(16, size_of::<&[String]>()); // ptr + len
    assert_eq!(8, align_of::<&[String]>());
}

#[test]
fn test_map() {
    assert_eq!(48, size_of::<HashMap<String, String>>());
    assert_eq!(8, align_of::<HashMap<String, String>>());
}

#[test]
fn test_ptr() {
    assert_eq!(8, size_of::<&u8>());
    assert_eq!(8, align_of::<&u8>());

    assert_eq!(8, size_of::<&mut u8>());
    assert_eq!(8, align_of::<&mut u8>());

    assert_eq!(8, size_of::<*const u8>());
    assert_eq!(8, align_of::<*mut u8>());

    assert_eq!(8, size_of::<Box<u8>>());
    assert_eq!(8, align_of::<Box<u8>>());

    assert_eq!(8, size_of::<Rc<u8>>());
    assert_eq!(8, align_of::<Rc<u8>>());

    assert_eq!(8, size_of::<Arc<u8>>());
    assert_eq!(8, align_of::<Arc<u8>>());

    assert_eq!(16, size_of::<&dyn Debug>()); // data ptr + vtable ptr
    assert_eq!(8, align_of::<&dyn Debug>());
}

#[test]
fn test_struct() {
    struct Account {
        id: u32,
        name: String,
        balance: u32,
    }
    assert_eq!(4 + 4 + 24, size_of::<Account>()); // id + balance + name
    assert_eq!(8, align_of::<Account>());

    #[repr(C)]
    struct AccountReprC {
        id: u32,
        name: String,
        balance: u32,
    }
    assert_eq!((4 + 4) + 24 + (4 + 4), size_of::<AccountReprC>()); // id + padding + name + balance + padding
    assert_eq!(8, align_of::<AccountReprC>());

    // 用于消除paddings, 减小内存
    #[repr(C, packed(1))]
    struct AccountReprCPacked {
        id: u32,
        name: String,
        balance: u32,
    }
    assert_eq!(4 + 24 + 4, size_of::<AccountReprCPacked>()); // no padding
    assert_eq!(1, align_of::<AccountReprCPacked>());

    // 用于添加paddings, 隔离字段
    #[repr(C, align(32))]
    struct AccountReprCAlign {
        id: u32,
        name: String,
        balance: u32,
    }
    assert_eq!(64, size_of::<AccountReprCAlign>());
    assert_eq!(32, align_of::<AccountReprCAlign>());
}

#[test]
fn test_enum() {
    enum MyEnum {
        A(u8),    // tag(1 bit) + padding(align_of(u8)*8-1 bits) + size_of(u8)
        B(usize), // tag(1bit) + padding(align_of(usize)*8-1 bits) + size_of(usize)
    }
    assert_eq!(8 + 8, size_of::<MyEnum>()); // tag(1bit) + padding(8*8-1 bits) + size_of(usize)
    assert_eq!(8, align_of::<MyEnum>());
}

#[test]
fn test_option() {
    assert_eq!(24, size_of::<Option<String>>()); // size_of(String) 不需要 tag, 如果高八位为0则为 None
    assert_eq!(8, align_of::<Option<String>>()); // align_of(String)

    assert_eq!(1, size_of::<Option<()>>()); // tag
    assert_eq!(1, align_of::<Option<()>>()); // tag
}

#[test]
fn test_result() {
    assert_eq!(24, size_of::<Result<String, ()>>()); // size_of(String) 不需要 tag, 如果高八位为0则为Error()
    assert_eq!(8, align_of::<Result<String, ()>>()); // align_of(String)

    assert_eq!(24, size_of::<Result<(), String>>()); // size_of(String) 不需要 tag, 如果高八位为0则为Ok()
    assert_eq!(8, align_of::<Result<(), String>>()); // align_of(String)

    assert_eq!(1, size_of::<Result<(), ()>>()); // tag
    assert_eq!(1, align_of::<Result<(), ()>>()); // tag

    assert_eq!(24, size_of::<Result<String, usize>>()); // size_of(String) 不需要 tag, 如果高八位为0则为Error(usize)
    assert_eq!(8, align_of::<Result<String, usize>>()); // align_of(String)

    assert_eq!(8 + 24, size_of::<Result<String, String>>()); // tag + padding + size_of(String)
    assert_eq!(8, align_of::<Result<String, usize>>()); // align_of(String)
}

#[test]
fn test_closure() {
    let name = 10_u32;

    let f = || println!("{}", &name);
    assert_eq!(8, size_of_val(&f));
    assert_eq!(8, align_of_val(&f));

    let f = move || println!("{}", &name);
    assert_eq!(4, size_of_val(&f));
    assert_eq!(4, align_of_val(&f));
}
