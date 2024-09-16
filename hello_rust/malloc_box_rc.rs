#![allow(dead_code)]
#![feature(slice_ptr_get)]

use std::{
    alloc::{alloc, dealloc, handle_alloc_error, Layout},
    ptr::NonNull,
    rc::Rc,
};

/// Rust 堆内存管理
///
/// 1) *cosnt T or *mut T: std::alloc::Allocator 手动管理
/// 2) Box<T>: Unique<T> 单一所有权
/// 3) Rc<T>: 共享所有权, 引用计数
///

#[test]
fn test_alloc() {
    struct MyVec {
        data_ptr: NonNull<[u8]>,
        cap: usize,
        len: usize,
    }

    impl MyVec {
        fn new(cap: usize) -> Self {
            let layout = Layout::array::<u8>(cap).unwrap();
            let data_ptr = unsafe {
                let ptr = alloc(layout);
                if ptr.is_null() {
                    handle_alloc_error(layout);
                }
                NonNull::slice_from_raw_parts(NonNull::new_unchecked(ptr), cap)
            };
            println!("alloc: cap: {}", cap);
            Self {
                data_ptr,
                cap,
                len: 0,
            }
        }

        unsafe fn check_cap(&self) -> bool {
            self.data_ptr.len() == self.cap
        }
    }

    impl Drop for MyVec {
        fn drop(&mut self) {
            unsafe {
                dealloc(
                    self.data_ptr.as_mut_ptr(),
                    Layout::array::<u8>(self.cap).unwrap(),
                );
            }
            println!("dealloc: cap: {}", self.cap);
        }
    }

    let v = MyVec::new(10);
    assert_eq!(10, v.cap);
    unsafe {
        assert!(v.check_cap());
    }
    let _ = v;
}

#[test]
fn test_box() {
    // 单一所有权，生命周期与栈内存直接关联
    let b = Box::new("foo".to_owned()); // allocator.alloc(..) 分配 String 到堆内存，返回指针给栈内存 b
    let _ = b; // 栈内存 b 被回收并调用 drop(b) 使用 allocator.dealloc(..) 回收对应的推内存
}

#[test]
fn test_rc() {
    // 共享所有权，生命周期与栈内存间接关联
    let rc = Rc::new("foo".to_owned()); // allocator.alloc(..) 分配 String 到堆内存，返回指针给栈内存 rc

    let rc2 = rc.clone(); // strong_ref_count + 1

    let _ = rc; // 栈内存 rc 被回收并调用 drop(rc) 对 strong_ref_count - 1
    let _ = rc2; // 栈内存 rc2 被回收并调用 drop(rc) 对 strong_ref_count - 1, 此时 strong_ref_count == 0, 使用 allocator.dealloc(..) 回收对应的推内存
}
