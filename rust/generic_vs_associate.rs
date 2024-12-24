///
/// trait 中关联类型和泛型的区别
///
///

// 使用泛型
trait GenericGetter<T> {
    fn get(&self) -> T;
}

// MyGetter 对 Getter 可以有不同泛型参数的多个实现

struct MyGetter {}

impl GenericGetter<&'static str> for MyGetter {
    fn get(&self) -> &'static str {
        "foo"
    }
}

impl GenericGetter<u32> for MyGetter {
    fn get(&self) -> u32 {
        10
    }
}

#[test]
fn test_trait_generic_type() {
    let g = MyGetter {};
    // println!("{}", g.get());  // 有多个实现，所以编译错误

    assert_eq!("foo", <MyGetter as GenericGetter<&'static str>>::get(&g)); // 需要指定具体实现
}

// 使用关联类型

trait AssociateGetter {
    type RetType;

    fn get(&self) -> Self::RetType;
}

// MyGetter2 对 AssociateGetter 有且只能有一个实现

struct MyGetter2 {}

impl AssociateGetter for MyGetter2 {
    type RetType = &'static str;
    fn get(&self) -> Self::RetType {
        "foo"
    }
}

// 编译错误, conflicting implementation
// impl AssociateGetter for MyGetter2 {
//     type RetType = u32;
//     fn get(&self) -> Self::RetType {
//         100
//     }
// }

#[test]
fn test_trait_associate_type() {
    let g = MyGetter2 {};
    assert_eq!("foo", g.get());
}

