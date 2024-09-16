#![allow(dead_code)]

fn print_title(title: &str) {
    println!("{}", format!("------ {:-<width$}", title, width = 30));
}

fn print_addr<T>(a: &T, tag: &str) {
    println!("{:<width$}: {}", tag, a as *const T as usize, width = 15)
}

fn borrow<T>(a: &T) {
    print_addr::<T>(a, "borrow");
}

fn borrow_mut<T>(a: &mut T) {
    print_addr::<T>(a, "borrow_mut")
}

fn copy_or_move<T>(a: T) {
    print_addr::<T>(&a, "copy");
}

fn copy_or_move_array(a: [&str; 2]) {
    print_addr(&a, "copy");
    print_addr(&a[0], "copy elements");
}

fn copy_or_move_vec<T>(a: Vec<T>) {
    print_addr(&a, "copy");
    print_addr(&a[0], "copy elements");
}

fn copy_or_move_string(a: String) {
    print_addr(&a, "copy");
    print_addr(&a.as_bytes()[0], "copy elements");
}

fn main() {
    #[derive(Debug)]
    struct P {
        x: u32,
        y: u32,
    }

    print_title("struct ");
    let mut a = P { x: 1, y: 2 };
    print_addr(&a, "origin");
    borrow(&a);
    borrow_mut(&mut a);
    copy_or_move(a);

    print_title("array ");
    let mut a = ["foo", "bar"];
    print_addr(&a, "origin");
    print_addr(&a[0], "origin elements");
    borrow(&a);
    borrow_mut(&mut a);
    copy_or_move_array(a);

    print_title("vec ");
    let mut a = vec!["foo", "bar"];
    print_addr(&a, "origin");
    print_addr(&a[0], "origin elements");
    borrow(&a);
    borrow_mut(&mut a);
    copy_or_move_vec(a);

    print_title("string ");
    let mut a = String::new();
    a.push_str("foo");
    a.push_str("bar");
    print_addr(&a, "origin");
    print_addr(&a.as_bytes()[0], "origin elements");
    borrow(&a);
    borrow_mut(&mut a);
    copy_or_move_string(a);
}
