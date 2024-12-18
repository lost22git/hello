#!/usr/bin/env swift

// typeof
// print(type(of: (1...10)))

// bool
assert(MemoryLayout<Bool>.size == 1)
assert(MemoryLayout<Bool>.alignment == 1)

// number
assert(MemoryLayout<Int>.size == 8)
assert(MemoryLayout<Int>.alignment == 8)

assert(MemoryLayout<Double>.size == 8)
assert(MemoryLayout<Double>.alignment == 8)

assert(MemoryLayout<Float>.size == 4)
assert(MemoryLayout<Float>.alignment == 4)

// char
assert(MemoryLayout<Character>.size == 16)
assert(MemoryLayout<Character>.alignment == 8)

// string
assert(MemoryLayout<String>.size == 16)
assert(MemoryLayout<String>.alignment == 8)

// array
assert(MemoryLayout<[Int]>.size == 8)
assert(MemoryLayout<[Int]>.alignment == 8)

// map
assert(MemoryLayout<[String:Int]>.size == 8)
assert(MemoryLayout<[String:Int]>.alignment == 8)

// tuple
assert(MemoryLayout<(Bool,Int)>.size == 1+7+8)
assert(MemoryLayout<(Bool,Int)>.alignment == 8)

assert(MemoryLayout<(canRead: Bool, count: Int)>.size == 1+7+8)
assert(MemoryLayout<(canRead: Bool, count: Int)>.alignment == 8)

// range
assert(MemoryLayout<Range<Int>>.size == 16)
assert(MemoryLayout<Range<Int>>.alignment == 8)

assert(MemoryLayout<ClosedRange<Int>>.size == 16)
assert(MemoryLayout<ClosedRange<Int>>.alignment == 8)

// optional
assert(MemoryLayout<Int?>.size == 9)
assert(MemoryLayout<Int?>.alignment == 8)

// struct
struct Book {
    var id: UInt32
    var name: String
    var price: UInt32
}

assert(MemoryLayout<Book>.size == 4 + 4 + 16 + 4) // why size != n * alignment 
assert(MemoryLayout<Book>.alignment == 8)
assert(MemoryLayout.offset(of: \Book.id) == 0)
assert(MemoryLayout.offset(of: \Book.name) == 8)
assert(MemoryLayout.offset(of: \Book.price) == 24)

struct Book2 {
    var id: UInt32
    var price: UInt32
    var name: String
}

assert(MemoryLayout<Book2>.size == 4 + 4 + 16)
assert(MemoryLayout<Book2>.alignment == 8)
assert(MemoryLayout.offset(of: \Book2.id) == 0)
assert(MemoryLayout.offset(of: \Book2.price) == 4)
assert(MemoryLayout.offset(of: \Book2.name) == 8)

// class
class BookClass {
    var id: UInt32
    var name: String
    var price: UInt32

    init(id: UInt32, name: String, price: UInt32) {
        self.id = id
        self.name = name
        self.price = price
    }
}

assert(MemoryLayout<BookClass>.size == 8)
assert(MemoryLayout<BookClass>.alignment == 8)

// function
assert(MemoryLayout<(Int)->Int>.size == 8 + 8) // data-ptr + func-ptr
assert(MemoryLayout<(Int)->Int>.alignment == 8)

