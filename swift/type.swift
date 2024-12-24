#!/usr/bin/env swift

/// https://developer.apple.com/documentation/swift/memorylayout

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
assert(MemoryLayout<Int?>.size == 8+1) // int + tag
assert(MemoryLayout<Int?>.alignment == 8)

// struct
struct Book {
    var id: UInt32
    var name: String
    var price: UInt32
}


assert(MemoryLayout<Book>.stride == 4 + 4 + 16 + 4 + 4) // with trailing paddings
assert(MemoryLayout<Book>.size == 4 + 4 + 16 + 4) // without trailing paddings 
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


// error 
enum IOError: Error {
    case fileNotFound(fd: Int)
    case eof
}

assert(MemoryLayout<IOError>.stride == 8 + 1 + 7) // int + tag + paddings
assert(MemoryLayout<IOError>.size == 8 + 1) // int + tag
assert(MemoryLayout<IOError>.alignment == 8)

enum IOError2: Error {
    case fileNotFound(file: String)
    case eof
}

assert(MemoryLayout<IOError2>.size == 16) // just string size, without tag, regard 0 as eof
assert(MemoryLayout<IOError2>.alignment == 8)

enum IOError3: Error {
    case fileNotFound(file: String)
    case eof
    case accessDenied
}
// https://github.com/TannerJin/Swift-MemoryLayout/blob/master/SwiftCore/String.swift
assert(MemoryLayout<IOError3>.size == 16)
assert(MemoryLayout<IOError3>.alignment == 8)

var value = IOError3.fileNotFound(file: "/tmp/tmp2/tmp3/tmp4/tmp5") 
withUnsafeBytes(of: &value) { pointer in
    print("fileNotFound:", [UInt8](pointer))
}

value = IOError3.eof
withUnsafeBytes(of: &value) { pointer in
    print("eof         :", [UInt8](pointer))
}

value = IOError3.accessDenied
withUnsafeBytes(of: &value) { pointer in
    print("accessDenied:", [UInt8](pointer))
}

enum IOError4: Error {
    case fileNotFound(file: String)
    case eof(file: String)
}

assert(MemoryLayout<IOError4>.size == 16 + 1)
assert(MemoryLayout<IOError4>.alignment == 8)
