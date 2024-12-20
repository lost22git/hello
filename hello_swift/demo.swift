#!/usr/bin/env swift

// string interpolation

let song_name = "Bohemian Rhapsody"
print("song: \(song_name)")

let a = 1
let b = 2
print("\(a) + \(b) = \(a + b)")

// multi lines string

print(
  """
  > Mama, just killed a man
  > Put a gun against his head, pulled my trigger, now he's dead...
  """)

// raw string

print(#">> the song name is "\#(song_name)""#)

// list

var list: [Int] = []
list.append(1)
assert(list == [1])
list[0] = 2
assert(list == [2])
assert(list.count == 1)
assert(list.capacity == 2)

// map

var map: [String: Int] = [:]
map["foo"] = 1
map["bar"] = 2
assert(map == ["foo": 1, "bar": 2])
assert(map.count == 2)
assert(map.capacity == 3)
for (k, v) in map {
  switch k {
  case "foo": assert(v == 1)
  case "bar": assert(v == 2)
  default: assert(false)
  }
}

// tuple

let t = (0, 1)
assert(0 == (t.0))
assert(1 == (t.1))
let (t0, t1) = t
assert(0 == t0)
assert(1 == t1)

// range

var sum = 0
for i in 1...3 {
  sum += i
}

assert(sum == (1 + 2 + 3))

sum = 0
for i in 1..<3 {
  sum += i
}

assert(sum == (1 + 2))

// optional

var maybe: String? = nil
// ?? default_value
assert("foo" == maybe ?? "foo")
maybe = "bar"
assert("bar" == maybe ?? "foo")
// ?. map
assert(3 == maybe?.count ?? 0)
if let v = maybe {  // if-let: if not-nil and unwrap
  assert(3 == (v.count))
}

// function

func tdivrem(a: Int, b: Int) -> (tdiv: Int, rem: Int) {
  return (a / b, a % b)
}

let (tdiv, rem) = (tdivrem(a: -5, b: 3))
assert(tdiv == -1)
assert(rem == -2)

// closure & trailing lambda

print([1,2].map{ (x)->Int in x + 1 })
print([1,2].map{ x in x + 1 })
print([1,2].map{ $0 + 1 })

// adt & pattern-matching

enum EbookFormat {
  case pdf, epub, mobi
}

struct Ebook {
  var pages: Int
  var format: EbookFormat
}

enum BookFormat {
  case paper(Int)  
  case ebook(Ebook) 

  var pages: Int {
    return switch self {
      case let .paper(pages): pages
      case let .ebook(ebook): ebook.pages
    }
  } 
}

// class

class Book: CustomStringConvertible {
  let id: Int
  let title: String
  var price: Double?
  var format: BookFormat

  init(id: Int, title: String, format: BookFormat) {
    self.id = id
    self.title = title
    self.format = format
  }
  
  // override CustomStringConvertible
  public var description: String {
    return
      #"Book{id:\#(self.id), title:"\#(self.title)", price:\#(self.price?.description ?? "nil"),format:\#(self.format)}"#
  }

  // computed property
  var pages: Int {
    return self.format.pages
  }
}
var book = Book(id: 1, title: "a book", format: .paper(99))
print(book)
book.price = 11.99
print(book)
book.format = .ebook(Ebook(pages: 120, format: .pdf))
print(book)
print("book pages:", book.pages)

// error handle

enum PortError: Error {
    case outOfRange
    case alreadyBound
}

@discardableResult
func testPort(port: Int) throws(PortError) -> Bool {
    guard (1..<65535).contains(port) else {
        throw PortError.outOfRange
    }
    throw PortError.alreadyBound
}

// error handle: try (propagate)

func foo() throws {
    try testPort(port: 8080)
}

// error handle: try-catch

do {
    try testPort(port: 8080) 
    assert(false)
} catch .outOfRange {
    assert(false)
} catch .alreadyBound {
    assert(true) // only this branch is reachable
} 

do {
    try testPort(port: -8080) 
    assert(false)
} catch .outOfRange {
    assert(true) // only this branch is reachable
} catch .alreadyBound {
    assert(false)
}

do {
    try testPort(port: 8080) 
    assert(false)
} catch {
    switch error {
    case .outOfRange: assert(false)
    case .alreadyBound: assert(true) // only this branch is reachable
    }
}

do {
    try testPort(port: -8080) 
    assert(false)
} catch {
    switch error {
    case .outOfRange: assert(true) // only this branch is reachable
    case .alreadyBound: assert(false)
    }
}

// error handle: try? (convert to optional)

assert(((try? testPort(port: 8080)) ?? true))

// error handle: try! (panic)
// try! testPort(port: 8080)

