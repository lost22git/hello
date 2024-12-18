#!/usr/bin/env swift

// string interpolation

let song_name = "Bohemian Rhapsody"
print("song: \(song_name)")

let a = 1
let b = 2
print("\(a) + \(b) = \(a + b)")


// multi lines string

print("""
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

var map: [String:Int] = [:]
map["foo"] = 1
map["bar"] = 2
assert(map == ["foo":1, "bar":2])
assert(map.count == 2)
assert(map.capacity == 3)
for (k,v) in map {
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

assert(sum == (1+2+3))

sum = 0
for i in 1..<3 {
    sum += i
}

assert(sum == (1+2))

// Optional

var maybe: String? = nil
assert("foo" == maybe ?? "foo") // ?? default_value
maybe = "bar"
assert("bar" == maybe ?? "foo")
assert(3 == maybe?.count ?? 0) // ?. map
if let v = maybe {  // if-let: if not-nil and unwrap 
    assert(3 == (v.count))
}


// function

func tdivrem(a:Int, b:Int) -> (tdiv:Int, rem:Int) {
    return (a/b, a%b)
}

let (tdiv, rem) = (tdivrem(a:-5, b:3))
assert(tdiv == -1)
assert(rem == -2)

// class

class Book: CustomStringConvertible {
    let id: Int
    let title: String
    var price: Double?

    init(id:Int, title:String) {
        self.id = id
        self.title = title
    }
 
    public var description: String {
       return #"Book{id:\#(self.id), title:"\#(self.title)", price:\#(self.price?.description ?? "nil")}"#
    }
}
var book = Book(id:1,title:"a book")
print(book)
book.price = 11.99
print(book)

