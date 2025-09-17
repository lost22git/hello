#!/usr/bin/env swift


protocol Name {
    var first: String { get }
    var last: String { get }
}

/// protocol default method
extension Name {
    var fullName: String { "\(first)-\(last)" } 
}

struct ArtistName {
    let first: String
    let last: String
    
    init(first: String, last: String) {
        self.first = first
        self.last = last
    }
}

extension ArtistName: Name { }

extension ArtistName: CustomStringConvertible {
    var description: String { fullName }
}

let name: ArtistName = .init(first: "Taylor", last: "Swift")
print("FirstName:", name.first)
print("LastName:", name.last)
print("FullName:", name.fullName)
print("Description: \(name)") 
