
protocol Name {
    var first: String { get }
    var last: String { get }
}

/// default method of protocol
extension Name {
    var fullName: String { "\(first)-\(last)" } 
}

/// defines a struct (default Copyable)
/// or defines a class with `class` (default ~Copyable)
struct ArtistName {
    
    let first: String
    let last: String

    /// initial constructor
    init(first: String, last: String) {
        self.first = first
        self.last = last
    }
    
}

/// extends methods for ArtistName
extension ArtistName {
    func fullNameUpCase() -> String {
        self.fullName.uppercased()
    }
}

/// extends methods of Name for ArtistName
extension ArtistName: Name { }

/// extends methods of CustomStringConvertible for ArtistName
extension ArtistName: CustomStringConvertible {
    var description: String { fullNameUpCase() }
}

/// dynamic dispatch
func printName(_ name: Name) {
    print("FirstName:", name.first)
    print("LastName:", name.last)
    print("FullName:", name.fullName)
    print("Description: \(name)") 
}

let name: ArtistName = .init(first: "Taylor", last: "Swift")
printName(name)


