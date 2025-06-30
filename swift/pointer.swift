func pointers() {
    var v: Int32 = 1
    
    // Pointer
    let p: UnsafePointer = .init(&v)
    print("p: \(p)")
    print("p.pointee: \(p.pointee)")

    // MutablePointer
    let mp: UnsafeMutablePointer = .init(&v)
    print("mp: \(mp)")
    print("mp.pointee: \(mp.pointee)")

    // RawPointer
    let rp: UnsafeRawPointer = .init(&v)
    print("rp: \(rp)")

    // RawMutablePointer
    let mrp: UnsafeMutableRawPointer = .init(&v)
    print("mrp: \(mrp)")

    // UnsafeRawPointer <=> UnsafeMutableRawPointer
    assert(rp == UnsafeRawPointer(mrp))
    assert(mrp == UnsafeRawPointer(rp))

    // UnsafePointer <=> UnsafeMutablePointer
    assert(p == UnsafePointer(mp))
    assert(mp == UnsafePointer(p))

    // UnsafeRawPointer <=> UnsafePointer
    assert(rp == UnsafeRawPointer(p))
    assert(p == rp.bindMemory(to:Int32.self, capacity: 4))

    // UnsafeMutableRawPointer <=> UnsafeMutablePointer
    assert(mrp == UnsafeMutableRawPointer(mp))
    assert(mp == mrp.bindMemory(to: Int32.self, capacity: 4))
}

func bufferPointers() {
    /// - UnsafeRawBufferPointer
    /// - UnsafeMutableRawBufferPointer
    /// - UnsafeBufferPointer
    /// - UnsafeMutableBufferPointer
    /// - ManagedBufferPointer
    
    let v:UInt32 = 257
    withUnsafePointer(to: v) { p in
        // UnsafeRawBufferPointer <= UnsafeRawPointer
        let rbp: UnsafeRawBufferPointer = .init(start: p, count: 4)
        let uint8_tuple = rbp.load(as: (UInt8,UInt8,UInt8,UInt8).self)
        assert(uint8_tuple == (1,1,0,0))
    }
    // withUnsafeBytes(of: v) { rbp in
    //     let uint8_tuple = rbp.load(as: (UInt8,UInt8,UInt8,UInt8).self)
    //     assert(uint8_tuple == (1,1,0,0))
    // }
    withUnsafeBytes(of: v) { rbp in
        let bytes = [UInt8].init(rbp.bindMemory(to: UInt8.self))
        assert(bytes == [1,1,0,0])
    }

}


func allocate() {
    // allocate memory
    let mp: UnsafeMutablePointer<UInt8> = .allocate(capacity: 4)
    defer { mp.deallocate() }
    // initialize memory
    mp.initialize(repeating: 1, count: 2) // ( 1,1,uninit,uninit )
    mp.advanced(by: 2).initialize(repeating: 0, count: 2) // ( 1,1,0,0 )
    // rebind memory to UInt32 in the block scope
    let v = mp.withMemoryRebound(to: UInt32.self, capacity: 4) { uint32_p in uint32_p.pointee }
    assert(257 == v)
    assert(type(of: mp) == UnsafeMutablePointer<UInt8>.self)

    // allocate memory
    let mrbp: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: 4)
    defer { mrbp.deallocate() }
    // initialize memory
    let _ = mrbp.initialize(from: [1,1,0,0])
    // rebind memory to UInt32 in the block scope
    let vv = mp.withMemoryRebound(to: UInt32.self, capacity: 4) { uint32_p in uint32_p.pointee }
    assert(257 == vv)
    assert(type(of: mrbp) == UnsafeMutableBufferPointer<UInt8>.self)
}

pointers()
bufferPointers()
allocate()

