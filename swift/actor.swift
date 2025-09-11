#!/usr/bin/env swift

actor Bucket {
   private var map: [String:String] = [:]

    func get(key:String) -> String? {
        return map[key]
    }

    @discardableResult
    func put(key:String, val:String) -> String? {
        let oldVal = map[key]
        map[key] = val
        return oldVal
    }

    @discardableResult
    func del(key:String) -> String? {
        return map.removeValue(forKey: key)
    }
}

actor BucketStore {
    private var map: [String:Bucket] = [:]

    func lookup(name: String, action: (Bucket) async -> Void) async {
        if let bucket = map[name] {
           await action(bucket)
        }
    }

    func put(name:String, bucket:Bucket) {
        map[name] = bucket
    }
}

func newBucket(bucketStore: BucketStore, name:String) async -> Bucket {
    let bucket = Bucket()
    await bucketStore.put(name: name, bucket: bucket)
    return bucket
}

// === Test ===

let bucketStore = BucketStore()
let bucketName = "langs"
let _ = await newBucket(bucketStore: bucketStore, name: bucketName)

let data = ["swift": "100", "dart": "101", "gleam": "102"]
for (k,v) in data {
    await bucketStore.lookup(name:bucketName) { bucket in
        Task { await bucket.put(key:k, val:v) }
    }
    try await Task.sleep(for: .milliseconds(100))
}

for k in data.keys {
    await bucketStore.lookup(name:bucketName) { bucket in
        print(k,"->",await bucket.get(key: k) as Any)
    }
}
