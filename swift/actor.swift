actor Counter {
    private(set) var value: Int = 0

    func update(by: Int) {
        value += by
    }
}

let counter = Counter()
for i in (1..<10) {
    await counter.update(by: i)
    let v = await counter.value
    print(v)
    try await Task.sleep(for: .seconds(1))
}
