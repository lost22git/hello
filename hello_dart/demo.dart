#!/usr/bin/env dart

void main() {
  testClass();
  testIterable();
}

void testClass() {
  var book = Book(1, "hello dart");
  print("book: ${book}");
}

class Book {
  final int id;
  final String name;

  Book(this.id, this.name);
  String toString() => 'Book{:id ${id} :name "${name}"}';
}

void testIterable() {
  Iterable<int> range(int from, int to, {int step = 1}) sync* {
    int cur = from;
    while (cur < to) {
      yield cur;
      cur += step;
    }
  }

  var list = range(-1, 10, step: 2).toList();
  var expect = <int>[-1, 1, 3, 5, 7, 9];
  for (int i = 0; i < list.length; i++) {
    assert(list[i] == expect[i]);
  }
}
