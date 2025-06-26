#!/usr/bin/env dart

void main() {
  // Null
  assert(null.runtimeType == Null);
  assert(null is Null);

  // bool
  assert(true.runtimeType == bool);
  assert(true is bool);

  // int
  assert((-1).runtimeType == int);
  assert((-1) is int);

  // double
  assert((-1.1).runtimeType == double);
  assert((-1.1) is double);

  // record (.aka tuple)
  assert(
    (1, name: "dart", true).runtimeType.toString() ==
        "(int, bool, {String name})",
  );
  assert((1, name: "dart", true) is (int, bool, {String name}));

  // list
  assert([].runtimeType == List<dynamic>);
  assert(<int>[].runtimeType == List<int>);
  assert([] is List<dynamic>);
  assert(<int>[] is List<int>);

  // map
  assert({"a": 1, "b": 2}.runtimeType.toString() == "_Map<String, int>");
  assert({"a": 1, "b": 2} is Map<String, int>);

  // set
  assert({1, 2, 3}.runtimeType.toString() == "_Set<int>");
  assert({1, 2, 3} is Set<int>);

  // class
  assert(Book(1, "hello dart").runtimeType == Book);
  assert(Book(1, "hello dart") is Book);

  // nullable
  Book? book = null;
  book = Book(1, "hello dart");
  assert(book.runtimeType == Book);
  assert(book is Book);
  book = null;
  assert(book.runtimeType == Null);
  assert(book == null);
}

class Book {
  final int id;
  final String title;

  Book(this.id, this.title);
  String toString() => 'Book{:id ${id} :name "${title}"}';
}
