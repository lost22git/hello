import 'dart:convert';
import 'dart:io';

import 'package:imgur/multipart.dart';
import 'package:test/test.dart';

void main() {
  test('multipart generate boundary', testMultipartGenerateBoundary);
}

void testMultipartGenerateBoundary() {
  assert(Multipart.generateBoundary().length == 40);
}

void testMultipartContentType() {
  assert(
    Multipart(sink: FakeIOSink(), boundary: 'xyz').contentType() ==
        'multipart/form-data; boundary="xyz"',
  );
}

class FakeIOSink implements IOSink {
  @override
  late Encoding encoding;

  @override
  void add(List<int> data) {
    // TODO: implement add
  }

  @override
  void addError(Object error, [StackTrace? stackTrace]) {
    // TODO: implement addError
  }

  @override
  Future addStream(Stream<List<int>> stream) {
    // TODO: implement addStream
    throw UnimplementedError();
  }

  @override
  Future close() {
    // TODO: implement close
    throw UnimplementedError();
  }

  @override
  // TODO: implement done
  Future get done => throw UnimplementedError();

  @override
  Future flush() {
    // TODO: implement flush
    throw UnimplementedError();
  }

  @override
  void write(Object? object) {
    // TODO: implement write
  }

  @override
  void writeAll(Iterable objects, [String separator = ""]) {
    // TODO: implement writeAll
  }

  @override
  void writeCharCode(int charCode) {
    // TODO: implement writeCharCode
  }

  @override
  void writeln([Object? object = ""]) {
    // TODO: implement writeln
  }
}
