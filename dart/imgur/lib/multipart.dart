import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

class Multipart {
  final IOSink sink;
  final String boundary;
  final String _boundaryBegin;
  final String _boundaryEnd;

  Multipart({required this.sink, required this.boundary})
    : _boundaryBegin = '--$boundary',
      _boundaryEnd = '--$boundary--';

  Multipart.sink(IOSink sink) : this(sink: sink, boundary: generateBoundary());

  String contentType() =>
      'multipart/form-data; boundary="$boundary"; charset=utf-8';

  /// generate boundary
  static String generateBoundary() {
    var rand = Random();
    return base64Encode(
      List.generate(30, (i) => rand.nextInt(256), growable: false),
    );
  }

  /// write file part
  /// [filename] is detected from [file] if is not given
  Future<void> writeFilePart(
    String name,
    File file, {
    String? filename,
    String contentType = 'application/octet-stream',
  }) async {
    filename = filename ?? file.uri.pathSegments.last;
    sink
      ..writeln(_boundaryBegin)
      ..writeln(
        'Content-Disposition: form-data; name="$name"; filename="$filename"',
      )
      ..writeln('Content-Type: $contentType')
      ..writeln();

    await sink.addStream(file.openRead());
    sink.writeln();
  }

  /// write string part
  void writeStringPart(String name, String value) {
    sink
      ..writeln(_boundaryBegin)
      ..writeln('Content-Disposition: form-data; name="$name"')
      ..writeln()
      ..writeln(value);
  }

  /// write done and return the [IOSink]
  IOSink done() {
    sink.writeln(_boundaryEnd);
    return sink;
  }
}
