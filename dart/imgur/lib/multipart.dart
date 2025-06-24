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
    const len = 30;
    var bytes = Uint8List(len);
    for (var i = 0; i < len; i++) {
      bytes[i] = rand.nextInt(256);
    }
    return base64Encode(bytes);
  }

  /// write file part
  /// [filename] comes from [file] if is not given by caller
  Future<void> writeFilePart(String name, File file, [String? filename]) async {
    filename = filename ?? file.uri.pathSegments.last;
    sink
      ..writeln(_boundaryBegin)
      ..writeln(
        'Content-Disposition: form-data; name="$name"; filename="$filename"',
      )
      ..writeln('Content-Type: application/octet-stream')
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
