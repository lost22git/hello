import 'dart:convert';
import 'dart:io';

import 'package:imgur/multipart.dart';
import 'package:imgur/result.dart';

void main(List<String> arguments) async {
  var file = File(arguments[0]);
  print(file.path);
  var result = await upload(file);
  print(result.data.datetime);
  print(result.data.link);
}

Future<Result> upload(File file) async {
  var url = Uri.parse(
    "https://api.imgur.com/3/image?client_id=546c25a59c58ad7",
  );

  var client = HttpClient();
  try {
    var request = await client.postUrl(url);

    var multipart = Multipart.sink(request);
    request.headers.set('Content-Type', multipart.contentType());
    await multipart.writeFilePart('image', file);
    multipart.done();

    var response = await request.close();
    var data = await response.transform(utf8.decoder).join();
    print(data);

    return Result.fromJson(jsonDecode(data));
  } finally {
    client.close();
  }
}
