import 'dart:convert';

import 'package:imgur/result.dart';
import 'package:imgur/result_data.dart';
import 'package:test/test.dart';

const unixTime = 1750680432;

void main() {
  test('result json', testResultJson);
}

void testResultJson() {
  var result = Result(
    111,
    true,
    ResultData(
      "3192898d-3cc1-439c-b81f-f034a17746a2",
      "cf191115-7fe4-4b14-8329-54e115ddd50d",
      "image/png",
      110,
      110,
      1101010,
      DateTime.fromMillisecondsSinceEpoch(unixTime * 1000, isUtc: true),
      "https://imge.xyz/abc",
      <String>["animal", "bird"],
    ),
  );

  var json = jsonEncode(result);

  var expectedJson =
      """{
              "status":111,
              "success":true,
              "data":{
                "id":"3192898d-3cc1-439c-b81f-f034a17746a2",
                "deletehash":"cf191115-7fe4-4b14-8329-54e115ddd50d",
                "type":"image/png",
                "width":110,
                "height":110,
                "size":1101010,
                "datetime":$unixTime,
                "link":"https://imge.xyz/abc",
                "tags":["animal","bird"]
              }
            }"""
          .replaceAll(RegExp(r"\s"), "");

  assert(expectedJson == json);

  var result2 = Result.fromJson(jsonDecode(json));

  assert(result.data.datetime == result2.data.datetime);
}
