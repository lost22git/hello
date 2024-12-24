import "dart:io";
import "dart:convert";

main() async {
  var client = HttpClient();

  try {
    var uri = Uri.parse("https://httpbin.org/ip");

    var request = await client.getUrl(uri);

    var response = await request.close();

    var data = await response.transform(utf8.decoder).join();

    var dataMap = jsonDecode(data);

    print("result:\n${dataMap}");
  } finally {
    client.close();
  }
}
