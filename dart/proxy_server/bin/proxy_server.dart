import 'dart:io';

void main(List<String> arguments) async {
  var server = await HttpServer.bind('0.0.0.0', 8080);
  print('Serving on ${server.address.address}:${server.port}');
  await server.forEach(handle);
}

void handle(HttpRequest request) {
  try {
    switch (request.uri.path) {
      case "/bug":
        throw "Boom!!!";
      case "/alive":
        request.response.ok("Hello");
      case "/proxy":
        proxyForward(request).catchError((e) {
          print('ERROR in proxyForward: $e');
          request.response.serverError();
        });
      default:
        request.response.notFound();
    }
  } catch (e) {
    print('ERROR in handle: $e');
    request.response.serverError();
  }
}

Future<void> proxyForward(HttpRequest request) async {
  var target = request.uri.queryParameters['target'];
  if (target == null) {
    request.response.badRequest('Missing query param: target');
    return;
  }

  var targetUrl = Uri.parse(target);
  var method = request.method;
  print("Proxy forwarding: [$method] $targetUrl - ${request.contentLength}");

  var client = HttpClient();
  try {
    var clientRequest = await client.openUrl(method, targetUrl);
    var clientResponse = await clientRequest.exchange(request);
    await clientResponse.pipe2(request.response);
  } finally {
    client.close();
  }
}

// === Extensions ===

extension on HttpClientRequest {
  Future<HttpClientResponse> exchange(HttpRequest httpRequest) async {
    headers.clear();
    httpRequest.headers.forEach(
      (k, vs) => headers.add(k, vs, preserveHeaderCase: true),
    );
    headers.host = uri.host;
    await addStream(httpRequest);
    return await close();
  }
}

extension on HttpClientResponse {
  Future<void> pipe2(HttpResponse httpResponse) async {
    httpResponse.statusCode = statusCode;
    httpResponse.headers.clear();
    headers.forEach(
      (k, vs) => httpResponse.headers.add(k, vs, preserveHeaderCase: true),
    );
    await pipe(httpResponse);
  }
}

extension on HttpResponse {
  void ok([Object obj = ""]) {
    statusCode = 200;
    write(obj);
    close();
  }

  void badRequest([Object obj = ""]) {
    statusCode = 400;
    write(obj);
    close();
  }

  void notFound() {
    statusCode = 404;
    close();
  }

  void serverError() {
    statusCode = 500;
    close();
  }
}
