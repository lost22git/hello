import mummy, mummy/routers
import std/httpclient
import std/strformat
import std/httpcore

converter toWebbyHttpHeaders(headers: httpcore.HttpHeaders): httpheaders.HttpHeaders =
  for k, v in pairs(headers):
    result[k] = v

converter toStdHttpHeaders(headers: httpheaders.HttpHeaders): httpcore.HttpHeaders =
  result = newHttpHeaders()
  for (k, v) in headers:
    result[k] = v

proc indexHandler(request: Request) =
  request.respond(200, body = "Hello mummy")

proc proxyHandler(req: Request) =
  var client = newHttpClient()
  defer:
    client.close()
  try:
    let uri = "https://httpbin.org" & req.path
    echo fmt"Do request: [{req.httpMethod}] {uri}"
    req.headers["Host"] = "httpbin.org"
    var response = client.request(
      url = uri, httpMethod = req.httpMethod, body = req.body, headers = req.headers
    )
    req.respond(response.code().int, response.headers, response.body)
  except:
    req.respond(500, body = getCurrentExceptionMsg())

var router: Router
router.get("/", indexHandler)
router.get("/**", proxyHandler)
router.post("/**", proxyHandler)

let server = newServer(router)
echo "Server is running on :8000"
server.serve(Port(8000))
