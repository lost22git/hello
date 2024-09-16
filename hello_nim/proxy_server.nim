#[
///usr/bin/env nim r -d:ssl "$0" "$@" ; exit $?
]#

import std/[strutils, strformat, os]
import std/[asyncnet, asynchttpserver, asyncdispatch, httpclient]

const PROXY = getEnv("HTTPS_PROXY", "http://localhost:55556")

proc respondStreaming(req: Request, resp: AsyncResponse) {.async.} =
  # send status line
  await req.client.send("HTTP/1.1 " & $resp.code() & "\c\L")
  # send headers
  await req.sendHeaders(resp.headers)
  await req.client.send("\c\L")
  # send body
  while true:
    let (hasData, data) = await resp.bodyStream.read()
    if not hasData:
      break
    else:
      await req.client.send(data)

proc respondFully(req: Request, resp: AsyncResponse) {.async.} =
  let body = await resp.body
  req.respond(resp.code(), body, resp.headers)

proc cb(req: Request) {.async.} =
  req.headers["host"] = "httpbin.org"
  var client = newAsyncHttpClient(proxy = newProxy(PROXY))
  defer:
    client.close()
  try:
    let uri = fmt"https://httpbin.org{req.url.path}"
    echo fmt"Do request: [{req.reqMethod}] {uri}"
    var resp = await client.request(
      url = uri, httpMethod = req.reqMethod, headers = req.headers, body = req.body
    )
    await req.respondStreaming(resp)
    # await req.respondFully(resp)
  except:
    await req.respond(Http500, getCurrentExceptionMsg())

proc main() {.async.} =
  var server = newAsyncHttpServer()
  server.listen(Port(8000))
  echo fmt"Proxy server is running on {server.getPort().uint16()}"
  while true:
    if server.shouldAcceptRequest():
      await server.acceptRequest(cb)
    else:
      await sleepAsync(500)

waitFor main()
