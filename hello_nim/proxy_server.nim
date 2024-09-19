#[
///usr/bin/env nim r -d:ssl "$0" "$@" ; exit $?
]#

import std/[strutils, strformat, os, uri]
import std/[asyncnet, asynchttpserver, asyncdispatch, httpclient]

const PROXY = getEnv("HTTPS_PROXY", "http://localhost:55556")

proc respondStreaming(req: Request, res: AsyncResponse) {.async.} =
  # send status line
  await req.client.send("HTTP/1.1 " & $res.code() & "\c\L")
  # send headers
  await req.sendHeaders(res.headers)
  await req.client.send("\c\L")
  # send body
  while true:
    let (hasData, data) = await res.bodyStream.read()
    if hasData:
      await req.client.send(data)
    else:
      break

proc respondFully(req: Request, res: AsyncResponse) {.async.} =
  let body = await res.body
  req.respond(res.code(), body, res.headers)

proc getProxyTarget(req: Request): string =
  let q = req.url.query
  for kv in q.split('&'):
    if kv.startsWith("target="):
      return kv[len("target=") .. ^1]
  raise newException(ValueError, "Param not found: target")

proc cb(req: Request) {.async.} =
  var client = newAsyncHttpClient(proxy = newProxy(PROXY))
  defer:
    client.close()
  try:
    let target = getProxyTarget(req).parseUri()
    req.headers["host"] = target.hostname
    echo fmt"Fetching [{req.reqMethod}] {target}"
    var res = await client.request(
      url = target, httpMethod = req.reqMethod, headers = req.headers, body = req.body
    )
    await req.respondStreaming(res)
    # await req.respondFully(res)
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
