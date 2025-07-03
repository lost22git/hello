#[
///usr/bin/env nim r -d:ssl "$0" "$@" ; exit $?
]#

import std/[strutils, strformat, os, uri]
import std/logging
import std/[asyncnet, asynchttpserver, asyncdispatch, httpclient]

const PROXY = getEnv("HTTPS_PROXY", "http://localhost:55556")

let globalLogger =
  newConsoleLogger(fmtStr = "$datetime $levelname - ", levelThreshold = lvlAll)

addHandler(globalLogger)

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
  raise newException(ValueError, "Missing query param: target")

proc proxyForward(req: Request) {.async.} =
  let target = req.getProxyTarget().parseUri()
  req.headers["host"] = target.hostname
  info "Forwarding", " [", req.reqMethod, "] ", target
  var client = newAsyncHttpClient()
  # var client = newAsyncHttpClient(proxy = newProxy(PROXY))
  defer:
    client.close()
  #!fmt: off
  var res = await client.request(
    url = target, 
    httpMethod = req.reqMethod, 
    headers = req.headers, 
    body = req.body
  )
  #!fmt: on
  await req.respondStreaming(res)
  # await req.respondFully(res)

proc cb(req: Request) {.async.} =
  try:
    await proxyForward(req)
  except:
    await req.respond(Http500, getCurrentExceptionMsg())

proc main() {.async.} =
  var server = newAsyncHttpServer()
  server.listen(Port(8080))
  info "Serving at ", server.getPort().uint16()
  while true:
    if server.shouldAcceptRequest():
      await server.acceptRequest(cb)
    else:
      await sleepAsync(500)

waitFor main()
