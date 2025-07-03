#[
///usr/bin/env nim r -d:ssl "$0" "$@" ; exit $?
]#

import
  std/[
    strutils,
    strformat,
    os,
    uri,
    logging,
    #
    asyncnet,
    asynchttpserver,
    asyncdispatch,
    #
    httpclient,
  ]

proc setupLogger() =
  let logger =
    newConsoleLogger(fmtStr = "$datetime $levelname - ", levelThreshold = lvlAll)
  addHandler(logger)

proc respondStreamingly(req: Request, res: AsyncResponse) {.async.} =
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

iterator queryParams(req: Request): (string, string) =
  let q = req.url.query
  for kvStr in q.split('&'):
    let kv = kvStr.split('=', maxsplit = 1)
    if len(kv) == 2:
      yield (kv[0], kv[1])

func queryParamFirstValue(req: Request, name: string): string =
  for (k, v) in req.queryParams():
    if k == name:
      return v
  return ""

proc getProxyTarget(req: Request): Uri =
  let target = req.queryParamFirstValue("target")
  if target == "":
    raise newException(ValueError, "Missing query param: target")
  return parseUri(target)

proc proxyForward(req: Request) {.async.} =
  let target = getProxyTarget(req)
  let `method` = req.reqMethod
  info "Forwarding to", " [", `method`, "] ", target
  req.headers["host"] = target.hostname
  #!fmt: off
  var client = newAsyncHttpClient()
  defer: client.close()
  var res = await client.request(
    url = target,
    httpMethod = `method`,
    headers = req.headers,
    body = req.body
  )
  #!fmt: on
  await req.respondStreamingly(res)

proc cb(req: Request) {.async.} =
  try:
    await req.proxyForward()
  except:
    error getCurrentExceptionMsg()
    await req.respond(Http500, getCurrentExceptionMsg())

proc startServer() {.async.} =
  var server = newAsyncHttpServer()
  server.listen(Port(8080))
  info "Serving at ", server.getPort().uint16()
  while true:
    if server.shouldAcceptRequest():
      await server.acceptRequest(cb)
    else:
      await sleepAsync(500)

setupLogger()
waitFor startServer()
