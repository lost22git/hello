import std/[strformat, strutils, sequtils, net, logging, typedThreads, exitprocs, oids]

func bigEndian16(data: openArray[char]): uint16 =
  (data[0].uint16 shl 8) + (data[1].uint16)

converter hostPortConvert(v: (string, Port)): (string, uint16) =
  let (host, port) = v
  return (host, port.uint16)

func `$`(v: (string, uint16)): string =
  fmt"{v[0]}:{v[1]}"

func `$`(v: (string, Port)): string =
  fmt"{v[0]}:{v[1]}"

setControlCHook(
  proc() {.noconv.} =
    echo "CTRL-C was pressed"
    quit()
)
addExitProc(
  proc() =
    echo "program is exiting"
)

# === Config ===

type ServerConfig = object
  host: string = "127.0.0.1"
  port: uint16 = 9933
  backlog: int32 = 128

type ClientConfig = object
  cnnTimeout = 3000

type Config = object
  server: ServerConfig
  client: ClientConfig
  logLevel: Level = lvlAll

when defined(release):
  let config = Config(logLevel: lvlInfo)
else:
  let config = Config()

# === Logging ===

let logger =
  newConsoleLogger(fmtStr = "$levelId$datetime - ", levelThreshold = config.logLevel)

addHandler(logger)

# === Proxy Protocol Handshake ===

type ProxyProtocol = enum
  Unknown
  Http
  Socks5

proc guessProxyProtocol(client: Socket): ProxyProtocol =
  ## guess proxy protocol

  var data = client.recv(1)
  if data[0].int == 0x05:
    return Socks5
  if data[0] == 'C':
    data = client.recv(6)
    if data == "ONNECT":
      return Http
  return Unknown

proc socks5ExtractServerAddr(client: Socket): (string, uint16) =
  ## extract remote server address from socks5 proxy handshake

  let addrType = client.recv(1)[0].int
  case addrType
  of 0x01: # IPV4
    var data: array[0 .. 3, uint8]
    discard client.recv(data.addr, 4)
    let ipv4 = IpAddress(family: IPv4, address_v4: data)
    let port = client.recv(2).bigEndian16()
    result = ($ipv4, port)
  of 0x03: # Domain name (len(1B)+name)
    let len = client.recv(1)[0].int
    let domain = client.recv(len)
    let port = client.recv(2).bigEndian16()
    result = (domain, port)
  of 0x04: # IPV6
    var data: array[0 .. 15, uint8]
    discard client.recv(data.addr, 16)
    let ipv6 = IpAddress(family: IPv6, address_v6: data)
    let port = client.recv(2).bigEndian16()
    result = ($ipv6, port)
  else:
    return

proc socks5Handshake(client: Socket): (string, uint16) =
  ## handle socks5 proxy handshake
  ##
  ## NOTE:
  ## we only support tcp and no auth now.

  let clientAddr = client.getPeerAddr()

  # 1. initial request
  let nauth = client.recv(1)[0].int
  discard client.recv(nauth)
  client.send("\x05\x00") # no auth
  info clientAddr, ": ", fmt"socks5 handshake: auth=0x00(no auth)"

  # 2. auth requeset (skip when no auth)

  # 3. client connection request
  let header = client.recv(3)
  # echo fmt"{header.repr =}"
  assert header[0].int == 0x05
  let cmd = header[1].int
  case cmd
  of 0x01: # establish a TCP/IP stream connection
    let serverAddr = socks5ExtractServerAddr(client)
    if serverAddr == default((string, uint16)):
      client.send("\x05\x08\x00\x01\x00\x00\x00\x00\x00\x00")
      raise
        newException(ValueError, "socks5 handshake error: address type not supported")
    else:
      info clientAddr, ": ", fmt"socks5 handshake: {serverAddr=}"
      client.send("\x05\x00\x00\x01\x00\x00\x00\x00\x00\x00")
      return serverAddr
  else:
    client.send("\x05\x07\x00\x01\x00\x00\x00\x00\x00\x00")
    raise newException(
      ValueError,
      fmt"socks5 handshake error: command({cmd}) not supported / protocol error",
    )

proc httpExtractServerAddr(client: Socket): (string, uint16) =
  ## extract remote server address from http proxy handshake
  ##
  ## NOTE:
  ## we find remote server address from `Host` header,
  ## and not authentication support now.

  var line = ""
  while true:
    client.readLine(line)
    if line == "\r\n":
      return
    if line.startsWith("Host: "):
      let split = line[6 ..^ 1].split(":")
      result = (split[0], split[1].parseInt().uint16)

proc httpHandshake(client: Socket): (string, uint16) =
  ## handle http proxy handshake

  let clientAddr = client.getPeerAddr()
  let serverAddr = httpExtractServerAddr(client)
  if serverAddr == default((string, uint16)):
    client.send("HTTP/1.1 400 Bad Request\r\nProxy-agent: MyProxy/1.0\r\n\r\n")
    raise newException(ValueError, "http handshake error: serverAddr not found")
  info clientAddr, ": ", fmt"http handshake: {serverAddr=}"
  client.send("HTTP/1.1 200 Connection established\r\nProxy-agent: MyProxy/1.0\r\n\r\n")
  return serverAddr

proc proxyProtocolHandshake(client: Socket): (string, uint16) =
  ## handle proxy protocol handshake
  ##
  ## supported proxy protocols:
  ## 1. http
  ## 2. socks5

  case guessProxyProtocol(client)
  of Http:
    result = httpHandshake(client)
  of Socks5:
    result = socks5Handshake(client)
  of Unknown:
    raise newException(ValueError, "unknown proxy protocol")

# === Client ===

type Client = ref object
  id: Oid
  config: ClientConfig
  sock: Socket
  remoteSock: Socket
  address: (string, uint16)
  remoteAddress: (string, uint16)
  runThread: Thread[Client]
  downstreamThread: Thread[Client]

func `$`(client: Client): string =
  return
    if client.remoteSock == nil:
      fmt"{client.id}<{client.address}>"
    else:
      fmt"{client.id}<{client.address},{client.remoteAddress}>"

proc close(client: Client) =
  ## close client

  if client.sock != nil:
    client.sock.close()
  if client.remoteSock != nil:
    client.remoteSock.close()

proc connectRemote(client: Client, remoteAddress: (string, uint16)) =
  ## connect to remote
  ##
  ## TODO:
  ## 1. doh resolve remoteAddress

  let remoteSock = newSocket(buffered = false)
  let (host, port) = remoteAddress
  remoteSock.connect(host, port.Port, timeout = client.config.cnnTimeout)
  client.remoteAddress = remoteAddress
  client.remoteSock = remoteSock

proc upstreaming(client: Client) =
  ## upstreaming
  ##
  ## TODO:
  ## 1. tls fragment to avoid exposing sni on tls handshake

  while true:
    let data = client.sock.recv(16384)
    if data == "":
      raise newException(ValueError, "upstream data is EOF (client is disconnected)")
    debug client, ": ", fmt"upstream {data.len=}"
    debug client, ": ", fmt"upstream {data=}"
    client.remoteSock.send(data)

proc downstreaming(client: Client) =
  ## downstreaming

  while true:
    let data = client.remoteSock.recv(16384)
    if data == "":
      raise newException(
        ValueError, "downstream data is EOF (remote server is disconnected)"
      )
    debug client, ": ", fmt"downstream {data.len=}"
    debug client, ": ", fmt"downstream {data=}"
    client.sock.send(data)

proc downstreamingThreadProc(client: Client) {.thread.} =
  ## a thread proc for downstreaming

  {.gcsafe.}:
    addHandler(logger)

  try:
    client.downstreaming()
  except Exception as e:
    if e.msg == "Bad file descriptor":
      return
    error client, ": ", fmt"downstream error: err={e.msg}"
    client.close()

proc streaming(client: Client) =
  ## client streaming
  ##
  ## main steps:
  ## 1. spawn a thread to downstreaming
  ## 2. upstreaming

  info client, ": ", "spawn to downstreaming"
  createThread(client.downstreamThread, downstreamingThreadProc, client)

  try:
    info client, ": ", "upstreaming"
    client.upstreaming()
  except Exception as e:
    if e.msg == "Bad file descriptor":
      return
    raise newException(ValueError, fmt"upstream error: err={e.msg}")

proc handleClient(client: Client) {.thread.} =
  ## handle a new client
  ##
  ## main steps:
  ## 1. handle proxy protocol handshake to get remote server address
  ## 2. connect to remote server address
  ## 3. client streaming

  {.gcsafe.}:
    addHandler(logger)

  info client, ": ", "client is connected"

  defer:
    info client, ": ", "client is closed"
    client.close()

  # 1. proxy protocol handshake
  var remoteAddress: (string, uint16)
  try:
    remoteAddress = proxyProtocolHandshake(client.sock)
  except Exception as e:
    error client, ": ", fmt"proxy protocol handshake error: err={e.msg}"
    return

  assert remoteAddress != default((string, uint16))

  # 2. client connect to remote server
  try:
    info client, ": ", fmt"connect remote server {remoteAddress}"
    client.connectRemote(remoteAddress)
  except Exception as e:
    error client, ": ", fmt"connect remote server error: err={e.msg}"
    return

  # 3. client streaming
  try:
    client.streaming()
  except Exception as e:
    error client, ": ", fmt"streaming error: err={e.msg}"
    return

# === Server ===

type Server = ref object
  config: ServerConfig
  sock: Socket
  runThread: Thread[Server]
  clientList: seq[Client]

proc close(server: Server) =
  ## close proxy server

  if server.sock != nil:
    server.sock.close()

proc closeAndWait(server: Server) =
  ## close proxy server and wait for it finished

  server.close()
  if server.runThread.running():
    joinThread(server.runThread)

proc start(server: Server) {.thread.} =
  ## start proxy server

  {.gcsafe.}:
    addHandler(logger)

  server.sock = newSocket(buffered = false)
  server.sock.setSockOpt(OptReusePort, true)
  server.sock.setSockOpt(OptReuseAddr, true)
  server.sock.bindAddr(Port(server.config.port), server.config.host)
  server.sock.listen(backlog = server.config.backlog)

  info fmt"server is listening at {server.sock.getLocalAddr()}"

  defer:
    info "server is closed"
    server.sock.close()
    for client in server.clientList:
      client.close()
    server.clientList.setLen(0)

  while true:
    server.clientList = server.clientList.filterIt(it.runThread.running())
    info fmt"{server.clientList.len=}"
    {.gcsafe.}:
      var client = Client(config: config.client, id: genOid())
    server.sock.accept(client.sock)
    client.address = client.sock.getPeerAddr()
    server.clientList.add(client)
    createThread(client.runThread, handleClient, client)

proc startAndWait(server: Server) =
  # start proxy server and wait for it finished
  createThread(server.runThread, start, server)
  joinThread(server.runThread)

var server = Server(config: config.server)
server.startAndWait()
