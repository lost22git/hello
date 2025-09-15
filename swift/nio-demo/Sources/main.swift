// The Swift Programming Language
// https://docs.swift.org/swift-book

import Logging
import NIOCore
import NIOPosix


class MyHandler : ChannelDuplexHandler {
    public typealias OutboundIn = ByteBuffer
    public typealias InboundIn = ByteBuffer
    public typealias InboundOut = ByteBuffer

    lazy var log: Logger = Logger(label: "MyHandler")

    // handler
    
    func handlerAdded(context: ChannelHandlerContext) {
        log[metadataKey: "channel-id"] = "\(ObjectIdentifier(context.channel))"
        log.info("handlerAdded")
    }

    func handlerRemoved(context: ChannelHandlerContext) {
        log.info("handlerRemoved")
    }

    // outbound
    
    func register(context: ChannelHandlerContext, promise: EventLoopPromise<Void>?) {
        log.info("register")
        context.register(promise: promise)
    }

    func write(context: ChannelHandlerContext, data: NIOAny, promise: EventLoopPromise<Void>?) {
        do {
            let buffer = self.unwrapOutboundIn(data)
            let str = try buffer.peekUTF8ValidatedString(length: buffer.readableBytes)
            log.info("write: \(str ?? "INVALID UTF-8")")
            context.write(data, promise: promise)
        } catch {
            context.fireErrorCaught(error)
        }
    }
    
    func flush(context: ChannelHandlerContext) {
        log.info("flush")
        context.flush()
    }

    func read(context: ChannelHandlerContext) {
        log.info("read")
        context.read()
    }

    func close(context: ChannelHandlerContext, mode: CloseMode, promise: EventLoopPromise<Void>?) {
        log.info("close: \(mode)")
        context.close(mode: mode, promise: promise)
    }

    func triggerUserOutboundEvent(context: ChannelHandlerContext, event: Any, promise: EventLoopPromise<Void>?) {
        log.info("triggerUserOutboundEvent: \(event)")
        context.triggerUserOutboundEvent(event, promise: promise)
    }
 
    // inbound

    func channelRegistered(context: ChannelHandlerContext) {
        log.info("channelRegistered")
        context.fireChannelRegistered()
    }

    func channelUnregistered(context: ChannelHandlerContext) {
        log.info("channelUnregistered")
        context.fireChannelUnregistered()   
    }

    func channelActive(context: ChannelHandlerContext) {
        log.info("channelActive")
        context.fireChannelActive()
    }

    func channelInactive(context: ChannelHandlerContext) {
        log.info("channelInactive")
        context.fireChannelInactive()
    }

    func channelRead(context: ChannelHandlerContext, data: NIOAny) {
        do {
            let buffer = self.unwrapInboundIn(data)
            let str = try buffer.peekUTF8ValidatedString(length: buffer.readableBytes)
            log.info("channelRead: \(str ?? "INVALID UTF-8")")
            let _ = context.channel.write(buffer)
        } catch {
            context.fireErrorCaught(error)
        }
    }

    func channelReadComplete(context: ChannelHandlerContext) {
        log.info("channelReadComplete")   
        context.channel.flush()
    }

    func channelWritabilityChanged(context: ChannelHandlerContext) {
        log.info("channelWritabilityChanged")   
        context.fireChannelWritabilityChanged()
    }

    func userInboundEventTriggered(context: ChannelHandlerContext, event: Any) {
        log.info("userInboundEventTriggered")
        context.fireUserInboundEventTriggered(event)
    }

    func errorCaught(context: ChannelHandlerContext, error: Error) {
        log.error("errorCaught", metadata: ["error": "\(error)"])   
        let _ = context.channel.close()
    }

}


let log = Logger(label: "Main")

let host = "127.0.0.1"
let port = 9933
let nthread = System.coreCount

let elgroup = MultiThreadedEventLoopGroup(numberOfThreads:nthread)
defer {
    log.info("Shutdown gracefully")
    elgroup.shutdownGracefully { err in
        if let ex = err {
            log.error("Error on shutdown gracefully",
                metadata: ["error":"\(ex)"])
        }
    }
}

let channelFut = ServerBootstrap(group: elgroup)
    .serverChannelOption(.backlog, value: 128)
    .serverChannelOption(.socketOption(.so_reuseaddr), value: 1)
    .childChannelInitializer { channel in
        channel.eventLoop.makeCompletedFuture {
            try channel.pipeline.syncOperations.addHandler(MyHandler())
        }
    }
    .childChannelOption(.socketOption(.so_reuseaddr), value: 1)
    .childChannelOption(.tcpOption(.tcp_nodelay), value: 1)
    .childChannelOption(.maxMessagesPerRead, value: 16)
    .childChannelOption(.recvAllocator, value: AdaptiveRecvByteBufferAllocator())
    .bind(host: host, port: port)
let channel  = try channelFut.wait()
log.info("Listening on \(channel.localAddress!)")
try channel.closeFuture.wait()
