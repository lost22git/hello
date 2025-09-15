// The Swift Programming Language
// https://docs.swift.org/swift-book

import NIOCore
import NIOPosix

class MyHandler : ChannelDuplexHandler {
    public typealias OutboundIn = ByteBuffer
    public typealias InboundIn = ByteBuffer
    public typealias InboundOut = ByteBuffer
}


let host = "127.0.0.1"
let port = 9933
let nthread = System.coreCount
let elgroup = MultiThreadedEventLoopGroup(numberOfThreads:nthread)
defer {
    elgroup.shutdownGracefully { err in
        if let ex = err {
            print("Error on shutdown gracefully: err=\(ex)")       
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
print("Listening on \(channel.localAddress!)")
let _ = Task {
    sleep(3)
    let _ = channel.close(mode: .all)
}
try channel.closeFuture.wait()
print("End")


