# Swift-Nio Demo

## Client Connect Flow

## Server Listen Flow

## Accepted Client Flow

A client accepted:

- handlerAdded
- register
- channelRegistered
- channelActive
- read

A message received from client:

- channelRead
- channelReadComplete
- read

A message sent to client:

- write
- flush

A client disconnnected:

- channelInactive
- channelUnregistered
- handlerRemoved

A channel closed:

- close
- channelInactive
- channelUnregistered
- handlerRemoved

An error occurred:

- errorCaught

## Components

### Channel

- Channel: wrapper of ChannelCore & ChannelPipeline
- ChannelCore: wrapper of nonblocking socket
- ChannelPipeline: pipeline of ChannelHandler(s)
- ChannelHandlerContext: a node of ChannelPipeline, a context of ChannelHandler
- ChannelHandler: a procedure of ChannelPipeline to handle messages of Channel; accesses Channel, ChannelPipeline and other components via ChannelHandlerContext

channels sources:
- SelectableChannel.swift
- BaseSocketChannel.swift
- BaseStreamSocketChannel.swift
- SocketChannel.swift

### ByteBuffer

### EventLoop

- SelectableEventLoop

## Compared with Netty

- ByteBufferAllocator: system memory allocator, no Arena memory pool
- ByteBuffer: Swift ARC rather than refcount manually
- no ChannelAttributes
