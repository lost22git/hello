require "socket"

module Netty
  enum CloseMode
    All
    Read
    Write
  end

  # === ChannelOption ===

  private macro defoptions(*options)
    record ChannelOption(T), name : ChannelOptionName, type : T.class
    enum ChannelOptionName
      {% for o in options %}
        {{ o.var.camelcase }}      
      {% end %}
    end

    module ChannelOptions
      {% for o in options %}
        class_getter {{ o.var }} = ChannelOption({{ o.type }}).new(name: :{{ o.var }}, type: {{ o.type }})
      {% end %}
    end
  end

  defoptions tcp_nodelay : Bool,
    so_broadcast : Bool,
    so_keepalive : Bool,
    so_linger : Int32,
    so_reuseaddr : Bool,
    so_reuseport : Bool,
    so_rcvbuf : Int32,
    so_sndbuf : Int32,
    auto_read : Bool,
    backlog : Int32,
    connect_timeout : Time::Span,
    read_timeout : Time::Span,
    write_timeout : Time::Span

  # === ChannelHandler ===

  module ChannelHandler
    def handler_added(context : ChannelHandlerContext)
    end

    def handler_removed(context : ChannelHandlerContext)
    end
  end

  module ChannelInboundHandler
    include ChannelHandler

    def channel_active(context : ChannelHandlerContext)
      context.fire_channel_active
    end

    def channel_inactive(context : ChannelHandlerContext)
      context.fire_channel_inactive
    end

    def channel_read(context : ChannelHandlerContext, data)
      context.fire_channel_read(data)
    end

    def channel_read_complete(context : ChannelHandlerContext)
      context.fire_channel_read_complete
    end

    def error_caught(context : ChannelHandlerContext, ex : Exception)
      context.fire_error_caught(ex)
    end
  end

  module ChannelOutboundHandler
    include ChannelHandler

    def read(context : ChannelHandlerContext)
      context.read
    end

    def write(context : ChannelHandlerContext, data)
      context.write(data)
    end

    def flush(context : ChannelHandlerContext)
      context.flush
    end

    def close(context : ChannelHandlerContext, mode : CloseMode = CloseMode::All)
      context.close(mode)
    end
  end

  # === ChannelInvoker ===

  module ChannelInboundInvoker
    abstract def fire_channel_active
    abstract def fire_channel_inactive
    abstract def fire_channel_read(data)
    abstract def fire_channel_read_complete
    abstract def fire_error_caught(ex : Exception)
  end

  module ChannelOutboundInvoker
    abstract def read
    abstract def write(data)
    abstract def flush
    abstract def close(mode : CloseMode = CloseMode::All)
  end

  module ChannelInvoker
    include ChannelInboundInvoker
    include ChannelOutboundInvoker
  end

  # === ChannelHandlerContext ===

  module ChannelHandlerContext
    include ChannelInvoker

    abstract def name : String
    abstract def handler : ChannelHandler
    abstract def pipeline : ChannelPipeline
    abstract def channel : Channel
  end

  class DChannelHandlerContext
    include ChannelHandlerContext

    getter name : String

    getter pipeline : ChannelPipeline

    @inbound_handler : ChannelInboundHandler?
    @outbound_handler : ChannelOutboundHandler?

    property prev : ChannelHandlerContext?
    property next : ChannelHandlerContext?

    def initialize(handler, @name, @pipeline)
      @inbound_handler = handler.as?(ChannelInboundHandler)
      @outbound_handler = handler.as?(ChannelOutboundHandler)
    end

    def handler : ChannelHandler
      return (@inbound_handler || @outbound_handler).not_nil!
    end

    def channel : Channel
      return self.pipeline.channel
    end

    private def invoke_handler_added
      self.handler.handler_added(self)
    end

    private def invoke_handler_removed
      self.handler.handler_removed(self)
    end

    # --- impl ChannelInboundInvoker ---

    {% for m in ChannelInboundInvoker.methods %}
      {% handler_method = m.name.[("fire_".size)..] %}
      {% args = m.args.splat %}
      {% args_name = m.args.map(&.name).splat %}

      def {{ m.name }}({{ args }})
        if cx = @next
          cx.invoke_{{ handler_method }}({{ args_name }})
        end
      end

      private def invoke_{{ handler_method }}({{ args }})
        if h = @inbound_handler
          h.{{ handler_method }}(self, {{ args_name }})
        else
          if cx = @next
            cx.invoke_{{ handler_method }}({{ args_name }})
          end
        end
      end
    {% end %}

    # --- impl ChannelOutboundInvoker ---

    {% for m in ChannelOutboundInvoker.methods %}
      {% handler_method = m.name %}
      {% args = m.args.splat %}
      {% args_name = m.args.map(&.name).splat %}

      def {{ m.name }}({{ args }})
        if cx = @prev
          cx.invoke_{{ handler_method }}({{ args_name }})
        end
      end

      private def invoke_{{ handler_method }}({{ args }})
        if h = @outbound_handler
          h.{{ handler_method }}(self, {{ args_name }})
        else
          if cx = @prev
            cx.invoke_{{ handler_method }}({{ args_name }})
          end
        end
      end
    {% end %}
  end

  # === ChannelPipeline ===

  module ChannelPipeline
    include ChannelInvoker

    abstract def channel : Channel
    abstract def add_handler_first(handler : ChannelHandler, name : String)
    abstract def add_handler_last(handler : ChannelHandler, name : String)
    abstract def add_handler_before(handler : ChannelHandler, name : String, relative : ChannelHandlerContext | String | ChannelHandler)
    abstract def add_handler_after(handler : ChannelHandler, name : String, relative : ChannelHandlerContext | String | ChannelHandler)
    abstract def remove_handler(target : ChannelHandlerContext | ChannelHandler | String)
  end

  private class HeadChannelHandler
    include ChannelOutboundHandler

    class_getter name = "Head"
    class_getter singleton = HeadChannelHandler.new

    def initialize
    end

    def make_context(pipeline : ChannelPipeline)
      return ChannelHandlerContext.new(self, @@name, pipeline)
    end

    {% for m in ChannelOutboundHandler.methods %}
      {% args = m.args.splat %}
      {% first_arg_name = m.args[0].name %}
      {% rest_args_name = m.args[1..].map(&.name).splat %}

      def {{ m.name }}({{ args }})
        {{ first_arg_name }}.channel.internal.{{ m.name }}0({{ rest_args_name }})
      end
    {% end %}
  end

  private class TailChannelHandler
    include ChannelInboundHandler

    class_getter name = "Tail"
    class_getter singleton = TailChannelHandler.new

    def initialize
    end

    def make_context(pipeline : ChannelPipeline)
      return ChannelHandlerContext.new(self, @@name, pipeline)
    end
  end

  class DChannelPipeline
    include ChannelPipeline

    getter channel : Channel

    @head : ChannelHandlerContext?
    @tail : ChannelHandlerContext?

    def initialize(@channel)
      @head = HeadChannelHandler.singleton.make_context(self)
      @tail = TailChannelHandler.singleton.make_context(self)
      @head.next = @tail
      @tail.prev = @head
    end

    private def find_handler_context(&) : ChannelHandlerContext?
      c = @head
      while c
        return c if yield c
        c = c.next
      end
    end

    private def first_inbound_context : ChannelHandlerContext?
      @head.try &.next
    end

    private def first_outbound_context : ChannelHandlerContext?
      @tail.try &.prev
    end

    def add_handler_first(handler : ChannelHandler, name : String)
      self.add_handler_after(handler, name, @head)
    end

    def add_handler_last(handler : ChannelHandler, name : String)
      self.add_handler_before(handler, name, @tail)
    end

    def add_handler_before(handler : ChannelHandler, name : String, relative : ChannelHandlerContext | String | ChannelHandler)
      rc = case relative
           when ChannelHandlerContext then relative
           when ChannelHandler        then find_handler_context { |c| c.handler == handler }
           when String                then find_handler_context { |c| c.name == name }
           end

      unless rc
        raise "Can't add handler since relative not found in Pipeline"
      end

      unless rc != @head
        raise "Can't add handler before Head in Pipeline"
      end

      c = ChannelHandlerContext.new(handler, name, self)
      c.prev = rc.prev
      c.prev.not_nil!.next = c
      c.next = rc
      rc.prev = c
      c.invoke_handler_added
    end

    def add_handler_after(handler : ChannelHandler, name : String, relative : ChannelHandlerContext | String | ChannelHandler)
      rc = case relative
           when ChannelHandlerContext then relative
           when ChannelHandler        then find_handler_context { |c| c.handler == handler }
           when String                then find_handler_context { |c| c.name == name }
           end

      unless rc
        raise "Can't add handler since relative not found in Pipeline"
      end

      unless rc != @tail
        raise "Can't add handler after Tail in Pipeline"
      end

      c = ChannelHandlerContext.new(handler, name, self)
      c.next = rc.next
      c.next.not_nil!.prev = c
      c.prev = rc
      rc.next = c
      c.invoke_handler_added
    end

    def remove_handler(target : ChannelHandlerContext | ChannelHandler | String)
      c = case target
          when ChannelHandlerContext then target
          when ChannelHandler        then find_handler_context { |c| c.handler == target }
          when String                then find_handler_context { |c| c.name == target }
          end

      unless c
        rasie "Can't remove handler since target not found in Pipeline"
      end

      unless c != @head && c != @tail
        raise "Can't remove Head or Tail in Pipeline"
      end

      c.prev.not_nil!.next = c.next
      c.next.not_nil!.prev = c.prev
      c.prev = nil
      c.next = nil
      c.invoke_handler_removed
    end

    # --- impl ChannelInboundInvoker ---

    {% for m in ChannelInboundInvoker.methods %}
      {% handler_method = m.name.[("fire_".size)..] %}
      {% args = m.args.splat %}
      {% args_name = m.args.map(&.name).splat %}

      def {{ m.name }}({{ args }})
        if cx = self.first_inbound_context
          cx.invoke_{{ handler_method }}({{ args_name }})
        end
      end
    {% end %}

    # --- impl ChannelOutboundInvoker ---

    {% for m in ChannelOutboundInvoker.methods %}
      {% handler_method = m.name %}
      {% args = m.args.splat %}
      {% args_name = m.args.map(&.name).splat %}

      def {{ m.name }}({{ args }})
        if cx = self.first_outbound_context
          cx.invoke_{{ handler_method }}({{ args_name }})
        end
      end
    {% end %}
  end

  # === Channel ===

  module ChannelInternal
    abstract def read0
    abstract def write0(data)
    abstract def flush0
    abstract def close0(mode : CloseMode = CloseMode::All)
  end

  module Channel
    include ChannelOutboundInvoker

    abstract def internal : ChannelInternal
    abstract def pipeline : ChannelPipeline?

    abstract def set_option(option : ChannelOption(T), val : T) forall T
    abstract def get_option(option : ChannelOption(T)) forall T
  end

  class DChannel
    include ChannelInternal
    include Channel

    getter pipeline : ChannelPipeline?

    def initialize
      @pipeline = ChannelPipeline.new(self)
    end

    def internal : ChannelInternal
      self
    end

    def set_option(option : ChannelOption(T), val : T) forall T
      # TODO
    end

    def get_option(option : ChannelOption(T)) forall T
      # TODO
    end

    # --- impl ChannelInternal ---

    def read0
      # TODO
    end

    def write0(data)
      # TODO
    end

    def flush0
      # TODO
    end

    def close0(mode : CloseMode = CloseMode::All)
      # TODO
    end

    # --- impl ChannelOutboundInvoker ---

    {% for m in ChannelOutboundInvoker.methods %}
      {% handler_method = m.name %}
      {% args = m.args.splat %}
      {% args_name = m.args.map(&.name).splat %}

      def {{ m.name }}({{ args }})
        if p = @pipeline
          p.{{ handler_method }}({{ args_name }})
        end
      end
    {% end %}
  end
end
