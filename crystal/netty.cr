require "socket"

module Netty
  enum CloseMode
    All
    Read
    Write
  end

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

  class ChannelHandlerContext
    include ChannelInvoker

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

    def channel : Channel
      return self.pipeline.channel
    end

    def handler : ChannelHandler
      return (@inbound_handler || @outbound_handler).not_nil!
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

  # === Channel ===

  class ChannelCore
  end

  class Channel
    # include ChannelOutboundInvoker

    getter core : ChannelCore

    getter pipeline : ChannelPipeline?

    def initialize
      @core = ChanneCore.new
      @pipeline = ChannelPipeline.new(self)
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

  # === ChannelPipeline ===

  private class HeadChannelHandler
    include ChannelOutboundHandler

    class_getter name = "Head"
    class_getter singleton = HeadChannelHandler.new

    def initialize
    end

    def make_context(pipeline : ChannelPipeline)
      return ChannelHandlerContext.new(self, @@name, pipeline)
    end

    def read(context : ChannelHandlerContext)
      context.channel.core.read
    end

    def write(context : ChannelHandlerContext, data)
      context.channel.core.write(data)
    end

    def flush(context : ChannelHandlerContext)
      context.channel.core.flush
    end

    def close(context : ChannelHandlerContext, mode : CloseMode = CloseMode::All)
      context.channel.core.close(mode)
    end
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

    def channel_read(context : ChannelHandlerContext, data)
      context.channel.core.channel_read(data)
    end

    def error_caught(context : ChannelHandlerContext, ex : Exception)
      context.channel.core.error_caught(ex)
    end
  end

  class ChannelPipeline
    include ChannelInvoker

    getter channel : Channel

    @head : ChannelHandlerContext?
    @tail : ChannelHandlerContext?

    def initialize(@channel)
      @head = HeadChannelHandler.singleton.make_context(self)
      @tail = TailChannelHandler.singleton.make_context(self)
      @head.next = @tail
      @tail.prev = @head
    end

    def add_handler_at_first(handler : ChannelHandler, name : String)
      self.add_handler_after(handler, name, @head)
    end

    def add_handler_at_last(handler : ChannelHandler, name : String)
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

    def find_handler_context(&) : ChannelHandlerContext?
      c = @head
      while c
        return c if yield c
        c = c.next
      end
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

    private def first_inbound_context : ChannelHandlerContext?
      @head.try &.next
    end

    private def first_outbound_context : ChannelHandlerContext?
      @tail.try &.prev
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
end
