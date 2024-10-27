struct Buffer(T)
  include Indexable::Mutable(T)
  getter cap : Int32

  def initialize(@cap : Int32)
    @data = Pointer(T).malloc(cap)
    @head = @tail = 0
  end

  @[AlwaysInline]
  def unsafe_fetch(index : Int)
    @data[mask(@head + index)]
  end

  @[AlwaysInline]
  def unsafe_put(index : Int, value : T)
    @data[mask(@head + index)] = value
  end

  def size : Int32
    result = @tail - @head
    return result if result >= 0
    result + @cap * 2
  end

  @[AlwaysInline]
  def empty? : Bool
    size() == 0
  end

  @[AlwaysInline]
  def full? : Bool
    size() == @cap
  end

  @[AlwaysInline]
  private def mask(index : Int32) : Int32
    index % @cap
  end

  @[AlwaysInline]
  private def mask2(index : Int32) : Int32
    index % (2 * @cap)
  end

  @[AlwaysInline]
  private def dec(index : Int32, count : Int32 = 1) : Int32
    index + (2 * @cap) - count
  end

  @[AlwaysInline]
  private def inc(index : Int32, count : Int32 = 1) : Int32
    index + count
  end

  def add_head(value : T)
    add_head(value) { raise "addHead: Buffer is full" }
  end

  def del_head : T?
    del_head { return nil }
  end

  def add_tail(value : T)
    add_tail(value) { raise "addTail: Buffer is full" }
  end

  def del_tail : T?
    del_tail { return nil }
  end

  def add_head(value : T, &)
    yield if full?
    @head = head = mask2(dec(@head))
    # puts "[add_head] #{self}"
    @data[mask(head)] = value
  end

  def del_head(&)
    yield if empty?
    head = @head
    @head = mask2(inc(head))
    # puts "[del_head] #{self}"
    @data[mask(head)]
  end

  def add_tail(value : T, &)
    yield if full?
    tail = @tail
    @tail = mask2(inc(tail))
    # puts "[add_tail] #{self}"
    @data[mask(tail)] = value
  end

  def del_tail(&)
    yield if empty?
    @tail = tail = mask2(dec(@tail))
    # puts "[del_tail] #{self}"
    @data[mask(tail)]
  end
end

require "spec"

describe Buffer do
  it "add/del" do
    buf = Buffer(String).new(3)
    buf.size.should eq 0
    buf.empty?.should be_true

    buf.del_head.should be_nil
    buf.del_tail.should be_nil

    buf.add_head "halo"
    buf.size.should eq 1
    buf.add_tail "foo"
    buf.size.should eq 2
    buf.add_tail "bar"
    buf.size.should eq 3
    buf.full?.should be_true
    expect_raises Exception do
      buf.add_head "zzz"
    end

    buf.del_head.should eq "halo"
    buf.size.should eq 2
    buf.del_tail.should eq "bar"
    buf.size.should eq 1
    buf.del_head.should eq "foo"
    buf.size.should eq 0
    buf.empty?.should be_true
    buf.del_tail.should be_nil
  end

  it "indexable" do
    buf = Buffer(String).new(3)
    buf.add_tail "foo"
    buf.add_tail "bar"

    buf[0] = "zzz"
    buf[0].should eq "zzz"
    buf[2]?.should be_nil

    buf.each_with_index do |i, v|
      v.should eq "bar" if i == 1
    end
  end
end
