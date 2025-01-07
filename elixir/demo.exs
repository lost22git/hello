#!/usr/bin/env elixir

ExUnit.start()

defmodule DemoTest do
  use ExUnit.Case, async: true

  @doc """
  floor divide and modulo
  """
  def fdiv_mod(x, y) do
    {Integer.floor_div(x, y), Integer.mod(x, y)}
  end

  @doc """
  truncate divide and remain
  """
  def tdiv_rem(x, y) do
    {div(x, y), rem(x, y)}
  end

  test "fdiv_mod" do
    assert fdiv_mod(-5, 3) == {-2, 1}
    assert fdiv_mod(5, -3) == {-2, -1}
    assert fdiv_mod(-5, -3) == {1, -2}
    assert fdiv_mod(5, 3) == {1, 2}
  end

  test "tdiv_rem" do
    assert tdiv_rem(-5, 3) == {-1, -2}
    assert tdiv_rem(5, -3) == {-1, 2}
    assert tdiv_rem(-5, -3) == {1, -2}
    assert tdiv_rem(5, 3) == {1, 2}
  end

  test "bool: ternary operator" do
    # in Elixir - <cond> && <expr> || <expr>
    # in Lua - <cond> and <expr> or <expr>
    # in Java - <cond> ? <expr> : <expr>
    v = true
    assert ((v && "true") || "false") == "true"
    v = false
    assert ((v && "true") || "false") == "false"
  end

  # in Elixir - <expr> && <expr> || <expr>
  # in Lua - <expr> and <expr> or <expr>
  test "nil: ternary operator" do
    v = nil
    assert ((v && 1) || :default) == :default
    # short version of (v && v || :default)
    assert (v || :default) == :default

    v = "Elixir"
    assert ((v && 1) || :default) == 1
    # short version of (v && v || :default)
    assert (v || :default) == v
  end

  test "bool: -> error" do
    v = false
    assert_raise RuntimeError, fn -> v || raise "assert not false" end
  end

  test "nil: -> error" do
    v = nil
    assert_raise RuntimeError, fn -> v || raise "assert not nil" end
  end

  test "atom: special atoms" do
    # assert :true == true
    # assert :false == false
    # assert :nil == nil
  end

  test "string: concat" do
    v = "Elixir"
    assert "the " <> v <> " book" == "the Elixir book"
  end

  test "string: interpolation" do
    v = "Elixir"
    assert "the #{v} book" == "the Elixir book"
  end

  # absolute indentation
  test "string: multilines" do
    v = "
the
  Elixir
book
"
    assert v == "\nthe\n  Elixir\nbook\n"
  end

  test "string: size" do
    v = "👨‍👩‍👦‍👦"
    assert byte_size(v) == 25
    assert String.length(v) == 1
  end

  test "string: codepoints and graphemes" do
    v = "👨‍👩‍👦‍👦"
    assert String.codepoints(v) == ["👨", "\u200d", "👩", "\u200d", "👦", "\u200d", "👦"]
    assert String.graphemes(v) == ["👨‍👩‍👦‍👦"]
    assert String.to_charlist(v) == [128_104, 8205, 128_105, 8205, 128_102, 8205, 128_102]
  end

  test "string: parse number" do
    assert String.to_integer("-10") == -10
    assert String.to_integer("1010", 2) == 10
    assert String.to_float("10.10") == 10.10
    assert String.to_float("1.0e-10") == 1.0e-10

    assert_raise ArgumentError, fn -> String.to_integer("foo") end
  end

  test "string: <-> atom" do
    assert String.to_atom("Elixir") == :"Elixir"
    assert String.to_existing_atom("Elixir") == :"Elixir"
    assert Atom.to_string(:"Elixir") == "Elixir"
  end

  # list is a singly linked list and an immutable list
  test "linked list" do
    a = [1, 2]
    assert length(a) == 2
    v = 1
    # prepend
    assert a == [v | [2]]
    assert a == [v] ++ [2]
    v = 2
    # append
    assert a == [1] ++ [v]

    b = [3, 4]
    c = a ++ b ++ a ++ b
    # just do once, not remove all matchings
    d = c -- [1, 3]
    assert c == [1, 2, 3, 4, 1, 2, 3, 4]
    assert d == [2, 4, 1, 2, 3, 4]

    # head
    assert hd(d) == 2
    # tail? no, rest!
    assert tl(d) == [4, 1, 2, 3, 4]

    # get at index
    assert Enum.at(d, 1) == 4

    # del at index
    assert List.delete_at(d, 1) == [2, 1, 2, 3, 4]

    # del elem, but just once
    assert List.delete(d, 2) == [4, 1, 2, 3, 4]

    # del all match
    assert Enum.reject(d, fn x -> x == 2 end) == [4, 1, 3, 4]
  end

  test "tuple: like static array in other languages" do
    # tuple <-> list
    assert List.to_tuple([1, 2]) == {1, 2}
    assert Tuple.to_list({1, 2}) == [1, 2]

    # size
    assert tuple_size({1, 2}) == 2

    # get elem at index
    assert elem({1, 2}, 1) == 2

    # set elem at index
    assert put_elem({1, 2}, 1, 3) == {1, 3}

    # insert at index
    assert Tuple.insert_at({1, 2}, 1, 3) == {1, 3, 2}

    # del at index
    assert Tuple.delete_at({1, 2}, 1) == {1}

    # dup
    assert Tuple.duplicate({1}, 2) == {{1}, {1}}
    assert Tuple.duplicate(1, 2) == {1, 1}

    # sum 
    assert Tuple.sum({1, 2}) == 3

    # product 
    assert Tuple.product({1, 2}) == 2
  end

  test "pattern matching" do
    # destructing 
    list = [1, 2, 3]
    [h | t] = list
    assert h == 1
    assert t == [2, 3]

    # rebind variable
    h = 2

    # `^` operator to pin variable
    # `^h = 1` is equals to `2 = 1`, so rasie `MatchError`
    assert_raise MatchError, fn -> ^h = 1 end

    # case
    case t do
      # never match this
      [x, y] when x > y -> assert false
      # fallback branch
      _ -> assert true
    end
  end

  test "if/cond" do
    x = 1
    # cond
    cond do
      x >= 2 -> assert false
      x <= 0 -> assert false
      true -> assert true
    end

    # if
    if x >= 2 do
      assert false
    else
      if(x <= 0) do
        assert false
      else
        assert true
      end
    end
  end

  test "anonymous functions" do
    # def a anonymous function to count length of String or List
    count = fn
      x when is_binary(x) -> String.length(x)
      x when is_list(x) -> length(x)
    end

    # `.()` call anonymous function
    assert count.("Elixir") == 6
    assert count.([1, 2, 3]) == 3

    # `&` is used to simply construct an anonymous function 
    str_len = &String.length/1
    assert str_len.("Elixir") == 6
    str_len = &String.length(&1)
    assert str_len.("Elixir") == 6
    inc = &(&1 + 1)
    assert inc.(10) == 11
    greet = &"Hello #{&1}"
    assert greet.("folks") == "Hello folks"
  end

  test "BitString: binary is special case of BitString" do
    assert <<256>> == <<256::8>>
    # truncate overflow
    assert <<256>> == <<0>>
    assert <<1::1, 0::1, 0::1, 1::1>> == <<0b1001::4>>

    assert is_bitstring(<<1::1, 0::1>>)
    assert !is_binary(<<1::1, 0::1>>)

    assert is_bitstring(<<1, 2>>)
    assert is_binary(<<1, 2>>)
    assert String.valid?(<<1, 2>>)
    assert String.valid?(<<127>>)
    assert !String.valid?(<<128>>)

    assert is_bitstring("Elixir")
    assert is_binary("Elixir")
    assert String.valid?("Elixir")

    <<first_char, rest::binary>> = "Elixir"
    assert first_char == ?E
    assert rest == "lixir"

    <<first_char::utf8, _::binary>> = "👨: Hi"
    assert first_char == ?👨
  end

  test "keywordlist" do
  end

  test "map" do
    m = %{:name => "Foo"}
    m = %{m | name: "Bar"}
    assert m[:name] == "Bar"
    assert m[:notfound] == nil

    assert m.name == "Bar"
    assert_raise KeyError, fn -> m.notfound end

    m = Map.put(m, :name, "Barbar")
    m = Map.put(m, :age, 11)
    assert %{:name => "Barbar", :age => 11} == m

    # not found
    assert Map.get(m, :notfound) == nil
    assert Map.get(m, :notfound, "fallback") == "fallback"

    assert map_size(m) == 2
    assert Map.keys(m) == [:name, :age]
    assert Map.values(m) == ["Barbar", 11]

    # nil value
    m = Map.put(m, "nil", nil)
    assert Map.get(m, "nil") == nil
    assert Map.get(m, "nil", "fallback") == nil
    assert Map.get(m, "nil") || "fallback" == "fallback"
    assert m["nil"] || "fallback" == "fallback"

    m = Map.delete(m, "nil")
    assert %{:name => "Barbar", :age => 11} == m
  end

  defmodule Book do
    defstruct [:name, tags: [], pubdate: ~D[2001-01-01]]
  end

  test "struct: internal impl is a map" do
    book = %Book{}
    assert book.name == nil
    book = %{book | name: "the Elixir book"}
    assert book.name == "the Elixir book"

    assert book == %Book{
             name: "the Elixir book",
             tags: [],
             pubdate: ~D[2001-01-01]
           }

    assert book.__struct__ == Book
    assert is_map(book)
  end

  defprotocol Count do
    @spec count(t) :: Integer.t()
    def count(value)
  end

  defimpl Count, for: BitString do
    def count(value) when is_binary(value) do
      String.length(value)
    end

    def count(value) do
      bit_size(value)
    end
  end

  defimpl Count, for: List do
    def count(value) do
      length(value)
    end
  end

  test "protocol" do
    assert Count.count(<<1, 2>>) == 2
    assert Count.count("Elixir") == 6
    assert Count.count(~c"Elixir") == 6
  end

  # more: https://hexdocs.pm/elixir/sigils.html
  test "sigils" do
    # charlist
    assert ~c"hello" == [?h, ?e, ?l, ?l, ?o]

    # raw string
    assert ~s[hello "Elixir"] == "hello \"Elixir\""

    # word list
    assert ~w[hello Elixir] == ["hello", "Elixir"]

    # atom list
    assert ~w[hello Elixir]a == [:hello, :"Elixir"]

    # regex
    assert "121-121" =~ ~r/\d+\-\d+/

    # date
    {:ok, date} = Date.from_iso8601("2002-02-02")
    assert ~D[2002-02-02] == date

    # time
    {:ok, time} = Time.from_iso8601("02:02:02")
    assert ~T[02:02:02] == time

    # naive datetime
    {:ok, naivedatetime} = NaiveDateTime.from_iso8601("2002-02-02 02:02:02")
    assert ~N[2002-02-02 02:02:02] == naivedatetime

    # utc datetime
    {:ok, utcdatetime, timeoffset} = DateTime.from_iso8601("2002-02-02T02:02:02+0100")
    assert ~U[2002-02-02T01:02:02Z] == utcdatetime
    assert timeoffset == 1 * 3600
  end

  test "for" do
    vv =
      for v <- 1..10 do
        v * v
      end

    assert vv == 1..10 |> Enum.map(&(&1 * &1))
  end
end
