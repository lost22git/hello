#!/usr/bin/env elixir

defmodule Rand do
  @doc """
  Generates random `n` bytes 
  """
  @spec bytes!(pos_integer()) :: binary()
  def bytes!(n) when n > 0 do
    {bytes, exit_code} =
      System.cmd(
        "curl",
        [
          "https://httpbin.org/bytes/#{n}",
          "-H",
          ~s("accept: application/octet-stream"),
          "-s"
        ],
        into: ""
      )

    cond do
      exit_code != 0 ->
        raise "Failed to run system process, exit-code: #{exit_code}."

      byte_size(bytes) != n ->
        raise "Failed to run system process, result's byte size isn't #{n}."

      true ->
        bytes
    end
  end

  @doc """
  Generates a random int value
  """
  @spec int!() :: integer()
  def int!() do
    bytes = __MODULE__.bytes!(4)
    <<x::signed-big-32>> = bytes
    x
  end

  @doc """
  Generates a random int value, range [0, max)
  """
  @spec int!(pos_integer()) :: non_neg_integer()
  def int!(max) when max > 0 do
    __MODULE__.int!() |> Integer.mod(max)
  end

  @doc """
  Generates `n` random ints.
  """
  @spec ints(integer()) :: [integer()]
  def ints(n) when n <= 0, do: []

  @spec ints(integer()) :: [integer()]
  def ints(n) when n > 0 do
    1..n
    |> Task.async_stream(
      fn _i ->
        try do
          __MODULE__.int!()
        rescue
          _ -> 0
        end
      end,
      timeout: 3000,
      on_timeout: :kill_task
    )
    |> Stream.map(fn
      {:ok, r} -> r
      {:exit, _} -> 0
    end)
    |> Enum.to_list()
  end
end

defmodule Qsort do
  def sort([]), do: []
  def sort([x]), do: [x]

  def sort([h | t]) do
    __MODULE__.sort(Enum.filter(t, &(&1 < h))) ++
      [h] ++
      __MODULE__.sort(Enum.filter(t, &(&1 >= h)))
  end
end

Rand.ints(11)
|> IO.inspect(label: "Before sort")
|> Qsort.sort()
|> IO.inspect(label: "After  sort")
