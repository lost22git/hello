#!/usr/bin/env elixir

# check Tail Call Optimization support

defmodule Fib do
  # this would check if TCO
  defp visit(0, a, _b), do: a
  defp visit(i, a, b), do: visit(i - 1, a + b, a)

  @doc """
  tail recur variant
  """
  def tail_recur(n) when n >= 0, do: visit(n, 1, 0)

  @doc """
  recur variant
  """
  def recur(0), do: 1
  def recur(1), do: 1
  def recur(n) when n > 1, do: recur(n - 1) + recur(n - 2)
end

n = 1111
Fib.tail_recur(n) |> IO.inspect(label: "tail_recur")
Fib.recur(n) |> IO.inspect(label: "recur")
