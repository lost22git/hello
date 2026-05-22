defmodule Fib do
  def fib(0), do: 1
  def fib(1), do: 1

  def fib(n) when n >= 2 do
    fib(n - 1) + fib(n - 2)
  end

  def main() do
    n = 40
    :io.format("~p~n", [fib(n)])
  end
end
