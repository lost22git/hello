#!/usr/bin/env elixir

# === Print ===

dbg(1 == 1)

IO.write("HELLO\n")
IO.write(["HE", ?L, ~c"LO", "\n"])

# new line
IO.puts("HELLO")
IO.puts(["HE", ?L, ~c"LO"])

# new line ( inspect given item )
IO.inspect(%{a: 1, b: 2})

# === Prompt ===

input = IO.gets("Input an int number: ")
{num, _} = Integer.parse(input)
IO.puts("Your input number is: #{num}")

# === Stream ===

# File Stream
File.stream!("./cli.exs", :line)
|> Stream.drop(3)
|> Stream.take(3)
|> Stream.each(&IO.puts/1)
|> Stream.run()

# File Stream (use IO.stream)
f = File.open!("./cli.exs", [:read])

try do
  IO.stream(f, :line)
  |> Stream.drop(3)
  |> Stream.take(3)
  |> Stream.each(&IO.puts/1)
  |> Stream.run()
after
  File.close(f)
end

# Interval Stream
Stream.interval(1000)
|> Stream.take(3)
|> Stream.each(&IO.puts("#{DateTime.utc_now()} : #{&1}"))
|> Stream.run()

# Create own Stream
Stream.resource(
  fn -> File.open!("./cli.exs") end,
  fn file ->
    case IO.read(file, :line) do
      data when is_binary(data) -> {[data], file}
      _ -> {:halt, file}
    end
  end,
  fn file -> File.close(file) end
)
|> Stream.drop(3)
|> Stream.take(3)
|> Stream.each(&IO.puts/1)
|> Stream.run()

# === Process ===

Process.list() |> IO.inspect(label: "process count:")

Process.alive?(self()) |> IO.inspect(label: "self alive?")

Process.info(self()) |> IO.inspect(label: "self info")

Process.get() |> IO.inspect(label: "self dict")

# register an alias for a pid
main_pid = self()
Process.register(main_pid, :main)

# spawn a process and monitor it 
{sender_pid, sender_monitor_ref} =
  spawn_monitor(fn ->
    # send messages to process
    send(:main, "HELLO")
    send(:main, "ELIXIR")
  end)

Process.sleep(100)

message_queue_len = Process.info(self())[:message_queue_len]
message_queue_len |> IO.inspect(label: :message_queue_len)

# receive messages of current process
for _ <- 1..3 do
  receive do
    {:DOWN, ^sender_monitor_ref, :process, ^sender_pid, code} ->
      "Process :sender was down with #{code}"
      |> IO.inspect(label: "DOWN")

    msg ->
      msg |> IO.inspect(label: "receive")

    1000 ->
      IO.puts("No message in 1s")
  end
end

message_queue_len = Process.info(self())[:message_queue_len]
message_queue_len |> IO.inspect(label: :message_queue_len)

# === GenServer ===

# === Task ===

work = fn secs ->
  Process.sleep(secs)
  secs
end

IO.puts("#{DateTime.utc_now()} awaiting")

Task.await_many(
  [1000, 2000]
  |> Enum.map(&Task.async(fn -> work.(&1) end))
)
|> IO.inspect(label: "results")

IO.puts("#{DateTime.utc_now()} done")

# Task Stream

IO.puts("#{DateTime.utc_now()} awaiting")

Task.async_stream([1000, 2000], fn x ->
  Process.sleep(x)
  x
end)
|> Stream.each(&IO.inspect(&1, label: "results"))
|> Stream.run()

IO.puts("#{DateTime.utc_now()} done")

# === JSON ===

defmodule Book do
  @derive JSON.Encoder
  defstruct [:id, :title, :created_at, :tags]
end

book = %Book{
  id: 1,
  title: "Elixir Book",
  created_at: DateTime.utc_now(),
  tags: [:programming, :elixir]
}

json_str =
  JSON.encode!(book) |> IO.inspect(label: "json encoded str")

json_io_data =
  JSON.encode_to_iodata!(book) |> IO.inspect(label: "json encoded io data")

decoded_data =
  JSON.decode!(json_str) |> IO.inspect(label: "json decoded")

# === with ===

# 3
with {:ok, a} <- {:ok, 1},
     {:ok, b} <- {:ok, 2} do
  a + b
end

# {:err1, 1}
with {:ok, a} <- {:err1, 1},
     {:ok, b} <- {:err2, 2} do
  a + b
end

# {:err2, 2}
with {:ok, a} <- {:ok, 1},
     {:ok, b} <- {:err2, 2} do
  a + b
end

# -1
with {:ok, a} <- {:err1, 1},
     {:ok, b} <- {:err2, 2} do
  a + b
else
  {:err1, _} -> -1
  {:err2, _} -> -2
end

# -2
with {:ok, a} <- {:ok, 1},
     {:ok, b} <- {:err2, 2} do
  a + b
else
  {:err1, _} -> -1
  {:err2, _} -> -2
end
