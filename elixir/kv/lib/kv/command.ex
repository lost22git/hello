defmodule KV.Command do
  @doc ~S"""
  Parses the given `line` into a command.

  ## Examples

      iex> KV.Command.parse "NEW shopping\r\n"
      {:ok, {:new, "shopping"}}

      iex> KV.Command.parse "NEW  shopping  \r\n"
      {:ok, {:new, "shopping"}}

      iex> KV.Command.parse "PUT shopping milk 1\r\n"
      {:ok, {:put, "shopping", "milk", "1"}}

      iex> KV.Command.parse "GET shopping milk\r\n"
      {:ok, {:get, "shopping", "milk"}}

      iex> KV.Command.parse "DEL shopping eggs\r\n"
      {:ok, {:del, "shopping", "eggs"}}

  Unknown commands or commands with the wrong number of
  arguments return an error:

      iex> KV.Command.parse "UNKNOWN shopping eggs\r\n"
      {:error, :unknown_command}

      iex> KV.Command.parse "GET shopping\r\n"
      {:error, :unknown_command}

  """
  def parse(line) do
    case String.split(line) do
      ["NEW", bucket] -> {:ok, {:new, bucket}}
      ["PUT", bucket, key, value] -> {:ok, {:put, bucket, key, value}}
      ["GET", bucket, key] -> {:ok, {:get, bucket, key}}
      ["DEL", bucket, key] -> {:ok, {:del, bucket, key}}
      _ -> {:error, :unknown_command}
    end
  end

  @doc """
  Runs the given command.
  """
  def run(command, socket)

  def run({:new, bucket}, socket) do
    KV.create_bucket(bucket)
    :gen_tcp.send(socket, "OK\r\n")
    :ok
  end

  def run({:put, bucket, key, value}, socket) do
    lookup(bucket, fn pid ->
      KV.Bucket.put(pid, key, value)
      :gen_tcp.send(socket, "OK\r\n")
      :ok
    end)
  end

  def run({:get, bucket, key}, socket) do
    lookup(bucket, fn pid ->
      val = KV.Bucket.get(pid, key)
      :gen_tcp.send(socket, "#{val}\r\nOK\r\n")
      :ok
    end)
  end

  def run({:del, bucket, key}, socket) do
    lookup(bucket, fn pid ->
      KV.Bucket.del(pid, key)
      :gen_tcp.send(socket, "OK\r\n")
      :ok
    end)
  end

  defp lookup(bucket, callback) do
    case KV.lookup_bucket(bucket) do
      nil -> {:error, :not_found}
      bucket -> callback.(bucket)
    end
  end
end
