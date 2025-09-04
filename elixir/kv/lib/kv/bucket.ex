defmodule KV.Bucket do
  use GenServer

  @doc """
  Starts a new bucket
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  @doc """
  Gets a value from `bucket` by `key`
  """
  def get(bucket, key) do
    GenServer.call(bucket, {:get, key})
  end

  @doc """
  Puts the value for the given `key` in the `bucket`
  """
  def put(bucket, key, val) do
    GenServer.call(bucket, {:put, key, val})
  end

  @doc """
  Deletes the value from `bucket`

  Returns the current value of `key`, if `key` exists.
  """
  def del(bucket, key) do
    GenServer.call(bucket, {:del, key})
  end

  @doc """
  Subscribes the current process to the `bucket`
  """
  def sub(bucket) do
    GenServer.cast(bucket, {:sub, self()})
  end

  @impl true
  def init(bucket) do
    state = %{
      bucket: bucket,
      subs: MapSet.new()
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    val = get_in(state.bucket[key])
    {:reply, val, state}
  end

  def handle_call({:put, key, val} = msg, _from, state) do
    state = put_in(state.bucket[key], val)
    broadcast(state, msg)
    {:reply, :ok, state}
  end

  def handle_call({:del, key} = msg, _from, state) do
    {val, state} = pop_in(state.bucket[key])
    broadcast(state, msg)
    {:reply, val, state}
  end

  @impl true
  def handle_cast({:sub, pid}, state) do
    Process.monitor(pid)
    state = update_in(state.subs, &MapSet.put(&1, pid))
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, _type, pid, _reason}, state) do
    state = update_in(state.subs, &MapSet.delete(&1, pid))
    {:noreply, state}
  end

  defp broadcast(state, msg) do
    for pid <- state.subs do
      send(pid, msg)
    end
  end
end
