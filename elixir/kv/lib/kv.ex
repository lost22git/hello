defmodule KV do
  use Application

  @moduledoc """
  Documentation for `KV`.
  """

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, name: KV, keys: :unique},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one},
      {Task.Supervisor, name: KV.ServerSupervisor},
      Supervisor.child_spec({Task, fn -> KV.Server.listen(8080) end}, restart: :permanent)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  @doc """
  Creates a bucket with given `name`
  """
  def create_bucket(name) do
    DynamicSupervisor.start_child(KV.BucketSupervisor, {KV.Bucket, name: via(name)})
  end

  @doc """
  Lookup a bucket by given `name`
  """
  def lookup_bucket(name) do
    GenServer.whereis(via(name))
  end

  defp via(name), do: {:via, Registry, {KV, name}}
end
