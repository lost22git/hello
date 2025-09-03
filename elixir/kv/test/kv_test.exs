defmodule KVTest do
  use ExUnit.Case, async: true

  test "creates and looks up buckets by any name" do
    name = "a unique name that won't be shared"
    assert is_nil(KV.lookup_bucket(name))

    assert {:ok, bucket} = KV.create_bucket(name)
    assert KV.lookup_bucket(name) == bucket

    assert KV.create_bucket(name) == {:error, {:already_started, bucket}}
  end
end
