defmodule KV.BucketTest do
  use ExUnit.Case, async: true

  test "stores value by key", config do
    {:ok, _} = KV.Bucket.start_link(name: config.test)

    assert KV.Bucket.get(config.test, "milk") == nil

    KV.Bucket.put(config.test, "milk", 10)
    assert KV.Bucket.get(config.test, "milk") == 10

    old_val = KV.Bucket.del(config.test, "milk")
    assert old_val == 10
  end

  test "stores value by key (start_supervised rather than start_link)", config do
    {:ok, _} = start_supervised({KV.Bucket, name: config.test})

    assert KV.Bucket.get(config.test, "milk") == nil

    KV.Bucket.put(config.test, "milk", 10)
    assert KV.Bucket.get(config.test, "milk") == 10

    old_val = KV.Bucket.del(config.test, "milk")
    assert old_val == 10
  end

  test "subscribes to puts and dels" do
    {:ok, bucket} = start_supervised(KV.Bucket)
    KV.Bucket.sub(bucket)

    KV.Bucket.put(bucket, "milk", 3)
    assert_receive {:put, "milk", 3}

    spawn(fn -> KV.Bucket.del(bucket, "milk") end)
    assert_receive {:del, "milk"}
  end
end
