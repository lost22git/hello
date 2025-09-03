defmodule KV.ServerTest do
  use ExUnit.Case, async: true

  @socket_options [:binary, packet: :line, active: false]

  setup config do
    {:ok, socket} = :gen_tcp.connect(~c"localhost", 8080, @socket_options)
    test_name = config.test |> Atom.to_string() |> String.replace(" ", "-")
    %{socket: socket, name: "#{config.module}-#{test_name}"}
  end

  test "server interation", %{socket: socket, name: name} do
    # NEW
    assert send_and_recv(socket, "NEW #{name}\r\n") == "OK\r\n"
    # PUT
    assert send_and_recv(socket, "PUT #{name} eggs 10\r\n") == "OK\r\n"
    # GET
    assert send_and_recv(socket, "GET #{name} eggs\r\n") == "10\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"
    # DEL
    assert send_and_recv(socket, "DEL #{name} eggs\r\n") == "OK\r\n"
  end

  test "unknown command", %{socket: socket, name: name} do
    assert send_and_recv(socket, "DELL #{name} eggs\r\n") == "UNKNOWN COMMAND\r\n"
    assert send_and_recv(socket, "\r\n") == "UNKNOWN COMMAND\r\n"
  end

  test "bucket not found", %{socket: socket, name: _name} do
    assert send_and_recv(socket, "GET bbb eggs\r\n") == "NOT FOUND\r\n"
  end

  defp send_and_recv(socket, command) do
    :ok = :gen_tcp.send(socket, command)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
