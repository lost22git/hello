import gleam/erlang/process
import mug.{type Socket}

pub fn interation_test() {
  with_connect(fn(socket) {
    // NEW
    assert send_and_recv(socket, <<"NEW shoppings\r\n":utf8>>)
      == <<"OK\r\n":utf8>>

    // PUT
    assert send_and_recv(socket, <<"PUT shoppings eggs 11\r\n":utf8>>)
      == <<"nil\r\nOK\r\n":utf8>>

    // PUT
    assert send_and_recv(socket, <<"PUT shoppings eggs 111\r\n":utf8>>)
      == <<"11\r\nOK\r\n":utf8>>

    // GET
    assert send_and_recv(socket, <<"GET shoppings eggs\r\n":utf8>>)
      == <<"111\r\nOK\r\n":utf8>>

    // DEL
    assert send_and_recv(socket, <<"DEL shoppings eggs\r\n":utf8>>)
      == <<"111\r\nOK\r\n":utf8>>

    // GET
    assert send_and_recv(socket, <<"GET shoppings eggs\r\n":utf8>>)
      == <<"nil\r\nOK\r\n":utf8>>
  })
}

pub fn subscribe_test() {
  with_connect2(fn(socket, subscribe_socket) {
    // NEW
    assert send_and_recv(socket, <<"NEW langs\r\n":utf8>>) == <<"OK\r\n":utf8>>

    // SUB
    let assert Ok(Nil) = mug.send(subscribe_socket, <<"SUB langs\r\n":utf8>>)

    process.sleep(1000)

    // PUT
    assert send_and_recv(socket, <<"PUT langs gleam 11\r\n":utf8>>)
      == <<"nil\r\nOK\r\n":utf8>>
    assert mug.receive(subscribe_socket, timeout_milliseconds: 1000)
      == Ok(<<"gleam SET TO 11\r\n":utf8>>)

    // DEL
    assert send_and_recv(socket, <<"DEL langs gleam\r\n":utf8>>)
      == <<"11\r\nOK\r\n":utf8>>
    assert mug.receive(subscribe_socket, timeout_milliseconds: 1000)
      == Ok(<<"gleam DELETED\r\n":utf8>>)
  })
}

fn with_connect(action: fn(Socket) -> Nil) {
  let assert Ok(socket) =
    mug.new("localhost", port: 8080)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  action(socket)

  let assert Ok(_) = mug.shutdown(socket)
}

fn with_connect2(action: fn(Socket, Socket) -> Nil) {
  let assert Ok(socket) =
    mug.new("localhost", port: 8080)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  let assert Ok(subscribe_socket) =
    mug.new("localhost", port: 8080)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  action(socket, subscribe_socket)

  let assert Ok(_) = mug.shutdown(socket)
  let assert Ok(_) = mug.shutdown(subscribe_socket)
}

fn send_and_recv(socket: Socket, command: BitArray) -> BitArray {
  let assert Ok(Nil) = mug.send(socket, command)
  let assert Ok(data) = mug.receive(socket, timeout_milliseconds: 1000)
  data
}
