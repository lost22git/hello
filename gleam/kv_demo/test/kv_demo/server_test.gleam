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

fn with_connect(action: fn(Socket) -> Nil) {
  let assert Ok(socket) =
    mug.new("localhost", port: 8080)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  action(socket)

  let assert Ok(_) = mug.shutdown(socket)
}

fn send_and_recv(socket: Socket, command: BitArray) -> BitArray {
  let assert Ok(Nil) = mug.send(socket, command)
  let assert Ok(data) = mug.receive(socket, timeout_milliseconds: 1000)
  data
}
