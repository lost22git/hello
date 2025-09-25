async function echo(conn: Deno.TcpConn, decoder: TextDecoder) {
  conn.setNoDelay(true);

  const id = crypto.randomUUID();

  console.log(`${id} %cconnected`, "color: green; font-weight: bold;");

  const data = new Uint8Array(100);

  while (true) {
    const n = await conn.read(data);
    if (n == null) {
      console.log(
        `${id} %cdisconnected (got EOF)`,
        "color: red; font-weight: bold;",
      );
      break;
    } else {
      const text = decoder.decode(data).replace("\n", "");
      console.log(`${id}=> %c${text}`, "color: blue;");
      conn.write(data);
    }
    data.fill(0);
  }
}

async function startEchoServer() {
  const listener = Deno.listen({ port: 9933 });
  const decoder = new TextDecoder();

  console.log(
    `Listening on %c${listener.addr.hostname}:${listener.addr.port}`,
    "color: red; font-weight: bold;",
  );

  for await (const conn of listener) {
    try {
      await echo(conn, decoder);
    } finally {
      conn.close();
    }
  }
}

if (import.meta.main) {
  await startEchoServer();
}
