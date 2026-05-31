mod HttpRandom {
  use Net.Http
  use Net.HttpResponse
  use Net.HttpRequest

  import java.nio.ByteBuffer
  import java.lang.Exception
  
  ///
  /// Get random `n` bytes.
  ///
  /// Via http api - httpbin.org/byts/{n}
  ///
  pub def randomBytes(n: Int32): Result[IoError, Vector[Int8]] \ { Http, Logger } = {
    let request = HttpRequest.get("https://httpbin.org/bytes/${n}")
                  |> HttpRequest.withAccept("application/octet-stream");
    run {
      forM (
        response <- Http.send(request);
        statusOkResponse <- HttpResponse.expect(response)
      ) yield HttpResponse.bodyBytes(statusOkResponse)
    } with Http.withLogging
  }
  
  ///
  /// Get a random Int32 value.
  ///
  pub def randomInt32(): Int32 \ { IO, Http, Logger } = {
    match randomBytes(4) {
      case Ok(bytes) => bytesToInt32(bytes)
      case Err(_) => 0i32
    }
  }

  ///
  /// Convert Vector[Int8] to Int32
  ///
  def bytesToInt32(bytes: Vector[Int8]): Int32 \ IO = {
    // Java interop - handle exception
    try {
      ByteBuffer.wrap(bytes).getInt()
    } catch {
      case _: Exception => 0i32
    }
  }
}
