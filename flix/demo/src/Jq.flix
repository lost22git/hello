mod Jq {
  use Sys.Process
  use Sys.StdOut
  use Sys.StdIn
  use CharacterSet.CharacterSet
  use IoError.ErrorKind

  import java.io.IOException
  import java.nio.charset.StandardCharsets
  import java.lang.{String => JString}
  import java.io.InputStream
  
  /// 
  /// Get pretty json of `text`
  ///
  pub def prettyJson(text: String): Result[IoError, String] \ { Process, IO } = {
    let charset = CharacterSet(StandardCharsets.UTF_8);
    forM ( 
      ph <- Process.exec("jq", Nil);
      StdOut(stdout_in) <- Process.stdout(ph);
      stdin <- Process.stdin(ph);
      StdIn(stdin_out) = stdin;
      _ <- stdin
           |> EncodingWriter.wrap(charset) 
           |> Writable.writeString(text);
      // TODO: how to close io safely in Flix ?
      _ = stdin_out.close();
      prettyJson <- readAll(stdout_in);
      // TODO: how to close io safely in Flix ?
      _ = stdout_in.close()
    ) yield prettyJson
  }

  def readAll(in: InputStream): Result[IoError, String] \ IO = {
      // Java interop
      // !!! Can't support finally block
      try {
        let bytes = in.readAllBytes();
        Ok(new JString(bytes))
      } catch {
        case e: IOException => Err(IoError(ErrorKind.Other, e.toString()))
      }
  }

  // def testPrettyJson(): Unit \ { IO, Process } = {
  //   let text = "{\"name\": \"pikachu\"}";
  //   match prettyJson(text) {
  //     case Ok(json) => println(json)
  //     case Err(exn) => println(exn)
  //   }
  // }
}
