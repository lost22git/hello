// def main(): Unit \ {Net, IO} =
//     run {
//         let url = "https://httpbin.org/ip";
//         Logger.info("Downloading URL: '${url}'");
//         match HttpWithResult.get(url, Map.empty()) {
//             case Result.Ok(response) =>
//                 let file = "data.txt";
//                 Logger.info("Saving response to file: '${file}'");
//                 let body = Http.Response.body(response);
//                 match FileWriteWithResult.write(str = body, file) {
//                     case Result.Ok(_) => 
//                         Logger.info("Response saved to file: '${file}'")
//                     case Result.Err(err) => 
//                         Logger.fatal("Unable to write file: '${err}'")
//                 }
//             case Result.Err(err) => 
//                 Logger.fatal("Unable to download URL: '${err}'")
//         }
//     } with FileWriteWithResult.runWithIO
//       with HttpWithResult.runWithIO
//       with Logger.runWithIO
//

def fib_stackoverflow(n: Int32): Int32 =
   if (n <= 1) 1 else fib_stackoverflow(n-1) + fib_stackoverflow(n-2) // no tail recur opt

def fib(n: Int32): Int32 = 
  def visit(i:Int32,a:Int32,b:Int32) = match i==n {
    case true => a + b
    case false => visit(i+1, a+b, a) // tail recur opt
  };
  if (n <= 1) 1 else visit(2,1,1)

def main(): Unit \ IO = 
  let n = 1_000_000;
  println("fib ${n}:  ${fib(n)}")  
