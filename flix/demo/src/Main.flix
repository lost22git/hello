use Time.Clock
use Math.Random
use Net.Http
use Sys.Process

// The main entry point.
def main(): Unit \ { IO, Clock, Http, Logger, Process, Chan, NonDet } =
  runFib();
  runPi();
  runPokemon();
  runQsort()

def runFib(): Unit \ { IO, Clock} = {
  println("=== runFib ===");
  let n = 40i64;
  let time = Clock.now();
  println(Fib.fib(n));
  println("Time: ${Clock.now() - time} ms.")
}

def runPi(): Unit \ IO = {
  println("=== runPi ===");
  let n = 1000000;
  println(Pi.calculatePi(n))
}

def runPokemon(): Unit \ { IO, Http, Logger, Process } = {
  println("=== runPokemon ===");
  let name = "pikachu";
  let result = forM (
    pokemonInfo <- Pokemon.findPokemon(name);
    prettyPokemonInfo <- Jq.prettyJson(pokemonInfo)
  ) yield prettyPokemonInfo;

  match result {
    case Ok(info) => println(info)
    case Err(exn) => println(exn)
  }
}

def runQsort(): Unit \ { IO, Chan, NonDet } = {
  println("=== runQsort ===");

  def sendRandomInt64(tx: Sender[Int64]): Unit \ { Chan, IO } = {
      run {
        Channel.send(Random.randomInt64(), tx)
      } with handler Random {
        // use custom effect handler
        def randomFloat64(k) = k(0f64) // TODO
        def randomInt64(k) = k(HttpRandom.randomInt32() |> Int32.toInt64)
      } 
        // transform { Http, Logger } effects to IO effect,
        // because just { IO, Chan } are allowed in `spawn` block ?
        with Http.runWithIO
        with Logger.runWithIO
  };

  // Mutation Region:
  //    define mutable data structure
  //    do mutation actions
  //    return immutable value
  region rc {
    // genernates random ints concurrently
    let n = 10;
    let (tx, rx) = Channel.buffered(n);
    foreach (_ <- Range(0, n)) {
      spawn sendRandomInt64(tx) @ rc
    };
    let a = Iterator.range(rc, 0, n)
            |> Iterator.map(_ -> Channel.recv(rx))
            |> Iterator.map(x -> Int64.modulo(x, 100i64))
            |> Iterator.toArray(rc);
    
    // perform qsort
    println("original: " + Array.toString(a));
    Qsort.qsort(rc, 0, Array.length(a) - 1, a);
    println("sorted: " + Array.toString(a))
  }
}
