//> using dep com.softwaremill.sttp.client4::core:4.0.25

// import scala.util.Random
import scala.util.Try
import scala.concurrent.*
import scala.concurrent.duration.Duration
import sttp.client4.*
import sttp.model.*

import java.nio.ByteBuffer
import java.util.concurrent.Executors
import java.lang.Thread

@main def main(): Unit =
  runFib()
  runQsort()

def runFib(): Unit =
  println("=== runFib ===")
  val n = 40
  println(fib(n))

def fib(n: Long): Long =
  if n < 2 then 1
  else fib(n - 1) + fib(n - 2)

def runQsort(): Unit =
  println("=== runQsort ===")
  val n = 10
  val max = 100
  // val rand = new Random()
  // val a = Array.fill(n)(rand.between(0, max))
  val a = randomInts(n).map(_ % max)
  println(s"a = ${a.mkString(",")}")
  qsort(a)
  println(s"sorted a = ${a.mkString(",")}")

def qsort[E](a: Array[E])(using ord: Ordering[E]): Unit =
  qsort(a, 0, a.length - 1)

def qsort[E](a: Array[E], l: Int, r: Int)(using ord: Ordering[E]): Unit =
  def swap(i: Int, j: Int): Unit =
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  def partition(l: Int, r: Int): Int =
    val pivot = a(r)
    var p = l
    for i <- l until r do
      if ord.lt(a(i), pivot) then
        swap(i, p)
        p += 1
    swap(r, p)
    p

  if a.isEmpty || l >= r then ()
  else
    val pp = partition(l, r)
    qsort(a, l, pp - 1)
    qsort(a, pp + 1, r)

def randomInts(n: Int): Array[Int] =
  def bytesToInt(bytes: Array[Byte]): Int =
    Try(
      ByteBuffer.wrap(bytes).getInt
    ).getOrElse(0)
  def randomInt(i: Int): Int =
    val backend = DefaultSyncBackend()
    try
      val uri = uri"https://httpbin.org/bytes/4"
      println(s"[tid=${Thread.currentThread.getId()},id=${i}] - ${uri}")
      val request = basicRequest
        .get(uri)
        .header(HeaderNames.Accept, "application/octet-stream")
      val response = Try(
        request.send(backend)
      ).toEither
      response
        .flatMap(_.body)
        .map(_.getBytes)
        .map(bytesToInt)
        .getOrElse(0)
    finally backend.close()

  // Run them on Virtual Thread
  given ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  // Generate random ints concurrently
  val futs = (1 to n)
    .map { i => Future { randomInt(i) } }
  val allFut = Future.sequence(futs)
  Await.result(allFut, Duration("60s")).toArray
