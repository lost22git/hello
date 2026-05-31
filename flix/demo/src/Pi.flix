mod Pi {
  ///
  /// Calculate π (using Leibniz Formula)
  ///
  pub def calculatePi(n: Int32): Float64 = {
    region rc {
      let result = Iterator.range(rc, 0, n)
      |> Iterator.map(x -> Int32.toFloat64(Int32.pow({base = -1}, x)) / Int32.toFloat64((2 * x + 1)))
      |> Iterator.foldLeft(acc -> x -> acc + x, 0f64);
      result * 4f64
    }
  }
}
