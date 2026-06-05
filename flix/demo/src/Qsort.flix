mod Qsort {
  pub def qsort(r: Region[r], low: Int32, high: Int32, a: Array[a, r]): Unit \ r with Order[a] = {
    if (low >= high)
      ()
    else
      let p = partition(r, low, high, a);
      qsort(r, low, p - 1, a);
      qsort(r, p + 1, high, a)
  }

  def partition(r: Region[r], low: Int32, high: Int32, a: Array[a, r]): Int32 \ r with Order[a] = {
    let pivot = Array.get(high, a);
    let pos = Ref.fresh(r, low);
    foreach (i <- Range(low, high + 1)) {
      if (Array.get(i, a) < pivot)
        let p = Ref.get(pos);
        Array.swap(p, i, a);
        Ref.put(p + 1, pos)
      else
        ()
    };
    let p = Ref.get(pos);
    Array.swap(p, high, a);
    p
  } 
}
