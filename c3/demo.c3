#!/usr/bin/env -S c3c compile-run --run-once
module demo;

import std::io;
import std::os::process;
import std::thread;

fn void main() {
  io::printfn("Num of CPU: %d", os::num_cpu());
  run_qsort();
}

fn void run_qsort() {
  io::printn("=== run_qsort ===");
  int[10] a;
  io::printn("random ints concurrently.");
  random_ints(a[..])!!;
  foreach (&v : a) {
    *v = *v % 100;
  }

  io::printn(a[..]);
  qsort{int}(a[..]);
  io::printn(a[..]);
}

<*
 @pure
*>
fn void qsort(Type[] a) <Type> {
  if (a.len < 2) return;
  int p = partition{Type}(a);
  if (p > 0) {
    qsort{Type}(a[..(p - 1)]);
  }
  if(p < a.len - 1) {
    qsort{Type}(a[(p + 1)..]);
  }
}

<*
 @pure
 @ensure return >= 0 && return < a.len
*>
fn int partition(Type[] a) <Type> {
  Type pivot = a[^1];
  int p = 0;
  foreach (&v : a) {
    if (*v < pivot) {
      @swap(a[p], *v);
      p += 1;
    }
  }
  @swap(a[p], a[^1]);
  return p;
}

fn void? random_ints(int[] result) {
  FixedThreadPool* pool = mem::alloc(FixedThreadPool);
  defer mem::free(pool);
  pool.init(os::num_cpu(), queue_size: 10)!;
  defer pool.destroy()!!;
  defer pool.join()!!;

  // random ints concurrently
  ThreadPoolFn task = fn(args) {
    int* i = (int*) args[0];
    io::printfn("[task-%d] is running...", *i);
    int** r = (int**) args[1];
    **r = random_int();
  };
  foreach (i, &v : result) {
    pool.push(task, i, v)!;
  }
}

fn int random_int() {
  char[4] bytes;
  if (try random_bytes(bytes[..])) {
    return bitorder::read(bytes, IntBE);
  } else {
    return 0;
  }
}

fn void? random_bytes(char[] bytes) {
  int len = bytes.len; 
  // scoped temp allocator (child of mem: global default allocator)
  // auto free when leaving the scope
  @pool_init(mem, 1024) {
    // tformat(...) same as format(mem, ...)
    String url = string::tformat("https://httpbin.org/bytes/%d", len);
    String[*] cmd = {
      "curl", url, "-H", "'Accept: application/octet-stream'",
      "--fail", "--retry", "3", "--retry-all-errors"
    };
    process::run_capture_stdout(bytes, &cmd)!;
  };
}
