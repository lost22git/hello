#!/usr/bin/env -S c3c compile-run --run-once

import std::io;

fn void main() {
  run_qsort();
}

fn void run_qsort() {
  io::printn("=== run_qsort ===");
  int[*] a = {4, 3, 1, 2};
  io::printn(a[..]);
  qsort{int}(a[..]);
  io::printn(a[..]);
}

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
