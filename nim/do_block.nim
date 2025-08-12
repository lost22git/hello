proc count(list: openArray[string], f: proc(s: string): int) =
  for s in list:
    echo s, "->", f(s)

count(["hello", "world"]) do(s: string) -> int:
  s.len
