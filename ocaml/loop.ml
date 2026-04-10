#!/usr/bin/env ocaml

(* for-to *)
print_endline "=== for-to ===";;

for i = 1 to 3 do
  print_endline @@ Int.to_string i
done
;;

(* for-downto *)
print_endline "=== for-downto ===";;

for i = 3 downto 1 do
  print_endline @@ Int.to_string i
done
;;

(* while *)
print_endline "=== while ===";;

let i = ref 1 in
while !i <= 3 do
  print_endline @@ Int.to_string !i;
  i := !i + 1
done
;;

(* iota *)
let iota ?(step = 1) ?(min = 0) max =
  let n = ((max - min) / step) + 1 in
  List.init n @@ fun x -> min + (x * step)
;;

assert ([ 0; 1; 2 ] = iota 2);;
assert ([ 0; 1; 2 ] = iota ~min:0 2);;
assert ([ 0; 3; 6; 9 ] = iota ~step:3 ~min:0 9);;
assert ([ 2; 5; 8 ] = iota ~step:3 ~min:2 9);;

(* custom Seq *)
let rec fib_seq a b () = Seq.Cons (a, fib_seq (a + b) a);;

assert ([ 1; 1; 2; 3; 5; 8; 13; 21 ] = (fib_seq 1 0 |> Seq.take 8 |> List.of_seq))
