(* usingnamespace *)
open Printf
open Book

let print_title title = print_endline ("<><><><> " ^ title)

(* in: scope local variable *)
let result =
  let i = 6 in
  let j = 7 in
  let () = printf "%d * %d\n" i j in
  i * j

let () = printf "result: %d\n" result

(* function *)
let () = print_title "function"

(* mul = fun x -> fun y -> x * y *)
let mul x y =
  let () = printf "x * y = %d * %d\n" x y in
  x * y

let result = mul 6 7
let () = printf "result: %d\n" result

(* partial application *)
let () = print_title "partial application"
let mul_6 = mul 6
let result = mul_6 7
let () = printf "result: %d\n" result
let mul_7 x = mul x 7
let result = mul_7 6
let () = printf "result: %d\n" result

(* pipeline operator *)
let () = print_title "pipeline operator"
let result = 7 |> mul 6 (* (mul 6) 7 *)
let () = printf "result: %d\n" result

(* sub module *)
(* divmod *)
let () = print_title "submodule divmod"

module DivMod = struct
  let divmod x y =
    let () = printf "%d divmod %d\n" x y in
    (x / y, x mod y)
end

let _div, _mod = DivMod.divmod (-11) 3
let () = printf "div: %d,  mod: %d\n" _div _mod

(* enum *)
let () = print_title "enum & enum union & record"

let ocaml_book =
  {
    id = 1;
    name = "the ocaml book";
    tags = [ "ocaml"; "programming" ];
    media_type = Ebook Pdf;
  }

let () = book_to_str ocaml_book |> print_endline

(* create record with old record *)
let ocaml_book2 =
  {
    ocaml_book with
    id = 3;
    name = "the ocaml book (2nd ver)";
    media_type = Paperbook (210., 297.);
  }

let () = book_to_str ocaml_book2 |> print_endline

(* mutable *)
let () = print_title "mutable"
let book_mutable = ref ocaml_book

(* `!` defer *)
(* `<-` set mutable fields *)
let () = !book_mutable.tags <- [ "ocaml" ]
let () = !book_mutable |> book_to_str |> print_endline

(* multiline string *)
let () = print_title "multiline string"

let poem = {|
《 梅 花 》

墙 角 数 枝 梅
凌 寒 独 自 开
遥 知 不 是 雪
为 有 暗 香 来
|}

let () = print_endline poem
