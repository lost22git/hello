(* in: scope local variable *)
let result =
  let i = 6 in
  let j = 7 in
  let () = Printf.printf "%d * %d\n" i j in
  i * j

let () = Printf.printf "result: %d\n" result

(* function *)
let () = print_endline "---- function"

(* mul = fun x -> fun y -> x * y *)
let mul x y =
  let () = Printf.printf "x * y = %d * %d\n" x y in
  x * y

let result = mul 6 7
let () = Printf.printf "result: %d\n" result

(* partial application *)
let () = print_endline "---- partial application"
let mul_6 = mul 6
let result = mul_6 7
let () = Printf.printf "result: %d\n" result
let mul_7 x = mul x 7
let result = mul_7 6
let () = Printf.printf "result: %d\n" result

(* pipeline operator *)
let () = print_endline "--- pipeline operator"
let result = 7 |> mul 6 (* (mul 6) 7 *)
let () = Printf.printf "result: %d\n" result

(* divmod *)
let () = print_endline "---- divmod"

let divmod x y =
  let () = Printf.printf "%d divmod %d\n" x y in
  (x / y, x mod y)

let _div, _mod = divmod (-11) 3
let () = Printf.printf "div: %d,  mod: %d\n" _div _mod

(* enum *)
let () = print_endline "---- enum & enum union & record"

type ebook_format =
  | Pdf [@warning "-unused-constructor"]
  | Epub [@warning "-unused-constructor"]
  | Mobi [@warning "-unused-constructor"]

(*

function is sugar of `match x with`

  let ebook_format_to_str format = match format with
    | Pdf -> "PDF"
    | Epub -> "EPUB"
    | Mobi -> "MOBI"
*)

let ebook_format_to_str = function
  | Pdf -> "PDF"
  | Epub -> "EPUB"
  | Mobi -> "MOBI"

(* enum union *)
type book_media_type =
  | Ebook of ebook_format [@warning "-unused-constructor"]
  | Paperbook of float * float [@warning "-unused-constructor"]

let book_media_type_to_str = function
  | Ebook format -> Printf.sprintf "Ebook(%s)" (ebook_format_to_str format)
  | Paperbook (width, height) ->
      Printf.sprintf "Paperbook(%.2f x %.2f)" width height

(* custom record type *)
type book = {
  id : int;
  name : string;
  mutable tags : string list;
  media_type : book_media_type;
}

let book_to_str { id; name; tags; media_type } =
  Printf.sprintf "Book{id=%d; name=\"%s\"; tags=%s; media_type=%s}" id name
    ("["
    ^ (tags |> List.map (Printf.sprintf "\"%s\"") |> String.concat "; ")
    ^ "]")
    (book_media_type_to_str media_type)

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
let () = print_endline "---- mutable"
let book_mutable = ref ocaml_book

(* `!` defer *)
(* `<-` set mutable fields *)
let () = !book_mutable.tags <- [ "ocaml" ]
let () = !book_mutable |> book_to_str |> print_endline
