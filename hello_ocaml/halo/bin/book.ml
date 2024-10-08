open Printf

(* enum *)
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
  | Ebook format -> sprintf "Ebook(%s)" (ebook_format_to_str format)
  | Paperbook (width, height) -> sprintf "Paperbook(%.2f x %.2f)" width height

(* custom record type *)
type book = {
  id : int;
  name : string;
  mutable tags : string list;
  media_type : book_media_type;
}

let book_to_str { id; name; tags; media_type } =
  sprintf "Book{id=%d; name=\"%s\"; tags=%s; media_type=%s}" id name
    ("[" ^ (tags |> List.map (sprintf "\"%s\"") |> String.concat "; ") ^ "]")
    (book_media_type_to_str media_type)
