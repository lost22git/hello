open Printf
open Lwt
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

let () = print_endline "=== http_client ==="

type response = { origin : string } [@@deriving show, yojson { strict = false }]

let decode_response json_str =
  json_str |> Yojson.Safe.from_string |> response_of_yojson

let main =
  let url = Uri.of_string "https://httpbin.org/ip" in
  Client.get url >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  printf "Code: %d\n" code;
  printf "Header:\n%s\n" (resp |> Response.headers |> Header.to_string);
  body |> Body.to_string >>= fun str ->
  printf "Body length: %d\n" (String.length str);
  print_endline str;
  Lwt.return str

let () =
  let body = Lwt_main.run main in
  match decode_response body with
  | Ok response -> print_endline ("Decoded:\n" ^ show_response response)
  | Error ex -> print_endline ("ERROR: " ^ ex)
