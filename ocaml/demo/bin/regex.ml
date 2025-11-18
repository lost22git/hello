let () = print_endline "=== regex ==="

let extract_numbers s =
  let re = Str.regexp "[0-9]+" in
  let rec aux pos acc =
    try
      let _ = Str.search_forward re s pos in
      let m = Str.matched_group 0 s in
      aux (Str.match_end ()) (m :: acc)
    with Not_found -> List.rev acc
  in
  aux 0 []

let () =
  let nums = extract_numbers "abc234bc33sdfs" in
  List.iter print_endline nums
