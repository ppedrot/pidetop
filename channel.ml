let out_channel = ref stdout
let in_channel = ref stdin

let initialize () = 
  output_string stderr "\002";
  flush stderr;
  in_channel := open_in_bin (fst !Flags.pide_fifos);
  out_channel := open_out_bin (snd !Flags.pide_fifos)

let chunk s = string_of_int (String.length s) ^ "\n" ^ s

let send header body = 
  output_string !out_channel (chunk header);
  output_string !out_channel (chunk body);
  flush !out_channel

(* TODO: This is a horribly C-like way of reading a chunk. Really necessary or better functions? *)
let read_chunk len = 
  let n =
    try int_of_string len
    with Failure _ -> raise (Failure ("Coq process: malformed chunk header \"" ^ len ^"\""))
  in
  let chunk = String.make n '\000' in
  really_input !in_channel chunk 0 n;
  chunk

let read_command () =
  try Some (List.map read_chunk (Str.split (Str.regexp ",") (input_line !in_channel)))
  with End_of_file -> None
