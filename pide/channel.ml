let out_channel = ref None
let in_channel = ref None

let initialize () = 
  output_string stderr "\002";
  flush stderr;
  let in_ch, out_ch = Spawned.get_channels () in
  in_channel := Some in_ch;
  out_channel := Some out_ch

let chunk s = string_of_int (String.length s) ^ "\n" ^ s

let send header body = 
  output_string (Option.get !out_channel) (chunk header);
  output_string (Option.get !out_channel) (chunk body);
  flush (Option.get !out_channel)

let read_chunk len = 
  let n =
    try int_of_string len
    with Failure _ ->
      raise (Failure ("Coq process: malformed chunk header \"" ^ len ^"\""))
  in
  let chunk = Bytes.make n '\000' in
  CThread.thread_friendly_really_read
    (Option.get !in_channel) chunk ~off:0 ~len:n;
  Bytes.to_string chunk

let delim = Str.regexp ","

let read_command () =
  try
    let line =
      CThread.thread_friendly_really_read_line (Option.get !in_channel) in
    let chunks = Str.split delim line in
    Some (List.map read_chunk chunks)
  with End_of_file -> None
