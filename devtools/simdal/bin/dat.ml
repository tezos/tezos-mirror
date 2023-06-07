type t = {
  file : string;
  sep : char;
  header : string array;
  rows : float array array;
}

let default_sep = ' '

let ( let* ) = Option.bind

let return = Option.some

let make ?(sep = default_sep) file header rows = {file; sep; header; rows}

let load_header ?(sep = default_sep) file =
  In_channel.with_open_text file @@ fun ic ->
  let* line = In_channel.input_line ic in
  return (String.split_on_char sep line |> Array.of_list)

let load ?(sep = default_sep) file =
  In_channel.with_open_text file @@ fun ic ->
  let read_line ic =
    let* line = In_channel.input_line ic in
    return (String.split_on_char sep line)
  in
  let rec loop acc =
    match read_line ic with
    | None -> List.rev acc
    | Some line ->
        let row = line |> List.map float_of_string |> Array.of_list in
        loop (row :: acc)
  in
  let* header = read_line ic in
  let header = Array.of_list header in
  let rows = loop [] |> Array.of_list in
  return {file; sep; header; rows}

let append_rows sep cols to_string oc rows =
  let write_line oc to_string row =
    let row_cols = Array.length row in
    if row_cols <> cols then
      Format.kasprintf
        failwith
        "Dat: row has wrong dimensions (expected %d, got %d)"
        cols
        row_cols ;
    let first = ref true in
    Array.iter
      (fun x ->
        if !first then first := false else output_char oc sep ;
        let s = to_string x in
        output_string oc s)
      row ;
    output_string oc "\n"
  in
  Array.iter (fun row -> write_line oc to_string row) rows

let write {file; sep; header; rows} =
  let cols = Array.length header in
  Out_channel.with_open_text file @@ fun oc ->
  append_rows sep cols Fun.id oc [|header|] ;
  append_rows sep cols Float.to_string oc rows

let append ?(sep = default_sep) ~into src =
  let* src = load ~sep src in
  let into_exists = Sys.file_exists into in
  (if into_exists then
   (* Check that header match *)
   match load_header ~sep into with
   | None -> invalid_arg "append: could not load header"
   | Some into_header -> (
       match Array.for_all2 String.equal into_header src.header with
       | (exception _) | false -> invalid_arg "append: header mismatch"
       | true -> ())) ;
  Out_channel.with_open_gen
    [Open_wronly; Open_creat; Open_text; Open_append]
    0o666
    into
    (fun oc ->
      let cols = Array.length src.header in
      if not into_exists then
        (* Target file does not exist: copy header of [src] in [into] *)
        append_rows sep cols Fun.id oc [|src.header|] ;
      append_rows sep cols Float.to_string oc src.rows) ;
  return ()
