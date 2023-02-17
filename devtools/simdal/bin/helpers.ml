(** {2 Generic helpers } *)

(** Measure the execution time of some function *)
let chrono name f =
  let t0 = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  Format.printf "%s: %f seconds@." name (t1 -. t0) ;
  res

(** [memoize f] memoizes all calls to [f]. There's no bound on the number of memoized
    inputs: be careful of memory leaks. *)
let memoize ?(init_size = 1000) f =
  let table = Hashtbl.create init_size in
  fun x ->
    match Hashtbl.find_opt table x with
    | None ->
        let res = f x in
        Hashtbl.add table x res ;
        res
    | Some res -> res

(** Print a markdown-friendly matrix *)
let pp_mat :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'r -> unit) ->
    header:'c array ->
    rows:'r array ->
    ('r -> 'c -> 'a) ->
    Format.formatter ->
    ?no_row_label:bool ->
    unit ->
    unit =
 fun pp_elt pp_col pp_row ~header ~rows f fmtr ?(no_row_label = false) () ->
  let ncols = Array.length header + 1 in
  let nrows = Array.length rows + 1 in
  let elts = Array.make_matrix nrows ncols "" in
  let max_lengths = Array.make ncols 0 in
  elts.(0).(0) <- "" ;
  for col = 0 to ncols - 2 do
    let s = Format.asprintf "%a" pp_col header.(col) in
    max_lengths.(col + 1) <- Int.max max_lengths.(col + 1) (String.length s) ;
    elts.(0).(col + 1) <- s
  done ;
  for row = 0 to nrows - 2 do
    let s = Format.asprintf "%a" pp_row rows.(row) in
    max_lengths.(0) <- Int.max max_lengths.(0) (String.length s) ;
    elts.(row + 1).(0) <- s
  done ;
  for row = 1 to nrows - 1 do
    for col = 1 to ncols - 1 do
      let s = Format.asprintf "%a" pp_elt (f rows.(row - 1) header.(col - 1)) in
      max_lengths.(col) <- Int.max max_lengths.(col) (String.length s) ;
      elts.(row).(col) <- s
    done
  done ;
  let whitespaced col s =
    let delta = max_lengths.(col) - String.length s in
    if delta = 0 then s else s ^ String.make delta ' '
  in
  let dashes col = String.make max_lengths.(col) '-' in
  Format.open_vbox 0 ;
  for row = 0 to nrows - 1 do
    Format.pp_print_string fmtr "| " ;
    let col_start = if no_row_label then 1 else 0 in
    for col = col_start to ncols - 1 do
      Format.pp_print_string fmtr (whitespaced col elts.(row).(col)) ;
      Format.pp_print_string fmtr " | "
    done ;
    Format.fprintf fmtr "@;" ;
    if row = 0 then (
      Format.pp_print_string fmtr "| " ;
      for col = col_start to ncols - 1 do
        Format.pp_print_string fmtr (dashes col) ;
        Format.pp_print_string fmtr " | "
      done ;
      Format.fprintf fmtr "@;")
  done ;
  Format.close_box ()
