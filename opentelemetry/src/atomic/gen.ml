let atomic_before_412 =
  {|
  type 'a t = {mutable x: 'a}
  let[@inline] make x = {x}
  let[@inline] get {x} = x
  let[@inline] set r x = r.x <- x

  let[@inline never] exchange r x =
    (* critical section *)
    let y = r.x in
    r.x <- x;
    (* end critical section *)
    y

  let[@inline never] compare_and_set r seen v =
    (* critical section *)
    if r.x == seen then (
      r.x <- v;
      true
    ) else false

  let[@inline never] fetch_and_add r x =
    let v = r.x in
    r.x <- x + r.x;
    v

  let[@inline never] incr r = r.x <- 1 + r.x
  let[@inline never] decr r = r.x <- r.x - 1
  |}

let atomic_after_412 = {|include Stdlib.Atomic|}

let write_file file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

let copy_file file1 file2 =
  let oc = open_out file2 in
  let ic = open_in file1 in
  let buf = Bytes.create 1024 in
  try
    while true do
      let n = input ic buf 0 (Bytes.length buf) in
      if n = 0 then raise End_of_file;
      output oc buf 0 n
    done
  with End_of_file -> ()

let () =
  let version = Scanf.sscanf Sys.ocaml_version "%d.%d.%s" (fun x y _ -> x, y) in
  write_file "atomic.ml"
    (if version >= (4, 12) then
       atomic_after_412
     else
       atomic_before_412);
  copy_file
    (if version >= (4, 12) then
       "atomic.post412.mli"
     else
       "atomic.pre412.mli")
    "atomic.mli";
  ()
