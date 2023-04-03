open Data_encoding

(* length in bytes for some num representations *)

let l_Z i =
  String.length (Stdlib.Result.get_ok (Binary.to_string z (Z.of_int64 i)))

let l_N i =
  String.length (Stdlib.Result.get_ok (Binary.to_string n (Z.of_int64 i)))

(* note about compact: we use a full byte of tag, but if there are two free bits
   of shared tag this is free. *)
let l_C i =
  String.length
    (Stdlib.Result.get_ok
       (Binary.to_string (Compact.make ~tag_size:`Uint8 Compact.int64) i))

(* [find l target 0L Int64.max_int] finds a pair [(a,b)] of int64 such that:
   - a + 1 = b
   - l a = target
   - l b > tagert
   I.e., [a] is the last of the int64 to use target bytes and [b] is the first
   to use more than target bytes
*)
let rec find l target low high =
  if low = Int64.sub high 1L then (low, high)
  else
    let llow = l low in
    let lhigh = l high in
    (*     Printf.printf "FIND: %Ld(%d) - %Ld(%d)\n%!" low llow high lhigh; *)
    assert (llow < lhigh) ;
    assert (llow <= target) ;
    assert (target < lhigh) ;
    let middle =
      let open Int64 in
      add (div low 2L) (div high 2L)
    in
    let middle =
      let open Int64 in
      min middle (sub high 1L)
    in
    let lmiddle = l middle in
    if lmiddle <= target then find l target middle high
    else find l target low middle

let cutoffs_Z = List.init 9 (fun i -> find l_Z (i + 1) 0L Int64.max_int)

let cutoffs_N = List.init 8 (fun i -> find l_N (i + 1) 0L Int64.max_int)

let cutoffs_C =
  (* because of the share tag, we round up to the next full bytes, but the sizes
     should be considered 1byte+2bits, 2bytes+2bits, and 4bytes+2bits *)
  List.map (fun i -> find l_C i 0L Int64.max_int) [2; 3; 5]

let () =
  (* print out interesting values here *)
  ()
