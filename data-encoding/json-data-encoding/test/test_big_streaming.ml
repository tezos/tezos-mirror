(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let big_encoding =
  let open Json_encoding in
  list
    (obj3 (req "z" (list null)) (req "o" (seq bool)) (req "t" (array string)))

let () = Random.self_init ()

let string () = String.init 8 (fun _ -> Char.unsafe_chr (65 + Random.int 26))

let bool i =
  (* bools are placed in a Seq, which is re-evaluated, so the "randomness"
     needs to be deterministic. *)
  i mod 3 = 0 || i mod 5 = 0

let seq_unfold f init =
  (* for compatibility with OCaml < 4.11 we redefine Seq.unfold *)
  let rec aux acc () =
    match f acc with
    | None -> Seq.Nil
    | Some (elt, acc) -> Seq.Cons (elt, aux acc)
  in
  aux init

let o3 len =
  let len = max len 0 in
  ( List.init len (fun _ -> ()),
    seq_unfold (fun l -> if l >= len then None else Some (bool l, l + 1)) 0,
    Array.init len (fun _ -> string ()) )

let big len = List.init len (fun _ -> o3 (len / 10))

let test1 len =
  Printf.printf "Testing big-value streaming (%d)\n%!" len ;
  let big_value = big len in
  let direct_seq = Json_encoding.construct_seq big_encoding big_value in
  let ezjsonm = Json_encoding.construct big_encoding big_value in
  let indirect_seq = Json_encoding.jsonm_lexeme_seq_of_ezjson ezjsonm in
  assert (List.of_seq direct_seq = List.of_seq indirect_seq) ;
  Printf.printf "Success for big-value streaming (%d)\n%!" len

let is_jsoo =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> true
  | Native | Bytecode | Other _ -> false

let () =
  test1 0 ;
  test1 16 ;
  test1 63 ;
  test1 514 ;
  test1 1001 ;
  (* This test running with js_of_ocaml consumes too much memory for
     the CI *)
  if not is_jsoo then test1 4321
