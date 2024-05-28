(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(* Testing
   -------
   Component:    Script
   Invocation:   dune exec tezt/tests/main.exe -- --file order_in_top_level.ml
   Subject:      Test that the storage, code, and parameter sections can appear in any order in a contract script.
*)

(** [interleave x l] inserts [x] into all positions of [l].
    For example, [interleave 1 [2; 3]] will return [[[1; 2; 3]; [2; 1; 3]; [2; 3; 1]]]. *)
let rec interleave (x : 'a) (l : 'a list) : 'a list list =
  match l with
  | [] -> [[x]]
  | h :: t -> (x :: h :: t) :: (interleave x t |> List.map (fun t' -> h :: t'))

(** [permutations l] returns all permutations of [l] *)
let rec permutations : 'a list -> 'a list list = function
  | [] -> [[]]
  | x :: xs -> permutations xs |> List.map (interleave x) |> List.flatten

let test_order_in_top_level =
  Protocol.register_test
    ~__FILE__
    ~title:"Order in top level of the script does not matter."
    ~tags:["client"; "script"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let top_level_elements =
    ["parameter nat"; "storage unit"; "code { CDR; NIL operation; PAIR }"]
  in
  permutations top_level_elements
  |> Lwt_list.iter_s @@ fun elements ->
     let script = String.concat ";\n" elements in
     let* () = Client.typecheck_script client ~scripts:[script] in
     unit

let register ~protocols = test_order_in_top_level protocols
