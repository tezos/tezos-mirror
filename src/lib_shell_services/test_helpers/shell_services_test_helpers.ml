(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lib_test.Qcheck_helpers

let raw_context_arb =
  let open Tezos_shell_services.Block_services in
  let module MapArb = MakeMapArb (TzString.Map) in
  let open QCheck in
  let {gen = bytes_gen; shrink = bytes_shrink_opt; _} = bytes_arb in
  let gen =
    let open Gen in
    (* Factor used to limit the depth of the tree. *)
    let max_depth_factor = 10 in
    fix
      (fun self current_depth_factor ->
        frequency
          [
            (max_depth_factor, map (fun b -> Key b) bytes_gen);
            (max_depth_factor, pure Cut);
            ( current_depth_factor,
              map
                (fun d -> Dir d)
                (MapArb.gen_of_size
                   (0 -- 10)
                   string
                   (self (current_depth_factor / 2))) );
          ])
      max_depth_factor
  in
  let rec shrink =
    let open Iter in
    function
    | Cut -> empty
    | Key bigger_bytes ->
        shrink Cut
        <+> ( of_option_shrink bytes_shrink_opt bigger_bytes
            >|= fun smaller_bytes -> Key smaller_bytes )
    | Dir bigger_raw_context_map ->
        shrink Cut <+> shrink (Key Bytes.empty)
        <+> ( MapArb.shrink
                ~key:Shrink.string
                ~value:shrink
                bigger_raw_context_map
            >|= fun smaller_dir -> Dir smaller_dir )
  in
  let print = Format.asprintf "%a" pp_raw_context in
  make ~print ~shrink gen

(** Strings that are valid Irmin hashes. Taken from the output of:

   [tezos-client rpc get /chains/main/blocks/head/context/merkle_tree/active_delegates_with_rolls]
 *)
let irmin_hashes =
  [
    "CoVbip7pyXZDp1umo3cGUbCWJUA8wDkPbWR56wKqS434DiDSwGWC";
    "CoVuTbwGSJyu9xD7vYYxxcqCFwCPf55UBqu8iqRcHYrs3Gu31v8y";
    "CoUiEnajKeukmYFUgWTJF2z3v24MycpTaomF8a9hRzVy7as9hvgy";
    "CoVngnGTJfudgcayQqtz2ZyWTUFB6zHmhvV1itjncRzYS4wndhH8";
    "CoVe9oDs8t8WgH9JHB3DqbvxCZw1Q5ky7qBsZfMiLeKe6RiSMHn1";
    "CoVXSYbKxP7jJL4ZSZnCsyynEZ6aeR7HR59UVKCDZdMGa8QCLFfW";
    "CoVSQwz1mSz28kNCBso3F3ZHuLj5GXwXovu4byqweTa96bJAzTX6";
    "CoVEow8t8iz6gfxB7daEHjFRD5suzdhb3wNZ5rMnoUTdjbYvxbez";
    "CoVZDcjRgjmKnAhetUtb1AVQYwuUi3fBK7js11vjBETPuG5FcU8o";
    "CoWRLgT2SwZkCWCwyTBxxkxPxYFvTWtHfKqX8MFQ1hNWL4SS1qdU";
    "CoWVK1YzoDnMGrNioKL9Mze6s4XX8Uw9Vp9hPHYXqHfaFpwnmXmA";
    "CoVnWzSVjbYHCQLD53JGJfWRSjUBrkbtCrNMgmsXX6bMhy7CE7E6";
  ]

let irmin_hash_arb = QCheck.oneofl ~print:Fun.id irmin_hashes

let merkle_node_arb =
  let open Tezos_shell_services.Block_services in
  let module MapArb = MakeMapArb (TzString.Map) in
  let open QCheck in
  let open Gen in
  let {gen = raw_context_gen; shrink = raw_context_shrink_opt; _} =
    raw_context_arb
  in
  let {gen = irmin_hash_gen; _} = irmin_hash_arb in
  let gen =
    let max_depth_factor = 4 in
    fix
      (fun self current_depth_factor ->
        frequency
          [
            ( max_depth_factor,
              map
                (fun (kind, hash) -> Hash (kind, hash))
                (pair (oneofl [Contents; Node]) irmin_hash_gen) );
            ( max_depth_factor,
              map (fun raw_context -> Data raw_context) raw_context_gen );
            ( current_depth_factor,
              map
                (fun merkle_node_map -> Continue merkle_node_map)
                (MapArb.gen_of_size
                   (0 -- 10)
                   string
                   (self (current_depth_factor / 2))) );
          ])
      max_depth_factor
  in
  let first_irmin_hash =
    List.hd irmin_hashes |> function None -> assert false | Some hash -> hash
  in
  let rec shrink =
    let open Iter in
    function
    | Hash _ -> empty
    | Data bigger_raw_context ->
        shrink (Hash (Contents, first_irmin_hash))
        <+> ( of_option_shrink raw_context_shrink_opt bigger_raw_context
            >|= fun smaller_raw_context -> Data smaller_raw_context )
    | Continue bigger_mnode ->
        shrink (Hash (Contents, first_irmin_hash))
        <+> shrink (Data Cut)
        <+> ( MapArb.shrink ~key:Shrink.string ~value:shrink bigger_mnode
            >|= fun smaller_mnode -> Continue smaller_mnode )
  in
  let print = Format.asprintf "%a" pp_merkle_node in
  make ~print ~shrink gen

let merkle_tree_arb =
  let open MakeMapArb (TzString.Map) in
  arb_of_size QCheck.Gen.(0 -- 10) QCheck.string merkle_node_arb
