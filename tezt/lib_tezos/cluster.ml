(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "cluster" ^ string_of_int index

let create ?path ?name count arguments =
  let name = match name with None -> fresh_name () | Some name -> name in
  List.map
    (fun i -> Node.create ?path ~name:(name ^ "." ^ string_of_int i) arguments)
    (range 1 count)

let meta_connect connect a b = List.iter (fun a -> List.iter (connect a) b) a

let rec meta_clique connect = function
  | [] -> ()
  | head :: tail ->
      List.iter (connect head) tail ;
      meta_clique connect tail

let meta_ring connect nodes =
  match nodes with
  | [] -> ()
  | first :: _ ->
      let rec loop = function
        | [] ->
            (* We checked that the list was not empty already. *)
            assert false
        | [last] -> connect last first
        | a :: (b :: _ as tail) ->
            connect a b ;
            loop tail
      in
      loop nodes

let meta_star connect center other_nodes =
  List.iter (connect center) other_nodes

let connect = meta_connect Node.add_peer

let clique = meta_clique Node.add_peer

let ring = meta_ring Node.add_peer

let star = meta_star Node.add_peer

let start ?(public = false) nodes =
  let start_node node =
    let* () = Node.identity_generate node in
    let* () = Node.config_init node [] in
    let* () = Node.run node (if public then [] else [Private_mode]) in
    Node.wait_for_ready node
  in
  Lwt_list.iter_p start_node nodes
