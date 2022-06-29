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

(* ------------------------------------------------------------------------- *)
(* Union find parameterized over a persistent store. *)

module type S = sig
  module M : Monads.State_sig

  type key = int

  val add : key -> unit M.t

  val find : key -> key M.t

  val union : key -> key -> key M.t

  val show : string M.t
end

module UF : S = struct
  type node = T of {rank : int} | Ptr of key

  and key = int

  module S =
    Stores.Map
      (Int_map)
      (struct
        type key = int

        type value = node

        let key_to_string = string_of_int

        let value_to_string (x : value) =
          match x with
          | T {rank} -> Printf.sprintf "[%d]" rank
          | Ptr k -> Printf.sprintf "ptr(%d)" k
      end)

  module M = Monads.Make_state_monad (S)

  let add (k : key) =
    let open M in
    set k (T {rank = 1})

  let rec get_root (k : key) (acc : key list) =
    let open M in
    get k >>= function
    | None ->
        let msg = Printf.sprintf "UF.get_root: invalid key %d" k in
        Stdlib.failwith msg
    | Some (T {rank}) ->
        let ptr_to_root = Ptr k in
        iter_list (fun key -> set key ptr_to_root) acc >>= fun () ->
        return (k, rank)
    | Some (Ptr k') -> get_root k' (k :: acc)

  let find (k : key) =
    let open M in
    get_root k [] >>= fun (res, _) -> return res

  let union k1 k2 =
    let open M in
    get_root k1 [] >>= fun (k1, rank1) ->
    get_root k2 [] >>= fun (k2, rank2) ->
    if k1 = k2 then return k1
    else if rank1 < rank2 then set k1 (Ptr k2) >>= fun () -> return k2
    else if rank1 > rank2 then set k2 (Ptr k1) >>= fun () -> return k1
    else
      let new_root = T {rank = rank1 + 1} in
      set k2 (Ptr k1) >>= fun () ->
      set k1 new_root >>= fun () -> return k1

  let show s = (S.to_string s, s)
end
