(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type _ key = ..

module IMap = Map.Make (Int)

type (_, _) eq = Eq : ('a, 'a) eq

module type KEY = sig
  type t

  type r

  val proj : 'a key -> (t * ('a, r) eq) option

  val compare : t -> t -> int
end

module type REGISTERED_KEY = sig
  val id : int

  include KEY
end

let registered_keys = ref IMap.empty

let register_key (module K : KEY) =
  let ((module K : REGISTERED_KEY) as key) =
    (module struct
      let id = IMap.cardinal !registered_keys

      include K
    end)
  in

  registered_keys := IMap.add K.id key !registered_keys

module Key = struct
  type 'a t = 'a key

  type ('a, 'b) acc =
    | Init : 'a t * 'b t -> ('a, 'b) acc
    | Compare_k1_with : 'a t * int -> ('a, 'b) acc
    | Compare_k2_with : int * 'b t -> ('a, 'b) acc
    | Res : ('a, 'b) Dmap.cmp -> ('a, 'b) acc

  let dmap_cmp_of_int (type a b c) (eq_a : (a, c) eq) (eq_b : (b, c) eq) x :
      (a, b) Dmap.cmp =
    match (eq_a, eq_b) with
    | Eq, Eq ->
        if x = 0 then (Dmap.Eq : (c, c) Dmap.cmp) else if x < 0 then Lt else Gt

  let dmap_cmp_of_int' (type a b) x : (a, b) Dmap.cmp = if x < 0 then Lt else Gt

  let compare (type a b) (k1 : a t) (k2 : b t) =
    let candidates = IMap.to_seq !registered_keys in
    let acc =
      Seq.fold_left
        (fun (acc : (a, b) acc) (i, (module K : REGISTERED_KEY)) ->
          match acc with
          | Init (k1, k2) -> (
              match (K.proj k1, K.proj k2) with
              | Some (t1, e), Some (t2, e') ->
                  Res (K.compare t1 t2 |> dmap_cmp_of_int e e')
              | Some _t1, None -> Compare_k2_with (i, k2)
              | None, Some _t2 -> Compare_k1_with (k1, i)
              | None, None -> Init (k1, k2))
          | Compare_k2_with (i, k2) -> (
              match K.proj k2 with
              | Some _ -> Res (Int.compare i K.id |> dmap_cmp_of_int')
              | None -> Compare_k2_with (i, k2))
          | Compare_k1_with (k1, i) -> (
              match K.proj k1 with
              | Some _ -> Res (Int.compare K.id i |> dmap_cmp_of_int')
              | None -> Compare_k1_with (k1, i))
          | Res x -> Res x)
        (Init (k1, k2))
        candidates
    in
    match acc with Res x -> x | _ -> Test.fail "Using an unregistered key"
end

module Plugins_state = Dmap.Make (Key)

type t = {
  home_dir : string;
  http_client : Http_client.t;
  mutable plugins_state : Plugins_state.t;
}

let add (type a) (key : a key) (value : a) state =
  state.plugins_state <- Plugins_state.add key value state.plugins_state

let find (type a) ?(default : a option) (key : a key) state =
  match default with
  | None -> Plugins_state.find key state.plugins_state
  | Some x -> (
      match Plugins_state.find_opt key state.plugins_state with
      | Some y -> y
      | None -> x)

let find_opt (type a) (key : a key) state =
  Plugins_state.find_opt key state.plugins_state

let initial_state ~home_dir () =
  {
    home_dir;
    http_client = Http_client.create ();
    plugins_state = Plugins_state.empty;
  }

let http_client {http_client; _} = http_client

let home_dir {home_dir; _} = home_dir
