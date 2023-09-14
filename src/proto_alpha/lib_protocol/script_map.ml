(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Script_typed_ir

let make x = Map_tag x

let get_module (Map_tag x) = x

let empty_from : type a b c. (a, b) map -> (a, c) map =
 fun (Map_tag (module Box)) ->
  Map_tag
    (module struct
      type key = a

      type value = c

      module OPS = Box.OPS

      let boxed = OPS.empty

      let size = 0
    end)

let empty : type a b. a comparable_ty -> (a, b) map =
 fun ty ->
  let module OPS = struct
    let key_size = Gas_comparable_input_size.size_of_comparable_value ty

    include Map.Make (struct
      type t = a

      let compare = Script_comparable.compare_comparable ty
    end)
  end in
  Map_tag
    (module struct
      type key = a

      type value = b

      module OPS = OPS

      let boxed = OPS.empty

      let size = 0
    end)

let get : type key value. key -> (key, value) map -> value option =
 fun k (Map_tag (module Box)) -> Box.OPS.find k Box.boxed

let update : type a b. a -> b option -> (a, b) map -> (a, b) map =
 fun k v (Map_tag (module Box)) ->
  let boxed, size =
    let contains =
      match Box.OPS.find k Box.boxed with None -> false | _ -> true
    in
    match v with
    | Some v -> (Box.OPS.add k v Box.boxed, Box.size + if contains then 0 else 1)
    | None -> (Box.OPS.remove k Box.boxed, Box.size - if contains then 1 else 0)
  in
  Map_tag
    (module struct
      type key = a

      type value = b

      module OPS = Box.OPS

      let boxed = boxed

      let size = size
    end)

let mem : type key value. key -> (key, value) map -> bool =
 fun k (Map_tag (module Box)) ->
  match Box.OPS.find k Box.boxed with None -> false | _ -> true

let fold :
    type key value acc.
    (key -> value -> acc -> acc) -> (key, value) map -> acc -> acc =
 fun f (Map_tag (module Box)) -> Box.OPS.fold f Box.boxed

let fold_es :
    type key value acc.
    (key -> value -> acc -> acc tzresult Lwt.t) ->
    (key, value) map ->
    acc ->
    acc tzresult Lwt.t =
 fun f (Map_tag (module Box)) -> Box.OPS.fold_es f Box.boxed

let size : type key value. (key, value) map -> Script_int.n Script_int.num =
 fun (Map_tag (module Box)) -> Script_int.(abs (of_int Box.size))

let map_es_in_context :
    type context key value value'.
    (context -> key -> value -> (value' * context) tzresult Lwt.t) ->
    context ->
    (key, value) map ->
    ((key, value') map * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun f ctxt (Map_tag (module Box)) ->
    let+ map, ctxt =
      Box.OPS.fold_es
        (fun key value (map, ctxt) ->
          let+ value, ctxt = f ctxt key value in
          (Box.OPS.add key value map, ctxt))
        Box.boxed
        (Box.OPS.empty, ctxt)
    in
    ( Map_tag
        (module struct
          type key = Box.key

          type value = value'

          module OPS = Box.OPS

          let boxed = map

          let size = Box.size
        end),
      ctxt )
