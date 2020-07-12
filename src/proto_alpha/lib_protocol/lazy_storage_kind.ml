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

module type Temp_id = sig
  type t

  val equal : t -> t -> bool

  val init : t

  val next : t -> t
end

module type ID = sig
  type t

  val compare : t -> t -> int

  val encoding : t Data_encoding.t

  val rpc_arg : t RPC_arg.arg

  val init : t

  (** In the protocol, to be used in parse_data only *)
  val parse_z : Z.t -> t

  (** In the protocol, to be used in unparse_data only *)
  val unparse_to_z : t -> Z.t

  val next : t -> t

  val of_legacy_USE_ONLY_IN_Legacy_big_map_diff : Z.t -> t

  val to_legacy_USE_ONLY_IN_Legacy_big_map_diff : t -> Z.t

  module Temp : Temp_id with type t = private t

  val is_temp : t -> bool

  val path_length : int

  val to_path : t -> string list -> string list

  val of_path : string list -> t option
end

module type Title = sig
  val title : string
end

module type TitleWithId = sig
  val title : string

  module Id : ID
end

module MakeId (Title : Title) : TitleWithId = struct
  let title = Title.title

  let title_words = String.map (function '_' -> ' ' | c -> c) title

  let rpc_arg_error = Format.sprintf "Cannot parse %s id" title_words

  let description = Format.sprintf "A %s identifier" title_words

  let name = title ^ "_id"

  let encoding_title = String.capitalize_ascii title_words ^ " identifier"

  module Id = struct
    type t = Z.t

    let compare = Z.compare

    let encoding =
      Data_encoding.def name ~title:encoding_title ~description Data_encoding.z

    let rpc_arg =
      let construct = Z.to_string in
      let destruct hash =
        match Z.of_string hash with
        | exception _ ->
            Error rpc_arg_error
        | id ->
            Ok id
      in
      RPC_arg.make ~descr:description ~name ~construct ~destruct ()

    let init = Z.zero

    let parse_z (z : Z.t) : t = z

    let unparse_to_z (z : t) : Z.t = z

    let next = Z.succ

    let of_legacy_USE_ONLY_IN_Legacy_big_map_diff (z : Z.t) : t = z

    let to_legacy_USE_ONLY_IN_Legacy_big_map_diff (z : t) : Z.t = z

    module Temp = struct
      type nonrec t = t

      let equal = Z.equal

      let init = Z.of_int ~-1

      let next z = Z.sub z Z.one
    end

    let is_temp z = Compare.Z.(z < Z.zero)

    let path_length = 1

    let to_path z l = Z.to_string z :: l

    let of_path = function
      | [] | _ :: _ :: _ ->
          None
      | [z] ->
          Some (Z.of_string z)
  end
end

module Big_map = struct
  include MakeId (struct
    let title = "big_map"
  end)

  type alloc = {key_type : Script_repr.expr; value_type : Script_repr.expr}

  type update = {
    key : Script_repr.expr;
        (** The key is ignored by [apply_update] but is shown in the receipt,
            as specified in [print_big_map_diff]. *)
    key_hash : Script_expr_hash.t;
    value : Script_repr.expr option;
  }

  type updates = update list

  let alloc_encoding =
    let open Data_encoding in
    conv
      (fun {key_type; value_type} -> (key_type, value_type))
      (fun (key_type, value_type) -> {key_type; value_type})
      (obj2
         (req "key_type" Script_repr.expr_encoding)
         (req "value_type" Script_repr.expr_encoding))

  let update_encoding =
    let open Data_encoding in
    conv
      (fun {key_hash; key; value} -> (key_hash, key, value))
      (fun (key_hash, key, value) -> {key_hash; key; value})
      (obj3
         (req "key_hash" Script_expr_hash.encoding)
         (req "key" Script_repr.expr_encoding)
         (opt "value" Script_repr.expr_encoding))

  let updates_encoding = Data_encoding.list update_encoding
end

type ('id, 'alloc, 'updates) t =
  | Big_map : (Big_map.Id.t, Big_map.alloc, Big_map.updates) t

type ex = E : (_, _, _) t -> ex

let all = [(0, E Big_map)]

type (_, _) cmp = Eq : ('a, 'a) cmp | Lt : (_, _) cmp | Gt : (_, _) cmp

let compare :
    type i1 a1 u1 i2 a2 u2.
    (i1, a1, u1) t -> (i2, a2, u2) t -> (i1 * a1 * u1, i2 * a2 * u2) cmp =
 fun k1 k2 -> match (k1, k2) with (Big_map, Big_map) -> Eq

module Temp_ids = struct
  type ('i, 'a, 'u) kind = ('i, 'a, 'u) t

  type t = {big_map : Big_map.Id.Temp.t}

  let init = {big_map = Big_map.Id.Temp.init}

  let fresh : type i a u. (i, a, u) kind -> t -> t * i =
   fun kind temp_ids ->
    match kind with
    | Big_map ->
        let big_map = Big_map.Id.Temp.next temp_ids.big_map in
        ({big_map}, (big_map :> Big_map.Id.t))
   [@@coq_axiom "gadt"]

  let fold_s :
      type i a u.
      (i, a, u) kind -> ('acc -> i -> 'acc Lwt.t) -> t -> 'acc -> 'acc Lwt.t =
   fun kind f temp_ids acc ->
    let helper (type j) (module Temp_id : Temp_id with type t = j) ~last f =
      let rec aux acc id =
        if Temp_id.equal id last then Lwt.return acc
        else f acc id >>= fun acc -> aux acc (Temp_id.next id)
      in
      aux acc Temp_id.init
    in
    match kind with
    | Big_map ->
        helper
          (module Big_map.Id.Temp)
          ~last:temp_ids.big_map
          (fun acc temp_id -> f acc (temp_id :> i))
   [@@coq_axiom "gadt"]
end
