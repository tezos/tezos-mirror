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

  (* Remove me after Granada *)
  val threshold_for_edo_florence_dangling_lazy_storage_cleanup : t

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

  val is_temp : t -> bool

  val path_length : int

  val to_path : t -> string list -> string list

  val of_path : string list -> t option

  val of_legacy_USE_ONLY_IN_Legacy_big_map_diff : Z.t -> t

  val to_legacy_USE_ONLY_IN_Legacy_big_map_diff : t -> Z.t
end

module type Title = sig
  val title : string
end

module type TitleWithId = sig
  val title : string

  module Id : sig
    include ID

    module Temp : Temp_id with type t = private t
  end

  module IdSet : Set.S with type elt = Id.t
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
        | exception _ -> Error rpc_arg_error
        | id -> Ok id
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

      (* Remove me after Granada *)
      let threshold_for_edo_florence_dangling_lazy_storage_cleanup =
        Z.of_int ~-99

      let next z = Z.sub z Z.one
    end

    let is_temp z = Compare.Z.(z < Z.zero)

    let path_length = 1

    let to_path z l = Z.to_string z :: l

    let of_path = function
      | [] | _ :: _ :: _ -> None
      | [z] -> Some (Z.of_string z)
  end

  module IdSet = Set.Make (Id)
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

module Sapling_state = struct
  include MakeId (struct
    let title = "sapling_state"
  end)

  type alloc = {memo_size : Sapling_repr.Memo_size.t}

  type updates = Sapling_repr.diff

  let alloc_encoding =
    let open Data_encoding in
    conv
      (fun {memo_size} -> memo_size)
      (fun memo_size -> {memo_size})
      (obj1 (req "memo_size" Sapling_repr.Memo_size.encoding))

  let updates_encoding = Sapling_repr.diff_encoding
end

(*
  When adding cases to this type, grep for [new lazy storage kind] in the code
  for locations to update.
  It must be:
    - the value [all] right below,
    - modules [Temp_ids], [IdSet] below,
    - the rest should be guided by type errors.
*)
type ('id, 'alloc, 'updates) t =
  | Big_map : (Big_map.Id.t, Big_map.alloc, Big_map.updates) t
  | Sapling_state
      : (Sapling_state.Id.t, Sapling_state.alloc, Sapling_state.updates) t

type ex = Ex_Kind : (_, _, _) t -> ex

(* /!\ Don't forget to add new lazy storage kinds here. /!\ *)
let all = [(0, Ex_Kind Big_map); (1, Ex_Kind Sapling_state)]

type (_, _) cmp = Eq : ('a, 'a) cmp | Neq

let equal :
    type i1 a1 u1 i2 a2 u2.
    (i1, a1, u1) t -> (i2, a2, u2) t -> (i1 * a1 * u1, i2 * a2 * u2) cmp =
 fun k1 k2 ->
  match (k1, k2) with
  | (Big_map, Big_map) -> Eq
  | (Sapling_state, Sapling_state) -> Eq
  | (Big_map, _) -> Neq
  | (_, Big_map) -> Neq

type ('i, 'a, 'u) kind = ('i, 'a, 'u) t

module Temp_ids = struct
  type t = {
    big_map : Big_map.Id.Temp.t;
    sapling_state : Sapling_state.Id.Temp.t;
  }

  let init =
    {big_map = Big_map.Id.Temp.init; sapling_state = Sapling_state.Id.Temp.init}

  (* Remove me after Granada *)
  let threshold_for_edo_florence_dangling_lazy_storage_cleanup =
    {
      big_map =
        Big_map.Id.Temp.threshold_for_edo_florence_dangling_lazy_storage_cleanup;
      sapling_state =
        Sapling_state.Id.Temp
        .threshold_for_edo_florence_dangling_lazy_storage_cleanup;
    }

  let fresh : type i a u. (i, a, u) kind -> t -> t * i =
   fun kind temp_ids ->
    match kind with
    | Big_map ->
        let big_map = Big_map.Id.Temp.next temp_ids.big_map in
        ({temp_ids with big_map}, (temp_ids.big_map :> Big_map.Id.t))
    | Sapling_state ->
        let sapling_state = Sapling_state.Id.Temp.next temp_ids.sapling_state in
        ( {temp_ids with sapling_state},
          (temp_ids.sapling_state :> Sapling_state.Id.t) )
   [@@coq_axiom_with_reason "gadt"]

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
    | Sapling_state ->
        helper
          (module Sapling_state.Id.Temp)
          ~last:temp_ids.sapling_state
          (fun acc temp_id -> f acc (temp_id :> i))
   [@@coq_axiom_with_reason "gadt"]
end

module IdSet = struct
  type t = {big_map : Big_map.IdSet.t; sapling_state : Sapling_state.IdSet.t}

  type 'acc fold_f = {f : 'i 'a 'u. ('i, 'a, 'u) kind -> 'i -> 'acc -> 'acc}

  let empty =
    {big_map = Big_map.IdSet.empty; sapling_state = Sapling_state.IdSet.empty}

  let mem (type i a u) (kind : (i, a, u) kind) (id : i) set =
    match (kind, set) with
    | (Big_map, {big_map; _}) -> Big_map.IdSet.mem id big_map
    | (Sapling_state, {sapling_state; _}) ->
        Sapling_state.IdSet.mem id sapling_state

  let add (type i a u) (kind : (i, a, u) kind) (id : i) set =
    match (kind, set) with
    | (Big_map, {big_map; _}) ->
        let big_map = Big_map.IdSet.add id big_map in
        {set with big_map}
    | (Sapling_state, {sapling_state; _}) ->
        let sapling_state = Sapling_state.IdSet.add id sapling_state in
        {set with sapling_state}

  let diff set1 set2 =
    let big_map = Big_map.IdSet.diff set1.big_map set2.big_map in
    let sapling_state =
      Sapling_state.IdSet.diff set1.sapling_state set2.sapling_state
    in
    {big_map; sapling_state}

  let fold (type i a u) (kind : (i, a, u) kind) (f : i -> 'acc -> 'acc) set
      (acc : 'acc) =
    match (kind, set) with
    | (Big_map, {big_map; _}) -> Big_map.IdSet.fold f big_map acc
    | (Sapling_state, {sapling_state; _}) ->
        Sapling_state.IdSet.fold f sapling_state acc

  let fold_all f set acc =
    List.fold_left
      (fun acc (_, Ex_Kind kind) -> fold kind (f.f kind) set acc)
      acc
      all
end
