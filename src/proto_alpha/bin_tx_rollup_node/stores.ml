(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2589
   Don't use Irmin for store but rather https://github.com/mirage/index *)

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let inode_child_order = `Seeded_hash

  let contents_length_header = Some `Varint
end

module Kv = struct
  open Irmin_pack_unix.KV (Conf)

  include Make (Irmin.Contents.String)
end

type t = Kv.t

let make_info message =
  let date = Unix.gettimeofday () |> Int64.of_float in
  Kv.Info.v ~author:"tx-rollup-node" date ~message

let load data_dir =
  let open Lwt_syntax in
  let* repository = Kv.Repo.v (Irmin_pack.config data_dir) in
  let* branch = Kv.main repository in
  let* () = Event.(emit irmin_store_loaded) data_dir in
  return_ok branch

let close data_dir =
  let open Lwt_syntax in
  let* repository = Kv.Repo.v (Irmin_pack.config data_dir) in
  let* () = Kv.Repo.close repository in
  return_unit

module type REF_CONF = sig
  val location : string list

  type value

  val value_encoding : value Data_encoding.t
end

module type MAP_CONF = sig
  include REF_CONF

  type key

  val key_to_string : key -> string
end

module type REF = sig
  type nonrec t = t

  type value

  val find : t -> value option Lwt.t

  val get : t -> value tzresult Lwt.t

  val set : t -> value -> unit tzresult Lwt.t
end

module type MAP = sig
  type nonrec t = t

  type key

  type value

  val mem : t -> key -> bool Lwt.t

  val find : t -> key -> value option Lwt.t

  val get : t -> key -> value tzresult Lwt.t

  val add : t -> key -> value -> unit tzresult Lwt.t

  val remove : t -> key -> unit tzresult Lwt.t
end

module Make_map (M : MAP_CONF) = struct
  type t = Kv.t

  type key = M.key

  type value = M.value

  let mk key =
    let loc = Kv.Path.v M.location in
    Kv.Path.rcons loc @@ M.key_to_string key

  let render_key key =
    Format.sprintf "%s/%s" (String.concat "/" M.location) (M.key_to_string key)

  let mem store key =
    let key = mk key in
    Kv.mem store key

  let encode key value =
    value
    |> Data_encoding.Binary.to_string M.value_encoding
    |> Result.fold ~ok:return ~error:(fun _ ->
           let json = Data_encoding.Json.construct M.value_encoding value in
           fail @@ Error.Tx_rollup_unable_to_encode_storable_value (key, json))

  let decode key value =
    value
    |> Data_encoding.Binary.of_string M.value_encoding
    |> Result.fold ~ok:return ~error:(fun _ ->
           fail @@ Error.Tx_rollup_unable_to_decode_stored_value (key, value))

  let find store raw_key =
    let open Lwt_syntax in
    let key = mk raw_key in
    let* binaries = Kv.find store key in
    let+ value =
      match binaries with
      | None -> Lwt.return None
      | Some x -> (
          let+ k = decode (render_key raw_key) x in
          match k with Ok x -> Some x | _ -> None)
    in
    value

  let get store raw_key =
    let open Lwt_syntax in
    let key = mk raw_key in
    let* binaries = Kv.get store key in
    decode (render_key raw_key) binaries

  let set rendered_key store key value =
    let open Lwt_tzresult_syntax in
    let info () = make_info rendered_key in
    let*! r = Kv.set ~info store key value in
    match r with
    | Error _ -> fail @@ Error.Tx_rollup_irmin_error "cannot store value"
    | Ok () -> return_unit

  let add store raw_key value =
    let open Lwt_result_syntax in
    let key = mk raw_key in
    let rendered_key = render_key raw_key in
    let* value = encode rendered_key value in
    set rendered_key store key value

  let remove store raw_key =
    let open Lwt_tzresult_syntax in
    let key = mk raw_key in
    let rendered_key = render_key raw_key in
    let info () = make_info rendered_key in
    let*! r = Kv.remove ~info store key in
    match r with
    | Error _ -> fail @@ Error.Tx_rollup_irmin_error "cannot remove value"
    | Ok () -> return_unit
end

module Make_ref (R : REF_CONF) = struct
  type t = Kv.t

  type value = R.value

  let key = Kv.Path.v R.location

  let rendered_key = String.concat "/" R.location

  let decode value =
    value
    |> Data_encoding.Binary.of_string R.value_encoding
    |> Result.fold ~ok:return ~error:(fun _ ->
           fail
           @@ Error.Tx_rollup_unable_to_decode_stored_value (rendered_key, value))

  let encode value =
    value
    |> Data_encoding.Binary.to_string R.value_encoding
    |> Result.fold ~ok:return ~error:(fun _ ->
           let json = Data_encoding.Json.construct R.value_encoding value in
           fail
           @@ Error.Tx_rollup_unable_to_encode_storable_value
                (rendered_key, json))

  let get store =
    let open Lwt_syntax in
    let* binaries = Kv.get store key in
    decode binaries

  let find store =
    let open Lwt_syntax in
    let* binaries = Kv.find store key in
    let+ value =
      match binaries with
      | None -> Lwt.return None
      | Some x -> (
          let+ k = decode x in
          match k with Ok x -> Some x | _ -> None)
    in
    value

  let set_aux store value =
    let open Lwt_tzresult_syntax in
    let info () = make_info rendered_key in
    let*! r = Kv.set ~info store key value in
    match r with
    | Error _ -> fail @@ Error.Tx_rollup_irmin_error "cannot store value"
    | Ok () -> return_unit

  let set store value =
    let open Lwt_result_syntax in
    let* value = encode value in
    set_aux store value
end

module Rollup_origination = Make_ref (struct
  let location = ["tx_rollup"; "origination"]

  type value = Block_hash.t * int32

  let value_encoding =
    Data_encoding.tup2 Block_hash.encoding Data_encoding.int32
end)

module L2_head = Make_ref (struct
  let location = ["tx_rollup"; "head"]

  type value = L2block.hash

  let value_encoding = L2block.Hash.encoding
end)

module Tezos_blocks = Make_map (struct
  let location = ["tezos"; "blocks"]

  type key = Block_hash.t

  type value = L2block.hash

  let key_to_string = Block_hash.to_string

  let value_encoding = L2block.Hash.encoding
end)

module Inboxes = Make_map (struct
  let location = ["tx_rollup"; "inboxes"]

  type key = L2block.hash

  type value = Inbox.t

  let key_to_string = L2block.Hash.to_b58check

  let value_encoding = Inbox.encoding
end)

module L2_blocks = Make_map (struct
  let location = ["tx_rollup"; "blocks"]

  type key = L2block.hash

  type value = L2block.header

  let key_to_string = L2block.Hash.to_b58check

  let value_encoding = L2block.header_encoding
end)

module Rollup_levels = Make_map (struct
  let location = ["tx_rollup"; "levels"]

  type key = L2block.level

  type value = L2block.hash

  let key_to_string = L2block.level_to_string

  let value_encoding = L2block.Hash.encoding
end)
