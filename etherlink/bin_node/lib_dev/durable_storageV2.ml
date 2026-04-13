(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Low-level durable storage primitives *)

let inspect_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* value = Pvm.Kernel.find_key_in_durable evm_state key in
  Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_bytes value

let modify_durable ?edit_readonly ~key ~value evm_state =
  Pvm.Kernel.set_durable_value ?edit_readonly evm_state key value

let delete_durable ~kind evm_state path =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn path in
  let* pvm_state = Pvm.Kernel.decode evm_state in
  let open Tezos_scoru_wasm.Wasm_pvm_state.Internal_state in
  let* durable =
    Tezos_scoru_wasm.Durable.delete ~kind (durable_of pvm_state.storage) key
  in
  Pvm.Kernel.encode
    {pvm_state with storage = update_durable pvm_state.storage durable}
    evm_state

let exists_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.exists durable key

let subkeys_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.list durable key

(* Typed path GADT *)

type _ path =
  | Raw_path : string -> bytes path
  | Chain_id : L2_types.chain_id path
  | Michelson_runtime_chain_id : L2_types.chain_id path
  | Kernel_version : string path

type 'a resolved = {
  path : string;
  decode : bytes -> 'a tzresult;
  encode : 'a -> string;
}

type 'a resolution =
  | Static of 'a resolved
  | Versioned of (storage_version:int -> 'a resolved)
[@@warning "-37"]

let resolve : type a. a path -> a resolution =
  let infallible_decode decode bytes = Ok (decode bytes) in
  function
  | Raw_path key ->
      Static
        {
          path = key;
          decode = infallible_decode Fun.id;
          encode = Bytes.to_string;
        }
  | Chain_id ->
      Static
        {
          path = Durable_storage_path.chain_id;
          decode = infallible_decode L2_types.Chain_id.decode_le;
          encode = L2_types.Chain_id.encode_le;
        }
  | Michelson_runtime_chain_id ->
      Static
        {
          path = Durable_storage_path.michelson_runtime_chain_id;
          decode = infallible_decode L2_types.Chain_id.decode_be;
          encode = L2_types.Chain_id.encode_be;
        }
  | Kernel_version ->
      Static
        {
          path = Durable_storage_path.kernel_version;
          decode = infallible_decode Bytes.to_string;
          encode = Fun.id;
        }

let storage_version state =
  let open Lwt_result_syntax in
  let decode bytes = Helpers.decode_z_le bytes |> Z.to_int in
  let*! bytes =
    inspect_durable state Durable_storage_path.storage_version_base
  in
  match bytes with
  | Some bytes -> return (decode bytes)
  | None -> (
      let*! bytes =
        inspect_durable state Durable_storage_path.storage_version_legacy
      in
      match bytes with Some bytes -> return (decode bytes) | None -> return 0)

let resolve_with_state (type a) (p : a path) (state : Pvm.State.t) :
    a resolved tzresult Lwt.t =
  let open Lwt_result_syntax in
  match resolve p with
  | Static r -> return r
  | Versioned f ->
      let* sv = storage_version state in
      return (f ~storage_version:sv)

let read (type a) (p : a path) (state : Pvm.State.t) : a tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let*! bytes_opt = inspect_durable state r.path in
  match bytes_opt with
  | Some bytes ->
      let*? v = r.decode bytes in
      return v
  | None -> failwith "No value found under %s" r.path

let read_opt (type a) (p : a path) (state : Pvm.State.t) :
    a option tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let*! bytes_opt = inspect_durable state r.path in
  match bytes_opt with
  | Some bytes ->
      let*? v = r.decode bytes in
      return_some v
  | None -> return_none

let write (type a) (p : a path) (value : a) (state : Pvm.State.t) :
    Pvm.State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let encoded = r.encode value in
  let*! state = modify_durable ~key:r.path ~value:encoded state in
  return state

let delete (type a) (p : a path) (state : Pvm.State.t) :
    Pvm.State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let*! state =
    delete_durable ~kind:Tezos_scoru_wasm.Durable.Value state r.path
  in
  return state

let delete_dir (type a) (p : a path) (state : Pvm.State.t) :
    Pvm.State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let*! state =
    delete_durable ~kind:Tezos_scoru_wasm.Durable.Directory state r.path
  in
  return state

let exists (type a) (p : a path) (state : Pvm.State.t) : bool tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let*! b = exists_durable state r.path in
  return b

let subkeys (type a) (p : a path) (state : Pvm.State.t) :
    string trace tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_with_state p state in
  let*! keys = subkeys_durable state r.path in
  return keys
