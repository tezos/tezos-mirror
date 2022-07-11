(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Wasm_pvm_sig

let chunk_size = 4_000

exception Malformed_origination_message of Data_encoding.Binary.read_error

exception Malformed_inbox_message of Data_encoding.Binary.read_error

exception Compute_step_expected_input

exception Set_input_step_expected_compute_step

exception Encoding_error of Data_encoding.Binary.write_error

exception Malformed_input_info_record

type internal_status = Gathering_floppies | Not_gathering_floppies

let internal_status_encoding =
  Data_encoding.string_enum
    [
      ("GatheringFloppies", Gathering_floppies);
      ("NotGatheringFloppies", Not_gathering_floppies);
    ]

(* An at most 4,096-byte fragment of a kernel. *)
type chunk = bytes

let chunk_encoding = Data_encoding.Bounded.bytes chunk_size

type floppy = {chunk : chunk; signature : Tezos_crypto.Signature.t}

let floppy_encoding =
  Data_encoding.(
    conv
      (fun {chunk; signature} -> (chunk, signature))
      (fun (chunk, signature) -> {chunk; signature})
      (obj2
         (req "chunk" chunk_encoding)
         (req "signature" Tezos_crypto.Signature.encoding)))

(* Encoding for message in "boot sector" *)
type origination_message =
  | Complete_kernel of bytes
  | Incomplete_kernel of chunk * Tezos_crypto.Signature.Public_key.t

let origination_message_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"complete"
        (Tag 0)
        (obj1 @@ req "complete_kernel" bytes)
        (function Complete_kernel s -> Some s | _ -> None)
        (fun s -> Complete_kernel s);
      case
        ~title:"incomplete"
        (Tag 1)
        (obj2
           (req "first_chunk" chunk_encoding)
           (req "public_key" Tezos_crypto.Signature.Public_key.encoding))
        (function Incomplete_kernel (s, pk) -> Some (s, pk) | _ -> None)
        (fun (s, pk) -> Incomplete_kernel (s, pk));
    ]

(* STORAGE KEYS *)

module Make (T : Tree.S) (Wasm : Wasm_pvm_sig.S with type tree = T.tree) :
  Wasm_pvm_sig.S with type tree = T.tree = struct
  type tree = Wasm.tree

  type 'a value = 'a Thunk.value

  module Thunk = Thunk.Make (T)
  module Decoding = Tree_decoding.Make (T)

  (* TYPES *)

  type state =
    (internal_status value * Tezos_crypto.Signature.Public_key.t value)
    * string value
    * input_info value
    * Z.t value
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3362
       Replace the [lazy_list] with [Chunked_byte_vector] to compose
       better with the [lib_webassembly]. *)
    * bytes value Thunk.Lazy_list.t

  let state_schema : state Thunk.schema =
    let open Thunk.Schema in
    obj5
      (req
         "pvm"
         (obj2
            (req "status" @@ encoding internal_status_encoding)
            (req "public-key"
            @@ encoding Tezos_crypto.Signature.Public_key.encoding)))
      (req "boot-sector" @@ encoding Data_encoding.string)
      (req "last-input-info" @@ encoding input_info_encoding)
      (req "internal-loading-kernel-tick" @@ encoding Data_encoding.n)
      (req "durable"
      @@ folders ["kernel"; "boot.wasm"]
      @@ Thunk.Lazy_list.schema (encoding chunk_encoding))

  let status_l : (state, internal_status value) Thunk.lens =
    Thunk.(tup5_0 ^. tup2_0)

  let public_key_l :
      (state, Tezos_crypto.Signature.Public_key.t value) Thunk.lens =
    Thunk.(tup5_0 ^. tup2_1)

  let boot_sector_l : (state, string value) Thunk.lens = Thunk.tup5_1

  let last_input_info_l : (state, input_info value) Thunk.lens = Thunk.tup5_2

  let internal_tick_l : (state, Z.t value) Thunk.lens = Thunk.tup5_3

  let chunks_l : (state, bytes value Thunk.Lazy_list.t) Thunk.lens =
    Thunk.tup5_4

  (* STORAGE/TREE INTERACTION *)

  let check_signature payload signature state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let*^ pk = state ^-> public_key_l in
    return @@ Tezos_crypto.Signature.check pk signature payload

  let store_chunk state chunk =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* chunks = state ^-> chunks_l in
    let* _ = Thunk.Lazy_list.cons chunks chunk in
    return ()

  let store_bytes state : bytes -> unit Lwt.t =
    let rec aux bytes =
      let open Lwt_syntax in
      let open Thunk.Syntax in
      let len = Bytes.length bytes in
      let* chunks = state ^-> chunks_l in
      if len = 0 then return ()
      else if len < chunk_size then
        let* _ = Thunk.Lazy_list.cons chunks bytes in
        return ()
      else
        let chunk = Bytes.sub bytes 0 chunk_size in
        let rst = Bytes.sub bytes chunk_size (len - chunk_size) in
        let* _ = Thunk.Lazy_list.cons chunks chunk in
        aux rst
    in
    aux

  let get_internal_ticks state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let*^? tick = state ^-> internal_tick_l in
    return @@ Option.value ~default:Z.zero tick

  let increment_ticks state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* tick = get_internal_ticks state in
    (state ^-> internal_tick_l) ^:= Z.succ tick

  (* PROCESS MESSAGES *)

  (* Process and store the kernel image in the origination message
     This message contains either the entire (small) kernel image or
     the first chunk of it. *)
  let origination_kernel_loading_step state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* () = increment_ticks state in
    let*^ boot_sector = state ^-> boot_sector_l in
    let boot_sector =
      Data_encoding.Binary.of_string_opt
        origination_message_encoding
        boot_sector
    in
    match boot_sector with
    | Some (Complete_kernel kernel) ->
        let* () = store_bytes state kernel in
        (state ^-> status_l) ^:= Not_gathering_floppies
    | Some (Incomplete_kernel (chunk, _boot_pk))
      when Bytes.length chunk < chunk_size ->
        (* Despite claiming the boot sector is not a complete kernel,
           it is not large enough to fill a chunk. *)
        let* () = store_chunk state chunk in
        (state ^-> status_l) ^:= Not_gathering_floppies
    | Some (Incomplete_kernel (chunk, boot_pk)) ->
        let* () = store_chunk state chunk in
        let* () = (state ^-> public_key_l) ^:= boot_pk in
        (state ^-> status_l) ^:= Gathering_floppies
    | None ->
        (* TODO: Add a proper [status] constructor *)
        return ()

  (* Process one sub-sequent kernel image chunks. If the chunk has
     zero length it * means we're done and we have the entire kernel
     image. *)
  let kernel_loading_step input message state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* () = (state ^-> last_input_info_l) ^:= input in
    let* () = increment_ticks state in
    (* TODO: check that the first byte of [message] is 0x01 (External). *)
    match
      Data_encoding.Binary.read
        floppy_encoding
        message
        1
        (String.length message - 1)
    with
    | Error error -> raise (Malformed_inbox_message error)
    | Ok (_, {chunk; signature}) ->
        let* check = check_signature chunk signature state in
        if check then
          let* () =
            if 0 < Bytes.length chunk then store_chunk state chunk
            else return ()
          in
          if Bytes.length chunk < chunk_size then
            (state ^-> status_l) ^:= Not_gathering_floppies
          else return ()
        else return ()

  (* Encapsulated WASM *)

  let compute_step tree =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let state = Thunk.decode state_schema tree in
    let*^? status = state ^-> status_l in
    match status with
    | Some Gathering_floppies -> raise Compute_step_expected_input
    | Some Not_gathering_floppies -> Wasm.compute_step tree
    | None ->
        let* () = origination_kernel_loading_step state in
        Thunk.encode tree state

  let set_input_step input message tree =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let state = Thunk.decode state_schema tree in
    let*^? status = state ^-> status_l in
    match status with
    | Some Gathering_floppies ->
        let* () = kernel_loading_step input message state in
        Thunk.encode tree state
    | Some Not_gathering_floppies -> Wasm.set_input_step input message tree
    | None -> raise Set_input_step_expected_compute_step

  let get_output = Wasm.get_output

  let get_info tree =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let state = Thunk.decode state_schema tree in
    let*^? status = state ^-> status_l in
    let* ticks = get_internal_ticks state in
    let*^? last_boot_read = state ^-> last_input_info_l in
    match status with
    | Some Gathering_floppies ->
        return
          {
            current_tick = ticks;
            last_input_read = last_boot_read;
            input_request = Input_required;
          }
    | Some Not_gathering_floppies ->
        let* inner_info = Wasm.get_info tree in
        return
          {
            inner_info with
            current_tick = Z.(add inner_info.current_tick ticks);
            last_input_read =
              Option.fold
                ~none:last_boot_read
                ~some:(fun x -> Some x)
                inner_info.last_input_read;
          }
    | None ->
        return
          {
            current_tick = ticks;
            last_input_read = None;
            input_request = No_input_required;
          }
end
