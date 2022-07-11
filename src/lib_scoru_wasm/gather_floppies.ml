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

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/3361
    Increase the SCORU message size limit, and bump value to be 4,096. *)
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

  (** The [state] type is a phantom type that is used to describe the
      data model of our PVM instrumentation. That is, values of type
      [state] are never constructed. On the contrary, we manipulate
      values of type [state Thunk.t]. *)
  type state =
    (internal_status value * Tezos_crypto.Signature.Public_key.t value)
    * string value
    * input_info value
    * Z.t value
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3362
       Replace the [lazy_list] with [Chunked_byte_vector] to compose
       better with the [lib_webassembly]. *)
    * bytes value Thunk.Lazy_list.t

  (** This [schema] decides how our data-model are encoded in a tree. *)
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

  (** [status_l] is a lens to access the current [internal_status] of
      the instrumented PVM. The instrumented PVM is either in the
      process of gathering floppies, or is done with this pre-boot
      step. *)
  let status_l : (state, internal_status value) Thunk.lens =
    Thunk.(tup5_0 ^. tup2_0)

  (** [public_key_l] is a lens to access the public key incoming
      chunks of kernel are expected to be signed with. The public key
      is submitted in the [origination_message], and used as long as
      the instrumented PVM is in the [Gather_floppies] state. *)
  let public_key_l :
      (state, Tezos_crypto.Signature.Public_key.t value) Thunk.lens =
    Thunk.(tup5_0 ^. tup2_1)

  (** [boot_sector_l] is a lens to access the [origination_message]
      provided at the origination of the rollup. This message is
      either a complete kernel, or a chunk of a kernel with a public
      key. *)
  let boot_sector_l : (state, string value) Thunk.lens = Thunk.tup5_1

  (** [last_input_info_l] is a lens to access the input that was last
      provided to the PVM, along with some metadata. It is updated
      after each chunk of kernel is received. *)
  let last_input_info_l : (state, input_info value) Thunk.lens = Thunk.tup5_2

  (** [interal_tick_l] is a lens to access the number of ticks
      performed by the “gather floppies” instrumentation, before the
      PVM starts its real execution. *)
  let internal_tick_l : (state, Z.t value) Thunk.lens = Thunk.tup5_3

  (** [chunks_l] is a lens to access the collection of chunks (saved
      as a so-called [Thunk.Lazy_map.t]) already received by the
      instrumented PVM. *)
  let chunks_l : (state, bytes value Thunk.Lazy_list.t) Thunk.lens =
    Thunk.tup5_4

  (** [check_signature payload signature state] returns [true] iff
      [signature] is a correct signature for [payload], that is, it
      has been computed with the companion secret key of the public
      key submitted in the [origination_message], and stored in
      [state] (see {!public_key_l}). *)
  let check_signature payload signature state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let*^ pk = state ^-> public_key_l in
    return @@ Tezos_crypto.Signature.check pk signature payload

  (** [store_chunk state chunk] adds [chunk] in the collection of
      chunks already collected by the instrumented PVM and stored in
      [state] (see {!chunks_l}). *)
  let store_chunk state chunk =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* chunks = state ^-> chunks_l in
    let* _ = Thunk.Lazy_list.cons chunks chunk in
    return ()

  (** [store_bytes state bytes] slices [bytes] into individual chunks,
      and adds them in the collection of chunks stored in [state] (see
      {!store_chunk}). *)
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

  (** [get_internal_ticks state] returns the number of ticks as stored
      in [state], or [0] if the values has not been initialized
      yet. *)
  let get_internal_ticks state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let*^? tick = state ^-> internal_tick_l in
    return @@ Option.value ~default:Z.zero tick

  (** [increment_ticks state] increments the number of ticks as stored
      in [state], or set it to [1] in case it has not been initialized
      yet. *)
  let increment_ticks state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* tick = get_internal_ticks state in
    (state ^-> internal_tick_l) ^:= Z.succ tick

  (* PROCESS MESSAGES *)

  (** [origination_kernel_loading_step state] processes and stores the
      (potentially incomplete) kernel image contained in the
      origination message.

      This message contains either the entire (small) kernel image or
      the first chunk of it (see {!origination_message}). *)
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

  (** [kernel_loading_step input_info message state] processes the
      sub-sequent kernel image chunk encoded in [message], as part of
      an input tick described by [input_info].

      If the chunk is not strictly equal to [chunk_size], then it is
      considered as the very last chunk of the kernel, meaning the
      instrumented PVM will update its status to start the regular
      execution of the PVM. An empty chunk is also allowed. *)
  let kernel_loading_step input message state =
    let open Lwt_syntax in
    let open Thunk.Syntax in
    let* () = (state ^-> last_input_info_l) ^:= input in
    let* () = increment_ticks state in
    if 0 < String.length message then
      (* It is safe to read the very first character stored in
         [message], that is [String.get] will not raise an exception. *)
      match
        ( String.get message 0,
          Data_encoding.Binary.read
            floppy_encoding
            message
            1
            (String.length message - 1) )
      with
      | '\001', Ok (_, {chunk; signature}) ->
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
      | '\001', Error error -> raise (Malformed_inbox_message error)
      | _, _ ->
          (* [message] is not an external message (its tag is not
             [0x01]), that is it is not a valid input. *)
          return ()
    else (* [message] is empty, that is it is not a valid input. *)
      return ()

  (* Encapsulated WASM *)

  (** [compute_step tree] instruments [Wasm.compute_step] to check the
      current status of the PVM.

      {ul
        {li If the status has not yet been initialized, it means it is
            the very first step of the rollup, and we interpret the
            [origination_message] that was provided at origination
            time.}
        {li If the status is [Gathering_floppies], then the PVM is
            expected to receive the next kernel chunk, and
            [compute_step] raises an exception.}
        {li If the status is [Not_gathering_floppies], then the PVM
            pre-boot has ended, the kernel has been provided, and
            [Wasm.compute_step] is called.}} *)
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

  (** [set_input_step input message tree] instruments
      [Wasm.set_input_step] to interpret incoming input messages as
      floppies (that is, a kernel chunk and a signature) when the PVM
      status is [Gathering_floppies].

      When the status is [Not_gathering_floppies] the pre-boot phase
      has ended and [Wasm.set_input_step] is called. If the status has
      not yet been initialized, this function raises an exception, as
      the origination message has yet to be interpreted. *)
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
            current_tick =
              (* We consider [Wasm] as a black box. In particular, we
                 don’t know where [Wasm] is storing the number of
                 internal ticks it has interpreted, hence the need to
                 add both tick counters (the one introduced by our
                 instrumentation, and the one maintained by
                 [Wasm]). *)
              Z.(add inner_info.current_tick ticks);
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
