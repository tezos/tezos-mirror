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
open Tezos_lazy_containers

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/3361
    Increase the SCORU message size limit, and bump value to be 4,096. *)
let chunk_size = 4_000

exception Compute_step_expected_input

exception Set_input_step_expected_compute_step

exception Encoding_error of Data_encoding.Binary.write_error

type internal_status =
  | Gathering_floppies of Tezos_crypto.Signature.Public_key.t
  | Not_gathering_floppies

let internal_status_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Gathering_floppies"
        (obj2
           (req "kind" (constant "gathering_floppies"))
           (req "public_key" Tezos_crypto.Signature.Public_key.encoding))
        (function Gathering_floppies pk -> Some ((), pk) | _ -> None)
        (fun ((), pk) -> Gathering_floppies pk);
      case
        (Tag 1)
        ~title:"Not_gathering_floppies"
        (obj1 (req "kind" (constant "Not_gathering_floppies")))
        (function Not_gathering_floppies -> Some () | _ -> None)
        (fun () -> Not_gathering_floppies);
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

module type S = sig
  include Wasm_pvm_sig.S

  module Internal_for_tests : sig
    include
      Wasm_pvm_sig.Internal_for_tests
        with type tree := tree
         and type tick_state := tick_state

    val get_internal_status : tree -> internal_status option Lwt.t

    val initial_tree_from_boot_sector : empty_tree:tree -> string -> tree Lwt.t
  end
end

module Make
    (T : Tezos_tree_encoding.TREE)
    (Wasm : Wasm_pvm_sig.S with type tree = T.tree) :
  S with type tree = T.tree and type tick_state = Wasm.tick_state = struct
  type tree = Wasm.tree

  type tick_state = Wasm.tick_state

  module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (T)

  (** The tick state of the [Gathering_floppies] instrumentation. *)
  type state = {
    internal_status : internal_status;
        (** The instrumented PVM is either in the process of gathering
            floppies, or is done with this pre-boot step. *)
    last_input_info : input_info option;
        (** This field is updated after each [read_input] step to
            reflect the progression of the PVM. *)
    kernel : Chunked_byte_vector.t;
        (** The kernel being incrementally loaded into memory. *)
    internal_tick : Z.t;
        (** A counter updated after each small step execution of the
            PVM. *)
  }

  let boot_sector_merklizer : string Tezos_tree_encoding.t =
    Tezos_tree_encoding.(value ["boot-sector"] Data_encoding.string)

  let state_merklizer : state Tezos_tree_encoding.t =
    let open Tezos_tree_encoding in
    conv
      (fun (internal_status, last_input_info, internal_tick, kernel) ->
        {internal_status; last_input_info; internal_tick; kernel})
      (fun {internal_status; last_input_info; internal_tick; kernel} ->
        (internal_status, last_input_info, internal_tick, kernel))
    @@ tup4
         ~flatten:true
         (value ["gather-floppies"; "status"] internal_status_encoding)
         (value ["gather-floppies"; "last-input-info"]
         @@ Data_encoding.option input_info_encoding)
         (value ["gather-floppies"; "internal-tick"] Data_encoding.n)
         (scope ["durable"; "kernel"; "boot.wasm"; "_"] chunked_byte_vector)

  (** [increment_ticks state] increments the number of ticks as stored
      in [state], or set it to [1] in case it has not been initialized
      yet. *)
  let increment_ticks state =
    {state with internal_tick = Z.succ state.internal_tick}

  type status =
    | Halted of string
        (** The PVM has not started yet, meaning the boot sector is
            still to be interpreted as an [origination_message]. *)
    | Running of state
        (** The boot sector has been correctly interpreted, and the
            PVM is running as expected. *)
    | Broken of {current_tick : Z.t}
        (** The boot sector was not a correctly encoded
            [originatiom_message], causing the PVM to enter a broken
            state. *)

  (** [broken_merklizer] is a partial schema to be used to encode the
      number of ticks of the PVM when it is stuck.

      It only tries to fetch the current tick (with the same key as
      the one used in [state_merklizer]. *)
  let broken_merklizer =
    Tezos_tree_encoding.value
      ["gather-floppies"; "internal-tick"]
      Data_encoding.n

  (** [read_state tree] fetches the current state of the PVM from
      [tree]. *)
  let read_state tree =
    let open Lwt_syntax in
    Lwt.catch
      (fun () ->
        (* First, we try to interpret [tree] as a [state]. *)
        let+ state = Tree_encoding_runner.decode state_merklizer tree in
        Running state)
      (fun _exn ->
        Lwt.catch
          (fun () ->
            (* If it fails, it means the PVM may be stuck. *)
            let+ current_tick =
              Tree_encoding_runner.decode broken_merklizer tree
            in
            Broken {current_tick})
          (fun _exn ->
            (* In case both previous attempts have failed, it means
               this is probably the very first tick of the PVM. *)
            let+ boot_sector =
              Tree_encoding_runner.decode boot_sector_merklizer tree
            in
            Halted boot_sector))

  (* PROCESS MESSAGES *)

  (** [origination_kernel_loading_step boot_sector] implements the
      tick consisting in initializing the [state] of the PVM from the
      [boot_sector] supplied at origination time, or [None] iff
      [boot_sector] cannot be decoded as a valid
      [origination_message]. *)
  let origination_kernel_loading_step boot_sector =
    let boot_sector =
      Data_encoding.Binary.of_string_opt
        origination_message_encoding
        boot_sector
    in
    match boot_sector with
    | Some (Complete_kernel kernel) ->
        let kernel = Chunked_byte_vector.of_bytes kernel in
        Some
          {
            internal_status = Not_gathering_floppies;
            last_input_info = None;
            internal_tick = Z.one;
            kernel;
          }
    | Some (Incomplete_kernel (chunk, _pk)) when Bytes.length chunk < chunk_size
      ->
        let kernel = Chunked_byte_vector.of_bytes chunk in
        Some
          {
            internal_status = Not_gathering_floppies;
            last_input_info = None;
            internal_tick = Z.one;
            kernel;
          }
    | Some (Incomplete_kernel (chunk, pk)) ->
        let kernel = Chunked_byte_vector.of_bytes chunk in
        Some
          {
            internal_status = Gathering_floppies pk;
            last_input_info = None;
            internal_tick = Z.one;
            kernel;
          }
    | None -> None

  let read_floppy message =
    let message_len = String.length message in
    if 0 < message_len then
      (* It is safe to read the very first character stored in
         [message], that is [String.get] will not raise an exception. *)
      match
        ( String.get message 0,
          Data_encoding.Binary.read floppy_encoding message 1 (message_len - 1)
        )
      with
      | '\001', Ok (_offset, floppy) -> Some floppy
      | '\001', Error _error -> None
      | _, _ -> None
    else (* [message] is empty, that is it is not a valid input. *)
      None

  (** [process_input_step input message state] interprets the incoming
      [message] as part of the input tick characterized by
      [input_info], and computes a new state for the instrumented PVM.

      It is expected that the instrumented PVM is expected to gather
      floppies, that is [exists pk. state.status = Gathering_floppies
      pk].

      If the chunk encoded in [message] is not strictly equal to
      {!chunk_size}, the instrumented PVM will consider the kernel to
      be completed, and switch to [Not_gathering_floppies]. *)
  let process_input_step input message state =
    let open Lwt_syntax in
    match state.internal_status with
    | Gathering_floppies pk -> (
        match read_floppy message with
        | Some {chunk; signature} ->
            let state = {state with last_input_info = Some input} in
            let offset = Chunked_byte_vector.length state.kernel in
            let len = Bytes.length chunk in
            if Tezos_crypto.Signature.check pk signature chunk then
              let* () =
                if 0 < len then (
                  Chunked_byte_vector.grow state.kernel (Int64.of_int len) ;
                  Chunked_byte_vector.store_bytes state.kernel offset chunk)
                else return_unit
              in
              return
                {
                  state with
                  internal_status =
                    (if len < chunk_size then Not_gathering_floppies
                    else state.internal_status);
                }
            else
              (* The incoming message does not come with a correct
                 signature: we ignore it. *)
              return state
        | None ->
            (* [message] is empty, that is it is not a valid input. *)
            return state)
    | Not_gathering_floppies -> raise (Invalid_argument "process_input_step")

  (* Encapsulated WASM *)

  (** [compute_step_gen wasm_step tree] instruments [wasm_step] to check the
      current status of the PVM. [wasm_step] must be a function that from a step
      computes another step.

      {ul
        {li If the state has not yet been initialized, it means it is
            the very first step of the rollup, and we interpret the
            [origination_message] that was provided at origination
            time.}
        {li If the status is [Gathering_floppies], then the PVM is
            expected to receive the next kernel chunk, and
            [compute_step] raises an exception.}
        {li If the status is [Not_gathering_floppies], then the PVM
            pre-boot has ended, the kernel has been provided, and
            [wasm_step] is called.}} *)
  let compute_step_gen wasm_step tree =
    let open Lwt_syntax in
    let* state = read_state tree in
    match state with
    | Broken {current_tick} ->
        Tree_encoding_runner.encode broken_merklizer (Z.succ current_tick) tree
    | Halted origination_message -> (
        match origination_kernel_loading_step origination_message with
        | Some state -> Tree_encoding_runner.encode state_merklizer state tree
        | None ->
            (* We could not interpret [origination_message],
               meaning the PVM is stuck. *)
            Tree_encoding_runner.encode broken_merklizer Z.one tree)
    | Running state -> (
        let state = increment_ticks state in
        match state.internal_status with
        | Gathering_floppies _ -> raise Compute_step_expected_input
        | Not_gathering_floppies -> wasm_step tree)

  let compute_step tree = compute_step_gen Wasm.compute_step tree

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
    let* state = read_state tree in
    match state with
    | Halted _ | Broken _ -> raise Set_input_step_expected_compute_step
    | Running state -> (
        let state = increment_ticks state in
        match state.internal_status with
        | Gathering_floppies _ ->
            let* state = process_input_step input message state in
            Tree_encoding_runner.encode state_merklizer state tree
        | Not_gathering_floppies -> Wasm.set_input_step input message tree)

  let reveal_step = Wasm.reveal_step

  let get_output = Wasm.get_output

  let get_info tree =
    let open Lwt_syntax in
    let* state = read_state tree in
    match state with
    | Broken {current_tick} ->
        return
          {
            current_tick;
            last_input_read = None;
            input_request = No_input_required;
          }
    | Halted _ ->
        return
          {
            current_tick = Z.zero;
            last_input_read = None;
            input_request = No_input_required;
          }
    | Running state -> (
        match state.internal_status with
        | Gathering_floppies _ ->
            return
              {
                current_tick = state.internal_tick;
                last_input_read = state.last_input_info;
                input_request = Input_required;
              }
        | Not_gathering_floppies ->
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
                  Z.(add inner_info.current_tick state.internal_tick);
                last_input_read =
                  Option.fold
                    ~none:state.last_input_info
                    ~some:(fun x -> Some x)
                    inner_info.last_input_read;
              })

  let compute_step_many ~max_steps =
    compute_step_gen (Wasm.compute_step_many ~max_steps)

  module Internal_for_tests = struct
    include Wasm.Internal_for_tests

    let get_internal_status tree =
      let open Lwt_syntax in
      let+ state = read_state tree in
      match state with Running state -> Some state.internal_status | _ -> None

    let initial_tree_from_boot_sector ~empty_tree boot_sector =
      match origination_kernel_loading_step boot_sector with
      | Some state ->
          Tree_encoding_runner.encode state_merklizer state empty_tree
      | None ->
          raise
            (Invalid_argument "initial_tree_from_boot_sector: wrong boot sector")
  end
end
