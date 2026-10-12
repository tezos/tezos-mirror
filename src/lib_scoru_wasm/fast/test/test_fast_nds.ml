(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_scoru_wasm_fast
    Invocation:   dune exec src/lib_scoru_wasm/fast/test/main.exe -- --file test_fast_nds.ml
    Subject:      The NDS host functions on the fast execution engine
                  (Wasmer) agree with the interpreter, state hash for
                  state hash and tick for tick.
*)

open Tezos_scoru_wasm
module Dual = Tezos_smart_rollup_wasm_in_memory_dual

(* -------------------------------------------------------------------- *)
(* The kernel                                                           *)
(* -------------------------------------------------------------------- *)

(* One external message is one command.  The payload lands at address
   1001 (its first byte, the external-message tag, at 1000):

   {v
     1001  command byte
     1002  db index (signed for the resize delta)
     1003  second db index (copy / move destination)
     1004  key length, u16 little-endian
     1006  offset
     1008  length (num_bytes / max_bytes), u16 little-endian
     1010  key bytes
     1300  value bytes
   v}

   Every command records the [i32] the host function answers under
   [/r/<command>], and [r] (read) and [h] (hash) also record the bytes
   they received under [/r/<command>d], so a divergence between the two
   engines shows up in the durable storage as well as in the NDS.

   The wide commands [I], [X], [J], [K] and [Z] pass the [i64] values of
   the table at 20300 ({!wide_values}) one after the other.  They record
   the answer for the value at index [i] under [/r/<command><i>]. *)
let kernel =
  {|
(module
  (import "smart_rollup_core" "read_input"
    (func $read_input (param i32 i32 i32) (result i32)))
  (import "smart_rollup_core" "store_write"
    (func $store_write (param i32 i32 i32 i32 i32) (result i32)))
  (import "smart_rollup_core" "store_delete"
    (func $store_delete (param i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_registry_resize"
    (func $nds_registry_resize (param i64) (result i32)))
  (import "smart_rollup_core" "nds_registry_copy"
    (func $nds_registry_copy (param i64 i64) (result i32)))
  (import "smart_rollup_core" "nds_registry_move"
    (func $nds_registry_move (param i64 i64) (result i32)))
  (import "smart_rollup_core" "nds_registry_clear"
    (func $nds_registry_clear (param i64) (result i32)))
  (import "smart_rollup_core" "nds_store_exists"
    (func $nds_store_exists (param i64 i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_store_read"
    (func $nds_store_read (param i64 i32 i32 i64 i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_store_write"
    (func $nds_store_write (param i64 i32 i32 i64 i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_store_set"
    (func $nds_store_set (param i64 i32 i32 i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_store_delete"
    (func $nds_store_delete (param i64 i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_store_value_size"
    (func $nds_store_value_size (param i64 i32 i32) (result i32)))
  (import "smart_rollup_core" "nds_database_get_hash"
    (func $nds_database_get_hash (param i64 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))
  (data (i32.const 20000) "/r/")
  (data (i32.const 20100) "/kernel/env/activate_nds")
  ;; The wide values, little-endian: -1, Int64.min_int, Int64.max_int,
  ;; 1 lsl 40 and -(1 lsl 40).
  (data (i32.const 20300)
    "\ff\ff\ff\ff\ff\ff\ff\ff"
    "\00\00\00\00\00\00\00\80"
    "\ff\ff\ff\ff\ff\ff\ff\7f"
    "\00\00\00\00\00\01\00\00"
    "\00\00\00\00\00\ff\ff\ff")

  (func $cmd (result i32) (i32.load8_u (i32.const 1001)))
  (func $db (result i64) (i64.load8_u (i32.const 1002)))
  (func $diff (result i64) (i64.load8_s (i32.const 1002)))
  (func $db2 (result i64) (i64.load8_u (i32.const 1003)))
  (func $key_len (result i32) (i32.load16_u (i32.const 1004)))
  (func $offset (result i64) (i64.load8_u (i32.const 1006)))
  (func $len (result i32) (i32.load16_u (i32.const 1008)))

  ;; Record [code] under /r/<cmd>, little-endian.
  (func $record (param $code i32)
    (i32.store8 (i32.const 20003) (call $cmd))
    (i32.store (i32.const 20200) (local.get $code))
    (drop (call $store_write
      (i32.const 20000) (i32.const 4) (i32.const 0) (i32.const 20200) (i32.const 4))))

  ;; Publish the [n] bytes at [addr] under /r/<cmd>d (store_write
  ;; overwrites in place, hence the delete first).
  (func $publish (param $addr i32) (param $n i32)
    (i32.store8 (i32.const 20003) (call $cmd))
    (i32.store8 (i32.const 20004) (i32.const 100))
    (drop (call $store_delete (i32.const 20000) (i32.const 5)))
    (drop (call $store_write
      (i32.const 20000) (i32.const 5) (i32.const 0) (local.get $addr) (local.get $n))))

  ;; Record exactly the [n] bytes received at 8000 under /r/<cmd>d, when
  ;; n >= 0.
  (func $record_data (param $n i32)
    (if (i32.ge_s (local.get $n) (i32.const 0))
      (then (call $publish (i32.const 8000) (local.get $n)))))

  ;; Record [code] under /r/<cmd><i>, with [i] as an ASCII digit.
  (func $record_at (param $i i32) (param $code i32)
    (i32.store8 (i32.const 20003) (call $cmd))
    (i32.store8 (i32.const 20004) (i32.add (i32.const 48) (local.get $i)))
    (i32.store (i32.const 20200) (local.get $code))
    (drop (call $store_write
      (i32.const 20000) (i32.const 5) (i32.const 0) (i32.const 20200) (i32.const 4))))

  ;; Call the wide command [c] with the wide value [v].
  (func $wide_call (param $c i32) (param $v i64) (result i32)
    ;; I: store_exists, db index = v
    (if (i32.eq (local.get $c) (i32.const 73))
      (then (return (call $nds_store_exists
        (local.get $v) (i32.const 1010) (call $key_len)))))
    ;; X: registry_clear, db index = v
    (if (i32.eq (local.get $c) (i32.const 88))
      (then (return (call $nds_registry_clear (local.get $v)))))
    ;; J: store_read db key -> 8000, offset = v, max_bytes = len
    (if (i32.eq (local.get $c) (i32.const 74))
      (then (return (call $nds_store_read
        (call $db) (i32.const 1010) (call $key_len) (local.get $v) (i32.const 8000) (call $len)))))
    ;; K: store_write db key <- value, offset = v, num_bytes = len
    (if (i32.eq (local.get $c) (i32.const 75))
      (then (return (call $nds_store_write
        (call $db) (i32.const 1010) (call $key_len) (local.get $v) (i32.const 1300) (call $len)))))
    ;; Z: registry_resize, diff = v
    (call $nds_registry_resize (local.get $v)))

  (func $handle
    (local $code i32)
    (local $c i32)
    (local $i i32)
    (local.set $c (call $cmd))
    ;; I, X, J, K, Z: one call per wide value.  Z starts at index 1,
    ;; because a diff of -1 is a valid shrink.
    (if (i32.or
          (i32.or (i32.eq (local.get $c) (i32.const 73)) (i32.eq (local.get $c) (i32.const 88)))
          (i32.or
            (i32.or (i32.eq (local.get $c) (i32.const 74)) (i32.eq (local.get $c) (i32.const 75)))
            (i32.eq (local.get $c) (i32.const 90))))
      (then
        (local.set $i (i32.eq (local.get $c) (i32.const 90)))
        (loop $next
          (call $record_at (local.get $i)
            (call $wide_call (local.get $c)
              (i64.load (i32.add (i32.const 20300) (i32.shl (local.get $i) (i32.const 3))))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br_if $next (i32.lt_u (local.get $i) (i32.const 5))))
        (return)))
    ;; A: request the NDS activation
    (if (i32.eq (local.get $c) (i32.const 65))
      (then
        (drop (call $store_write
          (i32.const 20100) (i32.const 24) (i32.const 0) (i32.const 1000) (i32.const 1)))
        (return)))
    ;; P: grow the registry, then trap with an integer overflow.  The
    ;; interpreter ends the kernel_run on that trap; the fast engine does
    ;; not recover from it and panics, so the caller's fallback replays
    ;; the whole kernel_run on the interpreter.
    (if (i32.eq (local.get $c) (i32.const 80))
      (then
        (call $record (call $nds_registry_resize (i64.const 1)))
        (drop (i32.div_s (i32.const -2147483648) (i32.const -1)))
        (return)))
    ;; z: registry_resize diff
    (if (i32.eq (local.get $c) (i32.const 122))
      (then (call $record (call $nds_registry_resize (call $diff))) (return)))
    ;; c: registry_copy db db2
    (if (i32.eq (local.get $c) (i32.const 99))
      (then (call $record (call $nds_registry_copy (call $db) (call $db2))) (return)))
    ;; m: registry_move db db2
    (if (i32.eq (local.get $c) (i32.const 109))
      (then (call $record (call $nds_registry_move (call $db) (call $db2))) (return)))
    ;; x: registry_clear db
    (if (i32.eq (local.get $c) (i32.const 120))
      (then (call $record (call $nds_registry_clear (call $db))) (return)))
    ;; e: store_exists db key
    (if (i32.eq (local.get $c) (i32.const 101))
      (then
        (call $record (call $nds_store_exists (call $db) (i32.const 1010) (call $key_len)))
        (return)))
    ;; r: store_read db key offset -> 8000, max_bytes = len
    (if (i32.eq (local.get $c) (i32.const 114))
      (then
        (local.set $code (call $nds_store_read
          (call $db) (i32.const 1010) (call $key_len) (call $offset) (i32.const 8000) (call $len)))
        (call $record (local.get $code))
        (call $record_data (local.get $code))
        (return)))
    ;; w: store_write db key offset <- value, num_bytes = len
    (if (i32.eq (local.get $c) (i32.const 119))
      (then
        (call $record (call $nds_store_write
          (call $db) (i32.const 1010) (call $key_len) (call $offset) (i32.const 1300) (call $len)))
        (return)))
    ;; s: store_set db key <- value, num_bytes = len
    (if (i32.eq (local.get $c) (i32.const 115))
      (then
        (call $record (call $nds_store_set
          (call $db) (i32.const 1010) (call $key_len) (i32.const 1300) (call $len)))
        (return)))
    ;; d: store_delete db key
    (if (i32.eq (local.get $c) (i32.const 100))
      (then
        (call $record (call $nds_store_delete (call $db) (i32.const 1010) (call $key_len)))
        (return)))
    ;; v: store_value_size db key
    (if (i32.eq (local.get $c) (i32.const 118))
      (then
        (call $record (call $nds_store_value_size (call $db) (i32.const 1010) (call $key_len)))
        (return)))
    ;; M: store_exists with a negative key length
    (if (i32.eq (local.get $c) (i32.const 77))
      (then
        (call $record (call $nds_store_exists
          (call $db) (i32.const 1010) (i32.const -1)))
        (return)))
    ;; N: database_get_hash with a negative max_bytes
    (if (i32.eq (local.get $c) (i32.const 78))
      (then
        (call $record (call $nds_database_get_hash
          (call $db) (i32.const 8000) (i32.const -1)))
        (return)))
    ;; Q: store_read into the last two bytes of the memory, then publish
    ;; the tail of the memory so a partial write shows up in the state
    (if (i32.eq (local.get $c) (i32.const 81))
      (then
        (call $record (call $nds_store_read
          (call $db) (i32.const 1010) (call $key_len) (call $offset) (i32.const 65534) (call $len)))
        (call $publish (i32.const 65530) (i32.const 6))
        (return)))
    ;; H: database_get_hash into the last two bytes of the memory, then
    ;; publish the tail of the memory
    (if (i32.eq (local.get $c) (i32.const 72))
      (then
        (call $record (call $nds_database_get_hash
          (call $db) (i32.const 65534) (call $len)))
        (call $publish (i32.const 65530) (i32.const 6))
        (return)))
    ;; R: store_read with max_bytes = -1
    (if (i32.eq (local.get $c) (i32.const 82))
      (then
        (call $record (call $nds_store_read
          (call $db) (i32.const 1010) (call $key_len) (call $offset) (i32.const 8000) (i32.const -1)))
        (return)))
    ;; S: store_read with max_bytes = Int32.min_int
    (if (i32.eq (local.get $c) (i32.const 83))
      (then
        (call $record (call $nds_store_read
          (call $db) (i32.const 1010) (call $key_len) (call $offset) (i32.const 8000)
          (i32.const -2147483648)))
        (return)))
    ;; O: store_set with a zero key length and a key offset past the end
    ;; of the memory
    (if (i32.eq (local.get $c) (i32.const 79))
      (then
        (call $record (call $nds_store_set
          (call $db) (i32.const 70000) (i32.const 0) (i32.const 1300) (call $len)))
        (return)))
    ;; h: database_get_hash db -> 8000, max_bytes = len
    (if (i32.eq (local.get $c) (i32.const 104))
      (then
        (local.set $code (call $nds_database_get_hash (call $db) (i32.const 8000) (call $len)))
        (call $record (local.get $code))
        (call $record_data (local.get $code))
        (return))))

  (func (export "kernel_run")
    (local $n i32)
    (block $done
      (loop $next
        (local.set $n (call $read_input (i32.const 0) (i32.const 1000) (i32.const 4096)))
        (br_if $done (i32.le_s (local.get $n) (i32.const 0)))
        ;; external messages carry the tag 1; internal ones the tag 0
        (if (i32.eq (i32.load8_u (i32.const 1000)) (i32.const 1))
          (then (call $handle)))
        (br $next))))
)
|}

(** A command payload, laid out as the kernel expects. *)
let message ?(db = 0) ?(db2 = 0) ?(key = "") ?(offset = 0) ?(len = 0)
    ?(value = "") cmd =
  let b = Bytes.make (299 + String.length value) '\000' in
  Bytes.set b 0 cmd ;
  Bytes.set_uint8 b 1 db ;
  Bytes.set_uint8 b 2 db2 ;
  Bytes.set_uint16_le b 3 (String.length key) ;
  Bytes.set_uint8 b 5 offset ;
  Bytes.set_uint16_le b 7 len ;
  Bytes.blit_string key 0 b 9 (String.length key) ;
  Bytes.blit_string value 0 b 299 (String.length value) ;
  Bytes.to_string b

let activate = message 'A'

let resize diff = message ~db:(diff land 0xff) 'z'

let set ?(db = 0) key value =
  message ~db ~key ~len:(String.length value) ~value 's'

let write ?(db = 0) ~offset key value =
  message ~db ~key ~offset ~len:(String.length value) ~value 'w'

let read ?(db = 0) ?(offset = 0) ~len key = message ~db ~key ~offset ~len 'r'

let exists ?(db = 0) key = message ~db ~key 'e'

let value_size ?(db = 0) key = message ~db ~key 'v'

let delete ?(db = 0) key = message ~db ~key 'd'

let copy ~src ~dst = message ~db:src ~db2:dst 'c'

let move ~src ~dst = message ~db:src ~db2:dst 'm'

let clear db = message ~db 'x'

let db_hash ?(db = 0) ~len () = message ~db ~len 'h'

(* The other commands read their key length with [i32.load16_u], so a
   dedicated command is the only way to pass a negative one. *)
let exists_negative_key_len () = message 'M'

(* A negative [max_bytes] is neither a truncation request nor a valid
   destination size. *)
let db_hash_negative_max_bytes () = message 'N'

(* [store_read] into the last two bytes of the memory: a value longer
   than that does not fit, so the write must not happen at all. *)
let read_into_last_bytes ~len key = message ~key ~len 'Q'

(* An empty key whose offset lies past the end of the memory: the read
   accesses no byte, so the address is all the two memory implementations
   can disagree on. *)
let set_empty_key_out_of_bounds value =
  message ~len:(String.length value) ~value 'O'

let grow_then_panic = message 'P'

(* [database_get_hash] into the last two bytes of the memory. *)
let db_hash_into_last_bytes ~len () = message ~len 'H'

(* [store_read] with [max_bytes = -1] ([R]) and [max_bytes =
   Int32.min_int] ([S]).  The layout reads [len] with [i32.load16_u], so
   only a dedicated command passes a negative [max_bytes]. *)
let read_negative_max_bytes key = message ~key 'R'

let read_min_int_max_bytes key = message ~key 'S'

(** The values of the table at 20300 in the kernel.  The wide commands
    pass them in this order as an [i64] argument. *)
let wide_values =
  [
    -1L;
    Int64.min_int;
    Int64.max_int;
    Int64.shift_left 1L 40;
    Int64.neg (Int64.shift_left 1L 40);
  ]

(* [store_exists] with each wide value as the db index. *)
let wide_db_exists key = message ~key 'I'

(* [registry_clear] with each wide value as the db index. *)
let wide_db_clear = message 'X'

(* [store_read] with each wide value as the offset. *)
let wide_offset_read ~len key = message ~key ~len 'J'

(* [store_write] with each wide value as the offset. *)
let wide_offset_write key value =
  message ~key ~len:(String.length value) ~value 'K'

(* [registry_resize] with each wide value except -1 as the diff. *)
let wide_resize = message 'Z'

(* [store_write] and [store_set] with [num_bytes = len].  [len] is a
   [u16], so it can exceed [Aux.input_output_max_size] (4096).  The
   message carries no value bytes. *)
let write_oversized ~len key = message ~key ~len 'w'

let set_oversized ~len key = message ~key ~len 's'

(* -------------------------------------------------------------------- *)
(* Driving both engines                                                 *)
(* -------------------------------------------------------------------- *)

(* NDS activated on chain at level 0. The NDS host functions are enabled
   at every level the tests feed. *)
let nds_activated_on_chain =
  Wasm_pvm_config.of_signals [("nds_host_functions", 0l)]

(** The interpreter and the fast engine over the same in-memory dual
    state, with the same feature configuration. *)
module Make_pair (C : sig
  val config : Wasm_pvm_config.t
end) =
struct
  module Slow =
    (val Dual.wasm_pvm_machine_dual
           ~config:C.config
           ~make_empty_nds:Octez_riscv_nds_memory.make_empty_normal_nds)

  module Fast =
    Tezos_scoru_wasm_fast.Pvm.Make_pvm
      (struct
        let config = C.config

        let make_empty_nds = Some Octez_riscv_nds_memory.make_empty_normal_nds
      end)
      (Dual.Dual_state)
end

module Driver (M : Wasm_pvm_sig.S with type state = Dual.Dual_state.state) =
struct
  let wasm_entrypoint = Constants.wasm_entrypoint

  (* The production budget of a [kernel_run]: the fast engine only
     engages when a whole [kernel_run] fits in [max_steps]. *)
  let ticks_per_snapshot = Z.of_int64 11_000_000_000L

  (* The fast engine does not start without reveal builtins; the kernel
     never reveals. *)
  let reveal_builtins _ = Stdlib.failwith "the kernel requested a reveal"

  let rec eval_until_input_requested ~hooks state ticks =
    let open Lwt.Syntax in
    let* info = M.get_info state in
    match info.input_request with
    | Input_required -> Lwt.return (state, ticks)
    | No_input_required ->
        let* state, n =
          M.compute_step_many
            ~hooks
            ~reveal_builtins
            ~wasm_entrypoint
            ~max_steps:Int64.max_int
            state
        in
        eval_until_input_requested ~hooks state (Int64.add ticks n)
    | Reveal_required _ -> Stdlib.failwith "the kernel requested a reveal"

  let boot ~hooks kernel =
    let open Lwt.Syntax in
    let* state = M.initial_state Wasm_pvm_state.V6 (M.empty_state ()) in
    let* state =
      M.install_boot_sector
        ~ticks_per_snapshot
        ~outbox_validity_period:10l
        ~outbox_message_limit:(Z.of_int 100)
        kernel
        state
    in
    let+ state, _ticks = eval_until_input_requested ~hooks state 0L in
    state

  let input_info level message_counter =
    Wasm_pvm_state.
      {
        inbox_level =
          Option.value_f ~default:(fun () -> assert false)
          @@ Tezos_base.Bounded.Non_negative_int32.of_value level;
        message_counter = Z.of_int message_counter;
      }

  let info_per_level =
    Data_encoding.(
      Binary.to_string_exn
        (tup2 Time.Protocol.encoding Block_hash.encoding)
        (Time.Protocol.epoch, Block_hash.zero))

  (** Feed one inbox level made of [payloads] as external messages and
      evaluate until the kernel asks for the next level.  Returns the
      state and the ticks the evaluation reported. *)
  let run_level ~hooks ~level payloads state =
    let open Lwt.Syntax in
    let open Pvm_input_kind.Internal_for_tests in
    let set counter payload state =
      M.set_input_step (input_info level counter) payload state
    in
    let* state = set 0 (to_binary_input (Internal Start_of_level) None) state in
    let* state =
      set
        1
        (to_binary_input (Internal Info_per_level) (Some info_per_level))
        state
    in
    let* state, counter =
      List.fold_left_s
        (fun (state, counter) payload ->
          let+ state =
            set counter (to_binary_input External (Some payload)) state
          in
          (state, counter + 1))
        (state, 2)
        payloads
    in
    let* state =
      set counter (to_binary_input (Internal End_of_level) None) state
    in
    eval_until_input_requested ~hooks state 0L
end

module Activated = Make_pair (struct
  let config = nds_activated_on_chain
end)

module Not_activated = Make_pair (struct
  let config = Wasm_pvm_config.empty
end)

module Slow_activated = Driver (Activated.Slow)
module Fast_activated = Driver (Activated.Fast)
module Slow_not_activated = Driver (Not_activated.Slow)
module Fast_not_activated = Driver (Not_activated.Fast)

(* -------------------------------------------------------------------- *)
(* Observing a state                                                    *)
(* -------------------------------------------------------------------- *)

let durable_value (state : Dual.Dual_state.state) key =
  let open Lwt.Syntax in
  let* durable = Dual.Dual_state.Encoding_runner.decode_durable_storage state in
  Durable.find_value_as_bytes durable (Durable.key_of_string_exn key)

(** The [i32] the kernel recorded for [cmd].  With [index], the [i32] a
    wide command recorded for the wide value at [index]. *)
let recorded_code ?index state cmd =
  let open Lwt.Syntax in
  let name =
    String.make 1 cmd ^ Option.fold ~none:"" ~some:string_of_int index
  in
  let+ value = durable_value state ("/r/" ^ name) in
  match value with
  | Some b when Bytes.length b = 4 -> Bytes.get_int32_le b 0
  | _ -> Stdlib.failwith (Printf.sprintf "no code recorded for command %s" name)

(** The bytes the kernel received for [cmd] ([r] or [h]). *)
let recorded_data state cmd =
  let open Lwt.Syntax in
  let+ value = durable_value state ("/r/" ^ String.make 1 cmd ^ "d") in
  match value with
  | Some b -> Bytes.to_string b
  | None ->
      Stdlib.failwith (Printf.sprintf "no data recorded for command %c" cmd)

let is_active (state : Dual.Dual_state.state) =
  match state.nds with Active _ -> true | Inactive -> false

let registry_size (state : Dual.Dual_state.state) =
  match state.nds with
  | Active nds -> Octez_riscv_nds_common.Nds.size nds
  | Inactive -> Stdlib.failwith "the NDS is not active"

let check_code ?index state cmd expected =
  let open Lwt.Syntax in
  let+ code = recorded_code ?index state cmd in
  if not (Int32.equal code expected) then
    Stdlib.failwith
      (Printf.sprintf
         "command %c%s answered %ld, expected %ld"
         cmd
         (Option.fold ~none:"" ~some:(Printf.sprintf " (wide value %d)") index)
         code
         expected)

(** Check that the wide command [cmd] answered [expected] for each wide
    value from index [from]. *)
let check_wide_codes ?(from = 0) state cmd expected =
  List.iteri_s
    (fun index _ ->
      if index < from then Lwt.return_unit
      else check_code ~index state cmd expected)
    wide_values

let check_data state cmd expected =
  let open Lwt.Syntax in
  let+ data = recorded_data state cmd in
  if not (String.equal data expected) then
    Stdlib.failwith
      (Printf.sprintf "command %c received %S, expected %S" cmd data expected)

let check_active state expected =
  if is_active state <> expected then
    Stdlib.failwith
      (Printf.sprintf
         "the NDS is %s, expected it %s"
         (if is_active state then "active" else "inactive")
         (if expected then "active" else "inactive"))

(** Hooks of a fast run.  [fallback] decides whether a fast-engine panic
    falls back on the interpreter; off, a missing import fails loudly
    instead of silently running the slow engine.  [counter] counts the
    [kernel_run]s the fast engine completed. *)
let fast_hooks ~fallback counter =
  Hooks.(
    no_hooks
    |> fast_exec_fallback fallback
    |> on_fast_exec_completed (fun () ->
           incr counter ;
           Lwt.return_unit))

let check_fast_engaged counter ~level =
  if !counter <= 0 then
    Stdlib.failwith
      (Printf.sprintf "the fast engine did not run at level %ld" level) ;
  counter := 0

let check_agree ~level (slow, slow_ticks) (fast, fast_ticks) =
  let open Lwt.Syntax in
  let* slow_hash = Dual.Dual_state.state_hash slow in
  let+ fast_hash = Dual.Dual_state.state_hash fast in
  let module H = Tezos_crypto.Hashed.Smart_rollup_state_hash in
  if not (H.equal slow_hash fast_hash) then
    Stdlib.failwith
      (Format.asprintf
         "state hashes differ at level %ld: slow %a, fast %a"
         level
         H.pp
         slow_hash
         H.pp
         fast_hash) ;
  if not (Int64.equal slow_ticks fast_ticks) then
    Stdlib.failwith
      (Printf.sprintf
         "tick counts differ at level %ld: slow %Ld, fast %Ld"
         level
         slow_ticks
         fast_ticks) ;
  if is_active slow <> is_active fast then
    Stdlib.failwith (Printf.sprintf "NDS activation differs at level %ld" level)

(* -------------------------------------------------------------------- *)
(* Tests                                                                *)
(* -------------------------------------------------------------------- *)

let error code = Host_funcs.Error.code code

(** Both engines run the same levels and agree after each; the slow
    state is checked against the semantics the interpreter documents,
    so agreement means the fast engine implements them too. *)
let test_engines_agree ~fallback () =
  let open Lwt_result_syntax in
  let*! kernel =
    Tezos_scoru_wasm_helpers_functor.Wasm_utils_functor.wat2wasm kernel
  in
  let counter = ref 0 in
  let slow_hooks = Hooks.no_hooks in
  let fast_hooks = fast_hooks ~fallback counter in
  let*! slow = Slow_activated.boot ~hooks:slow_hooks kernel in
  let*! fast = Fast_activated.boot ~hooks:fast_hooks kernel in
  check_active slow false ;
  check_active fast false ;
  let run ~level payloads slow fast =
    let*! slow =
      Slow_activated.run_level ~hooks:slow_hooks ~level payloads slow
    in
    let*! fast =
      Fast_activated.run_level ~hooks:fast_hooks ~level payloads fast
    in
    check_fast_engaged counter ~level ;
    let*! () = check_agree ~level slow fast in
    return (fst slow, fst fast)
  in
  (* Level 1: the kernel requests the activation; it fires at the
     reboot boundary that ends the level. *)
  let* slow, fast = run ~level:1l [activate] slow fast in
  check_active slow true ;
  check_active fast true ;
  (* Level 2: the registry and a first value. *)
  let* slow, fast =
    run
      ~level:2l
      [
        resize 1;
        resize 1;
        resize 0;
        set "k1" "hello";
        exists "k1";
        value_size "k1";
        read ~len:5 "k1";
        db_hash ~len:32 ();
      ]
      slow
      fast
  in
  let*! () = check_code slow 'z' 2l in
  let*! () = check_code slow 's' 0l in
  let*! () = check_code slow 'e' 1l in
  let*! () = check_code slow 'v' 5l in
  let*! () = check_code slow 'r' 5l in
  let*! () = check_data slow 'r' "hello" in
  let*! () = check_code slow 'h' 32l in
  let*! hash = recorded_data slow 'h' in
  if String.length hash <> 32 then
    Stdlib.failwith "database_get_hash did not write 32 bytes" ;
  (* Level 3: writes at an offset, and moving databases around. *)
  let* slow, fast =
    run
      ~level:3l
      [
        write ~offset:5 "k1" " world";
        read ~len:11 "k1";
        copy ~src:0 ~dst:1;
        exists ~db:1 "k1";
        move ~src:1 ~dst:0;
        exists ~db:1 "k1";
        clear 0;
        exists "k1";
        db_hash ~len:32 ();
      ]
      slow
      fast
  in
  let*! () = check_code slow 'w' 6l in
  let*! () = check_code slow 'r' 11l in
  let*! () = check_data slow 'r' "hello world" in
  let*! () = check_code slow 'c' 0l in
  let*! () = check_code slow 'm' 0l in
  let*! () = check_code slow 'x' 0l in
  let*! () = check_code slow 'e' 0l in
  let*! cleared_hash = recorded_data slow 'h' in
  if String.equal cleared_hash hash then
    Stdlib.failwith "clearing the database did not change its hash" ;
  (* Level 4: the error paths, then shrinking the registry away. *)
  let* slow, fast =
    run
      ~level:4l
      [
        exists ~db:5 "k1";
        read ~len:5 "missing";
        value_size "missing";
        set (String.make 257 'k') "v";
        read ~len:5000 "k1";
        db_hash ~len:8 ();
        copy ~src:0 ~dst:9;
        clear 9;
        delete "k1";
        resize (-1);
        resize (-1);
        resize (-1);
      ]
      slow
      fast
  in
  let*! () = check_code slow 'e' (error Nds_database_out_of_bounds) in
  let*! () = check_code slow 'r' (error Input_output_too_large) in
  let*! () = check_code slow 'v' (error Store_not_a_value) in
  let*! () = check_code slow 's' (error Store_key_too_large) in
  let*! () = check_code slow 'h' 8l in
  let*! () = check_data slow 'h' (String.sub cleared_hash 0 8) in
  let*! () = check_code slow 'c' (error Nds_database_out_of_bounds) in
  let*! () = check_code slow 'x' (error Nds_database_out_of_bounds) in
  let*! () = check_code slow 'd' 0l in
  let*! () = check_code slow 'z' (error Nds_resize_invalid) in
  (* Level 5: the empty key, including one read from out of bounds. *)
  let* slow, fast =
    run
      ~level:5l
      [resize 1; set_empty_key_out_of_bounds "v"; exists ""; value_size ""]
      slow
      fast
  in
  let*! () = check_code slow 'O' 0l in
  let*! () = check_code slow 'e' 1l in
  let*! () = check_code slow 'v' 1l in
  (* Level 6: a destination the value does not fit in, on its own level
     so that a mismatch is reported against this command. *)
  let* slow, fast =
    run ~level:6l [set "k2" "hello"; read_into_last_bytes ~len:5 "k2"] slow fast
  in
  let*! () = check_code slow 'Q' (error Memory_invalid_access) in
  let*! () = check_data slow 'Q' (String.make 6 '\000') in
  (* Level 7: a negative destination size, which must be answered and
     not raised. *)
  let* slow, fast = run ~level:7l [db_hash_negative_max_bytes ()] slow fast in
  let*! () = check_code slow 'N' (error Memory_invalid_access) in
  (* Level 8: answers that end exactly at the end of the memory.  Both
     host functions write them in full. *)
  let* slow, fast =
    run
      ~level:8l
      [
        set "k3" "hi";
        read_into_last_bytes ~len:2 "k3";
        db_hash ~len:32 ();
        db_hash_into_last_bytes ~len:2 ();
      ]
      slow
      fast
  in
  let*! () = check_code slow 'Q' 2l in
  let*! () = check_data slow 'Q' "\000\000\000\000hi" in
  let*! hash = recorded_data slow 'h' in
  let*! () = check_code slow 'H' 2l in
  let*! () = check_data slow 'H' ("\000\000\000\000" ^ String.sub hash 0 2) in
  (* Level 9: a negative [max_bytes] for [store_read], on a key that
     exists.  The host function answers [Memory_invalid_access] before
     it reads the NDS. *)
  let* slow, fast =
    run
      ~level:9l
      [read_negative_max_bytes "k2"; read_min_int_max_bytes "k2"]
      slow
      fast
  in
  let*! () = check_code slow 'R' (error Memory_invalid_access) in
  let*! () = check_code slow 'S' (error Memory_invalid_access) in
  (* Level 10: wide db indexes.  The registry holds one database, so
     each index is out of bounds. *)
  let* slow, fast =
    run ~level:10l [wide_db_exists "k2"; wide_db_clear] slow fast
  in
  let*! () = check_wide_codes slow 'I' (error Nds_database_out_of_bounds) in
  let*! () = check_wide_codes slow 'X' (error Nds_database_out_of_bounds) in
  (* Level 11: wide offsets on the 5-byte value of "k2".  A read past the
     end of the value answers [Store_invalid_access].  A write past the
     maximum value size answers [Store_value_size_exceeded], before any
     allocation.  The value stays the same. *)
  let* slow, fast =
    run
      ~level:11l
      [
        wide_offset_read ~len:5 "k2";
        wide_offset_write "k2" "x";
        read ~len:5 "k2";
      ]
      slow
      fast
  in
  let*! () = check_wide_codes slow 'J' (error Store_invalid_access) in
  let*! () = check_wide_codes slow 'K' (error Store_value_size_exceeded) in
  let*! () = check_data slow 'r' "hello" in
  (* Level 12: wide resize diffs.  Each new size is negative or larger
     than [Int32.max_int], so the registry keeps its size. *)
  let* slow, fast = run ~level:12l [wide_resize] slow fast in
  let*! () = check_wide_codes ~from:1 slow 'Z' (error Nds_resize_invalid) in
  if
    not
      (Int64.equal (registry_size slow) 1L
      && Int64.equal (registry_size fast) 1L)
  then Stdlib.failwith "a wide resize diff changed the size of the registry" ;
  (* Level 13: [num_bytes] over [Aux.input_output_max_size] (4096).  The
     NDS stores nothing. *)
  let* slow, _fast =
    run
      ~level:13l
      [
        write_oversized ~len:5000 "k4"; set_oversized ~len:5000 "k4"; exists "k4";
      ]
      slow
      fast
  in
  let*! () = check_code slow 'w' (error Input_output_too_large) in
  let*! () = check_code slow 's' (error Input_output_too_large) in
  let*! () = check_code slow 'e' 0l in
  return_unit

(** When NDS is not activated on chain, both engines answer
    [Nds_not_enabled], and the activation request of the kernel stays
    pending. *)
let test_nds_not_activated_on_chain () =
  let open Lwt_result_syntax in
  let*! kernel =
    Tezos_scoru_wasm_helpers_functor.Wasm_utils_functor.wat2wasm kernel
  in
  let counter = ref 0 in
  let slow_hooks = Hooks.no_hooks in
  let fast_hooks = fast_hooks ~fallback:false counter in
  let*! slow = Slow_not_activated.boot ~hooks:slow_hooks kernel in
  let*! fast = Fast_not_activated.boot ~hooks:fast_hooks kernel in
  let run ~level payloads slow fast =
    let*! slow =
      Slow_not_activated.run_level ~hooks:slow_hooks ~level payloads slow
    in
    let*! fast =
      Fast_not_activated.run_level ~hooks:fast_hooks ~level payloads fast
    in
    check_fast_engaged counter ~level ;
    let*! () = check_agree ~level slow fast in
    return (fst slow, fst fast)
  in
  let* slow, fast = run ~level:1l [activate] slow fast in
  check_active slow false ;
  check_active fast false ;
  let* slow, fast = run ~level:2l [resize 1; exists "k1"] slow fast in
  check_active slow false ;
  check_active fast false ;
  let*! () = check_code slow 'z' Host_funcs.nds_disabled_code in
  let*! () = check_code slow 'e' Host_funcs.nds_disabled_code in
  return_unit

(** When NDS is not activated on chain, a negative key length raises in
    the tick computation on both engines. The interpreter commits
    [Stuck]. The fast engine raises, and the interpreter fallback replays
    the [kernel_run], so both engines end in the same state. *)
let test_negative_key_len_not_activated_on_chain () =
  let open Lwt_result_syntax in
  let*! kernel =
    Tezos_scoru_wasm_helpers_functor.Wasm_utils_functor.wat2wasm kernel
  in
  let completed = ref 0 and panics = ref 0 in
  let slow_hooks = Hooks.no_hooks in
  let fast_hooks =
    Hooks.(
      fast_hooks ~fallback:true completed
      |> on_fast_exec_panicked (fun _exn ->
             incr panics ;
             Lwt.return_unit))
  in
  let*! slow = Slow_not_activated.boot ~hooks:slow_hooks kernel in
  let*! fast = Fast_not_activated.boot ~hooks:fast_hooks kernel in
  let payloads = [exists_negative_key_len ()] in
  let*! slow =
    Slow_not_activated.run_level ~hooks:slow_hooks ~level:1l payloads slow
  in
  let*! fast =
    Fast_not_activated.run_level ~hooks:fast_hooks ~level:1l payloads fast
  in
  let*! () = check_agree ~level:1l slow fast in
  if !panics <> 1 then
    Stdlib.failwith
      (Printf.sprintf "expected one fast-engine panic, got %d" !panics) ;
  return_unit

(** When NDS is activated on chain but the storage is still
    [Durable_only], a call raises the dispatch exception on both engines.
    Neither engine turns it into a [Stuck] state or answers it. *)
let test_without_storage_raises () =
  let open Lwt_result_syntax in
  let*! kernel =
    Tezos_scoru_wasm_helpers_functor.Wasm_utils_functor.wat2wasm kernel
  in
  let expect_raise name run =
    let*! raised =
      Lwt.catch
        (fun () ->
          let*! _ = run () in
          Lwt.return_false)
        (function
          | Tezos_webassembly_interpreter.Eval.Nds_host_func_without_nds_storage
            ->
              Lwt.return_true
          | exn -> Lwt.reraise exn)
    in
    if not raised then
      Stdlib.failwith
        (name
       ^ " engine answered an NDS call on Durable_only storage instead of \
          raising") ;
    return_unit
  in
  let* () =
    expect_raise "slow" @@ fun () ->
    let*! slow = Slow_activated.boot ~hooks:Hooks.no_hooks kernel in
    Slow_activated.run_level ~hooks:Hooks.no_hooks ~level:1l [exists "k1"] slow
  in
  let counter = ref 0 in
  let hooks = fast_hooks ~fallback:false counter in
  expect_raise "fast" @@ fun () ->
  let*! fast = Fast_activated.boot ~hooks kernel in
  Fast_activated.run_level ~hooks ~level:1l [exists "k1"] fast

(** When the fast engine panics halfway through a [kernel_run] that
    already mutated the NDS, the interpreter fallback replays from the
    snapshot taken before the run, so the mutation is applied once — as
    on a pure interpreter run — and not twice. *)
let test_fallback_replays_from_snapshot () =
  let open Lwt_result_syntax in
  let*! kernel =
    Tezos_scoru_wasm_helpers_functor.Wasm_utils_functor.wat2wasm kernel
  in
  let completed = ref 0 and panics = ref 0 in
  let slow_hooks = Hooks.no_hooks in
  let fast_hooks =
    Hooks.(
      fast_hooks ~fallback:true completed
      |> on_fast_exec_panicked (fun _exn ->
             incr panics ;
             Lwt.return_unit))
  in
  let*! slow = Slow_activated.boot ~hooks:slow_hooks kernel in
  let*! fast = Fast_activated.boot ~hooks:fast_hooks kernel in
  let run ~level payloads slow fast =
    let*! slow =
      Slow_activated.run_level ~hooks:slow_hooks ~level payloads slow
    in
    let*! fast =
      Fast_activated.run_level ~hooks:fast_hooks ~level payloads fast
    in
    let*! () = check_agree ~level slow fast in
    return (fst slow, fst fast)
  in
  let* slow, fast = run ~level:1l [activate] slow fast in
  let* slow, fast = run ~level:2l [resize 1; set "k1" "v"] slow fast in
  check_fast_engaged completed ~level:2l ;
  if !panics <> 0 then Stdlib.failwith "the fast engine panicked too early" ;
  let* slow, fast = run ~level:3l [grow_then_panic] slow fast in
  if !panics <> 1 then
    Stdlib.failwith
      (Printf.sprintf "expected one fast-engine panic, got %d" !panics) ;
  if !completed <> 0 then
    Stdlib.failwith "the fast engine reported a completed kernel_run" ;
  let check_size state engine =
    let size = registry_size state in
    if not (Int64.equal size 2L) then
      Stdlib.failwith
        (Printf.sprintf
           "%s: the registry holds %Ld databases, expected 2 (the growth \
            applied once)"
           engine
           size)
  in
  check_size slow "interpreter" ;
  check_size fast "fast engine with fallback" ;
  return_unit

let tests =
  [
    Tztest.tztest
      "NDS kernel: the fast engine agrees with the interpreter"
      `Quick
      (test_engines_agree ~fallback:false);
    Tztest.tztest
      "NDS kernel: the fast engine with the interpreter fallback enabled \
       agrees with the interpreter"
      `Quick
      (test_engines_agree ~fallback:true);
    Tztest.tztest
      "NDS not activated on chain: both engines answer Nds_not_enabled"
      `Quick
      test_nds_not_activated_on_chain;
    Tztest.tztest
      "NDS not activated on chain: a negative key length raises on both engines"
      `Quick
      test_negative_key_len_not_activated_on_chain;
    Tztest.tztest
      "NDS call on Durable_only storage raises on both engines"
      `Quick
      test_without_storage_raises;
    Tztest.tztest
      "NDS fallback: the interpreter replays from the handle snapshot"
      `Quick
      test_fallback_replays_from_snapshot;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru-wasm-fast"
    [("NDS host functions", tests)]
  |> Lwt_main.run
