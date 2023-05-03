(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech  <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component:    Lib_scoru_wasm input
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_get_set.ml
    Subject:      Input tests for the tezos-scoru-wasm library
*)

open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Wasm_utils
open Tztest_helper

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary
module Vector = Tezos_lazy_containers.Lazy_vector.Int32Vector

let empty_tree () =
  let open Lwt_syntax in
  let* index = Context.init "/tmp" in
  let empty_store = Context.empty index in
  return @@ Context.Tree.empty empty_store

type Tezos_tree_encoding.tree_instance += Tree of Context.tree

module Tree : Tezos_tree_encoding.TREE with type tree = Context.tree = struct
  type tree = Context.tree

  include Context.Tree

  let select = function
    | Tree t -> t
    | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

  let wrap t = Tree t
end

module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (Tree)

let current_tick_encoding =
  Tezos_tree_encoding.value ["wasm"; "current_tick"] Data_encoding.n

(* Replicates the encoding of buffers from [Wasm_pvm] as part of the pvm_state. *)
let buffers_encoding =
  Tezos_tree_encoding.scope ["pvm"; "buffers"] Wasm_encoding.buffers_encoding

let zero =
  WithExceptions.Option.get
    ~loc:__LOC__
    (Bounded.Non_negative_int32.of_value 0l)

let initialise_tree () =
  Wasm_utils.initial_tree ~from_binary:true "arbitrary boot sector"

let make_inbox_info ~inbox_level ~message_counter =
  Wasm_pvm_state.
    {
      inbox_level =
        WithExceptions.Option.get
          ~loc:__LOC__
          (Bounded.Non_negative_int32.of_value (Int32.of_int inbox_level));
      message_counter = Z.of_int message_counter;
    }

let make_output_info ~outbox_level ~message_index =
  Wasm_pvm_state.
    {
      outbox_level =
        WithExceptions.Option.get
          ~loc:__LOC__
          (Bounded.Non_negative_int32.of_value (Int32.of_int outbox_level));
      message_index = Z.of_int message_index;
    }

(** Artificial initialization of the raw_level and message id. Again, in practice
    these will normally be initialized  by the origination step and modified by
    subsequent read_input steps.*)
let add_input_info ~inbox_level ~message_counter tree =
  let open Lwt_syntax in
  let* tree =
    Tree_encoding_runner.encode
      (Tezos_tree_encoding.value_option
         ["wasm"; "input"]
         Wasm_pvm_sig.input_info_encoding)
      (Some (make_inbox_info ~inbox_level ~message_counter))
      tree
  in
  Lwt.return tree

(** Simple test checking get_info after the initialization. Note that we also
    check that if the tree has no last_input_read set the response to [get_info]
    has [None] as [last_input_read] *)
let test_get_info ~version () =
  let open Lwt_syntax in
  let* tree = initialise_tree ~version () in
  let expected_info ~inbox_level ~message_counter =
    let open Wasm_pvm_state in
    let last_input_read =
      Some (make_inbox_info ~inbox_level ~message_counter)
    in
    {current_tick = Z.zero; last_input_read; input_request = Input_required}
  in
  let* actual_info = Wasm.get_info tree in
  assert (actual_info.last_input_read = None) ;
  (* We're adding input info by writing directly to the tree. This is for
     testing that the encoder in [Wasm_pvm] reads the corresponding paths. *)
  let* tree = add_input_info ~inbox_level:5 ~message_counter:10 tree in
  let* actual_info = Wasm.get_info tree in
  assert (actual_info = expected_info ~inbox_level:5 ~message_counter:10) ;
  Lwt_result_syntax.return_unit

let encode_tick_state tree =
  let open Lwt_syntax in
  (* Encode the tag. *)
  let* tree =
    Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["wasm"; "tag"] Data_encoding.string)
      "collect"
      tree
  in
  (* Encode the value. *)
  let* tree =
    Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["wasm"; "value"] Data_encoding.unit)
      ()
      tree
  in
  return tree

(** Tests that, after set_input the resulting tree decodes to the correct values.
    In particular it does check that [get_info] produces the expected value. *)
let test_set_input ~version () =
  let open Lwt_syntax in
  let* tree = initialise_tree ~version () in
  let* tree = encode_tick_state tree in
  let* tree =
    Wasm.set_input_step
      {inbox_level = zero; message_counter = Z.of_int 1}
      "\000\000hello"
      tree
  in
  let* buffers =
    Tree_encoding_runner.decode
      (Tezos_tree_encoding.option Wasm_pvm.durable_buffers_encoding)
      tree
  in
  let buffers =
    match buffers with Some buffers -> buffers | None -> assert false
  in
  let* result_message = Input_buffer.dequeue buffers.input in
  let* current_tick = Tree_encoding_runner.decode current_tick_encoding tree in
  let expected_info =
    let open Wasm_pvm_state in
    let last_input_read =
      Some (make_inbox_info ~inbox_level:0 ~message_counter:1)
    in
    {current_tick = Z.one; last_input_read; input_request = Input_required}
  in
  let* actual_info = Wasm.get_info tree in
  assert (actual_info = expected_info) ;
  assert (current_tick = Z.one) ;
  assert (Bytes.to_string result_message.payload = "\000\000hello") ;
  Lwt_result_syntax.return_unit

(** Given a [config] whose output has a given payload at position (0,0), if we
encode [config] into a tree [get_output output_info tree] produces the same
payload. Here the output_info is { outbox_level = 0; message_index = 0 } *)
let test_get_output ~version () =
  let open Lwt_syntax in
  let* tree = initialise_tree ~version () in
  let* tree = add_input_info tree ~inbox_level:5 ~message_counter:10 in
  let output = Tezos_webassembly_interpreter.Eval.default_output_buffer () in
  let* Output_buffer.{outbox_level; message_index} =
    Output_buffer.push_message output @@ Bytes.of_string "hello"
  in
  assert (outbox_level = 0l && Z.equal message_index Z.zero) ;
  let buffers = Eval.{input = Input_buffer.alloc (); output} in
  let* tree =
    Tree_encoding_runner.encode
      (Tezos_tree_encoding.option buffers_encoding)
      (Some buffers)
      tree
  in
  let output_info = make_output_info ~outbox_level:0 ~message_index:0 in
  let* payload = Wasm.get_output output_info tree in
  assert (payload = Some "hello") ;
  Lwt_result_syntax.return_unit

let tests =
  tztests_with_pvm
    ~versions:[V0; V1]
    [
      ("Get info", `Quick, test_get_info);
      ("Set input", `Quick, test_set_input);
      ("Get output", `Quick, test_get_output);
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Set/get", tests)]
  |> Lwt_main.run
