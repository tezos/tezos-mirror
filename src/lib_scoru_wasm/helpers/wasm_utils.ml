(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Tezos_scoru_wasm
include Wasm_utils_functor

module Kernels = struct
  (* Kernel failing at `kernel_run` invocation. *)
  let unreachable_kernel = "unreachable"
end

let project_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some x -> x
  | None -> (
      match Sys.getenv_opt "PWD" with
      | Some x -> x
      | None ->
          (* For some reason, under [dune runtest], [PWD] and
             [getcwd] have different values. [getcwd] is in
             [_build/default], and [PWD] is where [dune runtest] was
             executed, which is closer to what we want. *)
          Sys.getcwd ())

let ( // ) = Filename.concat

let test_with_kernel kernel (test : string -> (unit, _) result Lwt.t) () =
  let open Lwt_result_syntax in
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "../test/wasm_kernels"
    // (kernel ^ ".wasm")
  in

  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Input kernel_file (fun channel ->
        let*! kernel = Lwt_io.read channel in
        test kernel)
  in
  return_unit

let read_test_messages names =
  let locate_file name =
    project_root // Filename.dirname __FILE__ // "../test/messages" // name
  in
  List.map_s
    (fun name ->
      let message_file = locate_file name in
      Lwt_io.with_file ~mode:Lwt_io.Input message_file Lwt_io.read)
    names

(** Can be passed to be used as a host function
    [compute_step_many ~write_debug:write_debug_on_stdout ...] *)
let write_debug_on_stdout =
  Tezos_scoru_wasm.Builtins.Printer
    (fun msg -> Lwt.return @@ Format.printf "%s\n%!" msg)

module Make (Ctx : Tezos_tree_encoding.Encodings_util.S) :
  Wasm_utils_intf.S with type t = Ctx.t and type tree = Ctx.Tree.tree =
  Make (Ctx) (Wasm_pvm.Make_machine (Ctx.Tree))
    (Tezos_scoru_wasm_fast.Pvm.Make (Ctx.Tree))

module In_memory_context =
  Tezos_tree_encoding.Encodings_util.Make (Tezos_context_memory.Context_binary)
include Make (In_memory_context)
