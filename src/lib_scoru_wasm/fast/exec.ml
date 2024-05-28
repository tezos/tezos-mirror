(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
module Wasmer = Tezos_wasmer

include (Wasm_vm : Wasm_vm_sig.S)

let compiler_env_variable = "OCTEZ_WASMER_COMPILER"

let get_compiler () =
  let compiler_choice =
    Option.map String.lowercase_ascii (Sys.getenv_opt compiler_env_variable)
  in
  match compiler_choice with
  | Some "singlepass" -> Wasmer.Config.SINGLEPASS
  | Some "cranelift" -> Wasmer.Config.CRANELIFT
  | Some compiler ->
      Format.sprintf
        "Unknown Wasmer compiler %S (selected via %s environment variable)"
        compiler
        compiler_env_variable
      |> Stdlib.failwith
  | None -> Wasmer.Config.CRANELIFT

let engine =
  Lazy.from_fun @@ fun () ->
  Wasmer.Engine.create Wasmer.Config.{compiler = get_compiler ()}

(** This store will largely remain empty. It is mainly useful when a function
    wants to access the underlying engine, but its interface requires a store.

    This is the case with [wasm_module_new]. In Wasmer, the underlying function
    only needs access to the engine, but the standardised WebAssembly C API
    dictates that the function requires a store parameter. *)
let static_store = Lazy.map Wasmer.Store.create engine

(** Allocate a new store, pass it to the given function and destroy the
    store once the function is done.

    We do this to combat an issue with Wasmer 3.3.0 where the store objects
    belonging to a module instance are kept alive after the instance has been
    destroyed. *)
let with_store f =
  let store = Wasmer.Store.create (Lazy.force engine) in
  Lwt.finalize
    (fun () -> f store)
    (fun () ->
      Wasmer.Store.delete store ;
      Lwt.return_unit)

let load_kernel durable =
  let store = Lazy.force static_store in
  Module_cache.load_kernel store durable

let compute ?(wasm_entrypoint = Constants.wasm_entrypoint) ~version
    ~reveal_builtins ~write_debug durable buffers =
  let open Lwt.Syntax in
  let* module_ = load_kernel durable in

  let main_mem : (unit -> Wasmer.Memory.t) option ref = ref None in
  let retrieve_mem () =
    match !main_mem with Some x -> x () | None -> assert false
  in

  let host_state = Funcs.{retrieve_mem; buffers; durable} in
  let host_funcs =
    Funcs.make ~version ~reveal_builtins ~write_debug host_state
  in

  let with_durable f =
    let+ durable = f host_state.durable in
    host_state.durable <- durable
  in

  with_store @@ fun store ->
  let* instance = Wasmer.Instance.create store module_ host_funcs in

  let* () =
    (* At this point we know that the kernel is valid because we parsed and
       instantiated it. It is now safe to set it as the fallback kernel. *)
    with_durable Wasm_vm.save_fallback_kernel
  in

  let exports = Wasmer.Exports.from_instance instance in
  let kernel_run =
    Wasmer.(Exports.fn exports wasm_entrypoint (producer nothing))
  in

  main_mem := Some (fun () -> Wasmer.Exports.mem0 exports) ;

  let* () =
    Lwt.finalize kernel_run (fun () ->
        (* Make sure that the instance is deleted regardless of whether
           [kernel_run] succeeds or not. *)
        main_mem := None ;
        Wasmer.Exports.delete exports ;
        Wasmer.Instance.delete instance ;
        Lwt.return_unit)
  in

  let* durable = Wasm_vm.patch_flags_on_eval_successful host_state.durable in
  (* TODO: #4283
     The module is cached, but the cash is never cleaned.
     This is the point where it was scrubed before.*)
  Lwt.return durable
