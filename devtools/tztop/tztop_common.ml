(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

let patch_env_loading () =
  let open Tezos_base.TzPervasives in
  let preloaded_cmis : Persistent_env.Persistent_signature.t String.Hashtbl.t =
    Tezos_protocol_compiler.Compiler.preloaded_cmis
  in
  Persistent_env.Persistent_signature.load :=
    fun ~unit_name ->
      match
        String.Hashtbl.find preloaded_cmis (String.capitalize_ascii unit_name)
      with
      | Some v -> Some v
      | None -> Tezos_protocol_compiler.Compiler.default_load ~unit_name

let directive_string_fn fn_name =
  match Hashtbl.find_opt Toploop.directive_table fn_name with
  | Some (Toploop.Directive_string fn) -> fn
  | _ ->
      Printf.printf
        "Tztop failed to load due to an internal error\n\
         Unable to find directive %s\n"
        fn_name ;
      exit (-1)

let use_output command = directive_string_fn "use_output" command

let load_stdlib () = directive_string_fn "load" "stdlib.cma"

let load_dune_libs directory = use_output @@ "dune top " ^ directory
