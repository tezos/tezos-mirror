(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Alpha_context

type identifier = string

let identifier_of_contract addr = Contract_hash.to_b58check addr

let contract_of_identifier identifier =
  let open Result_syntax in
  match Contract_hash.of_b58check_opt identifier with
  | Some addr -> return addr
  | None -> tzfail (Contract_repr.Invalid_contract_notation identifier)

type cached_contract = Script.t * Script_ir_translator.ex_script

let load_and_elaborate ctxt addr =
  let open Lwt_result_syntax in
  let* ctxt, script = Contract.get_script ctxt addr in
  match script with
  | None -> return (ctxt, None)
  | Some script ->
      Script_ir_translator.(
        let* ex_script, ctxt =
          parse_script
            ctxt
            script
            ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
            ~allow_forged_tickets_in_storage:true
            ~allow_forged_lazy_storage_id_in_storage:true
        in
        (* We consume gas after the fact in order to not have to instrument
           [script_size] (for efficiency).
           This is safe, as we already pay gas proportional to storage size
           in [parse_script] beforehand. *)
        let size, cost = script_size ex_script in
        let*? ctxt = Gas.consume ctxt cost in
        return (ctxt, Some (script, ex_script, size)))

module Client = struct
  type cached_value = cached_contract

  let namespace = Cache.create_namespace "contract"

  let cache_index = 0

  let value_of_identifier ctxt identifier =
    let open Lwt_result_syntax in
    (*

       I/O, deserialization, and elaboration of contracts scripts
       are cached.

    *)
    let*? addr = contract_of_identifier identifier in
    let* (_ : context), result = load_and_elaborate ctxt addr in
    match result with
    | None ->
        (* [value_of_identifier ctxt k] is applied to identifiers stored
           in the cache. Only script-based contracts that have been
           executed are in the cache. Hence, [get_script] always
           succeeds for these identifiers if [ctxt] and the [cache] are
           properly synchronized by the shell. *)
        failwith "Script_cache: Inconsistent script cache."
    | Some (unparsed_script, ir_script, _) -> return (unparsed_script, ir_script)
end

module Cache = (val Cache.register_exn (module Client))

let find ctxt addr =
  let open Lwt_result_syntax in
  let identifier = identifier_of_contract addr in
  let* contract_opt = Cache.find ctxt identifier in
  match contract_opt with
  | Some (unparsed_script, ex_script) ->
      return (ctxt, identifier, Some (unparsed_script, ex_script))
  | None -> (
      let* ctxt, result = load_and_elaborate ctxt addr in
      match result with
      | None -> return (ctxt, identifier, None)
      | Some (unparsed_script, script_ir, size) ->
          let cached_value = (unparsed_script, script_ir) in
          let*? ctxt =
            Cache.update ctxt identifier (Some (cached_value, size))
          in
          return (ctxt, identifier, Some (unparsed_script, script_ir)))

let update ctxt identifier updated_script approx_size =
  Cache.update ctxt identifier (Some (updated_script, approx_size))

let entries ctxt =
  let open Result_syntax in
  Cache.list_identifiers ctxt
  |> List.map_e @@ fun (identifier, age) ->
     let+ contract = contract_of_identifier identifier in
     (contract, age)

let contract_rank ctxt addr =
  Cache.identifier_rank ctxt (identifier_of_contract addr)

let size = Cache.size

let size_limit = Cache.size_limit
