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
  match Contract_hash.of_b58check_opt identifier with
  | Some addr -> Ok addr
  | None -> error (Contract_repr.Invalid_contract_notation identifier)

type cached_contract = Script.t * Script_ir_translator.ex_script

let load_and_elaborate ctxt addr =
  Contract.get_script ctxt addr >>=? fun (ctxt, script) ->
  match script with
  | None -> return (ctxt, None)
  | Some script ->
      Script_ir_translator.(
        parse_script
          ctxt
          script
          ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
          ~allow_forged_in_storage:true
        >>=? fun (ex_script, ctxt) ->
        (* We consume gas after the fact in order to not have to instrument
           [script_size] (for efficiency).
           This is safe, as we already pay gas proportional to storage size
           in [parse_script] beforehand. *)
        let size, cost = script_size ex_script in
        Gas.consume ctxt cost >>?= fun ctxt ->
        return (ctxt, Some (script, ex_script, size)))

module Client = struct
  type cached_value = cached_contract

  let namespace = Cache.create_namespace "contract"

  let cache_index = 0

  let value_of_identifier ctxt identifier =
    (*

       I/O, deserialization, and elaboration of contracts scripts
       are cached.

    *)
    contract_of_identifier identifier >>?= fun addr ->
    load_and_elaborate ctxt addr >>=? function
    | _, None ->
        (* [value_of_identifier ctxt k] is applied to identifiers stored
           in the cache. Only script-based contracts that have been
           executed are in the cache. Hence, [get_script] always
           succeeds for these identifiers if [ctxt] and the [cache] are
           properly synchronized by the shell. *)
        failwith "Script_cache: Inconsistent script cache."
    | _, Some (unparsed_script, ir_script, _) ->
        return (unparsed_script, ir_script)
end

module Cache = (val Cache.register_exn (module Client))

let find ctxt addr =
  let identifier = identifier_of_contract addr in
  Cache.find ctxt identifier >>=? function
  | Some (unparsed_script, ex_script) ->
      return (ctxt, identifier, Some (unparsed_script, ex_script))
  | None -> (
      load_and_elaborate ctxt addr >>=? function
      | ctxt, None -> return (ctxt, identifier, None)
      | ctxt, Some (unparsed_script, script_ir, size) ->
          let cached_value = (unparsed_script, script_ir) in
          Lwt.return
            ( Cache.update ctxt identifier (Some (cached_value, size))
            >>? fun ctxt ->
              ok (ctxt, identifier, Some (unparsed_script, script_ir)) ))

let update ctxt identifier updated_script approx_size =
  Cache.update ctxt identifier (Some (updated_script, approx_size))

let entries ctxt =
  Cache.list_identifiers ctxt
  |> List.map_e @@ fun (identifier, age) ->
     contract_of_identifier identifier >|? fun contract -> (contract, age)

let contract_rank ctxt addr =
  Cache.identifier_rank ctxt (identifier_of_contract addr)

let size = Cache.size

let size_limit = Cache.size_limit

let insert ctxt addr updated_script approx_size =
  let identifier = identifier_of_contract addr in
  Cache.update ctxt identifier (Some (updated_script, approx_size))
