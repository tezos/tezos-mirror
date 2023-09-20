(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context

module Contract_entity = struct
  include Contract (* t, Compare, encoding *)

  let of_source s =
    Contract.of_b58check s |> Environment.wrap_tzresult
    |> record_trace (error_of_fmt "bad contract notation")
    |> Lwt.return

  let to_source s = return (Contract.to_b58check s)

  let name = "contract"
end

module Raw_contract_alias = Client_aliases.Alias (Contract_entity)

module Contract_alias = struct
  let find cctxt s =
    Raw_contract_alias.find_opt cctxt s >>=? function
    | Some v -> return (s, v)
    | None -> (
        Client_keys_v0.Public_key_hash.find_opt cctxt s >>=? function
        | Some v -> return (s, Contract.implicit_contract v)
        | None -> failwith "no contract or key named %s" s)

  let find_key cctxt name =
    Client_keys_v0.Public_key_hash.find cctxt name >>=? fun v ->
    return (name, Contract.implicit_contract v)

  let rev_find cctxt c =
    match Contract.is_implicit c with
    | Some hash -> (
        Client_keys_v0.Public_key_hash.rev_find cctxt hash >>=? function
        | Some name -> return_some ("key:" ^ name)
        | None -> return_none)
    | None -> Raw_contract_alias.rev_find cctxt c

  let get_contract cctxt s =
    match String.split ~limit:1 ':' s with
    | ["key"; key] -> find_key cctxt key
    | _ -> find cctxt s

  let autocomplete cctxt =
    Client_keys_v0.Public_key_hash.autocomplete cctxt >>=? fun keys ->
    Raw_contract_alias.autocomplete cctxt >>=? fun contracts ->
    return (List.map (( ^ ) "key:") keys @ contracts)

  let alias_param ?(name = "name") ?(desc = "existing contract alias") next =
    let desc =
      desc ^ "\n"
      ^ "Can be a contract alias or a key alias (autodetected in order).\n\
         Use 'key:name' to force the later."
    in
    Tezos_clic.(
      param
        ~name
        ~desc
        (parameter ~autocomplete (fun cctxt p -> get_contract cctxt p))
        next)

  let find_destination cctxt s =
    match String.split ~limit:1 ':' s with
    | ["alias"; alias] -> find cctxt alias
    | ["key"; text] ->
        Client_keys_v0.Public_key_hash.find cctxt text >>=? fun v ->
        return (s, Contract.implicit_contract v)
    | _ -> (
        find cctxt s >>= function
        | Ok v -> return v
        | Error k_errs -> (
            Contract_entity.of_source s >>= function
            | Ok v -> return (s, v)
            | Error c_errs -> Lwt.return_error (k_errs @ c_errs)))

  let destination_parameter () =
    Tezos_clic.parameter
      ~autocomplete:(fun cctxt ->
        autocomplete cctxt >>=? fun list1 ->
        Client_keys_v0.Public_key_hash.autocomplete cctxt >>=? fun list2 ->
        return (list1 @ list2))
      find_destination

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      String.concat
        "\n"
        [
          desc;
          "Can be an alias, a key, or a literal (autodetected in order).\n\
           Use 'text:literal', 'alias:name', 'key:name' to force.";
        ]
    in
    Tezos_clic.param ~name ~desc (destination_parameter ()) next

  let destination_arg ?(name = "dst") ?(doc = "destination contract") () =
    let doc =
      String.concat
        "\n"
        [
          doc;
          "Can be an alias, a key, or a literal (autodetected in order).\n\
           Use 'text:literal', 'alias:name', 'key:name' to force.";
        ]
    in
    Tezos_clic.arg ~long:name ~doc ~placeholder:name (destination_parameter ())

  let name cctxt contract =
    rev_find cctxt contract >>=? function
    | None -> return (Contract.to_b58check contract)
    | Some name -> return name
end

let list_contracts cctxt =
  Raw_contract_alias.load cctxt >>=? fun raw_contracts ->
  List.map_s (fun (n, v) -> Lwt.return ("", n, v)) raw_contracts
  >>= fun contracts ->
  Client_keys_v0.Public_key_hash.load cctxt >>=? fun keys ->
  (* List accounts (implicit contracts of identities) *)
  List.map_es
    (fun (n, v) ->
      Raw_contract_alias.mem cctxt n >>=? fun mem ->
      let p = if mem then "key:" else "" in
      let v' = Contract.implicit_contract v in
      return (p, n, v'))
    keys
  >>=? fun accounts -> return (contracts @ accounts)

let get_delegate cctxt ~chain ~block source =
  Alpha_services.Contract.delegate_opt cctxt (chain, block) source
