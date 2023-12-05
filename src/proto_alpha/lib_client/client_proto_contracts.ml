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
    |> record_trace_eval (fun () -> error_of_fmt "bad contract notation")
    |> Lwt.return

  let to_source s =
    let open Lwt_result_syntax in
    return (Contract.to_b58check s)

  let name = "contract"
end

module Raw_contract_alias = Client_aliases.Alias (Contract_entity)

module Originated_contract_alias = struct
  let find cctxt s =
    let open Lwt_result_syntax in
    let* contract_opt = Raw_contract_alias.find_opt cctxt s in
    match contract_opt with
    | Some (Contract.Originated v) -> return v
    | Some (Implicit _) -> failwith "contract %s is an implicit account" s
    | None -> failwith "no contract named %s" s

  let of_literal s =
    let open Lwt_result_syntax in
    let*! contract_opt = Contract_entity.of_source s in
    match contract_opt with
    | Ok (Contract.Originated v) -> return v
    | Ok (Implicit _) -> failwith "contract %s is an implicit account" s
    | Error _ as err -> Lwt.return err

  let find_destination cctxt s =
    let open Lwt_result_syntax in
    match String.split ~limit:1 ':' s with
    | ["alias"; alias] -> find cctxt alias
    | ["text"; text] -> of_literal text
    | _ -> (
        let*! contract_hash_opt = of_literal s in
        match contract_hash_opt with
        | Ok _ as ok_v -> Lwt.return ok_v
        | Error k_errs -> (
            let*! contract_hash_opt = find cctxt s in
            Lwt.return
            @@
            match contract_hash_opt with
            | Ok _ as ok_v -> ok_v
            | Error c_errs -> Error (c_errs @ k_errs)))

  let destination_parameter () =
    Tezos_clic.parameter
      ~autocomplete:Raw_contract_alias.autocomplete
      find_destination

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      String.concat
        "\n"
        [
          desc;
          "Can be a literal or an alias (autodetected in order).\n\
           Use 'text:literal' or 'alias:name' to force.";
        ]
    in
    Tezos_clic.param ~name ~desc (destination_parameter ()) next

  let destination_arg ?(name = "dst") ?(doc = "destination contract") () =
    let doc =
      String.concat
        "\n"
        [
          doc;
          "Can be a literal or an alias (autodetected in order).\n\
           Use 'text:literal' or 'alias:name' to force.";
        ]
    in
    Tezos_clic.arg ~long:name ~doc ~placeholder:name (destination_parameter ())
end

module Contract_alias = struct
  let find cctxt s =
    let open Lwt_result_syntax in
    let* contract_opt = Raw_contract_alias.find_opt cctxt s in
    match contract_opt with
    | Some v -> return v
    | None -> (
        let* pkh_opt = Client_keys.Public_key_hash.find_opt cctxt s in
        match pkh_opt with
        | Some v -> return (Contract.Implicit v)
        | None -> failwith "no contract or key named %s" s)

  let find_key cctxt name =
    let open Lwt_result_syntax in
    let* v = Client_keys.Public_key_hash.find cctxt name in
    return (Contract.Implicit v)

  let rev_find cctxt (c : Contract.t) =
    let open Lwt_result_syntax in
    match c with
    | Implicit hash -> (
        let* str_opt = Client_keys.Public_key_hash.rev_find cctxt hash in
        match str_opt with
        | Some name -> return_some ("key:" ^ name)
        | None -> return_none)
    | Originated _ -> Raw_contract_alias.rev_find cctxt c

  let get_contract cctxt s =
    match String.split ~limit:1 ':' s with
    | ["key"; key] -> find_key cctxt key
    | _ -> find cctxt s

  let autocomplete cctxt =
    let open Lwt_result_syntax in
    let* keys = Client_keys.Public_key_hash.autocomplete cctxt in
    let* contracts = Raw_contract_alias.autocomplete cctxt in
    return (List.map (( ^ ) "key:") keys @ contracts)

  let alias_param ?(name = "name") ?(desc = "existing contract alias") next =
    let desc =
      desc ^ "\n"
      ^ "Can be a contract alias or a key alias (autodetected in order).\n\
         Use 'key:name' to force the later."
    in
    Tezos_clic.(param ~name ~desc (parameter ~autocomplete get_contract) next)

  let find_destination cctxt s =
    let open Lwt_result_syntax in
    match String.split ~limit:1 ':' s with
    | ["alias"; alias] -> find cctxt alias
    | ["key"; text] ->
        let* v = Client_keys.Public_key_hash.find cctxt text in
        return (Contract.Implicit v)
    | ["text"; text] -> Contract_entity.of_source text
    | _ -> (
        let*! contract_opt = Contract_entity.of_source s in
        match contract_opt with
        | Ok v -> return v
        | Error c_errs -> (
            let*! contract_opt = find cctxt s in
            match contract_opt with
            | Ok v -> return v
            | Error k_errs -> Lwt.return_error (c_errs @ k_errs)))

  let destination_parameter () =
    let open Lwt_result_syntax in
    Tezos_clic.parameter
      ~autocomplete:(fun cctxt ->
        let* list1 = autocomplete cctxt in
        let* list2 = Client_keys.Public_key_hash.autocomplete cctxt in
        return (list1 @ list2))
      find_destination

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      String.concat
        "\n"
        [
          desc;
          "Can be a literal, an alias, or a key (autodetected in order).\n\
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
          "Can be a literal, an alias, or a key (autodetected in order).\n\
           Use 'text:literal', 'alias:name', 'key:name' to force.";
        ]
    in
    Tezos_clic.arg ~long:name ~doc ~placeholder:name (destination_parameter ())

  let name cctxt contract =
    let open Lwt_result_syntax in
    let* str_opt = rev_find cctxt contract in
    match str_opt with
    | None -> return (Contract.to_b58check contract)
    | Some name -> return name
end

let list_contracts cctxt =
  let open Lwt_result_syntax in
  let* raw_contracts = Raw_contract_alias.load cctxt in
  let contracts = List.map (fun (n, v) -> ("", n, v)) raw_contracts in
  let* keys = Client_keys.Public_key_hash.load cctxt in
  (* List accounts (implicit contracts of identities) *)
  let* accounts =
    List.map_es
      (fun (n, v) ->
        let* mem = Raw_contract_alias.mem cctxt n in
        let p = if mem then "key:" else "" in
        let v' = Contract.Implicit v in
        return (p, n, v'))
      keys
  in
  return (contracts @ accounts)

let get_delegate cctxt ~chain ~block source =
  Alpha_services.Contract.delegate_opt cctxt (chain, block) source
