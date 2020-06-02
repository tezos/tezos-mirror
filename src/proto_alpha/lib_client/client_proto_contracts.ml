(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
  type t = Contract.t

  let encoding = Contract.encoding

  let of_source s =
    match Contract.of_b58check s with
    | Error _ as err ->
        Lwt.return (Environment.wrap_error err)
        |> trace (failure "bad contract notation")
    | Ok s ->
        return s

  let to_source s = return (Contract.to_b58check s)

  let name = "contract"
end

module Raw_contract_alias = Client_aliases.Alias (Contract_entity)

module Contract_alias = struct
  let find cctxt s =
    Raw_contract_alias.find_opt cctxt s
    >>=? function
    | Some v ->
        return (s, v)
    | None -> (
        Client_keys.Public_key_hash.find_opt cctxt s
        >>=? function
        | Some v ->
            return (s, Contract.implicit_contract v)
        | None ->
            failwith "no contract or key named %s" s )

  let find_key cctxt name =
    Client_keys.Public_key_hash.find cctxt name
    >>=? fun v -> return (name, Contract.implicit_contract v)

  let rev_find cctxt c =
    match Contract.is_implicit c with
    | Some hash -> (
        Client_keys.Public_key_hash.rev_find cctxt hash
        >>=? function
        | Some name -> return_some ("key:" ^ name) | None -> return_none )
    | None ->
        Raw_contract_alias.rev_find cctxt c

  let get_contract cctxt s =
    match String.split ~limit:1 ':' s with
    | ["key"; key] ->
        find_key cctxt key
    | _ ->
        find cctxt s

  let autocomplete cctxt =
    Client_keys.Public_key_hash.autocomplete cctxt
    >>=? fun keys ->
    Raw_contract_alias.autocomplete cctxt
    >>=? fun contracts -> return (List.map (( ^ ) "key:") keys @ contracts)

  let alias_param ?(name = "name") ?(desc = "existing contract alias") next =
    let desc =
      desc ^ "\n"
      ^ "Can be a contract alias or a key alias (autodetected in order).\n\
         Use 'key:name' to force the later."
    in
    Clic.(
      param
        ~name
        ~desc
        (parameter ~autocomplete (fun cctxt p -> get_contract cctxt p))
        next)

  let destination_parameter () =
    Clic.parameter
      ~autocomplete:(fun cctxt ->
        autocomplete cctxt
        >>=? fun list1 ->
        Client_keys.Public_key_hash.autocomplete cctxt
        >>=? fun list2 -> return (list1 @ list2))
      (fun cctxt s ->
        match String.split ~limit:1 ':' s with
        | ["alias"; alias] ->
            find cctxt alias
        | ["key"; text] ->
            Client_keys.Public_key_hash.find cctxt text
            >>=? fun v -> return (s, Contract.implicit_contract v)
        | _ -> (
            find cctxt s
            >>= function
            | Ok v ->
                return v
            | Error k_errs -> (
                Contract_entity.of_source s
                >>= function
                | Ok v ->
                    return (s, v)
                | Error c_errs ->
                    Lwt.return_error (k_errs @ c_errs) ) ))

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      String.concat
        "\n"
        [ desc;
          "Can be an alias, a key, or a literal (autodetected in order).\n\
           Use 'text:literal', 'alias:name', 'key:name' to force." ]
    in
    Clic.param ~name ~desc (destination_parameter ()) next

  let destination_arg ?(name = "dst") ?(doc = "destination contract") () =
    let doc =
      String.concat
        "\n"
        [ doc;
          "Can be an alias, a key, or a literal (autodetected in order).\n\
           Use 'text:literal', 'alias:name', 'key:name' to force." ]
    in
    Clic.arg ~long:name ~doc ~placeholder:name (destination_parameter ())

  let name cctxt contract =
    rev_find cctxt contract
    >>=? function
    | None -> return (Contract.to_b58check contract) | Some name -> return name
end

module Baker_alias = struct
  open Clic
  open Client_context

  type t = Baker_hash.t

  let entity_name = "baker contract"

  let of_source s =
    Contract_entity.of_source s
    >>=? fun c ->
    match Contract.is_baker c with
    | Some s ->
        return s
    | None ->
        failwith "bad baker notation"

  let load wallet =
    Raw_contract_alias.load wallet
    >>=? fun contracts ->
    return
    @@ List.filter_map
         (fun (alias, contract) ->
           let open Option in
           Contract.is_baker contract >>| fun baker -> (alias, baker))
         contracts

  let autocomplete wallet =
    load wallet
    >>= function
    | Error _ -> return_nil | Ok list -> return (List.map fst list)

  let find (wallet : #wallet) name =
    load wallet
    >>=? fun list ->
    try return (List.assoc name list)
    with Not_found -> failwith "no %s alias named %s" entity_name name

  let rev_find wallet baker =
    Raw_contract_alias.rev_find wallet @@ Contract.baker_contract baker

  let name wallet baker =
    Raw_contract_alias.name wallet @@ Contract.baker_contract baker

  let to_source baker =
    Raw_contract_alias.to_source @@ Contract.baker_contract baker

  let alias_parameter () =
    parameter ~autocomplete (fun cctxt s ->
        find cctxt s >>=? fun v -> return (s, v))

  let alias_param ?(name = "name")
      ?(desc = "existing " ^ entity_name ^ " alias") next =
    param ~name ~desc (alias_parameter ()) next

  let parse_source_string cctxt s =
    match String.split ~limit:1 ':' s with
    | ["alias"; alias] ->
        find cctxt alias
    | ["text"; text] ->
        of_source text
    | ["file"; path] ->
        cctxt#read_file path >>=? of_source
    | _ -> (
        find cctxt s
        >>= function
        | Ok v ->
            return v
        | Error a_errs -> (
            cctxt#read_file s >>=? of_source
            >>= function
            | Ok v ->
                return v
            | Error r_errs -> (
                of_source s
                >>= function
                | Ok v ->
                    return v
                | Error s_errs ->
                    let all_errs = List.flatten [a_errs; r_errs; s_errs] in
                    Lwt.return_error all_errs ) ) )

  let source_param ?(name = "bkr") ?(desc = "source " ^ entity_name) next =
    let desc =
      Format.asprintf
        "%s\n\
         Can be a %s name, a file or a raw %s literal. If the parameter is \
         not the name of an existing %s, the client will look for a file \
         containing a %s, and if it does not exist, the argument will be read \
         as a raw %s.\n\
         Use 'alias:name', 'file:path' or 'text:literal' to disable autodetect."
        desc
        entity_name
        entity_name
        entity_name
        entity_name
        entity_name
    in
    param ~name ~desc (parameter parse_source_string) next

  let source_arg ?(long = "source " ^ entity_name) ?(placeholder = "bkr")
      ?(doc = "") () =
    let doc =
      Format.asprintf
        "%s\n\
         Can be a %s name, a file or a raw %s literal. If the parameter is \
         not the name of an existing %s, the client will look for a file \
         containing a %s, and if it does not exist, the argument will be read \
         as a raw %s.\n\
         Use 'alias:name', 'file:path' or 'text:literal' to disable autodetect."
        doc
        entity_name
        entity_name
        entity_name
        entity_name
        entity_name
    in
    arg ~long ~placeholder ~doc (parameter parse_source_string)
end

module Baker_or_pkh_alias = struct
  open Clic
  open Client_context

  type t = Contract.t

  let entity_name = "baker contract"

  let of_source s =
    Contract_entity.of_source s
    >>=? fun c ->
    match Contract.is_baker c with
    | Some _ ->
        return c
    | None -> (
        Client_keys.Public_key_hash.of_source s
        >>= function
        | Ok pkh ->
            return (Contract.implicit_contract pkh)
        | _ ->
            failwith "bad baker notation" )

  let load wallet =
    Client_keys.Public_key_hash.load wallet
    >>=? fun pkh_contracts ->
    Raw_contract_alias.load wallet
    >>=? fun baker_contracts ->
    let pkh_contracts =
      List.map
        (fun (alias, pkh) -> (alias, Contract.implicit_contract pkh))
        pkh_contracts
    in
    let baker_contracts =
      List.filter_map
        (fun (alias, contract) ->
          let open Option in
          Contract.is_baker contract >>| fun _ -> (alias, contract))
        baker_contracts
    in
    return @@ baker_contracts @ pkh_contracts

  let autocomplete wallet =
    load wallet
    >>= function
    | Error _ -> return_nil | Ok list -> return (List.map fst list)

  let find (wallet : #wallet) name =
    load wallet
    >>=? fun list ->
    try return (List.assoc name list)
    with Not_found -> failwith "no %s alias named %s" entity_name name

  let alias_parameter () =
    parameter ~autocomplete (fun cctxt s ->
        find cctxt s >>=? fun v -> return (s, v))

  let alias_param ?(name = "name")
      ?(desc = "existing " ^ entity_name ^ " alias") next =
    param ~name ~desc (alias_parameter ()) next

  let parse_source_string cctxt s =
    match String.split ~limit:1 ':' s with
    | ["alias"; alias] ->
        find cctxt alias
    | ["text"; text] ->
        of_source text
    | ["file"; path] ->
        cctxt#read_file path >>=? of_source
    | _ -> (
        find cctxt s
        >>= function
        | Ok v ->
            return v
        | Error a_errs -> (
            cctxt#read_file s >>=? of_source
            >>= function
            | Ok v ->
                return v
            | Error r_errs -> (
                of_source s
                >>= function
                | Ok v ->
                    return v
                | Error s_errs ->
                    let all_errs = List.flatten [a_errs; r_errs; s_errs] in
                    Lwt.return_error all_errs ) ) )

  let source_param ?(name = "bkr") ?(desc = "source " ^ entity_name) next =
    let desc =
      Format.asprintf
        "%s\n\
         Can be a %s name, a file or a raw %s literal. If the parameter is \
         not the name of an existing %s, the client will look for a file \
         containing a %s, and if it does not exist, the argument will be read \
         as a raw %s.\n\
         Use 'alias:name', 'file:path' or 'text:literal' to disable autodetect."
        desc
        entity_name
        entity_name
        entity_name
        entity_name
        entity_name
    in
    param ~name ~desc (parameter parse_source_string) next

  let source_arg ?(long = "source " ^ entity_name) ?(placeholder = "src")
      ?(doc = "") () =
    let doc =
      Format.asprintf
        "%s\n\
         Can be a %s name, a file or a raw %s literal. If the parameter is \
         not the name of an existing %s, the client will look for a file \
         containing a %s, and if it does not exist, the argument will be read \
         as a raw %s.\n\
         Use 'alias:name', 'file:path' or 'text:literal' to disable autodetect."
        doc
        entity_name
        entity_name
        entity_name
        entity_name
        entity_name
    in
    arg ~long ~placeholder ~doc (parameter parse_source_string)
end

let list_contracts cctxt =
  Raw_contract_alias.load cctxt
  >>=? fun raw_contracts ->
  Lwt_list.map_s (fun (n, v) -> Lwt.return ("", n, v)) raw_contracts
  >>= fun contracts ->
  Client_keys.Public_key_hash.load cctxt
  >>=? fun keys ->
  (* List accounts (implicit contracts of identities) *)
  map_s
    (fun (n, v) ->
      Raw_contract_alias.mem cctxt n
      >>=? fun mem ->
      let p = if mem then "key:" else "" in
      let v' = Contract.implicit_contract v in
      return (p, n, v'))
    keys
  >>=? fun accounts -> return (contracts @ accounts)

let get_delegate cctxt ~chain ~block source =
  Alpha_services.Contract.delegate_opt cctxt (chain, block) source

let get_baker_consensus_key ?level ?offset cctxt ~chain baker =
  Alpha_services.Baker.consensus_key
    ?level
    ?offset
    cctxt
    (chain, `Head 0)
    baker
  >>=? fun pk ->
  let pkh = Signature.Public_key.hash pk in
  Client_keys.get_key cctxt pkh
  >>=? fun (name, _, sk_uri) ->
  Tezos_signer_backends.Encrypted.decrypt_list cctxt [name]
  >>=? fun () -> return (name, pkh, pk, sk_uri)

let baker_of_contract cctxt contract =
  let not_a_baker () =
    failwith
      "The provided contract %a is not baker or an active consensus key of a \
       baker"
      Contract.pp
      contract
  in
  match Alpha_context.Contract.is_baker contract with
  | None -> (
    match Alpha_context.Contract.is_implicit contract with
    | Some pkh -> (
        Alpha_services.Helpers.is_baker_consensus_key_opt
          cctxt
          (cctxt#chain, cctxt#block)
          pkh
        >>=? function
        | Some baker_hash -> return baker_hash | None -> not_a_baker () )
    | None ->
        not_a_baker () )
  | Some baker_hash ->
      return baker_hash
