(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let custom_root =
  (RPC_path.(open_root / "context" / "contracts")
    : RPC_context.t RPC_path.context)

let big_map_root =
  (RPC_path.(open_root / "context" / "big_maps")
    : RPC_context.t RPC_path.context)

type info = {
  balance : Tez.t;
  delegate : public_key_hash option;
  counter : counter option;
  script : Script.t option;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun {balance; delegate; script; counter} ->
      (balance, delegate, script, counter))
    (fun (balance, delegate, script, counter) ->
      {balance; delegate; script; counter})
  @@ obj4
       (req "balance" Tez.encoding)
       (opt "delegate" Signature.Public_key_hash.encoding)
       (opt "script" Script.encoding)
       (opt "counter" n)

module S = struct
  open Data_encoding

  let balance =
    RPC_service.get_service
      ~description:
        "Access the spendable balance of a contract, excluding frozen bonds."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "balance")

  let frozen_bonds =
    RPC_service.get_service
      ~description:"Access the frozen bonds of a contract."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "frozen_bonds")

  let balance_and_frozen_bonds =
    RPC_service.get_service
      ~description:
        "Access the sum of the spendable balance and frozen bonds of a \
         contract. This sum is part of the contract's stake, and it is exactly \
         the contract's stake if the contract is not a delegate."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "balance_and_frozen_bonds")

  let manager_key =
    RPC_service.get_service
      ~description:"Access the manager of a contract."
      ~query:RPC_query.empty
      ~output:(option Signature.Public_key.encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "manager_key")

  let delegate =
    RPC_service.get_service
      ~description:"Access the delegate of a contract, if any."
      ~query:RPC_query.empty
      ~output:Signature.Public_key_hash.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "delegate")

  let counter =
    RPC_service.get_service
      ~description:"Access the counter of a contract, if any."
      ~query:RPC_query.empty
      ~output:z
      RPC_path.(custom_root /: Contract.rpc_arg / "counter")

  let script =
    RPC_service.get_service
      ~description:"Access the code and data of the contract."
      ~query:RPC_query.empty
      ~output:Script.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "script")

  let storage =
    RPC_service.get_service
      ~description:"Access the data of the contract."
      ~query:RPC_query.empty
      ~output:Script.expr_encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "storage")

  let entrypoint_type =
    RPC_service.get_service
      ~description:"Return the type of the given entrypoint of the contract"
      ~query:RPC_query.empty
      ~output:Script.expr_encoding
      RPC_path.(
        custom_root /: Contract.rpc_arg / "entrypoints" /: Entrypoint.rpc_arg)

  let list_entrypoints =
    RPC_service.get_service
      ~description:"Return the list of entrypoints of the contract"
      ~query:RPC_query.empty
      ~output:
        (obj2
           (dft
              "unreachable"
              (Data_encoding.list
                 (obj1
                    (req
                       "path"
                       (Data_encoding.list
                          Michelson_v1_primitives.prim_encoding))))
              [])
           (req "entrypoints" (assoc Script.expr_encoding)))
      RPC_path.(custom_root /: Contract.rpc_arg / "entrypoints")

  let contract_big_map_get_opt =
    RPC_service.post_service
      ~description:
        "Access the value associated with a key in a big map of the contract \
         (deprecated)."
      ~query:RPC_query.empty
      ~input:
        (obj2
           (req "key" Script.expr_encoding)
           (req "type" Script.expr_encoding))
      ~output:(option Script.expr_encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "big_map_get")

  let big_map_get =
    RPC_service.get_service
      ~description:"Access the value associated with a key in a big map."
      ~query:RPC_query.empty
      ~output:Script.expr_encoding
      RPC_path.(big_map_root /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg)

  type big_map_get_all_query = {offset : int option; length : int option}

  let rpc_arg_uint : int RPC_arg.t =
    let int_of_string s =
      int_of_string_opt s
      |> Option.to_result
           ~none:(Format.sprintf "Cannot parse integer value %s" s)
      >>? fun i ->
      if Compare.Int.(i < 0) then
        Error (Format.sprintf "Negative integer: %d" i)
      else Ok i
    in
    RPC_arg.make
      ~name:"uint"
      ~descr:"A non-negative integer (greater than or equal to 0)."
      ~destruct:int_of_string
      ~construct:string_of_int
      ()

  let big_map_get_all_query : big_map_get_all_query RPC_query.t =
    let open RPC_query in
    query (fun offset length -> {offset; length})
    |+ opt_field
         ~descr:
           "Skip the first [offset] values. Useful in combination with \
            [length] for pagination."
         "offset"
         rpc_arg_uint
         (fun t -> t.offset)
    |+ opt_field
         ~descr:
           "Only retrieve [length] values. Useful in combination with [offset] \
            for pagination."
         "length"
         rpc_arg_uint
         (fun t -> t.length)
    |> seal

  let big_map_get_all =
    RPC_service.get_service
      ~description:
        "Get the (optionally paginated) list of values in a big map. Order of \
         values is unspecified, but is guaranteed to be consistent."
      ~query:big_map_get_all_query
      ~output:(list Script.expr_encoding)
      RPC_path.(big_map_root /: Big_map.Id.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:"Access the complete status of a contract."
      ~query:RPC_query.empty
      ~output:info_encoding
      RPC_path.(custom_root /: Contract.rpc_arg)

  let list =
    RPC_service.get_service
      ~description:
        "All existing contracts (including non-empty default contracts)."
      ~query:RPC_query.empty
      ~output:(list Contract.encoding)
      custom_root

  module Sapling = struct
    (*
      Sapling: these RPCs are like Sapling RPCs (sapling_services.ml)
      specialized for contracts containing a single sapling state.
    *)

    let single_sapling_get_id ctxt contract_id =
      Contract.get_script ctxt contract_id >>=? fun (ctxt, script) ->
      match script with
      | None -> return (None, ctxt)
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.parse_script
            ctxt
            ~legacy:true
            ~allow_forged_in_storage:true
            script
          >|= fun tzresult ->
          tzresult >>? fun (Ex_script (Script script), ctxt) ->
          Script_ir_translator.get_single_sapling_state
            ctxt
            script.storage_type
            script.storage

    let make_service
        Sapling_services.S.Args.{name; description; query; output; f} =
      let name = "single_sapling_" ^ name in
      let path = RPC_path.(custom_root /: Contract.rpc_arg / name) in
      let service = RPC_service.get_service ~description ~query ~output path in
      ( service,
        fun ctxt contract_id q () ->
          single_sapling_get_id ctxt contract_id >>=? fun (sapling_id, ctxt) ->
          Option.map_es (fun sapling_id -> f ctxt sapling_id q) sapling_id )

    let get_diff = make_service Sapling_services.S.Args.get_diff

    let register () =
      let reg chunked (service, f) =
        Services_registration.opt_register1 ~chunked service f
      in
      reg false get_diff

    let mk_call1 (service, _f) ctxt block id q =
      RPC_context.make_call1 service ctxt block id q ()
  end
end

let[@coq_axiom_with_reason "gadt"] register () =
  let open Services_registration in
  register0 ~chunked:true S.list (fun ctxt () () -> Contract.list ctxt >|= ok) ;
  let register_field ~chunked s f =
    opt_register1 ~chunked s (fun ctxt contract () () ->
        Contract.exists ctxt contract >>=? function
        | true -> f ctxt contract >|=? Option.some
        | false -> return_none)
  in
  let register_opt_field ~chunked s f =
    opt_register1 ~chunked s (fun ctxt contract () () ->
        Contract.exists ctxt contract >>=? function
        | true -> f ctxt contract
        | false -> return_none)
  in
  let do_big_map_get ctxt id key =
    let open Script_ir_translator in
    let ctxt = Gas.set_unlimited ctxt in
    Big_map.exists ctxt id >>=? fun (ctxt, types) ->
    match types with
    | None -> return_none
    | Some (_, value_type) -> (
        parse_big_map_value_ty ctxt ~legacy:true (Micheline.root value_type)
        >>?= fun (Ex_ty value_type, ctxt) ->
        Big_map.get_opt ctxt id key >>=? fun (_ctxt, value) ->
        match value with
        | None -> return_none
        | Some value ->
            parse_data
              ctxt
              ~legacy:true
              ~allow_forged:true
              value_type
              (Micheline.root value)
            >>=? fun (value, ctxt) ->
            unparse_data ctxt Readable value_type value
            >|=? fun (value, _ctxt) -> Some (Micheline.strip_locations value))
  in
  let do_big_map_get_all ?offset ?length ctxt id =
    let open Script_ir_translator in
    let ctxt = Gas.set_unlimited ctxt in
    Big_map.exists ctxt id >>=? fun (ctxt, types) ->
    match types with
    | None -> raise Not_found
    | Some (_, value_type) ->
        parse_big_map_value_ty ctxt ~legacy:true (Micheline.root value_type)
        >>?= fun (Ex_ty value_type, ctxt) ->
        Big_map.list_values ?offset ?length ctxt id >>=? fun (ctxt, values) ->
        List.fold_left_s
          (fun acc value ->
            acc >>?= fun (ctxt, rev_values) ->
            parse_data
              ctxt
              ~legacy:true
              ~allow_forged:true
              value_type
              (Micheline.root value)
            >>=? fun (value, ctxt) ->
            unparse_data ctxt Readable value_type value
            >|=? fun (value, ctxt) ->
            (ctxt, Micheline.strip_locations value :: rev_values))
          (Ok (ctxt, []))
          values
        >|=? fun (_ctxt, rev_values) -> List.rev rev_values
  in
  register_field ~chunked:false S.balance Contract.get_balance ;
  register_field ~chunked:false S.frozen_bonds Contract.get_frozen_bonds ;
  register_field
    ~chunked:false
    S.balance_and_frozen_bonds
    Contract.get_balance_and_frozen_bonds ;
  opt_register1 ~chunked:false S.manager_key (fun ctxt contract () () ->
      match Contract.is_implicit contract with
      | None -> return_none
      | Some mgr -> (
          Contract.is_manager_key_revealed ctxt mgr >>=? function
          | false -> return_some None
          | true ->
              Contract.get_manager_key ctxt mgr >|=? fun key -> Some (Some key))) ;
  register_opt_field ~chunked:false S.delegate Delegate.find ;
  opt_register1 ~chunked:false S.counter (fun ctxt contract () () ->
      match Contract.is_implicit contract with
      | None -> return_none
      | Some mgr ->
          Contract.get_counter ctxt mgr >|=? fun counter -> Some counter) ;
  register_opt_field ~chunked:true S.script (fun c v ->
      Contract.get_script c v >|=? fun (_, v) -> v) ;
  register_opt_field ~chunked:true S.storage (fun ctxt contract ->
      Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
      match script with
      | None -> return_none
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
          >>=? fun (ex_script, ctxt) ->
          unparse_script ctxt Readable ex_script >>=? fun (script, ctxt) ->
          Script.force_decode_in_context
            ~consume_deserialization_gas:When_needed
            ctxt
            script.storage
          >>?= fun (storage, _ctxt) -> return_some storage) ;
  opt_register2 ~chunked:true S.entrypoint_type (fun ctxt v entrypoint () () ->
      Contract.get_script_code ctxt v >>=? fun (_, expr) ->
      match expr with
      | None -> return_none
      | Some expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = true in
          let open Script_ir_translator in
          Script.force_decode_in_context
            ~consume_deserialization_gas:When_needed
            ctxt
            expr
          >>?= fun (expr, _) ->
          parse_toplevel ctxt ~legacy expr >>=? fun ({arg_type; _}, ctxt) ->
          Lwt.return
            ( parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type
            >>? fun (Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _)
              ->
              Gas_monad.run ctxt
              @@ Script_ir_translator.find_entrypoint
                   ~error_details:Informative
                   arg_type
                   entrypoints
                   entrypoint
              >>? fun (r, ctxt) ->
              r |> function
              | Ok (Ex_ty_cstr {ty; _}) ->
                  unparse_ty ~loc:() ctxt ty >|? fun (ty_node, _) ->
                  Some (Micheline.strip_locations ty_node)
              | Error _ -> Result.return_none )) ;
  opt_register1 ~chunked:true S.list_entrypoints (fun ctxt v () () ->
      Contract.get_script_code ctxt v >>=? fun (_, expr) ->
      match expr with
      | None -> return_none
      | Some expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = true in
          let open Script_ir_translator in
          Script.force_decode_in_context
            ~consume_deserialization_gas:When_needed
            ctxt
            expr
          >>?= fun (expr, _) ->
          parse_toplevel ctxt ~legacy expr >>=? fun ({arg_type; _}, ctxt) ->
          Lwt.return
            ( parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type
            >|? fun (Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _)
              ->
              let (unreachable_entrypoint, map) =
                Script_ir_translator.list_entrypoints arg_type entrypoints
              in
              Some
                ( unreachable_entrypoint,
                  Entrypoint.Map.fold
                    (fun entry (_, ty) acc ->
                      (Entrypoint.to_string entry, Micheline.strip_locations ty)
                      :: acc)
                    map
                    [] ) )) ;
  opt_register1
    ~chunked:true
    S.contract_big_map_get_opt
    (fun ctxt contract () (key, key_type) ->
      Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
      let key_type_node = Micheline.root key_type in
      Script_ir_translator.parse_comparable_ty ctxt key_type_node
      >>?= fun (Ex_comparable_ty key_type, ctxt) ->
      Script_ir_translator.parse_comparable_data
        ctxt
        key_type
        (Micheline.root key)
      >>=? fun (key, ctxt) ->
      Script_ir_translator.hash_comparable_data ctxt key_type key
      >>=? fun (key, ctxt) ->
      match script with
      | None -> return_none
      | Some script -> (
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
          >>=? fun (Ex_script (Script script), ctxt) ->
          Script_ir_translator.collect_lazy_storage
            ctxt
            script.storage_type
            script.storage
          >>?= fun (ids, _ctxt) ->
          match Script_ir_translator.list_of_big_map_ids ids with
          | [] | _ :: _ :: _ -> return_some None
          | [id] -> do_big_map_get ctxt id key >|=? Option.some)) ;
  opt_register2 ~chunked:true S.big_map_get (fun ctxt id key () () ->
      do_big_map_get ctxt id key) ;
  register1 ~chunked:true S.big_map_get_all (fun ctxt id {offset; length} () ->
      do_big_map_get_all ?offset ?length ctxt id) ;
  register_field ~chunked:false S.info (fun ctxt contract ->
      Contract.get_balance ctxt contract >>=? fun balance ->
      Delegate.find ctxt contract >>=? fun delegate ->
      (match Contract.is_implicit contract with
      | Some manager ->
          Contract.get_counter ctxt manager >>=? fun counter ->
          return_some counter
      | None -> return_none)
      >>=? fun counter ->
      Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
      (match script with
      | None -> return (None, ctxt)
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
          >>=? fun (ex_script, ctxt) ->
          unparse_script ctxt Readable ex_script >|=? fun (script, ctxt) ->
          (Some script, ctxt))
      >|=? fun (script, _ctxt) -> {balance; delegate; script; counter}) ;
  S.Sapling.register ()

let list ctxt block = RPC_context.make_call0 S.list ctxt block () ()

let info ctxt block contract =
  RPC_context.make_call1 S.info ctxt block contract () ()

let balance ctxt block contract =
  RPC_context.make_call1 S.balance ctxt block contract () ()

let frozen_bonds ctxt block contract =
  RPC_context.make_call1 S.frozen_bonds ctxt block contract () ()

let balance_and_frozen_bonds ctxt block contract =
  RPC_context.make_call1 S.balance_and_frozen_bonds ctxt block contract () ()

let manager_key ctxt block mgr =
  RPC_context.make_call1
    S.manager_key
    ctxt
    block
    (Contract.implicit_contract mgr)
    ()
    ()

let delegate ctxt block contract =
  RPC_context.make_call1 S.delegate ctxt block contract () ()

let delegate_opt ctxt block contract =
  RPC_context.make_opt_call1 S.delegate ctxt block contract () ()

let counter ctxt block mgr =
  RPC_context.make_call1
    S.counter
    ctxt
    block
    (Contract.implicit_contract mgr)
    ()
    ()

let script ctxt block contract =
  RPC_context.make_call1 S.script ctxt block contract () ()

let script_opt ctxt block contract =
  RPC_context.make_opt_call1 S.script ctxt block contract () ()

let storage ctxt block contract =
  RPC_context.make_call1 S.storage ctxt block contract () ()

let entrypoint_type ctxt block contract entrypoint =
  RPC_context.make_call2 S.entrypoint_type ctxt block contract entrypoint () ()

let list_entrypoints ctxt block contract =
  RPC_context.make_call1 S.list_entrypoints ctxt block contract () ()

let storage_opt ctxt block contract =
  RPC_context.make_opt_call1 S.storage ctxt block contract () ()

let big_map_get ctxt block id key =
  RPC_context.make_call2 S.big_map_get ctxt block id key () ()

let contract_big_map_get_opt ctxt block contract key =
  RPC_context.make_call1 S.contract_big_map_get_opt ctxt block contract () key

let single_sapling_get_diff ctxt block id ?offset_commitment ?offset_nullifier
    () =
  S.Sapling.(mk_call1 get_diff)
    ctxt
    block
    id
    Sapling_services.{offset_commitment; offset_nullifier}
