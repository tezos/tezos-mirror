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
  ( RPC_path.(open_root / "context" / "contracts")
    : RPC_context.t RPC_path.context )

let big_map_root =
  ( RPC_path.(open_root / "context" / "big_maps")
    : RPC_context.t RPC_path.context )

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
      ~description:"Access the balance of a contract."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "balance")

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
        custom_root /: Contract.rpc_arg / "entrypoints" /: RPC_arg.string)

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
      Contract.get_script ctxt contract_id
      >>=? fun (ctxt, script) ->
      match script with
      | None ->
          raise Not_found
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.parse_script
            ctxt
            ~legacy:true
            ~allow_forged_in_storage:true
            script
          >|= fun tzresult ->
          tzresult
          >>? fun (Ex_script script, ctxt) ->
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
          single_sapling_get_id ctxt contract_id
          >>=? fun (sapling_id, ctxt) -> f ctxt sapling_id q )

    let get_diff = make_service Sapling_services.S.Args.get_diff

    let register () =
      let reg (service, f) = Services_registration.register1 service f in
      reg get_diff

    let mk_call1 (service, _f) ctxt block id q =
      RPC_context.make_call1 service ctxt block id q ()
  end
end

let register () =
  let open Services_registration in
  register0 S.list (fun ctxt () () -> Contract.list ctxt >|= ok) ;
  let register_field s f =
    register1 s (fun ctxt contract () () ->
        Contract.exists ctxt contract
        >>=? function true -> f ctxt contract | false -> raise Not_found)
  in
  let register_opt_field s f =
    register_field s (fun ctxt a1 ->
        f ctxt a1 >|=? function None -> raise Not_found | Some v -> v)
  in
  let do_big_map_get ctxt id key =
    let open Script_ir_translator in
    let ctxt = Gas.set_unlimited ctxt in
    Big_map.exists ctxt id
    >>=? fun (ctxt, types) ->
    match types with
    | None ->
        raise Not_found
    | Some (_, value_type) -> (
        parse_big_map_value_ty ctxt ~legacy:true (Micheline.root value_type)
        >>?= fun (Ex_ty value_type, ctxt) ->
        Big_map.get_opt ctxt id key
        >>=? fun (_ctxt, value) ->
        match value with
        | None ->
            raise Not_found
        | Some value ->
            parse_data
              ctxt
              ~legacy:true
              ~allow_forged:true
              value_type
              (Micheline.root value)
            >>=? fun (value, ctxt) ->
            unparse_data ctxt Readable value_type value
            >|=? fun (value, _ctxt) -> Micheline.strip_locations value )
  in
  register_field S.balance Contract.get_balance ;
  register1 S.manager_key (fun ctxt contract () () ->
      match Contract.is_implicit contract with
      | None ->
          raise Not_found
      | Some mgr -> (
          Contract.is_manager_key_revealed ctxt mgr
          >>=? function
          | false ->
              return_none
          | true ->
              Contract.get_manager_key ctxt mgr >>=? return_some )) ;
  register_opt_field S.delegate Delegate.get ;
  register1 S.counter (fun ctxt contract () () ->
      match Contract.is_implicit contract with
      | None ->
          raise Not_found
      | Some mgr ->
          Contract.get_counter ctxt mgr) ;
  register_opt_field S.script (fun c v ->
      Contract.get_script c v >|=? fun (_, v) -> v) ;
  register_opt_field S.storage (fun ctxt contract ->
      Contract.get_script ctxt contract
      >>=? fun (ctxt, script) ->
      match script with
      | None ->
          return_none
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
          >>=? fun (Ex_script script, ctxt) ->
          unparse_script ctxt Readable script
          >>=? fun (script, ctxt) ->
          Script.force_decode_in_context ctxt script.storage
          >>?= fun (storage, _ctxt) -> return_some storage) ;
  register2 S.entrypoint_type (fun ctxt v entrypoint () () ->
      Contract.get_script_code ctxt v
      >>=? fun (_, expr) ->
      match expr with
      | None ->
          raise Not_found
      | Some expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = true in
          let open Script_ir_translator in
          Lwt.return
            ( Script.force_decode_in_context ctxt expr
            >>? fun (expr, _) ->
            parse_toplevel ~legacy expr
            >>? (fun (arg_type, _, _, root_name) ->
                  parse_parameter_ty ctxt ~legacy arg_type
                  >>? fun (Ex_ty arg_type, _) ->
                  Script_ir_translator.find_entrypoint
                    ~root_name
                    arg_type
                    entrypoint)
            |> function
            | Ok (_f, Ex_ty ty) ->
                unparse_ty ctxt ty
                >|? fun (ty_node, _) -> Micheline.strip_locations ty_node
            | Error _ ->
                raise Not_found )) ;
  register1 S.list_entrypoints (fun ctxt v () () ->
      Contract.get_script_code ctxt v
      >>=? fun (_, expr) ->
      match expr with
      | None ->
          raise Not_found
      | Some expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = true in
          let open Script_ir_translator in
          Lwt.return
            ( Script.force_decode_in_context ctxt expr
            >>? fun (expr, _) ->
            parse_toplevel ~legacy expr
            >>? (fun (arg_type, _, _, root_name) ->
                  parse_parameter_ty ctxt ~legacy arg_type
                  >>? fun (Ex_ty arg_type, _) ->
                  Script_ir_translator.list_entrypoints
                    ~root_name
                    arg_type
                    ctxt)
            >|? fun (unreachable_entrypoint, map) ->
            ( unreachable_entrypoint,
              Entrypoints_map.fold
                (fun entry (_, ty) acc ->
                  (entry, Micheline.strip_locations ty) :: acc)
                map
                [] ) )) ;
  register1 S.contract_big_map_get_opt (fun ctxt contract () (key, key_type) ->
      Contract.get_script ctxt contract
      >>=? fun (ctxt, script) ->
      Script_ir_translator.parse_comparable_ty ctxt (Micheline.root key_type)
      >>?= fun (Ex_comparable_ty key_type, ctxt) ->
      Script_ir_translator.parse_comparable_data
        ctxt
        key_type
        (Micheline.root key)
      >>=? fun (key, ctxt) ->
      Script_ir_translator.hash_comparable_data ctxt key_type key
      >>=? fun (key, ctxt) ->
      match script with
      | None ->
          raise Not_found
      | Some script -> (
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
          >>=? fun (Ex_script script, ctxt) ->
          Script_ir_translator.collect_lazy_storage
            ctxt
            script.storage_type
            script.storage
          >>?= fun (ids, _ctxt) ->
          match Script_ir_translator.list_of_big_map_ids ids with
          | [] | _ :: _ :: _ ->
              return_none
          | [id] -> (
            try do_big_map_get ctxt id key >>=? return_some
            with Not_found -> return_none ) )) ;
  register2 S.big_map_get (fun ctxt id key () () -> do_big_map_get ctxt id key) ;
  register_field S.info (fun ctxt contract ->
      Contract.get_balance ctxt contract
      >>=? fun balance ->
      Delegate.get ctxt contract
      >>=? fun delegate ->
      ( match Contract.is_implicit contract with
      | Some manager ->
          Contract.get_counter ctxt manager
          >>=? fun counter -> return_some counter
      | None ->
          return_none )
      >>=? fun counter ->
      Contract.get_script ctxt contract
      >>=? fun (ctxt, script) ->
      ( match script with
      | None ->
          return (None, ctxt)
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
          >>=? fun (Ex_script script, ctxt) ->
          unparse_script ctxt Readable script
          >|=? fun (script, ctxt) -> (Some script, ctxt) )
      >|=? fun (script, _ctxt) -> {balance; delegate; script; counter}) ;
  S.Sapling.register ()

let list ctxt block = RPC_context.make_call0 S.list ctxt block () ()

let info ctxt block contract =
  RPC_context.make_call1 S.info ctxt block contract () ()

let balance ctxt block contract =
  RPC_context.make_call1 S.balance ctxt block contract () ()

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
