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

open Alpha_context

let custom_root =
  (RPC_path.(open_root / "context" / "contracts") : RPC_context.t RPC_path.context)

type info = {
  balance: Tez.t ;
  delegate: public_key_hash option ;
  counter: counter option ;
  script: Script.t option ;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun {balance ; delegate ; script ; counter } ->
      (balance, delegate, script, counter))
    (fun (balance, delegate, script, counter) ->
      {balance ; delegate ; script ; counter}) @@
  obj4
    (req "balance" Tez.encoding)
    (opt "delegate" Signature.Public_key_hash.encoding)
    (opt "script" Script.encoding)
    (opt "counter" n)

module S = struct

  open Data_encoding

  let balance =
    RPC_service.get_service
      ~description: "Access the balance of a contract."
      ~query: RPC_query.empty
      ~output: Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "balance")

  let manager_key =
    RPC_service.get_service
      ~description: "Access the manager of a contract."
      ~query: RPC_query.empty
      ~output: (option Signature.Public_key.encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "manager_key")

  let delegate =
    RPC_service.get_service
      ~description: "Access the delegate of a contract, if any."
      ~query: RPC_query.empty
      ~output: Signature.Public_key_hash.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "delegate")

  let counter =
    RPC_service.get_service
      ~description: "Access the counter of a contract, if any."
      ~query: RPC_query.empty
      ~output: z
      RPC_path.(custom_root /: Contract.rpc_arg / "counter")

  let script =
    RPC_service.get_service
      ~description: "Access the code and data of the contract."
      ~query: RPC_query.empty
      ~output: Script.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "script")

  let storage =
    RPC_service.get_service
      ~description: "Access the data of the contract."
      ~query: RPC_query.empty
      ~output: Script.expr_encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "storage")

  let big_map_get =
    RPC_service.post_service
      ~description: "Access the value associated with a key in the big map storage  of the contract."
      ~query: RPC_query.empty
      ~input: (obj2
                 (req "key" Script.expr_encoding)
                 (req "type" Script.expr_encoding))
      ~output: (option Script.expr_encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "big_map_get")

  let info =
    RPC_service.get_service
      ~description: "Access the complete status of a contract."
      ~query: RPC_query.empty
      ~output: info_encoding
      RPC_path.(custom_root /: Contract.rpc_arg)

  let list =
    RPC_service.get_service
      ~description:
        "All existing contracts (including non-empty default contracts)."
      ~query: RPC_query.empty
      ~output: (list Contract.encoding)
      custom_root

end

let register () =
  let open Services_registration in
  register0 S.list begin fun ctxt () () ->
    Contract.list ctxt >>= return
  end ;
  let register_field s f =
    register1 s (fun ctxt contract () () ->
        Contract.exists ctxt contract >>=? function
        | true -> f ctxt contract
        | false -> raise Not_found) in
  let register_opt_field s f =
    register_field s
      (fun ctxt a1 ->
         f ctxt a1 >>=? function
         | None -> raise Not_found
         | Some v -> return v) in
  register_field S.balance Contract.get_balance ;
  register1 S.manager_key
    (fun ctxt contract () () ->
       match Contract.is_implicit contract with
       | None -> raise Not_found
       | Some mgr ->
           Contract.is_manager_key_revealed ctxt mgr >>=? function
           | false -> return_none
           | true -> Contract.get_manager_key ctxt mgr >>=? return_some) ;
  register_opt_field S.delegate Delegate.get ;
  register1 S.counter
    (fun ctxt contract () () ->
       match Contract.is_implicit contract with
       | None -> raise Not_found
       | Some mgr -> Contract.get_counter ctxt mgr) ;
  register_opt_field S.script
    (fun c v -> Contract.get_script c v >>=? fun (_, v) -> return v) ;
  register_opt_field S.storage (fun ctxt contract ->
      Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
      match script with
      | None -> return_none
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          parse_script ctxt ~legacy:true script >>=? fun (Ex_script script, ctxt) ->
          unparse_script ctxt Readable script >>=? fun (script, ctxt) ->
          Script.force_decode ctxt script.storage >>=? fun (storage, _ctxt) ->
          return_some storage) ;
  register1 S.big_map_get (fun ctxt contract () (key, key_type) ->
      let open Script_ir_translator in
      let ctxt = Gas.set_unlimited ctxt in
      Lwt.return (parse_packable_ty ctxt ~legacy:true (Micheline.root key_type))
      >>=? fun (Ex_ty key_type, ctxt) ->
      parse_data ctxt ~legacy:true key_type (Micheline.root key) >>=? fun (key, ctxt) ->
      hash_data ctxt key_type key >>=? fun (key_hash, ctxt) ->
      Contract.Big_map.get_opt ctxt contract key_hash >>=? fun (_ctxt, value) ->
      return value) ;
  register_field S.info (fun ctxt contract ->
      Contract.get_balance ctxt contract >>=? fun balance ->
      Delegate.get ctxt contract >>=? fun delegate ->
      begin match Contract.is_implicit contract with
        | Some manager ->
            Contract.get_counter ctxt manager >>=? fun counter ->
            return_some counter
        | None -> return None
      end >>=? fun counter ->
      Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
      begin match script with
        | None -> return (None, ctxt)
        | Some script ->
            let ctxt = Gas.set_unlimited ctxt in
            let open Script_ir_translator in
            parse_script ctxt ~legacy:true script >>=? fun (Ex_script script, ctxt) ->
            unparse_script ctxt Readable script >>=? fun (script, ctxt) ->
            return (Some script, ctxt)
      end >>=? fun (script, _ctxt) ->
      return { balance ; delegate ; script ; counter })

let list ctxt block =
  RPC_context.make_call0 S.list ctxt block () ()

let info ctxt block contract =
  RPC_context.make_call1 S.info ctxt block contract () ()

let balance ctxt block contract =
  RPC_context.make_call1 S.balance ctxt block contract () ()

let manager_key ctxt block mgr =
  RPC_context.make_call1 S.manager_key ctxt block (Contract.implicit_contract mgr) () ()

let delegate ctxt block contract =
  RPC_context.make_call1 S.delegate ctxt block contract () ()

let delegate_opt ctxt block contract =
  RPC_context.make_opt_call1 S.delegate ctxt block contract () ()

let counter ctxt block mgr =
  RPC_context.make_call1 S.counter ctxt block (Contract.implicit_contract mgr) () ()

let script ctxt block contract =
  RPC_context.make_call1 S.script ctxt block contract () ()

let script_opt ctxt block contract =
  RPC_context.make_opt_call1 S.script ctxt block contract () ()

let storage ctxt block contract =
  RPC_context.make_call1 S.storage ctxt block contract () ()

let storage_opt ctxt block contract =
  RPC_context.make_opt_call1 S.storage ctxt block contract () ()

let big_map_get_opt ctxt block contract key =
  RPC_context.make_call1 S.big_map_get ctxt block contract () key
