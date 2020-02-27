(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type parameters = {
  context_root : string;
  protocol_root : string;
  genesis : Genesis.t;
  sandbox_parameters : Data_encoding.json option;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
}

type request =
  | Init
  | Validate of {
      chain_id : Chain_id.t;
      block_header : Block_header.t;
      predecessor_block_header : Block_header.t;
      operations : Operation.t list list;
      max_operations_ttl : int;
    }
  | Commit_genesis of {chain_id : Chain_id.t}
  | Fork_test_chain of {
      context_hash : Context_hash.t;
      forked_header : Block_header.t;
    }
  | Terminate
  | Restore_context_integrity

let request_pp ppf = function
  | Init ->
      Format.fprintf ppf "process handshake"
  | Validate {block_header; chain_id; _} ->
      Format.fprintf
        ppf
        "block validation %a for chain %a"
        Block_hash.pp_short
        (Block_header.hash block_header)
        Chain_id.pp_short
        chain_id
  | Commit_genesis {chain_id} ->
      Format.fprintf
        ppf
        "commit genesis block for chain %a"
        Chain_id.pp_short
        chain_id
  | Fork_test_chain {forked_header; _} ->
      Format.fprintf
        ppf
        "test chain fork on block %a"
        Block_hash.pp_short
        (Block_header.hash forked_header)
  | Terminate ->
      Format.fprintf ppf "terminate validation process"
  | Restore_context_integrity ->
      Format.fprintf ppf "restore context integrity"

let magic = Bytes.of_string "TEZOS_FORK_VALIDATOR_MAGIC_0"

let parameters_encoding =
  let open Data_encoding in
  conv
    (fun { context_root;
           protocol_root;
           genesis;
           user_activated_upgrades;
           user_activated_protocol_overrides;
           sandbox_parameters } ->
      ( context_root,
        protocol_root,
        genesis,
        user_activated_upgrades,
        user_activated_protocol_overrides,
        sandbox_parameters ))
    (fun ( context_root,
           protocol_root,
           genesis,
           user_activated_upgrades,
           user_activated_protocol_overrides,
           sandbox_parameters ) ->
      {
        context_root;
        protocol_root;
        genesis;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        sandbox_parameters;
      })
    (obj6
       (req "context_root" string)
       (req "protocol_root" string)
       (req "genesis" Genesis.encoding)
       (req "user_activated_upgrades" User_activated.upgrades_encoding)
       (req
          "user_activated_protocol_overrides"
          User_activated.protocol_overrides_encoding)
       (opt "sandbox_parameters" json))

let request_encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"init"
        empty
        (function Init -> Some () | _ -> None)
        (fun () -> Init);
      case
        (Tag 1)
        ~title:"validate"
        (obj5
           (req "chain_id" Chain_id.encoding)
           (req "block_header" (dynamic_size Block_header.encoding))
           (req "pred_header" (dynamic_size Block_header.encoding))
           (req "max_operations_ttl" int31)
           (req "operations" (list (list (dynamic_size Operation.encoding)))))
        (function
          | Validate
              { chain_id;
                block_header;
                predecessor_block_header;
                max_operations_ttl;
                operations } ->
              Some
                ( chain_id,
                  block_header,
                  predecessor_block_header,
                  max_operations_ttl,
                  operations )
          | _ ->
              None)
        (fun ( chain_id,
               block_header,
               predecessor_block_header,
               max_operations_ttl,
               operations ) ->
          Validate
            {
              chain_id;
              block_header;
              predecessor_block_header;
              max_operations_ttl;
              operations;
            });
      case
        (Tag 2)
        ~title:"commit_genesis"
        (obj1 (req "chain_id" Chain_id.encoding))
        (function
          | Commit_genesis {chain_id} ->
              Some chain_id
          | Init
          | Validate _
          | Fork_test_chain _
          | Terminate
          | Restore_context_integrity ->
              None)
        (fun chain_id -> Commit_genesis {chain_id});
      case
        (Tag 3)
        ~title:"fork_test_chain"
        (obj2
           (req "context_hash" Context_hash.encoding)
           (req "forked_header" Block_header.encoding))
        (function
          | Fork_test_chain {context_hash; forked_header} ->
              Some (context_hash, forked_header)
          | _ ->
              None)
        (fun (context_hash, forked_header) ->
          Fork_test_chain {context_hash; forked_header});
      case
        (Tag 4)
        ~title:"terminate"
        unit
        (function Terminate -> Some () | _ -> None)
        (fun () -> Terminate);
      case
        (Tag 5)
        ~title:"restore_integrity"
        unit
        (function Restore_context_integrity -> Some () | _ -> None)
        (fun () -> Restore_context_integrity) ]

let send pin encoding data =
  let msg = Data_encoding.Binary.to_bytes_exn encoding data in
  Lwt_io.write_int pin (Bytes.length msg)
  >>= fun () ->
  Lwt_io.write pin (Bytes.to_string msg) >>= fun () -> Lwt_io.flush pin

let recv_result pout encoding =
  Lwt_io.read_int pout
  >>= fun count ->
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly pout buf 0 count
  >>= fun () ->
  Lwt.return
    (Data_encoding.Binary.of_bytes_exn
       (Error_monad.result_encoding encoding)
       buf)

let recv pout encoding =
  Lwt_io.read_int pout
  >>= fun count ->
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly pout buf 0 count
  >>= fun () -> Lwt.return (Data_encoding.Binary.of_bytes_exn encoding buf)
