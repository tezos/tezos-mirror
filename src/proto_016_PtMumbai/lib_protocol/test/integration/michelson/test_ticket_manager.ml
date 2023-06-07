(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component: Protocol (Ticket_balance_key)
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_ticket_manager.ml
    Subject: Tests that compare the ticket-balance table against tickets in the
             contract storages. The tests include a lot of operations that
             sends and store tickets. After each operation we check that the
             ticket balance table reflects the actual tickets stored.
             This relies on the invariant that any ticket-token referenced by
             the table corresponds to a ticket in a storage. Currently, storage
             is the only place to actually keep existing tickets.
*)

open Protocol
open Alpha_context
open Lwt_result_syntax

let wrap m = m >|= Environment.wrap_tzresult

type init_env = {
  block : Block.t;
  baker : Tezos_crypto.Signature.public_key_hash;
  contract : Contract.t;
}

let init_env () =
  let* block, baker, contract, _src2 = Contract_helpers.init () in
  return {block; baker; contract}

let collect_token_amounts ctxt tickets =
  let accum (tokens, ctxt) ticket =
    let token, amount =
      Ticket_scanner.ex_token_and_amount_of_ex_ticket ticket
    in
    let tokens = (token, Script_int.(to_zint (amount :> n num))) :: tokens in
    return (tokens, ctxt)
  in
  List.fold_left_es accum ([], ctxt) tickets

let tokens_of_value ~include_lazy ctxt ty x =
  let*? has_tickets, ctxt = Ticket_scanner.type_has_tickets ctxt ty in
  let* tickets, ctxt =
    Ticket_scanner.tickets_of_value ~include_lazy ctxt has_tickets x
  in
  let* tas, ctxt = collect_token_amounts ctxt tickets in
  let* bm, ctxt =
    Ticket_token_map.of_list
      ctxt
      ~merge_overlap:(fun ctxt v1 v2 -> ok (Z.add v1 v2, ctxt))
      tas
  in
  Lwt.return @@ Ticket_token_map.to_list ctxt bm

(* Extract ticket-token balance of storage *)
let ticket_balance_of_storage ctxt (contract : Alpha_context.Contract.t) =
  match contract with
  | Implicit _ -> return ([], ctxt)
  | Originated contract_hash -> (
      let* ctxt, script =
        wrap @@ Alpha_context.Contract.get_script ctxt contract_hash
      in
      match script with
      | None -> return ([], ctxt)
      | Some script ->
          let* ( Script_ir_translator.Ex_script
                   (Script {storage; storage_type; _}),
                 ctxt ) =
            wrap
              (Script_ir_translator.parse_script
                 ctxt
                 ~elab_conf:(Script_ir_translator_config.make ~legacy:true ())
                 ~allow_forged_in_storage:true
                 script)
          in
          let* tokens, ctxt =
            wrap (tokens_of_value ~include_lazy:true ctxt storage_type storage)
          in
          let* tokens, ctxt =
            wrap
            @@ List.fold_left_es
                 (fun (acc, ctxt) (ex_token, amount) ->
                   let* key, ctxt =
                     Ticket_balance_key.of_ex_token
                       ctxt
                       ~owner:(Contract contract)
                       ex_token
                   in
                   let acc = (key, amount) :: acc in
                   return (acc, ctxt))
                 ([], ctxt)
                 tokens
          in
          return (tokens, ctxt))

let transaction block ~sender ~recipient ~amount ~parameters =
  let parameters = Script.lazy_expr @@ Expr.from_string parameters in
  let* block = Incremental.begin_construction block in
  let* operation =
    Op.transaction
      (I block)
      ~gas_limit:Max
      ~entrypoint:Entrypoint.default
      ~parameters
      ~fee:Tez.zero
      sender
      recipient
      (Tez.of_mutez_exn amount)
  in
  let* block = Incremental.add_operation block operation in
  Incremental.finalize_block block

let all_contracts current_block =
  let* ctxt =
    Incremental.begin_construction current_block >|=? Incremental.alpha_ctxt
  in
  Lwt.map Result.ok @@ Contract.list ctxt

(* Fetch all added contracts between [before] and [after]. *)
let new_contracts ~before ~after =
  let* cs1 = all_contracts before in
  let* cs2 = all_contracts after in
  let not_in_cs1 =
    let module S = Set.Make (String) in
    let set = S.of_list @@ List.map Contract.to_b58check cs1 in
    fun c -> not @@ S.mem (Contract.to_b58check c) set
  in
  return @@ List.filter not_in_cs1 cs2

let get_first_two_new_contracts before f =
  let* after = f before in
  let* c = new_contracts ~before ~after in
  match c with
  | c1 :: c2 :: _ -> return (c1, c2, after)
  | _ -> failwith "Expected two new contracts"

let show_key_balance key balance =
  let key = String.escaped @@ Format.asprintf "%a" Ticket_hash.pp key in
  let balance = Z.to_string balance in
  (key, balance)

let compare_key_balance (k1, b1) (k2, b2) =
  match String.compare k1 k2 with
  | n when n <> 0 -> n
  | _ -> String.compare b1 b2

let normalize_balances key_balances =
  List.filter_map
    (fun (key, balance) ->
      if Z.equal balance Z.zero then None
      else Some (show_key_balance key balance))
    key_balances
  |> List.sort compare_key_balance

let equal_key_balances kb1 kb2 =
  let compare x y = compare_key_balance x y = 0 in
  List.for_all2
    ~when_different_lengths:"Length of key-balances don't match"
    compare
    kb1
    kb2

let show_balance_table kvs =
  let show_rows kvs =
    let key_col_length =
      List.fold_left (fun mx (s, _) -> max mx (String.length s)) 0 kvs
    in
    let column align col_length s =
      let space =
        Stdlib.List.init (col_length - String.length s) (fun _ -> " ")
        |> String.concat ""
      in
      match align with
      | `Left -> Printf.sprintf "%s%s" s space
      | `Right -> Printf.sprintf "%s%s" space s
    in
    List.map
      (fun (k, v) ->
        Printf.sprintf
          "| %s  | %s |"
          (column `Left key_col_length k)
          (column `Right 8 v))
      kvs
    |> String.concat "\n"
  in
  show_rows (("Token x Content x Owner", "Balance") :: kvs)

let validate_ticket_balances block =
  let* contracts = all_contracts block in
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let* kvs_storage, ctxt =
    List.fold_left_es
      (fun (acc, ctxt) contract ->
        let* lists, ctxt = ticket_balance_of_storage ctxt contract in
        return (lists @ acc, ctxt))
      ([], ctxt)
      contracts
  in
  let* kvs_balance, _ctxt =
    wrap
    @@ List.fold_left_es
         (fun (acc, ctxt) (key, _) ->
           let* balance, ctxt = Ticket_balance.get_balance ctxt key in
           let acc =
             match balance with None -> acc | Some b -> (key, b) :: acc
           in
           return (acc, ctxt))
         ([], ctxt)
         kvs_storage
  in
  let kvs_storage = normalize_balances kvs_storage in
  let kvs_balance = normalize_balances kvs_balance in
  let print_both () =
    print_endline "Storage table:" ;
    print_endline @@ show_balance_table kvs_storage ;
    print_endline "Balance table:" ;
    print_endline @@ show_balance_table kvs_balance
  in
  match equal_key_balances kvs_balance kvs_storage with
  | Ok true -> return ()
  | Ok false ->
      print_both () ;
      failwith "Storage and balance don't match"
  | Error e ->
      print_both () ;
      failwith "%s" e

module Ticket_manager = struct
  (* A rather complicated script for various ticket operations.
     See documentation for [action] below for a list of the params.
  *)
  let script =
    {|
      { parameter
        (or (or (or (or %add (pair %add_lazy (pair int string) nat) (pair %add_strict string nat))
                    (pair %create (pair string nat) key_hash))
                (or (or %remove (or (unit %remove_all_lazy) (int %remove_lazy)) (unit %remove_strict))
                    (big_map %replace_big_map int (ticket string))))
            (or %send
              (or (pair %send_lazy int address) (pair %send_new (pair string nat) address))
              (or (unit %send_self_replace_big_map) (address %send_strict)))) ;
      storage (pair (list %list (ticket string)) (big_map %map int (ticket string))) ;
      code { NIL (ticket string) ;
            SWAP ;
            UNPAIR ;
            IF_LEFT
              { DIG 2 ;
                DROP ;
                IF_LEFT
                  { IF_LEFT
                      { IF_LEFT
                          { # `add_lazy` entrypoint
                            UNPAIR ;
                            UNPAIR ;
                            DIG 3 ;
                            DIG 3 ;
                            SWAP ;
                            UNPAIR ;
                            SWAP ;
                            DUP 3 ;
                            DUP 6 ;
                            TICKET ;
                            ASSERT_SOME ;
                            SOME ;
                            DUP 5 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            DUP 3 ;
                            DUP 6 ;
                            TICKET ;
                            ASSERT_SOME ;
                            SOME ;
                            DUP 5 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            DIG 2 ;
                            DIG 4 ;
                            TICKET ;
                            ASSERT_SOME ;
                            SOME ;
                            DIG 3 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            SWAP ;
                            PAIR ;
                            NIL operation ;
                            PAIR }
                          {
                            # `add_strict` entrypoint
                            UNPAIR ;
                            DIG 2 ;
                            UNPAIR ;
                            DIG 3 ;
                            DIG 3 ;
                            TICKET ;
                            ASSERT_SOME ;
                            CONS ;
                            PAIR ;
                            NIL operation ;
                            PAIR } }
                      { # `create` entrypoint
                        UNPAIR ;
                        UNPAIR ;
                        DUP 3 ;
                        DUG 2 ;
                        TICKET ;
                        ASSERT_SOME ;
                        PUSH mutez 0 ;
                        DIG 2 ;
                        SOME ;
                        CREATE_CONTRACT
                          { parameter (ticket string) ;
                            storage (ticket string) ;
                            code { JOIN_TICKETS ;
                                    IF_NONE
                                      { PUSH string "Failed to join tickets" ; FAILWITH }
                                      { NIL operation ; PAIR } } } ;
                        SWAP ;
                        DROP ;
                        EMPTY_BIG_MAP int (ticket string) ;
                        PUSH nat 99 ;
                        PUSH string "NEW_TICKET_IN_ORIGINATED_CONTRACT" ;
                        TICKET ;
                        ASSERT_SOME ;
                        SOME ;
                        PUSH int 1 ;
                        GET_AND_UPDATE ;
                        DROP ;
                        PUSH mutez 0 ;
                        DIG 3 ;
                        SOME ;
                        CREATE_CONTRACT
                          { parameter (ticket string) ;
                            storage (big_map int (ticket string)) ;
                            code { UNPAIR ;
                                    SOME ;
                                    PUSH int 1 ;
                                    GET_AND_UPDATE ;
                                    DROP ;
                                    NIL operation ;
                                    PAIR } } ;
                        SWAP ;
                        DROP ;
                        DIG 2 ;
                        NIL operation ;
                        DIG 2 ;
                        CONS ;
                        DIG 2 ;
                        CONS ;
                        PAIR } }
                  { IF_LEFT
                      { IF_LEFT
                          { IF_LEFT
                              { # `remove_all_lazy` entrypoint
                                DROP ;
                                CAR ;
                                EMPTY_BIG_MAP int (ticket string) ;
                                SWAP ;
                                PAIR ;
                                NIL operation ;
                                PAIR }
                              { # `remove` entrypoint
                                SWAP ;
                                UNPAIR ;
                                SWAP ;
                                NONE (ticket string) ;
                                DIG 3 ;
                                GET_AND_UPDATE ;
                                DROP ;
                                SWAP ;
                                PAIR ;
                                NIL operation ;
                                PAIR } }
                          { # `remove_strict` entrypoint
                            DROP ; CDR ; NIL (ticket string) ; PAIR ; NIL operation ; PAIR } }
                      { # `replace_big_map` entrypoint
                        SWAP ;
                        CAR ;
                        SWAP ;
                        NONE (ticket string) ;
                        PUSH int 1 ;
                        GET_AND_UPDATE ;
                        DROP ;
                        PUSH nat 1 ;
                        PUSH string "ADDED_BY_REPLACE_BIG_MAP" ;
                        TICKET ;
                        ASSERT_SOME ;
                        SOME ;
                        PUSH int 11 ;
                        GET_AND_UPDATE ;
                        DROP ;
                        NONE (ticket string) ;
                        PUSH int 11 ;
                        GET_AND_UPDATE ;
                        DROP ;
                        PUSH nat 1 ;
                        PUSH string "ADDED_BY_REPLACE_BIG_MAP" ;
                        TICKET ;
                        ASSERT_SOME ;
                        SOME ;
                        PUSH int 11 ;
                        GET_AND_UPDATE ;
                        DROP ;
                        SWAP ;
                        PAIR ;
                        NIL operation ;
                        PAIR } } }
              { IF_LEFT
                  { DIG 2 ;
                    DROP ;
                    IF_LEFT
                      { # `send_lazy` entrypoint
                        UNPAIR ;
                        DIG 2 ;
                        UNPAIR ;
                        DIG 3 ;
                        CONTRACT (ticket string) ;
                        IF_NONE
                          { DIG 2 ;
                            DROP ;
                            PUSH string "Contract of type `ticket(string)` not found" ;
                            FAILWITH }
                          { DIG 2 ;
                            NONE (ticket string) ;
                            DIG 4 ;
                            GET_AND_UPDATE ;
                            IF_NONE
                              { SWAP ; DROP ; PUSH string "Could not find ticket" ; FAILWITH }
                              { DIG 2 ;
                                PUSH mutez 0 ;
                                DIG 2 ;
                                TRANSFER_TOKENS ;
                                SWAP ;
                                DIG 2 ;
                                PAIR ;
                                NIL operation ;
                                DIG 2 ;
                                CONS ;
                                PAIR } } }
                      { # `send_new` entrypoint
                        UNPAIR ;
                        UNPAIR ;
                        DIG 3 ;
                        DIG 3 ;
                        SWAP ;
                        UNPAIR ;
                        DIG 2 ;
                        CONTRACT (ticket string) ;
                        IF_NONE
                          { DIG 2 ;
                            DROP ;
                            DIG 2 ;
                            DROP ;
                            PUSH string "Contract of type `ticket(string)` not found" ;
                            FAILWITH }
                          { PUSH mutez 0 ;
                            DIG 5 ;
                            DIG 5 ;
                            TICKET ;
                            ASSERT_SOME ;
                            TRANSFER_TOKENS ;
                            DUG 2 ;
                            PAIR ;
                            NIL operation ;
                            DIG 2 ;
                            CONS ;
                            PAIR } } }
                  { IF_LEFT
                      { # `send_self_replace_big_map` entrypoint
                        DROP ;
                        SWAP ;
                        DROP ;
                        UNPAIR ;
                        SELF_ADDRESS ;
                        CONTRACT
                          (or (or (or (or %add (pair %add_lazy (pair int string) nat) (pair %add_strict string nat))
                                      (pair %create (pair string nat) key_hash))
                                  (or (or %remove (or (unit %remove_all_lazy) (int %remove_lazy)) (unit %remove_strict))
                                      (big_map %replace_big_map int (ticket string))))
                              (or %send
                                  (or (pair %send_lazy int address) (pair %send_new (pair string nat) address))
                                  (or (unit %send_self_replace_big_map) (address %send_strict)))) ;
                        IF_NONE
                          { PUSH string "Failed to get self-contract" ; FAILWITH }
                          { EMPTY_BIG_MAP int (ticket string) ;
                            DIG 3 ;
                            PUSH nat 1 ;
                            PUSH string "ADDED_BY_SEND_SELF_REPLACE" ;
                            TICKET ;
                            ASSERT_SOME ;
                            SOME ;
                            PUSH int 10 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            NONE (ticket string) ;
                            PUSH int 2 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            DIG 2 ;
                            PUSH mutez 0 ;
                            DIG 2 ;
                            RIGHT (or (or unit int) unit) ;
                            RIGHT
                              (or (or (pair (pair int string) nat) (pair string nat)) (pair (pair string nat) key_hash)) ;
                            LEFT (or (or (pair int address) (pair (pair string nat) address)) (or unit address)) ;
                            TRANSFER_TOKENS ;
                            SWAP ;
                            PUSH nat 1 ;
                            PUSH string "ADDED_BY_SEND_SELF_REPLACE_TO_STORAGE" ;
                            TICKET ;
                            ASSERT_SOME ;
                            SOME ;
                            PUSH int 11 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            DIG 2 ;
                            PAIR ;
                            NIL operation ;
                            DIG 2 ;
                            CONS ;
                            PAIR } }
                      { # `send_strict` entrypoint
                        SWAP ;
                        UNPAIR ;
                        DIG 2 ;
                        CONTRACT (ticket string) ;
                        IF_NONE
                          { DROP ;
                            PUSH string "Contract of type `ticket(string)` not found" ;
                            FAILWITH }
                          { NONE (ticket string) ;
                            DIG 2 ;
                            ITER { SWAP ; IF_NONE { SOME } { PAIR ; JOIN_TICKETS } } ;
                            IF_NONE
                              { DROP ; PUSH string "Couldn't produce a ticket" ; FAILWITH }
                              { SWAP ;
                                PUSH mutez 0 ;
                                DIG 2 ;
                                TRANSFER_TOKENS ;
                                SWAP ;
                                DIG 2 ;
                                PAIR ;
                                NIL operation ;
                                DIG 2 ;
                                CONS ;
                                PAIR } } } } } } }
    |}

  type remove_args = Remove_lazy of int | Remove_strict | Remove_all_lazy

  type send_args =
    | Send_lazy of {index : int; recipient : Contract.t}
    | Send_strict of Contract.t
    | Send_new of {content : string; amount : int; recipient : Contract.t}
    | Send_self_replace_big_map

  type add_args =
    | Add_lazy of {index : int; content : string; amount : int}
    | Add_strict of {content : string; amount : int}

  type action =
    | Remove of remove_args
    | Add of add_args
    | Create of {content : string; amount : int; originator : Contract.t}
    | Send of send_args

  let remove_lazy ~index = Remove (Remove_lazy index)

  let remove_strict = Remove Remove_strict

  let add_lazy ~index ~content ~amount = Add (Add_lazy {index; content; amount})

  let add_strict ~content ~amount = Add (Add_strict {content; amount})

  let create ~content ~amount ~originator = Create {content; amount; originator}

  let send_lazy ~index ~recipient = Send (Send_lazy {index; recipient})

  let send_strict ~recipient = Send (Send_strict recipient)

  let send_new ~content ~amount ~recipient =
    Send (Send_new {content; amount; recipient})

  let remove_all_lazy = Remove Remove_all_lazy

  let send_self_replace_big_map = Send Send_self_replace_big_map

  let string_args = function
    | Remove (Remove_lazy ix) ->
        Printf.sprintf "Left (Right (Left (Left (Right %d))))" ix
    | Remove Remove_strict -> "Left (Right (Left (Right Unit)))"
    | Remove Remove_all_lazy -> "Left (Right (Left (Left (Left Unit))))"
    | Add (Add_lazy {index; content; amount}) ->
        Printf.sprintf
          {|Left (Left (Left (Left (Pair (Pair %d "%s") %d))))|}
          index
          content
          amount
    | Add (Add_strict {content; amount}) ->
        Printf.sprintf
          {|Left (Left (Left (Right (Pair "%s" %d))))|}
          content
          amount
    | Create {content; amount; originator} ->
        Printf.sprintf
          {|Left (Left (Right (Pair (Pair "%s" %d) "%s")))|}
          content
          amount
          (Contract.to_b58check originator)
    | Send (Send_lazy {index; recipient}) ->
        Printf.sprintf
          {|Right (Left (Left (Pair %d "%s")))|}
          index
          (Contract.to_b58check recipient)
    | Send (Send_strict contract) ->
        Printf.sprintf
          {|Right (Right (Right "%s"))|}
          (Contract.to_b58check contract)
    | Send (Send_new {content; amount; recipient}) ->
        Printf.sprintf
          {|(Right (Left (Right (Pair (Pair "%s" %d) "%s"))))|}
          content
          amount
          (Contract.to_b58check recipient)
    | Send Send_self_replace_big_map -> "Right (Right (Left Unit))"

  let originate block ~originator baker =
    Contract_helpers.originate_contract_from_string
      ~script
      ~storage:"Pair {} {}"
      ~source_contract:originator
      ~baker
      block

  let transaction block ~sender ~ticket_manager ~parameters =
    let parameters = string_args parameters in
    transaction block ~sender ~recipient:ticket_manager ~amount:0L ~parameters
end

(* Sets up the environment and returns a function for running a transaction
   and validating the balance table against tickets in the storage once
   completed. *)
let setup_test () =
  let module TM = Ticket_manager in
  let* {block; baker; contract = originator} = init_env () in
  let* ticket_manager, _script, block = TM.originate block ~originator baker in
  let test block parameters =
    let* b =
      TM.transaction block ~sender:originator ~ticket_manager ~parameters
    in
    let* () = validate_ticket_balances b in
    return b
  in
  return (test, originator, block)

(** Test create new contracts and send tickets to them. *)
let test_create_contract_and_send_tickets () =
  let module TM = Ticket_manager in
  let* test, originator, b = setup_test () in

  (* Call the `create` endpoint that creates two new ticket receiver contracts:
     - Both contracts accepts a single ticket as an argument.
     - The first holds a big-map with tickets in its storage.
     - The second holds a ticket in its storage and only accepts "green" tickets.
     - The second contract joins all received tickets.
  *)
  let* ticket_receiver_green_1, ticket_receiver_green_2, b =
    get_first_two_new_contracts b @@ fun b ->
    test b @@ TM.create ~content:"Green" ~amount:1 ~originator
  in
  (*
   * Invoke the second ticket receiver contract:
    - Remove all strict tickets
    - Replace them with a list of 2 green ones
    - Send all (joined) strict tickets to the ticket receiver
   *)
  let* b = test b @@ TM.remove_strict in
  let* b = test b @@ TM.add_strict ~content:"Green" ~amount:1 in
  let* b = test b @@ TM.add_strict ~content:"Green" ~amount:1 in
  let* b = test b @@ TM.send_strict ~recipient:ticket_receiver_green_1 in
  (* Send a new ticket *)
  let* b =
    test
      b
      (TM.send_new
         ~content:"Green"
         ~amount:10
         ~recipient:ticket_receiver_green_1)
  in
  (* Add a green ticket to the lazy storage at index 1 and send it to the green
     ticket-receiver *)
  let* b = test b @@ TM.add_lazy ~index:1 ~content:"Green" ~amount:10 in
  let* (_b : Block.t) =
    test b @@ TM.send_lazy ~index:1 ~recipient:ticket_receiver_green_2
  in
  return ()

(** Tets add and remove tickets from lazy storage. *)
let test_add_remove_from_lazy_storage () =
  let module TM = Ticket_manager in
  let* tm, _, b = setup_test () in
  let* b = tm b @@ TM.add_lazy ~index:1 ~content:"Red" ~amount:10 in
  let* b = tm b @@ TM.add_lazy ~index:2 ~content:"Green" ~amount:10 in
  let* b = tm b @@ TM.add_lazy ~index:3 ~content:"Blue" ~amount:10 in
  (* Remove ticket at index 1. *)
  let* b = tm b @@ TM.remove_lazy ~index:1 in
  let* b = tm b @@ TM.add_lazy ~index:1 ~content:"Red" ~amount:1 in
  let* b = tm b @@ TM.add_lazy ~index:2 ~content:"Green" ~amount:2 in
  let* b = tm b @@ TM.add_lazy ~index:3 ~content:"Blue" ~amount:3 in
  (* Remove the big-map. *)
  let* b = tm b TM.remove_all_lazy in
  (* Add back a ticket at index 1. *)
  let* (_b : Block.t) =
    tm b @@ TM.add_lazy ~index:1 ~content:"Red" ~amount:10
  in
  return ()

(** Test send to self and replace big-map. *)
let test_send_self_replace_big_map () =
  let module TM = Ticket_manager in
  let* tm, _, b = setup_test () in
  (* Send self replace bigmap *)
  let* b = tm b @@ TM.add_lazy ~index:1 ~content:"Red" ~amount:1 in
  let* b = tm b @@ TM.add_lazy ~index:2 ~content:"Green" ~amount:1 in
  let* b = tm b @@ TM.add_lazy ~index:3 ~content:"Blue" ~amount:1 in
  let* b = tm b @@ TM.send_self_replace_big_map in
  let* b = tm b @@ TM.send_self_replace_big_map in
  let* (_b : Block.t) = tm b @@ TM.send_self_replace_big_map in
  return ()

(** Test add to and remove from strict storage. *)
let test_add_remove_strict () =
  let module TM = Ticket_manager in
  let* tm, _, b = setup_test () in
  (* Add some more strict tickets *)
  let* b = tm b @@ TM.add_strict ~content:"Red" ~amount:1 in
  let* b = tm b @@ TM.add_strict ~content:"Red" ~amount:2 in
  let* b = tm b @@ TM.add_strict ~content:"Red" ~amount:20 in
  let* b = tm b @@ TM.add_strict ~content:"Green" ~amount:1 in
  let* b = tm b @@ TM.add_strict ~content:"Red" ~amount:1 in

  (* Remove strict tickets *)
  let* b = tm b @@ TM.remove_strict in
  let* (_b : Block.t) = tm b @@ TM.add_strict ~content:"Red" ~amount:1 in
  return ()

(** Test mixed operations. *)
let test_mixed_operations () =
  let module TM = Ticket_manager in
  let* tm, _, b = setup_test () in
  (* Add some more strict tickets *)
  let* b = tm b @@ TM.add_strict ~content:"Red" ~amount:1 in
  let* b = tm b @@ TM.add_strict ~content:"Green" ~amount:1 in
  (* Add some lazy tickets *)
  let* b = tm b @@ TM.add_lazy ~index:1 ~content:"Red" ~amount:1 in
  let* b = tm b @@ TM.add_lazy ~index:2 ~content:"Green" ~amount:1 in
  let* b = tm b @@ TM.add_lazy ~index:3 ~content:"Blue" ~amount:1 in
  (* Remove strict and lazy *)
  let* b = tm b @@ TM.remove_strict in
  let* (_b : Block.t) = tm b @@ TM.remove_all_lazy in
  return ()

let tests =
  [
    Tztest.tztest "Create contract" `Quick test_create_contract_and_send_tickets;
    Tztest.tztest
      "Send self replace big-map"
      `Quick
      test_send_self_replace_big_map;
    Tztest.tztest
      "Add and remove from strict storage"
      `Quick
      test_add_remove_strict;
    Tztest.tztest
      "Add and remove from lazy storage"
      `Quick
      test_add_remove_from_lazy_storage;
    Tztest.tztest "Mix of operations" `Quick test_mixed_operations;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("ticket manager", tests)]
  |> Lwt_main.run
