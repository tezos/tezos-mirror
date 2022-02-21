(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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
open Protocol.Tx_rollup_l2_storage_sig

(* Build a Tezos context with binary trees *)
module Store = struct
  open Tezos_context_encoding.Context

  module Conf : Irmin_pack.Conf.S = struct
    let entries = 2

    let stable_hash = 2

    let inode_child_order = `Seeded_hash
  end

  (* We could directly use a simpler encoding for commits
     instead of keeping the same as in the current context. *)
  include
    Irmin_pack_mem.Make (Node) (Commit) (Conf) (Metadata) (Contents) (Path)
      (Branch)
      (Hash)
end

module Irmin_storage :
  STORAGE
    with type t = Store.tree
     and type 'a m = ('a, Environment.Error_monad.error) result Lwt.t = struct
  type t = Store.tree

  type 'a m = ('a, Environment.Error_monad.error) result Lwt.t

  let path k = [Bytes.to_string k]

  let get store key =
    let open Lwt_syntax in
    let* res = Store.Tree.find store (path key) in
    return_ok res

  let set store key value =
    let open Lwt_syntax in
    let* store = Store.Tree.add store (path key) value in
    return_ok store

  module Syntax = struct
    include Lwt_result_syntax

    let fail : Environment.Error_monad.error -> 'a m =
     fun e -> Lwt.return (Error e)

    let catch (m : 'a m) k h =
      Lwt.bind m (function Ok x -> k x | Error e -> h e)

    let list_fold_left_m = List.fold_left_es
  end
end

module Context_l2 = Protocol.Tx_rollup_l2_context.Make (Irmin_storage)
module Apply_l2 = Protocol.Tx_rollup_l2_apply.Make (Context_l2)

let empty_storage : Irmin_storage.t = Store.Tree.empty ()

let empty_context : Context_l2.t = empty_storage

let rng_state = Random.State.make_self_init ()

let gen_l2_address () =
  let seed =
    Bytes.init 32 (fun _ -> char_of_int @@ Random.State.int rng_state 255)
  in
  let secret_key = Bls12_381.Signature.generate_sk seed in
  let public_key = Bls12_381.Signature.MinPk.derive_pk secret_key in
  (secret_key, public_key, Tx_rollup_l2_address.of_bls_pk public_key)

let make_unit_ticket_key ctxt ticketer address =
  let open Tezos_micheline.Micheline in
  let open Michelson_v1_primitives in
  let ticketer =
    Bytes
      ( 0,
        Data_encoding.Binary.to_bytes_exn
          Alpha_context.Contract.encoding
          ticketer )
  in
  let ty = Prim (0, T_unit, [], []) in
  let contents = Prim (0, D_Unit, [], []) in
  let owner =
    String (dummy_location, Tx_rollup_l2_address.to_b58check address)
  in
  match Alpha_context.Ticket_hash.make ctxt ~ticketer ~ty ~contents ~owner with
  | Ok (x, _) -> x
  | Error _ -> raise (Invalid_argument "make_unit_ticket_key")

let gen_n_address n =
  List.init ~when_negative_length:[] n (fun _ -> gen_l2_address ()) |> function
  | Ok addresses -> addresses
  | _ -> raise (Invalid_argument "Failed to forge addresses")

let gen_n_ticket_hash n =
  let x =
    Lwt_main.run
      ( Context.init n >>=? fun (b, contracts) ->
        Incremental.begin_construction b >|=? Incremental.alpha_ctxt
        >>=? fun ctxt ->
        let addressess = gen_n_address n in
        let tickets =
          List.map2
            ~when_different_lengths:[]
            (fun contract (_, _, address) ->
              make_unit_ticket_key ctxt contract address)
            contracts
            addressess
        in
        match tickets with
        | Ok x -> return x
        | Error _ -> raise (Invalid_argument "Failed to forge tickets") )
  in
  match x with
  | Ok x -> x
  | Error _ -> raise (Invalid_argument "Failed to forge tickets")

let sign_transaction :
    Bls12_381.Signature.sk list ->
    ('signer, 'content) Tx_rollup_l2_batch.V1.transaction ->
    Tx_rollup_l2_batch.V1.signature list =
 fun sks transaction ->
  let ops_nb = List.length transaction in
  assert (Compare.List_length_with.(sks = ops_nb)) ;

  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      transaction
  in

  List.map (fun sk -> Bls12_381.Signature.MinPk.Aug.sign sk buf) sks

type Environment.Error_monad.error += Test_error of string

let fail_msg msg = Context_l2.Syntax.fail (Test_error msg)

let () =
  let open Data_encoding in
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"tx_rollup_l2_apply_test_error"
    ~title:"Test error"
    ~description:"Something went wrong during a test"
    (obj1 (req "msg" string))
    (function Test_error s -> Some s | _ -> None)
    (fun s -> Test_error s)

let expect_error ?msg_if_valid ?msg_if_error f err =
  let open Context_l2.Syntax in
  let default = "<no message provided>" in
  let msg_if_valid = Option.value ~default msg_if_valid in
  let msg_if_error = Option.value ~default msg_if_error in

  catch
    f
    (fun _ -> fail_msg msg_if_valid)
    (fun err' -> if err = err' then return () else fail_msg msg_if_error)

let nth_exn l n =
  match List.nth l n with
  | Some x -> x
  | None -> raise (Invalid_argument "nth_exn")
