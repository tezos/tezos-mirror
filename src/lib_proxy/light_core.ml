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

(** Module providing the light's mode implementation of Proxy.CORE *)

module Logger = Light_logger.Logger
module Store = Tezos_context_memory.Context
module Consensus = Light_consensus
module Block_services = Tezos_shell_services.Block_services
module Proof = Tezos_context_sigs.Context.Proof_types

let key_to_string = String.concat ";"

let chain_n_block_to_string chain block =
  Format.asprintf
    "chain: %s, block: %s"
    (Block_services.chain_to_string chain)
    (Block_services.to_string block)

let light_failwith (pgi : Proxy.proxy_getter_input) ?(warn_symbolic = false) msg
    =
  let open Lwt_syntax in
  let symbolic_block = Light.hash_of_block pgi.block |> Option.is_none in
  let full_msg =
    Format.sprintf
      "Light mode (%s): %s%s"
      (chain_n_block_to_string pgi.chain pgi.block)
      msg
      (if warn_symbolic && symbolic_block then
       Format.sprintf
         ". Because requested block is symbolic: %s (it has no hash), it could \
          be that the different endpoints are mapping this symbolic identifier \
          to different concrete blocks. If you are using the 'head' identifier \
          (or 'head~1', etc.) in a RPC path, replace it with a concrete hash."
       @@ Block_services.to_string pgi.block
      else "")
  in
  let* () = Logger.(emit failing full_msg) in
  failwith "%s" full_msg

let get_core (module Light_proto : Light_proto.PROTO_RPCS)
    (printer : Tezos_client_base.Client_context.printer)
    ({endpoints; min_agreement} : Light.sources) =
  (module struct
    type irmin = {repo : Store.Tree.repo; root : Store.tree}

    (** Do not access directly, use [get_irmin] instead *)
    let irmin_ref : irmin option ref = ref None

    let mk_empty_irmin () : irmin Lwt.t =
      let open Lwt_syntax in
      let+ repo = Store.Tree.make_repo () in
      let root =
        Store.Tree.empty (Tezos_context_memory.Context.make_empty_context ())
      in
      {repo; root}

    (* Don't update the irmin ref when looking for key, so as not to add the
       empty tree. *)
    let get_irmin () : irmin Lwt.t =
      match !irmin_ref with
      | None -> mk_empty_irmin ()
      | Some res -> Lwt.return res

    module Storelike = struct
      include Store

      let find = Store.Tree.find

      let find_tree = Store.Tree.find_tree

      let unshallow = Tree.unshallow
    end

    module Get_data = Tezos_context_sigs.Context.With_get_data ((
      Storelike : Tezos_context_sigs.Context.Storelike))

    (** Returns an {!irmin} value but don't update {!irmin_ref}. We want to
        update only when the consensus has been checked, not before! *)
    let stage key (tree : (Storelike.tree, bytes) Either.t) =
      let open Lwt_syntax in
      let* irmin = get_irmin () in
      let* root =
        match tree with
        | Either.Left tree -> Store.Tree.add_tree irmin.root key tree
        | Either.Right value -> Store.Tree.add irmin.root key value
      in
      return_ok {irmin with root}

    let rec get_first_merkle_tree chain block key leaf_kind tried_endpoints_rev
        remaining_endpoints =
      let open Lwt_syntax in
      match remaining_endpoints with
      | [] -> Lwt.return_none
      | ((uri, rpc_context) as hd_endpoint) :: tl_remaining_endpoints -> (
          let* proof_opt =
            Light_proto.merkle_tree
              Proxy.{rpc_context; chain; block; mode = Client}
              key
              leaf_kind
          in
          match proof_opt with
          | Ok (Some mproof) -> (
              let* verification =
                Store.verify_tree_proof
                  mproof
                  (Get_data.get_data Proof.Raw_context [key])
              in
              match verification with
              | Ok (_, [(k, Some tree)]) when k = key ->
                  let other_endpoints =
                    List.rev_append tried_endpoints_rev tl_remaining_endpoints
                  in
                  Lwt.return_some (mproof, tree, other_endpoints)
              | _ ->
                  (* Here we skip an endpoint that succeeded with a proof that doesn't map 'key'.
                     This is okay. *)
                  get_first_merkle_tree
                    chain
                    block
                    key
                    leaf_kind
                    (hd_endpoint :: tried_endpoints_rev)
                    tl_remaining_endpoints)
          | Ok None ->
              (* Here we skip an endpoint that succeeded but returned None
                 This means the endpoint's context does not map 'key'.
                 This is okay. *)
              get_first_merkle_tree
                chain
                block
                key
                leaf_kind
                (hd_endpoint :: tried_endpoints_rev)
                tl_remaining_endpoints
          | Error trace ->
              let* () =
                printer#warning
                  "Light mode: endpoint %s failed providing merkle tree of key \
                   %s (%s): %a"
                  (Uri.to_string uri)
                  (key_to_string key)
                  (chain_n_block_to_string chain block)
                  pp_print_trace
                  trace
              in
              get_first_merkle_tree
                chain
                block
                key
                leaf_kind
                (hd_endpoint :: tried_endpoints_rev)
                tl_remaining_endpoints)

    (** Returns the Merkle tree of the first successful call to an endpoint
        in [endpoints], together with the other [endpoints] so that the caller
        is able to ask these endpoints whether they agree Merkle-hash wise.

        If [Some (mtree, other_endpoints)] is returned, it is guaranteed that
        [List.length endpoints = List.length other_endpoints + 1] *)
    let get_first_merkle_tree chain block key leaf_kind :
        (Proof.tree Proof.t
        * (Storelike.tree, bytes) Either.t
        * (Uri.t * Tezos_rpc.Context.simple) list)
        option
        Lwt.t =
      get_first_merkle_tree chain block key leaf_kind [] endpoints

    let get key =
      let open Lwt_syntax in
      let* () = Logger.(emit api_get @@ key_to_string key) in
      let* {root; _} = get_irmin () in
      Store.Tree.find_tree root key

    module Consensus = Light_consensus.Make (Light_proto)

    let do_rpc ({chain; block; _} as pgi : Proxy.proxy_getter_input) key =
      let open Lwt_result_syntax in
      let*! () = Logger.(emit api_do_rpc @@ key_to_string key) in
      let*! mproof_and_i_opt =
        get_first_merkle_tree chain block key Proof.Raw_context
      in
      let nb_endpoints = List.length endpoints in
      match mproof_and_i_opt with
      | None ->
          light_failwith pgi
          @@ Format.sprintf
               "None of the %d endpoints could provide data for key: %s"
               nb_endpoints
               (key_to_string key)
      | Some (mproof, tree, validating_endpoints) -> (
          let* staged = stage key tree in
          let*! () =
            Logger.(
              emit
                staged_data
                (key_to_string key, List.length validating_endpoints))
          in
          let input : Light_consensus.input =
            {printer; min_agreement; chain; block; key; mproof}
          in
          let*! r = Consensus.consensus input validating_endpoints in
          match r with
          | false ->
              light_failwith pgi ~warn_symbolic:true
              @@ Format.sprintf "Consensus cannot be reached for key: %s"
              @@ key_to_string key
          | true ->
              irmin_ref := Some staged ;
              return_unit)
  end : Proxy.CORE)
