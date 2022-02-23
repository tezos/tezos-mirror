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

module Internal = Light_internal
module Logger = Light_logger.Logger
module Merkle = Internal.Merkle
module Store = Local_context
module Consensus = Light_consensus
module Block_services = Tezos_shell_services.Block_services

let key_to_string = Internal.key_to_string

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

    let shallow_tree_of_merkle_tree repo mtree =
      let open Lwt_result_syntax in
      let* tree = Merkle.merkle_tree_to_irmin_tree repo mtree in
      return (Store.shallow_of_tree repo tree)

    let hash_of_merkle_tree repo mtree =
      let open Lwt_result_syntax in
      let* tree = Merkle.merkle_tree_to_irmin_tree repo mtree in
      return (Store.Tree.hash tree)

    let get_irmin_and_update_root pgi mtree =
      let open Lwt_syntax in
      match !irmin_ref with
      | None -> (
          let* repo = Store.Tree.make_repo () in
          let* r = shallow_tree_of_merkle_tree repo mtree in
          match r with
          | Ok root ->
              let irmin = {repo; root} in
              irmin_ref := Some irmin ;
              Lwt.return_ok irmin
          | Error msg -> light_failwith pgi msg)
      | Some res -> (
          let* r = hash_of_merkle_tree res.repo mtree in
          match r with
          | Ok merkle_hash ->
              let tree_hash = Store.Tree.hash res.root in
              if not (Context_hash.equal tree_hash merkle_hash) then
                light_failwith pgi
                @@ Format.asprintf
                     "Hash of the irmin tree %a does not correspond to hash of \
                      the merkle tree %a"
                     Context_hash.pp
                     tree_hash
                     Context_hash.pp
                     merkle_hash
              else Lwt.return_ok res
          | Error msg -> light_failwith pgi msg)

    (* Don't update the irmin ref when looking for key, so as not to add the
       empty tree. *)
    let get_irmin_key () : irmin Lwt.t =
      let open Lwt_syntax in
      match !irmin_ref with
      | None ->
          let* repo = Store.Tree.make_repo () in
          let root = Store.Tree.empty Store.empty in
          let irmin = {repo; root} in
          Lwt.return irmin
      | Some res -> Lwt.return res

    let rec get_first_merkle_tree chain block key leaf_kind tried_endpoints_rev
        remaining_endpoints =
      let open Lwt_syntax in
      match remaining_endpoints with
      | [] -> Lwt.return_none
      | ((uri, rpc_context) as hd_endpoint) :: tl_remaining_endpoints -> (
          let* raw_context =
            Light_proto.merkle_tree
              Proxy.{rpc_context; chain; block; mode = Client}
              key
              leaf_kind
          in
          match raw_context with
          | Ok (Some mtree) ->
              let other_endpoints =
                List.rev tried_endpoints_rev @ tl_remaining_endpoints
              in
              Lwt.return_some (mtree, other_endpoints)
          | Ok None ->
              (* Here we ignore an endpoint that succeeded but returned None
                 This means the endpoint's context does not map 'key'.
                 It's okay. *)
              let* () =
                printer#warning
                  "Light mode: endpoint %s does not map key %s (%s). Skipping \
                   it."
                  (Uri.to_string uri)
                  (key_to_string key)
                  (chain_n_block_to_string chain block)
              in
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
        (Block_services.merkle_tree * (Uri.t * RPC_context.simple) list) option
        Lwt.t =
      get_first_merkle_tree chain block key leaf_kind [] endpoints

    let get key =
      let open Lwt_syntax in
      let* () = Logger.(emit api_get @@ key_to_string key) in
      let* {root; _} = get_irmin_key () in
      Store.Tree.find_tree root key

    module Consensus = Light_consensus.Make (Light_proto)

    let do_rpc ({chain; block; _} as pgi : Proxy.proxy_getter_input) key =
      let open Lwt_result_syntax in
      let ( let** ) v f =
        Lwt.bind v (function
            | Error msg -> light_failwith pgi msg
            | Ok x -> f x)
      in
      let*! () = Logger.(emit api_do_rpc @@ key_to_string key) in
      let*! mtree_and_i_opt =
        get_first_merkle_tree chain block key Block_services.Raw_context
      in
      let nb_endpoints = List.length endpoints in
      match mtree_and_i_opt with
      | None ->
          light_failwith pgi
          @@ Format.sprintf
               "None of the %d endpoints could provide data for key: %s"
               nb_endpoints
               (key_to_string key)
      | Some (mtree, validating_endpoints) -> (
          let* {root; repo} = get_irmin_and_update_root pgi mtree in
          let** root' = Merkle.union_irmin_tree_merkle_tree repo root mtree in
          let*! () =
            Logger.(
              emit
                staged_data
                (key_to_string key, List.length validating_endpoints))
          in
          let input : Light_consensus.input =
            {printer; min_agreement; chain; block; key; mtree; tree = root'}
          in
          let*! r = Consensus.consensus input validating_endpoints in
          match r with
          | false ->
              light_failwith pgi ~warn_symbolic:true
              @@ Format.sprintf "Consensus cannot be reached for key: %s"
              @@ key_to_string key
          | true ->
              irmin_ref := Some {repo; root = root'} ;
              return_unit)
  end : Proxy.CORE)
