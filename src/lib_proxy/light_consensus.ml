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

module Internal = Light_internal
module Store = Local_context
module Merkle = Internal.Merkle

type input = {
  printer : Tezos_client_base.Client_context.printer;
  min_agreement : float;
  chain : Tezos_shell_services.Block_services.chain;
  block : Tezos_shell_services.Block_services.block;
  key : string list;
  mtree : Tezos_shell_services.Block_services.merkle_tree;
  tree : Store.tree;
}

let min_agreeing_endpoints min_agreement nb_endpoints =
  min_agreement *. float_of_int nb_endpoints |> Float.ceil |> int_of_float

module Make (Light_proto : Light_proto.PROTO_RPCS) = struct
  type validation_result = Valid | Invalid of string

  (** Checks that the data-less merkle tree [incoming_mtree]
      provided by the endpoint [uri] meets these two conditions:

      * Its shape matches the shape of the tree provided by the
        endpoint providing data ([data_tree])
      * It agrees with the tree at [key] in [store_tree]. *)
  let validate uri key store_tree
      (data_tree : Tezos_shell_services.Block_services.merkle_tree)
      (incoming_mtree :
        Tezos_shell_services.Block_services.merkle_tree option tzresult) =
    match incoming_mtree with
    | Error trace ->
        Lwt.return
        @@ Invalid
             (Format.asprintf
                "Light mode: endpoint %s failed to provide merkle tree for key \
                 %s. Error is: %a"
                (Uri.to_string uri)
                (Internal.key_to_string key)
                pp_print_trace
                trace)
    | Ok None ->
        Lwt.return
        @@ Invalid
             (Format.asprintf
                "Light mode: endpoint %s doesn't contain key %s"
                (Uri.to_string uri)
                (Internal.key_to_string key))
    | Ok (Some mtree) -> (
        match Internal.Merkle.trees_shape_match key data_tree mtree with
        | Error errors ->
            assert (errors <> []) ;
            Lwt.return
            @@ Invalid
                 Format.(
                   asprintf
                     "@[<v 0>Shape of Merkle tree provided by endpoint %a \
                      doesn't match shape of data tree sent by the first \
                      endpoint:@,\
                      %a@]"
                     Uri.pp
                     uri
                     (pp_print_list pp_print_string)
                     errors)
        | Ok () -> (
            let open Lwt_syntax in
            let* r = Merkle.contains_merkle_tree store_tree mtree in
            match r with
            | Ok _ -> Lwt.return Valid
            | Error err ->
                Lwt.return
                @@ Invalid
                     (Format.sprintf
                        "When checking that local in-memory tree contains the \
                         merkle tree provided by endpoint %s: %s"
                        (Uri.to_string uri)
                        err)))

  let count_valids validations =
    let count_valid = function Valid -> 1 | Invalid _ -> 0 in
    List.fold_left
      (fun sum validation -> sum + count_valid validation)
      0
      validations

  let warn_invalids (printer : Tezos_client_base.Client_context.printer)
      validations =
    Lwt_list.iter_s
      (fun v ->
        match v with
        | Valid -> Lwt.return_unit
        | Invalid errmsg -> printer#warning "%s\n" errmsg)
      validations

  let consensus
      ({printer; min_agreement; chain; block; key; mtree; tree} : input)
      validating_endpoints =
    let open Lwt_syntax in
    (* + 1 because there's the endpoint that provides data, that doesn't
       validate *)
    let nb_endpoints = List.length validating_endpoints + 1 in
    let min_agreeing_endpoints =
      min_agreeing_endpoints min_agreement nb_endpoints
    in
    assert (min_agreeing_endpoints <= nb_endpoints) ;
    (* When checking that shapes agree, we must ignore the key where the
       data is, because the validating endpoints return trees that do NOT
       contain this key. *)
    let check_merkle_tree_with_endpoint (uri, rpc_context) =
      let* other_mtree =
        Light_proto.merkle_tree
          {rpc_context; chain; block; mode = Client}
          key
          Tezos_shell_services.Block_services.Hole
      in
      validate uri key tree mtree other_mtree
    in
    let* validations =
      Lwt_list.map_p check_merkle_tree_with_endpoint validating_endpoints
    in
    (* +1 because the endpoint that provided data obviously agrees *)
    let nb_agreements = count_valids validations + 1 in
    let agreement_reached = nb_agreements >= min_agreeing_endpoints in
    let* () = warn_invalids printer validations in
    let* () =
      if agreement_reached then Lwt.return_unit
      else
        printer#warning
          "Light mode: min_agreement=%f, %d endpoints, %s%d agreeing \
           endpoints, whereas %d (%d*%f rounded up) is the minimum; so about \
           to fail."
          min_agreement
          nb_endpoints
          (if nb_agreements > 0 then "only " else "")
          nb_agreements
          min_agreeing_endpoints
          nb_endpoints
          min_agreement
    in
    return agreement_reached
end
