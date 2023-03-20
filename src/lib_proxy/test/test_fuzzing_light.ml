(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Client
    Invocation:   dune exec src/lib_proxy/test/main.exe
    Dependencies: src/lib_proxy/test/light_lib.ml
                  src/lib_proxy/test/test_light.ml
    Description:  Most generators in this module are recursive / nested, hence
                  width and depth of structures is fine-tuned.
*)

module Store = Tezos_context_memory.Context
module Proof = Tezos_context_sigs.Context.Proof_types
open Qcheck2_helpers

open Tezos_proxy_test_helpers_shell_services.Test_helpers_shell_services

module Consensus = struct
  let chain, block = (`Main, `Head 0)

  class mock_rpc_context : Tezos_rpc.Context.simple =
    object
      method call_service
          : 'm 'p 'q 'i 'o.
            (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
            'p ->
            'q ->
            'i ->
            'o tzresult Lwt.t =
        assert false
    end

  let mk_rogue_tree (mproof : Proof.tree Proof.t) (seed : int list) :
      (Proof.tree Proof.t, string) result =
    let tree_proof_eq = Proof.Internal_for_tests.tree_proof_eq in
    let rec gen_rec ~rand attempts_left =
      if attempts_left = 0 then Error "mk_rogue_tree: giving up"
      else
        let gen = merkle_proof_gen in
        let generated, _, _ = QCheck2.Gen.generate1 ~rand gen in
        if tree_proof_eq mproof generated then gen_rec ~rand (attempts_left - 1)
        else Ok generated
    in
    let rand = Random.State.make (Array.of_list seed) in
    gen_rec ~rand 128

  (* [mock_light_rpc mproof [(endpoint1, true); (endpoint2, false)] seed]
     returns an instance of [Tezos_proxy.Light_proto.PROTO_RPCS]
     that always returns a rogue (illegal) variant of [mproof] when querying [endpoint1],
     [mproof] when querying [endpoint2], and [None] otherwise *)
  let mock_light_rpc mproof endpoints_and_rogueness seed =
    (module struct
      (** Use physical equality on [rpc_context] because they are identical objects. *)
      let merkle_tree (pgi : Tezos_proxy.Proxy.proxy_getter_input) _ _ =
        List.assq pgi.rpc_context endpoints_and_rogueness
        |> Option.map (fun is_rogue ->
               if is_rogue then
                 match mk_rogue_tree mproof seed with
                 | Ok rogue_mtree -> rogue_mtree
                 | _ -> QCheck2.assume_fail ()
               else mproof)
        |> Lwt.return_ok
    end : Tezos_proxy.Light_proto.PROTO_RPCS)

  let mock_printer () =
    let rev_logs : string list ref = ref [] in
    object
      inherit
        Tezos_client_base.Client_context.simple_printer
          (fun _channel log ->
            rev_logs := log :: !rev_logs ;
            Lwt.return_unit)

      method get_logs = List.rev !rev_logs
    end

  (* used for debugging *)
  let _print_keys l =
    let l = List.map (fun s -> "\"" ^ s ^ "\"") l in
    "[" ^ String.concat "; " l ^ "]"

  (** [test_consensus min_agreement nb_honest nb_rogue key mproof randoms consensus_expected]
      checks that a consensus run with [nb_honest] honest nodes (i.e. that return [mproof] when requesting [key]),
      [nb_rogue] rogue nodes (i.e. that falsify data with the [mk_rogue_*] functions when requesting [key])
      returns [consensus_expected]. [randoms] is used to inject randomness in the rogue behaviour. *)
  let test_consensus min_agreement nb_honest nb_rogue key mproof randoms
      consensus_expected =
    let open Lwt_syntax in
    assert (nb_honest >= 0) ;
    assert (nb_rogue >= 0) ;
    let honests = List.repeat nb_honest false in
    let rogues = List.repeat nb_rogue true in
    let endpoints_and_rogueness =
      List.map
        (fun is_rogue -> (new mock_rpc_context, is_rogue))
        (honests @ rogues)
    in
    let (module Light_proto) =
      mock_light_rpc mproof endpoints_and_rogueness randoms
    in
    let module Consensus = Tezos_proxy.Light_consensus.Make (Light_proto) in
    let printer = mock_printer () in
    let input : Tezos_proxy.Light_consensus.input =
      {
        printer = (printer :> Tezos_client_base.Client_context.printer);
        min_agreement;
        chain;
        block;
        key;
        mproof;
      }
    in
    let validating_endpoints =
      List.mapi
        (fun n (endpoint, _is_rogue) ->
          let uri = Printf.sprintf "http://foobar:%d" n |> Uri.of_string in
          (uri, endpoint))
        endpoints_and_rogueness
    in
    let+ consensus_reached = Consensus.consensus input validating_endpoints in
    qcheck_eq ~pp:Format.pp_print_bool consensus_expected consensus_reached
end

let add_test_consensus (min_agreement, honest, rogue, consensus_expected) =
  let open QCheck2 in
  (* Because the node providing data always agrees, [honest] must be > 0 *)
  assert (honest > 0) ;
  (* Because we test consensus, to which the node providing data
     doesn't participate: *)
  let honest = honest - 1 in
  Test.make
    ~name:
      (Printf.sprintf
         "min_agreement=%f, honest=%d, rogue=%d consensus_expected=%b"
         min_agreement
         honest
         rogue
         consensus_expected)
    ~print:Print.(pair print_merkle_proof (list int))
    Gen.(pair merkle_proof_gen (small_list int))
  @@ fun ((mproof, _, key), randoms) ->
  Consensus.test_consensus
    min_agreement
    honest
    rogue
    key
    mproof
    randoms
    consensus_expected
  |> Lwt_main.run

let test_consensus_spec =
  let open QCheck2 in
  let open Gen in
  let min_agreement_gen = 0 -- 100 in
  let honest_gen = 1 -- 1000 in
  let rogue_gen = 0 -- 1000 in
  Test.make
    ~name:
      "test_consensus min_agreement honest rogue ... = min_agreeing_endpoints \
       min_agreement (honest + rogue + 1) <= honest"
    ~print:
      Print.(pair (triple int int int) (pair print_merkle_proof (list int)))
    (pair
       (triple min_agreement_gen honest_gen rogue_gen)
       (pair merkle_proof_gen (small_list int)))
  @@ fun ((min_agreement_int, honest, rogue), ((mproof, _, key), seed)) ->
  assert (0 <= min_agreement_int && min_agreement_int <= 100) ;
  let min_agreement = Float.of_int min_agreement_int /. 100. in
  assert (0.0 <= min_agreement && min_agreement <= 1.0) ;
  assert (0 < honest && honest <= 1024) ;
  assert (0 <= rogue && rogue <= 1024) ;
  let consensus_expected =
    (* +1 because there's the endpoint providing data, which always agrees *)
    let honest = honest + 1 in
    let nb_endpoints = honest + rogue in
    honest
    >= Tezos_proxy.Light_consensus.min_agreeing_endpoints
         min_agreement
         nb_endpoints
  in
  Consensus.test_consensus
    min_agreement
    honest
    rogue
    key
    mproof
    seed
    consensus_expected
  |> Lwt_main.run

let () =
  Alcotest.run
    "Mode Light"
    [
      ( "Consensus consistency examples",
        (* These tests are kinda superseded by the fuzzing tests
           ([test_consensus_spec]) below. However, I want to keep them for
           documentation purposes, because they provide examples. In addition,
           if tests break in the future, these ones will be easier to
           debug than the most general ones. *)
        qcheck_wrap ~rand:(Random.State.make [|348980449|])
        @@ List.map
             add_test_consensus
             [
               (* min_agreement, nb honest nodes, nb rogue nodes, consensus expected *)
               (1.0, 2, 0, true);
               (1.0, 3, 0, true);
               (1.0, 4, 0, true);
               (1.0, 2, 1, false);
               (* Next one should fail because 3*0.7 |> ceil == 3 whereas only 2 nodes agree *)
               (0.7, 2, 1, false);
               (0.7, 1, 2, false);
               (0.7, 1, 3, false);
               (0.01, 1, 1, true);
               (0.01, 1, 2, true);
               (* Passes because 0.01 *. (1 + 99) |> ceil == 1 and the node providing data is always there *)
               (0.01, 1, 99, true);
               (* But then 0.01 *. (1 + 100) |> ceil == 2: *)
               (0.01, 1, 100, false);
               (0.6, 2, 1, true);
               (0.6, 3, 1, true);
               (0.6, 4, 1, true);
               (0.6, 5, 1, true);
               (0.5, 2, 2, true);
               (0.01, 1, 2, true);
             ] );
      ("Consensus consistency", qcheck_wrap [test_consensus_spec]);
    ]
