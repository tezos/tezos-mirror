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
    Invocation:   dune build @src/lib_proxy/runtest
    Dependencies: src/lib_proxy/test/light_lib.ml
                  src/lib_proxy/test/test_light.ml
    Description:  Most generators in this module are recursive / nested, hence
                  width and depth of structures is fine-tuned.
*)

module Internal = Tezos_proxy.Light_internal
module Merkle = Internal.Merkle
module Store = Tezos_proxy.Local_context
open Lib_test.Qcheck2_helpers

open Tezos_shell_services_test_helpers.Shell_services_test_helpers

(** [list1_gen gen] generates non-empty lists using [gen]. *)
let list1_gen gen =
  QCheck2.Gen.(
    list_size (1 -- 20) gen |> add_shrink_invariant (fun l -> l <> []))

let irmin_tree_gen =
  let module StringList = struct
    type t = string list

    let compare = Stdlib.compare
  end in
  let module StringListMap = Stdlib.Map.Make (StringList) in
  let open MakeMapGen (StringListMap) in
  let open QCheck2.Gen in
  let+ entries = small_list (pair (small_list string) bytes_gen) in
  List.fold_left_s
    (fun built_tree (path, bytes) -> Store.Tree.add built_tree path bytes)
    (Store.Tree.empty Store.empty)
    entries
  |> Lwt_main.run

let print_tree = Format.asprintf "%a" Store.Tree.pp

let get_ok = function Ok x -> x | Error s -> QCheck2.Test.fail_report s

(** Test that [merkle_tree_to_irmin_tree] preserves the tree's structure
    by checking that it yields the same [simple_tree]
    as when using [merkle_tree_to_simple_tree]
  *)
let test_merkle_tree_to_irmin_tree_preserves_simple_tree =
  QCheck2.Test.make
    ~name:
      "merkle_tree_to_irmin_tree mtree |> irmin_tree_to_simple_tree = \
       merkle_tree_to_simple_tree mtree"
    ~print:print_merkle_tree
    merkle_tree_gen
  @@ fun mtree ->
  let repo = Lwt_main.run (Store.Tree.make_repo ()) in
  let merkle_irmin_tree =
    Lwt_main.run @@ Merkle.merkle_tree_to_irmin_tree repo mtree |> get_ok
  in
  let of_irmin_tree =
    Lwt_main.run @@ Light_lib.irmin_tree_to_simple_tree merkle_irmin_tree
  in
  (* Because Irmin does not add empty subtrees, [merkle_tree_to_irmin_tree]
     removes empty subtrees internally. We simulate the same behavior
     before calling [merkle_tree_to_simple_tree] (that doesn't go through
     Irmin APIs, and hence doesn't remove empty subtrees internally). *)
  let of_merkle_tree =
    Light_lib.merkle_tree_to_simple_tree @@ Light_lib.merkle_tree_rm_empty mtree
  in
  Light_lib.check_simple_tree_eq of_irmin_tree of_merkle_tree

let filter_none : 'a option list -> 'a list = List.filter_map Fun.id

let rec remove_data_in_node =
  let open Tezos_shell_services.Block_services in
  function
  | Hash _ as x -> Some x
  | Data _ -> None
  | Continue mtree ->
      let mtree' = remove_data_in_tree mtree in
      if String.Map.is_empty mtree' then None else Some (Continue mtree')

and remove_data_in_tree mtree =
  let pairs = String.Map.bindings mtree in
  let pairs' = Light_lib.Bifunctor.second remove_data_in_node pairs in
  let lift_opt (x, y_opt) =
    match y_opt with None -> None | Some y -> Some (x, y)
  in
  let pairs'' = List.map lift_opt pairs' |> filter_none in
  List.to_seq pairs'' |> String.Map.of_seq

(** Test that translating a [merkle_tree] to an Irmin tree yields
    an Irmin tree that is included in the original [merkle_tree].
    This function specifically tests function [merkle_tree_to_irmin_tree]. *)
let test_contains_merkle_tree =
  QCheck2.Test.make
    ~name:"contains_merkle_tree (merkle_tree_to_irmin_tree mtree) mtree = true"
    ~print:print_merkle_tree
    merkle_tree_gen
  @@ fun mtree ->
  (* Because contains_merkle_tree doesn't support Data nodes, we need to
     remove them. That's because contains_merkle_tree is only called
     during the consensus phase, in which there should not be Data nodes
     (there should only be hashes). *)
  let mtree = remove_data_in_tree mtree in
  let repo = Lwt_main.run (Store.Tree.make_repo ()) in
  let irmin_tree =
    Lwt_main.run @@ Merkle.merkle_tree_to_irmin_tree repo mtree |> get_ok
  in
  let contains_res =
    Lwt_main.run @@ Merkle.contains_merkle_tree irmin_tree mtree
  in
  match contains_res with
  | Ok _ -> true
  | Error msg -> QCheck2.Test.fail_report msg

(** Test that unioning an empty irmin tree and a merkle tree should yield
    the same irmin tree as if it was built directly from the merkle tree *)
let test_union_irmin_empty =
  QCheck2.Test.make
    ~name:
      "union_irmin_tree_merkle_tree empty mtree = merkle_tree_to_irmin_tree \
       mtree"
    ~print:print_merkle_tree
    merkle_tree_gen
  @@ fun mtree ->
  let repo = Lwt_main.run (Store.Tree.make_repo ()) in
  let direct_tree =
    Lwt_main.run @@ Merkle.merkle_tree_to_irmin_tree repo mtree |> get_ok
  in
  let shallow_tree = Store.shallow_of_tree repo direct_tree in
  let union_tree =
    Lwt_main.run @@ Merkle.union_irmin_tree_merkle_tree repo shallow_tree mtree
    |> get_ok
  in
  Light_lib.check_irmin_tree_eq direct_tree union_tree

(** Test that unioning an irmin tree - built by converting a merkle tree -
    and a merkle tree, yields the merkle tree.
    Tests both [Merkle.merkle_tree_to_irmin_tree]
    and [Merkle.union_irmin_tree_merkle_tree] *)
let test_union_translation =
  QCheck2.Test.make
    ~name:
      "union_irmin_tree_merkle_tree (merkle_tree_to_irmin_tree mtree) mtree = \
       merkle_tree_to_irmin_tree mtree"
    ~print:print_merkle_tree
    merkle_tree_gen
  @@ fun mtree ->
  let repo = Lwt_main.run (Store.Tree.make_repo ()) in
  let direct_tree =
    Lwt_main.run @@ Merkle.merkle_tree_to_irmin_tree repo mtree |> get_ok
  in
  (* union shouldn't do anything, because the irmin tree given ([direct_tree])
     already contains all content from [mtree] *)
  let id_union_tree =
    Lwt_main.run @@ Merkle.union_irmin_tree_merkle_tree repo direct_tree mtree
    |> get_ok
  in
  Light_lib.check_irmin_tree_eq direct_tree id_union_tree

let rec union_merkle_node n1 n2 =
  let open Tezos_shell_services.Block_services in
  match (n1, n2) with
  | (Hash h1, Hash h2) when h1 = h2 -> Some n1
  | (Data raw_context1, Data raw_context2) when raw_context1 = raw_context2 ->
      Some n1
  | (Continue mtree1, Continue mtree2) -> (
      match union_merkle_tree mtree1 mtree2 with
      | None -> None
      | Some u -> Some (Continue u))
  | _ -> None

and union_merkle_tree t1 t2 =
  let conflict = ref false in
  let merge =
    String.Map.union
      (fun _key val1 val2 ->
        let node = union_merkle_node val1 val2 in
        if Option.is_none node then conflict := true ;
        node)
      t1
      t2
  in
  if !conflict then None else Some merge

(** Test that unioning [Merkle.union_irmin_tree_merkle_tree] yields
    the same result as [union_merkle_tree]

    This test is commented out because the intermediate tree [irmin_union1] is
    an inconsistent subpart a valid merkle tree. Only the union of
    [irmin_union1] and [mtree2] is a valid merkle tree, but because these are
    added in two steps, the second call to [union_irmin_tree_merkle_tree]
    fails. *)
let _test_union_direct =
  let open QCheck2 in
  Test.make
    ~name:
      "union_irmin_tree_merkle_tree (merkle_tree_to_irmin_tree mtree) mtree = \
       merkle_tree_to_irmin_tree mtree"
    ~print:(Print.pair print_merkle_tree print_merkle_tree)
    (Gen.pair merkle_tree_gen merkle_tree_gen)
  @@ fun (mtree1, mtree2) ->
  match union_merkle_tree mtree1 mtree2 with
  | None ->
      (* trees are incompatible *)
      assume_fail ()
  | Some merkle_union ->
      let repo = Lwt_main.run (Store.Tree.make_repo ()) in
      let irmin_union1 =
        Lwt_main.run
        @@ Merkle.union_irmin_tree_merkle_tree
             repo
             (Store.Tree.empty Store.empty)
             mtree1
        |> get_ok
      in
      let irmin_union12 =
        Lwt_main.run
        @@ Merkle.union_irmin_tree_merkle_tree repo irmin_union1 mtree2
        |> get_ok
      in
      let irmin_direct =
        Lwt_main.run @@ Merkle.merkle_tree_to_irmin_tree repo merkle_union
        |> get_ok
      in
      Light_lib.check_irmin_tree_eq irmin_union12 irmin_direct

(** Test that [Merkle.union_irmin_tree_merkle_tree] commutes i.e.
    that [Merkle.union_irmin_tree_merkle_tree t1 t2] yields the same
    value as [Merkle.union_irmin_tree_merkle_tree t2 t1].

    Commented out for similar reasons as [test_union_direct] above: we cannot
    build trees that correspond to valid merkle trees in two steps. *)
let _test_union_commutation =
  let open QCheck2 in
  Test.make
    ~name:
      "union_irmin_tree_merkle_tree (union_irmin_tree_merkle_tree empty \
       mtree1) mtree2 = union_irmin_tree_merkle_tree \
       (union_irmin_tree_merkle_tree empty mtree2) mtree1"
    ~print:(Print.pair print_merkle_tree print_merkle_tree)
    (Gen.pair merkle_tree_gen merkle_tree_gen)
  @@ fun (mtree1, mtree2) ->
  match union_merkle_tree mtree1 mtree2 with
  | None ->
      (* rule out incompatible trees *)
      assume_fail ()
  | Some _ ->
      let repo = Lwt_main.run (Store.Tree.make_repo ()) in
      let union2 t1 t2 =
        let intermediate =
          Lwt_main.run
          @@ Merkle.union_irmin_tree_merkle_tree
               repo
               (Store.Tree.empty Store.empty)
               t1
          |> get_ok
        in
        Lwt_main.run @@ Merkle.union_irmin_tree_merkle_tree repo intermediate t2
        |> get_ok
      in
      let union_12 = union2 mtree1 mtree2 in
      let union_21 = union2 mtree2 mtree1 in
      Light_lib.check_irmin_tree_eq union_12 union_21

(** Test that unioning an irmin tree with an empty merkle tree yield
    the input irmin tree *)
let test_union_merkle_empty =
  QCheck2.Test.make
    ~name:"union_irmin_tree_merkle_tree tree empty = tree"
    irmin_tree_gen
  @@ fun tree ->
  let repo = Lwt_main.run (Store.Tree.make_repo ()) in
  let res =
    Lwt_main.run
    @@ Merkle.union_irmin_tree_merkle_tree repo tree String.Map.empty
    |> get_ok
  in
  Light_lib.check_irmin_tree_eq tree res

(** Test that comparing the tree shape correctly ignores the key *)
let test_shape_ignores_key =
  let open QCheck2 in
  Test.make
    ~name:"trees_shape_match ignores the key"
    ~print:
      Print.(
        quad print_merkle_tree (list string) print_merkle_node print_merkle_node)
    Gen.(quad merkle_tree_gen (list string) merkle_node_gen merkle_node_gen)
  @@ fun (tree, key, node1, node2) ->
  let open Tezos_shell_services.Block_services in
  let is_continue = function Continue _ -> true | _ -> false in
  (* If both are [Continue] then they are trees with child nodes, hence
     shape comparison will fail. *)
  assume @@ not (is_continue node1 && is_continue node2) ;
  let rec deep_add current_key value mtree =
    match current_key with
    | [last_fragment] -> String.Map.add last_fragment value mtree
    | hd_key :: tl_key ->
        String.Map.update
          hd_key
          (fun mnode_opt ->
            let subtree =
              match mnode_opt with
              | Some (Continue subtree) -> subtree
              | _ -> String.Map.empty
            in
            Some (Continue (deep_add tl_key value subtree)))
          mtree
    | [] -> mtree
  in
  let tree1 = deep_add key node1 tree in
  let tree2 = deep_add key node2 tree in
  let result = Internal.Merkle.trees_shape_match key tree1 tree2 in
  qcheck_eq'
    ~pp:
      Format.(
        pp_print_result
          ~ok:(fun ppf () -> Format.fprintf ppf "()")
          ~error:(pp_print_list pp_print_string))
    ~expected:(Ok ())
    ~actual:result
    ()

module HashStability = struct
  let make_tree_shallow repo tree =
    let hash = Store.Tree.hash tree in
    let data =
      match Store.Tree.kind tree with
      | `Value -> `Value hash
      | `Tree -> `Node hash
    in
    Store.Tree.shallow repo data

  (** Sub-par pseudo-random shallower, based on the tree and sub-trees hashes.
      The resulting tree may or may not be shallowed (i.e. exactly the same as
      the input one). *)
  let rec make_partial_shallow_tree repo tree =
    let open Lwt_syntax in
    if (Store.Tree.hash tree |> Context_hash.hash) mod 2 = 0 then
      (* Full shallow *)
      Lwt.return @@ make_tree_shallow repo tree
    else
      (* Maybe shallow some sub-trees *)
      let* dir = Store.Tree.list tree [] in
      Lwt_list.fold_left_s
        (fun wip_tree (key, sub_tree) ->
          let* partial_shallowed_sub_tree =
            make_partial_shallow_tree repo sub_tree
          in
          Store.Tree.add_tree wip_tree [key] partial_shallowed_sub_tree)
        tree
        dir

  (** Provides a tree and a potentially shallowed (partially, totally or not at all) equivalent tree.
      Randomization of shallowing is sub-par (based on tree hash) because
      otherwise it would be very difficult to provide shrinking. Note that
      this will no be a problem once QCheck provides integrated shrinking. *)
  let tree_and_shallow_gen =
    let open QCheck2.Gen in
    let repo = Lwt_main.run (Store.Tree.make_repo ()) in
    let+ tree = irmin_tree_gen in
    (tree, Lwt_main.run (make_partial_shallow_tree repo tree))

  let print_tree_and_shallow = QCheck2.Print.pair print_tree print_tree

  (** Test that replacing Irmin subtrees by their [Store.Tree.shallow]
      value leaves the top-level [Store.Tree.hash] unchanged.

      This test was also proposed to Irmin in
      https://github.com/mirage/irmin/pull/1291 *)
  let test_hash_stability =
    let open QCheck2 in
    Test.make
      ~name:"Shallowing trees does not change their top-level hash"
      ~print:print_tree_and_shallow
      tree_and_shallow_gen
    @@ fun (tree, shallow_tree) ->
    let hash = Store.Tree.hash tree in
    let shallow_hash = Store.Tree.hash shallow_tree in
    if Context_hash.equal hash shallow_hash then true
    else
      Test.fail_reportf
        "@[<v 2>Equality check failed!@,\
         expected:@,\
         %a@,\
         actual:@,\
         %a@,\
         expected hash:@,\
         %a@,\
         actual hash:@,\
         %a@]"
        Store.Tree.pp
        tree
        Store.Tree.pp
        shallow_tree
        Context_hash.pp
        hash
        Context_hash.pp
        shallow_hash
end

let check_tree_eq = qcheck_eq ~pp:Store.Tree.pp ~eq:Store.Tree.equal

module AddTree = struct
  (** Test that getting a tree that was just set returns this tree.

      This test was also proposed to Irmin in
      https://github.com/mirage/irmin/pull/1291 *)
  let test_add_tree =
    let open QCheck2 in
    Test.make
      ~name:
        "let tree' = Store.Tree.add_tree tree key at_key in \
         Store.Tree.find_tree tree' key = at_key"
      ~print:
        Print.(
          triple HashStability.print_tree_and_shallow (list string) print_tree)
      Gen.(
        triple
          HashStability.tree_and_shallow_gen
          (list1_gen string)
          irmin_tree_gen)
      (fun ( ((_, tree) : _ * Store.tree),
             (key : Store.key),
             (added : Store.tree) ) ->
        let tree' = Store.Tree.add_tree tree key added |> Lwt_main.run in
        let tree_opt_set_at_key =
          Store.Tree.find_tree tree' key |> Lwt_main.run
        in
        match tree_opt_set_at_key with
        | None -> check_tree_eq (Store.Tree.empty Store.empty) added
        | Some tree_set_at_key -> check_tree_eq added tree_set_at_key)
end

module Consensus = struct
  let (chain, block) = (`Main, `Head 0)

  class mock_rpc_context : RPC_context.simple =
    object
      method call_service
          : 'm 'p 'q 'i 'o.
            (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t ->
            'p ->
            'q ->
            'i ->
            'o tzresult Lwt.t =
        assert false
    end

  let mk_rogue_tree (mtree : Tezos_shell_services.Block_services.merkle_tree)
      (seed : int list) :
      (Tezos_shell_services.Block_services.merkle_tree, string) result =
    let merkle_tree_eq = Tezos_shell_services.Block_services.merkle_tree_eq in
    let rec gen_rec ~rand attempts_left =
      if attempts_left = 0 then Error "mk_rogue_tree: giving up"
      else
        let gen = merkle_tree_gen in
        let generated = QCheck2.Gen.generate1 ~rand gen in
        if merkle_tree_eq mtree generated then gen_rec ~rand (attempts_left - 1)
        else Ok generated
    in
    let rand = Random.State.make (Array.of_list seed) in
    if (not String.Map.(is_empty mtree)) && Random.State.int rand 10 = 0 then
      (* The empty tree is an important edge case, hence this conditional *)
      Ok String.Map.empty
    else gen_rec ~rand 128

  (* [mock_light_rpc mtree [(endpoint1, true); (endpoint2, false)] seed]
     returns an instance of [Tezos_proxy.Light_proto.PROTO_RPCS]
     that always returns a rogue (illegal) variant of [mtree] when querying [endpoint1],
     [mtree] when querying [endpoint2], and [None] otherwise *)
  let mock_light_rpc mtree endpoints_and_rogueness seed =
    (module struct
      (** Use physical equality on [rpc_context] because they are identical objects. *)
      let merkle_tree (pgi : Tezos_proxy.Proxy.proxy_getter_input) _ _ =
        List.assq pgi.rpc_context endpoints_and_rogueness
        |> Option.map (fun is_rogue ->
               if is_rogue then
                 match mk_rogue_tree mtree seed with
                 | Ok rogue_mtree -> rogue_mtree
                 | _ -> QCheck2.assume_fail ()
               else mtree)
        |> return
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

  (** [test_consensus min_agreement nb_honest nb_rogue key mtree randoms consensus_expected]
      checks that a consensus run with [nb_honest] honest nodes (i.e. that return [mtree] when requesting [key]),
      [nb_rogue] rogue nodes (i.e. that falsify data with the [mk_rogue_*] functions when requesting [key])
      returns [consensus_expected]. [randoms] is used to inject randomness in the rogue behaviour. *)
  let test_consensus min_agreement nb_honest nb_rogue key mtree randoms
      consensus_expected =
    let open Lwt_syntax in
    assert (nb_honest >= 0) ;
    assert (nb_rogue >= 0) ;
    (* Because the consensus algorithm expects the merkle tree not to contain
       data: *)
    let mtree = remove_data_in_tree mtree in
    let honests = List.repeat nb_honest false in
    let rogues = List.repeat nb_rogue true in
    let endpoints_and_rogueness =
      List.map
        (fun is_rogue -> (new mock_rpc_context, is_rogue))
        (honests @ rogues)
    in
    let (module Light_proto) =
      mock_light_rpc mtree endpoints_and_rogueness randoms
    in
    let module Consensus = Tezos_proxy.Light_consensus.Make (Light_proto) in
    let printer = mock_printer () in
    let repo = Lwt_main.run (Store.Tree.make_repo ()) in
    let* tree =
      Internal.Merkle.merkle_tree_to_irmin_tree repo mtree >|= get_ok
    in
    let input : Tezos_proxy.Light_consensus.input =
      {
        printer = (printer :> Tezos_client_base.Client_context.printer);
        min_agreement;
        chain;
        block;
        key;
        mtree;
        tree;
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
    ~print:Print.(triple print_merkle_tree (list string) (list int))
    Gen.(
      triple
        merkle_tree_gen
        (small_list (small_string ?gen:None))
        (small_list int))
  @@ fun (mtree, key, randoms) ->
  Consensus.test_consensus
    min_agreement
    honest
    rogue
    key
    mtree
    randoms
    consensus_expected
  |> Lwt_main.run

let test_consensus_spec =
  let open QCheck2 in
  let open Gen in
  let min_agreement_gen = 0 -- 100 in
  let honest_gen = 1 -- 1000 in
  let rogue_gen = 0 -- 1000 in
  let key_gen = small_list (small_string ?gen:None) in
  Test.make
    ~name:
      "test_consensus min_agreement honest rogue ... = min_agreeing_endpoints \
       min_agreement (honest + rogue + 1) <= honest"
    ~print:
      Print.(
        pair
          (quad int int int (list string))
          (pair print_merkle_tree (list int)))
    (pair
       (quad min_agreement_gen honest_gen rogue_gen key_gen)
       (pair merkle_tree_gen (small_list int)))
  @@ fun ((min_agreement_int, honest, rogue, key), (mtree, seed)) ->
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
    mtree
    seed
    consensus_expected
  |> Lwt_main.run

let () =
  Alcotest.run
    "Mode Light"
    [
      ( "Hash stability",
        qcheck_wrap [HashStability.test_hash_stability; AddTree.test_add_tree]
      );
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
      ( "Merkle tree to Irmin tree",
        qcheck_wrap
          [
            test_merkle_tree_to_irmin_tree_preserves_simple_tree;
            test_contains_merkle_tree;
            test_union_irmin_empty;
            test_union_translation;
            (* test_union_direct; *)
            (* test_union_commutation; *)
            test_union_merkle_empty;
          ] );
      ("Tree shape validation", qcheck_wrap [test_shape_ignores_key]);
    ]
