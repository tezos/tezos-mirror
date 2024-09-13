(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (validate manager)
    Invocation: dune exec src/proto_021_PsquebeC/lib_protocol/test/integration/validate/main.exe \
                  -- --file test_covalidity.ml
    Subject:    Validation of operation.
*)
open Validate_helpers

open Generator_descriptors
open Valid_operations_generators
open Protocol
open Alpha_context

let register_test =
  Tezt_helpers.register_test ~__FILE__ ~file_tags:["validation"; "operation"]

(** Values of number of bootstraps to create.*)

let default_nb_bootstrap = 7

let nb_permutations = 30

let op_of_voting_period : Voting_period.kind -> op_kind =
  let open Voting_period in
  function
  | Proposal -> KProposals
  | Exploration -> KBallotExp
  | Promotion -> KBallotProm
  | _ -> assert false

type seed_gen = Nonce | Vdf

let pp_seed fmt = function
  | Nonce -> Format.fprintf fmt "nonce"
  | Vdf -> Format.fprintf fmt "vdf"

let op_of_seed_gen = function Nonce -> KNonce | Vdf -> KVdf

let is_not_preattestation op =
  let open Protocol.Alpha_context in
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with Single (Preattestation _) -> false | _ -> true

module OpkindMap = Map.Make (struct
  type t = op_kind

  let compare = compare
end)

let partition_op_kind op_kinds =
  List.fold_left
    (fun map op_kind ->
      OpkindMap.update
        op_kind
        (function None -> Some 1 | Some c -> Some (succ c))
        map)
    OpkindMap.empty
    op_kinds

let print_candidates candidates =
  Format.printf
    "@\n@[<v 2>%d operations generated:@ %a@]@."
    (List.length candidates)
    Format.(
      pp_print_list ~pp_sep:pp_print_cut (fun fmt (op, c) ->
          Format.fprintf fmt "%d: %a" c pp_op_kind op))
    (List.map op_kind_of_packed_operation candidates
    |> partition_op_kind |> OpkindMap.bindings)

(** Test that for the set of covalid operations which kinds belongs to [ks] in a
    state, any permutation is covalid and can be baked into a valid block. *)
let covalid_permutation_and_bake ks nb_bootstrap =
  let open Lwt_result_syntax in
  let* state, candidates =
    covalid ks ~nb_bootstrap ~max_batch_size:Operation_generator.max_batch_size
  in
  print_candidates candidates ;
  let* () = sequential_validate state.block candidates in
  let rec loop = function
    | 0 -> return_unit
    | n ->
        let operations =
          QCheck2.Gen.shuffle_l candidates
          |> QCheck2.Gen.generate1
          |> List.sort Protocol.Alpha_context.Operation.compare_by_passes
          |> List.rev_filter is_not_preattestation
        in
        (* Ensure that we can validate and apply this permutation *)
        let* (_ : Block.t) =
          Block.bake ~allow_manager_failures:true state.block ~operations
        in
        loop (pred n)
  in
  loop nb_permutations

(** {2 Tests} *)

let name voting_period reveal =
  Format.asprintf
    "scenario: '%a' period, '%a' seed"
    Voting_period.pp_kind
    voting_period
    pp_seed
    reveal

(** Test [covalid_permutation_and_bake]. *)
let test_covalid voting_period seed_gen =
  Generators.wrap
    ~name:(name voting_period seed_gen)
    ~gen:QCheck2.Gen.unit
    (fun () ->
      let open Lwt_result_syntax in
      let ks =
        op_of_voting_period voting_period
        :: op_of_seed_gen seed_gen :: non_exclusive_kinds
      in
      let* () = covalid_permutation_and_bake ks default_nb_bootstrap in
      return_true)

let tests =
  (* Create a list of all permutation of voting period and all
     possible nonce generation *)
  let voting_periods = [Voting_period.Proposal; Exploration; Promotion] in
  let nonce_gens = [Nonce; Vdf] in
  List.fold_left
    (fun acc voting_period ->
      List.fold_left
        (fun acc nonce_gen -> test_covalid voting_period nonce_gen :: acc)
        acc
        nonce_gens)
    []
    voting_periods
  |> Qcheck2_helpers.qcheck_wrap_lwt

let () = List.iter (fun (s, _, f) -> register_test ~title:s f) tests
