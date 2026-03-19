(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
module S = Dal_slot_repr
module Slot_index = Dal_slot_index_repr
module P = S.Page
module Hist = S.History
module Ihist = Hist.Internal_for_tests

(** Error used below for functions that don't return their failures in the monad
   error. *)
type error += Test_failure of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:(Protocol.name ^ "_test_failure")
    ~title:"Test failure"
    ~description:"Test failure."
    ~pp:(fun ppf e -> Format.fprintf ppf "Test failure: %s" e)
    (obj1 (req "error" string))
    (function Test_failure e -> Some e | _ -> None)
    (fun e -> Test_failure e)

let dal_prover_srs =
  lazy
    (Cryptobox.init_prover_dal
       ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
       ~fetch_trusted_setup:false
       ())

let mk_cryptobox dal_params =
  let open Lwt_result_syntax in
  let* () = Lazy.force dal_prover_srs in
  match Cryptobox.make dal_params with
  | Ok dal -> return dal
  | Error (`Fail s) -> fail [Test_failure s]

let derive_dal_parameters (reference : Cryptobox.parameters) ~redundancy_factor
    ~constants_divider =
  {
    S.Parameters.redundancy_factor;
    page_size = reference.page_size / constants_divider;
    slot_size = reference.slot_size / constants_divider;
    number_of_shards = reference.number_of_shards / constants_divider;
  }

let content_slot_id cell = (Hist.content_id cell).header_id

let dal_attestations slot_indexes =
  let open Alpha_context.Dal in
  let number_of_slots = Default_parameters.constants_test.dal.number_of_slots in
  let number_of_lags =
    List.length Default_parameters.constants_test.dal.attestation_lags
  in
  List.fold_left
    (fun acc slot_index ->
      Attestations.commit
        acc
        ~number_of_slots
        ~number_of_lags
        ~lag_index:(number_of_lags - 1)
        slot_index)
    Attestations.empty
    slot_indexes

let has_assigned_shards ctxt ?level pkh =
  let open Lwt_result_syntax in
  let* dal_committee = Context.Dal.shards ctxt ?level ~delegates:[pkh] () in
  match dal_committee with
  | [] -> return_false
  | [({delegate; _} : Plugin.RPC.Dal.S.shards_assignment)] ->
      assert (Signature.Public_key_hash.equal pkh delegate) ;
      return_true
  | _ -> fail [Test_failure "unexpected Dal.shards RPC result"]

module Make (Parameters : sig
  val dal_parameters : Alpha_context.Constants.Parametric.dal

  val cryptobox : Cryptobox.t tzresult Lwt.t Lazy.t
end) =
struct
  (* Some global constants. *)

  let params = Parameters.dal_parameters.cryptobox_parameters

  let cryptobox = Parameters.cryptobox

  let genesis_history = Hist.genesis

  let genesis_history_cache = Hist.History_cache.empty ~capacity:3000L

  let level_one = Raw_level_repr.(succ root)

  let level_ten = Raw_level_repr.(of_int32_exn 10l)

  (* Helper functions. *)

  let get_history cache h =
    Hist.History_cache.(Map.find h (view cache)) |> Lwt.return

  let dal_mk_polynomial_from_slot slot_data =
    let open Lwt_result_syntax in
    let* cryptobox = Lazy.force cryptobox in
    match Cryptobox.polynomial_from_slot cryptobox slot_data with
    | Ok p -> return p
    | Error (`Slot_wrong_size s) ->
        fail
          [
            Test_failure
              (Format.sprintf "polynomial_from_slot: Slot_wrong_size (%s)" s);
          ]

  let dal_commit cryptobox polynomial =
    let open Result_syntax in
    match Cryptobox.commit cryptobox polynomial with
    | Ok cm -> return cm
    | Error
        ((`Invalid_degree_strictly_less_than_expected _ | `Prover_SRS_not_loaded)
         as commit_error) ->
        fail [Test_failure (Cryptobox.string_of_commit_error commit_error)]

  let dal_mk_prove_page polynomial page_id =
    let open Lwt_result_syntax in
    let* cryptobox = Lazy.force cryptobox in
    match Cryptobox.prove_page cryptobox polynomial page_id.P.page_index with
    | Ok p -> return p
    | Error `Page_index_out_of_range ->
        fail [Test_failure "compute_proof_segment: Page_index_out_of_range"]
    | Error
        ((`Invalid_degree_strictly_less_than_expected _ | `Prover_SRS_not_loaded)
         as commit_error) ->
        fail [Test_failure (Cryptobox.string_of_commit_error commit_error)]

  let mk_slot ?(level = level_one) ?(index = Slot_index.zero)
      ?(fill_function = fun _i -> 'x') () =
    let open Lwt_result_syntax in
    let slot_data = Bytes.init params.slot_size fill_function in
    let* polynomial = dal_mk_polynomial_from_slot slot_data in
    let* cryptobox = Lazy.force cryptobox in
    let*? commitment = dal_commit cryptobox polynomial in
    return
      ( slot_data,
        polynomial,
        S.Header.{id = {published_level = level; index}; commitment} )

  let mk_page_id published_level slot_index page_index =
    P.{slot_id = {published_level; index = slot_index}; page_index}

  let no_data = Some (fun ~default_char:_ _ -> None)

  let mk_page_info ?(default_char = 'x') ?level ?slot_index
      ?(page_index = P.Index.zero) ?(custom_data = None) (slot : S.Header.t)
      polynomial =
    let open Lwt_result_syntax in
    let level =
      match level with None -> slot.id.published_level | Some level -> level
    in
    let slot_index =
      match slot_index with None -> slot.id.index | Some index -> index
    in
    let page_id = mk_page_id level slot_index page_index in
    let* page_proof = dal_mk_prove_page polynomial page_id in
    match custom_data with
    | None ->
        let page_data = Bytes.make params.page_size default_char in
        return (Some (page_data, page_proof), page_id)
    | Some mk_data -> (
        match mk_data ~default_char params.page_size with
        | None -> return (None, page_id)
        | Some page_data -> return (Some (page_data, page_proof), page_id))

  let succ_slot_index index =
    Option.value_f
      Slot_index.(
        of_int_opt
          ~number_of_slots:Parameters.dal_parameters.number_of_slots
          (to_int index + 1))
      ~default:(fun () -> Slot_index.zero)

  let next_char c = Char.(chr ((code c + 1) mod 255))

  (** Auxiliary test function used by both unit and PBT tests: This function
      produces a proof from the given information and verifies the produced result,
      if any. The result of each step is checked with [check_produce_result] and
      [check_verify_result], respectively. *)
  let produce_and_verify_proof ~check_produce ?check_verify ~get_history
      skip_list ~page_info ~page_id =
    let open Lwt_result_wrap_syntax in
    let*!@ res =
      Hist.produce_proof
        params
        ~precheck_lag:(Lag_interval (0, Hist.legacy_attestation_lag))
        ~page_id_is_valid:(fun ~lag_check:_ _page_id -> true)
        ~page_info
        ~attestation_threshold_percent:None
        ~restricted_commitments_publishers:None
        page_id
        ~get_history
        skip_list
    in
    let* () = check_produce res page_info in
    match check_verify with
    | None -> return_unit
    | Some check_verify ->
        let*? proof, _input_opt = res in
        let@ res =
          Hist.verify_proof
            params
            ~precheck_lag:(Lag_interval (0, Hist.legacy_attestation_lag))
            ~page_id_is_valid:(fun ~lag_check:_ _page_id -> true)
            page_id
            skip_list
            proof
        in
        check_verify res page_info

  (* Some check functions. *)

  (** Check that/if the returned content is the expected one. *)
  let assert_content_is ~__LOC__ ~expected returned =
    Assert.equal
      ~loc:__LOC__
      (Option.equal Bytes.equal)
      "Returned %s doesn't match the expected one"
      (fun fmt opt ->
        match opt with
        | None -> Format.fprintf fmt "<None>"
        | Some bs -> Format.fprintf fmt "<Some:%s>" (Bytes.to_string bs))
      returned
      expected

  let expected_data page_info proof_status =
    match (page_info, proof_status) with
    | Some (d, _p), `Confirmed -> Some d
    | None, `Confirmed -> assert false
    | _ -> None

  let proof_status_to_string = function
    | `Confirmed -> "CONFIRMED"
    | `Unconfirmed -> "UNCONFIRMED"

  let successful_check_produce_result ~__LOC__ proof_status res page_info =
    let open Lwt_result_syntax in
    let* proof, input_opt = Assert.get_ok ~__LOC__ res in
    let* () =
      if Hist.Internal_for_tests.proof_statement_is proof proof_status then
        return_unit
      else
        failwith
          "Expected to have a %s page proof. Got %a@."
          (proof_status_to_string proof_status)
          (Hist.pp_proof ~serialized:false)
          proof
    in
    assert_content_is
      ~__LOC__
      input_opt
      ~expected:(expected_data page_info proof_status)

  let failing_check_produce_result ~__LOC__ ~expected_error res _page_info =
    Assert.proto_error ~loc:__LOC__ res (fun e ->
        match (e, expected_error) with
        | Hist.Dal_page_proof_error s, Hist.Dal_page_proof_error expected ->
            String.equal s expected
        | ( Hist.Unexpected_page_size {expected_size = e1; page_size = p1},
            Hist.Unexpected_page_size {expected_size = e2; page_size = p2} ) ->
            e1 = e2 && p1 = p2
        | _ -> false)

  let successful_check_verify_result ~__LOC__ proof_status res page_info =
    let open Lwt_result_syntax in
    let* content = Assert.get_ok ~__LOC__ res in
    let expected = expected_data page_info proof_status in
    assert_content_is ~__LOC__ ~expected content

  (** Checks if the two provided Page.proof are equal. *)
  let eq_page_proof =
    let bytes_opt_of_proof page_proof =
      Data_encoding.Binary.to_bytes_opt P.proof_encoding page_proof
    in
    fun pp1 pp2 ->
      Option.equal Bytes.equal (bytes_opt_of_proof pp1) (bytes_opt_of_proof pp2)

  let slot_confirmed_but_page_data_not_provided ~__LOC__ =
    failing_check_produce_result
      ~__LOC__
      ~expected_error:
        (Hist.Dal_page_proof_error
           "The page ID's slot is confirmed, but no page content and proof are \
            provided.")

  let slot_not_confirmed_but_page_data_provided ~__LOC__ =
    failing_check_produce_result
      ~__LOC__
      ~expected_error:
        (Hist.Dal_page_proof_error
           "The page ID's slot is not confirmed, but page content and proof \
            are provided.")

  let bad_history_cache ~__LOC__ =
    failing_check_produce_result
      ~__LOC__
      ~expected_error:
        (Hist.Dal_page_proof_error
           "Skip_list.search returned Nearest', while all given levels to \
            produce proofs are supposed to be in the skip list.")
end

let dal_content_of_z z =
  let open Lwt_result_wrap_syntax in
  let*?@ attestations =
    Alpha_context.Dal.Attestations.Internal_for_tests.of_z z
  in
  return Alpha_context.{attestations}

let dal_content_of_int_list
    ?(number_of_slots = Default_parameters.constants_test.dal.number_of_slots)
    attested_slots =
  let open Alpha_context in
  let number_of_lags =
    List.length Default_parameters.constants_test.dal.attestation_lags
  in
  let dal_attestations =
    List.fold_left
      (fun dal_attestations slot ->
        match Dal.Slot_index.of_int ~number_of_slots slot with
        | Ok slot_index ->
            Dal.Attestations.commit
              dal_attestations
              ~number_of_slots
              ~number_of_lags
              ~lag_index:(number_of_lags - 1)
              slot_index
        | Error err ->
            Test.fail ~__LOC__ "%a" Environment.Error_monad.pp_trace err)
      Dal.Attestations.empty
      attested_slots
  in
  {attestations = dal_attestations}

let various_dal_contents =
  let number_of_slots = Default_parameters.constants_test.dal.number_of_slots in
  None
  :: List.map
       (fun l -> Some (dal_content_of_int_list ~number_of_slots l))
       [[]; [0]; [1; 3]; Misc.(0 --> (number_of_slots - 1))]

(* Associates each committee member with an element of
   dal_content_list in order, going back to the beginning of
   dal_content_list if dal_content_list is shorter. In other words,
   the member at position i in committee receives the dal_content in
   dal_content_list at position (i modulo (length of
   dal_content_list)). *)
let committee_with_cycling_dal_contents
    (dal_content_list : Alpha_context.dal_content option list)
    (committee : 'a list) : ('a * Alpha_context.dal_content option) list =
  let rec aux acc remaining_dal_contents_to_pick committee =
    match (committee, remaining_dal_contents_to_pick) with
    | [], _ -> List.rev acc
    | _, [] ->
        (* We have gone through the whole dal_content_list and there
           are still committee members waiting to receive a dal
           content: reset remaining_dal_contents_to_pick to
           dal_content_list. *)
        aux acc dal_content_list committee
    | member :: rest_committee, dal :: rest_dal ->
        aux ((member, dal) :: acc) rest_dal rest_committee
  in
  aux [] dal_content_list committee

let committee_with_various_dal_contents committee =
  committee_with_cycling_dal_contents various_dal_contents committee
