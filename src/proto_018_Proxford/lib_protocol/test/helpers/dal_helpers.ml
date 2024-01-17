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

let mk_cryptobox dal_params =
  let open Result_syntax in
  let parameters =
    Cryptobox.Internal_for_tests.parameters_initialisation dal_params
  in
  let () = Cryptobox.Internal_for_tests.load_parameters parameters in
  match Cryptobox.make dal_params with
  | Ok dal -> return dal
  | Error (`Fail s) -> fail [Test_failure s]

let derive_dal_parameters (reference : Cryptobox.parameters) ~redundancy_factor
    ~constants_divider =
  {
    S.redundancy_factor;
    page_size = reference.page_size / constants_divider;
    slot_size = reference.slot_size / constants_divider;
    number_of_shards = reference.number_of_shards / constants_divider;
  }

module Make (Parameters : sig
  val dal_parameters : Alpha_context.Constants.Parametric.dal

  val cryptobox : Cryptobox.t Lazy.t
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

  let get_history cache h = Hist.History_cache.find h cache |> Lwt.return

  let dal_mk_polynomial_from_slot slot_data =
    let open Result_syntax in
    let cryptobox = Lazy.force cryptobox in
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
    let open Result_syntax in
    let cryptobox = Lazy.force cryptobox in
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
    let open Result_syntax in
    let slot_data = Bytes.init params.slot_size fill_function in
    let* polynomial = dal_mk_polynomial_from_slot slot_data in
    let cryptobox = Lazy.force cryptobox in
    let* commitment = dal_commit cryptobox polynomial in
    return
      ( slot_data,
        polynomial,
        S.Header.{id = {published_level = level; index}; commitment} )

  let mk_page_id published_level slot_index page_index =
    P.{slot_id = {published_level; index = slot_index}; page_index}

  let no_data = Some (fun ~default_char:_ _ -> None)

  let mk_page_info ?(default_char = 'x') ?level ?(page_index = P.Index.zero)
      ?(custom_data = None) (slot : S.Header.t) polynomial =
    let open Result_syntax in
    let level =
      match level with None -> slot.id.published_level | Some level -> level
    in
    let page_id = mk_page_id level slot.id.index page_index in
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
      Hist.produce_proof params ~page_info page_id ~get_history skip_list
    in
    let* () = check_produce res page_info in
    match check_verify with
    | None -> return_unit
    | Some check_verify ->
        let*? proof, _input_opt = res in
        let@ res = Hist.verify_proof params page_id skip_list proof in
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
        | Hist.Dal_proof_error s, Hist.Dal_proof_error expected ->
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
        (Hist.Dal_proof_error
           "The page ID's slot is confirmed, but no page content and proof are \
            provided.")

  let slot_not_confirmed_but_page_data_provided ~__LOC__ =
    failing_check_produce_result
      ~__LOC__
      ~expected_error:
        (Hist.Dal_proof_error
           "The page ID's slot is not confirmed, but page content and proof \
            are provided.")
end
