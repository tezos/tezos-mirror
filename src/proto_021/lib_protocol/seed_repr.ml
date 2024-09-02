(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* Tezos Protocol Implementation - Random number generation *)

type seed = B of State_hash.t

type nonce = bytes

type vdf_setup = Vdf.discriminant * Vdf.challenge

type vdf_solution = Vdf.result * Vdf.proof

let seed_to_bytes x =
  let seed_to_state_hash (B b) = b in
  State_hash.to_bytes (seed_to_state_hash x)

let vdf_setup_encoding =
  let open Data_encoding in
  let vdf_discriminant_encoding =
    conv_with_guard
      Vdf.discriminant_to_bytes
      (fun b ->
        Option.to_result
          ~none:"VDF discriminant could not be deserialised"
          (Vdf.discriminant_of_bytes_opt b))
      (Fixed.(bytes Hex) Vdf.discriminant_size_bytes)
  in
  let vdf_challenge_encoding =
    conv_with_guard
      Vdf.challenge_to_bytes
      (fun b ->
        Option.to_result
          ~none:"VDF challenge could not be deserialised"
          (Vdf.challenge_of_bytes_opt b))
      (Fixed.(bytes Hex) Vdf.form_size_bytes)
  in
  tup2 vdf_discriminant_encoding vdf_challenge_encoding

let vdf_solution_encoding =
  let open Data_encoding in
  let vdf_result_encoding =
    conv_with_guard
      Vdf.result_to_bytes
      (fun b ->
        Option.to_result
          ~none:"VDF result could not be deserialised"
          (Vdf.result_of_bytes_opt b))
      (Fixed.(bytes Hex) Vdf.form_size_bytes)
  in
  let vdf_proof_encoding =
    conv_with_guard
      Vdf.proof_to_bytes
      (fun b ->
        Option.to_result
          ~none:"VDF proof could not be deserialised"
          (Vdf.proof_of_bytes_opt b))
      (Fixed.(bytes Hex) Vdf.form_size_bytes)
  in
  tup2 vdf_result_encoding vdf_proof_encoding

let pp_solution ppf solution =
  let result, proof = solution in
  Format.fprintf
    ppf
    "@[<v 2>VDF result: %a"
    Hex.pp
    (Hex.of_bytes (Vdf.result_to_bytes result)) ;
  Format.fprintf
    ppf
    "@,VDF proof: %a"
    Hex.pp
    (Hex.of_bytes (Vdf.proof_to_bytes proof)) ;
  Format.fprintf ppf "@]"

let nonce_encoding = Data_encoding.Fixed.(bytes Hex) Constants_repr.nonce_length

let zero_bytes = Bytes.make Nonce_hash.size '\000'

let state_hash_encoding =
  let open Data_encoding in
  conv
    State_hash.to_bytes
    State_hash.of_bytes_exn
    (Fixed.(bytes Hex) Nonce_hash.size)

let seed_encoding =
  let open Data_encoding in
  conv (fun (B b) -> b) (fun b -> B b) state_hash_encoding

let update_seed (B state) nonce =
  B (State_hash.hash_bytes [State_hash.to_bytes state; nonce])

type error += Unexpected_nonce_length (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"unexpected_nonce_length"
    ~title:"Unexpected nonce length"
    ~description:"Nonce length is incorrect."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Nonce length is not %i bytes long as it should."
        Constants_repr.nonce_length)
    Data_encoding.empty
    (function Unexpected_nonce_length -> Some () | _ -> None)
    (fun () -> Unexpected_nonce_length)

let make_nonce nonce =
  let open Result_syntax in
  if Compare.Int.(Bytes.length nonce <> Constants_repr.nonce_length) then
    tzfail Unexpected_nonce_length
  else return nonce

let hash nonce = Nonce_hash.hash_bytes [nonce]

let check_hash nonce hash =
  Compare.Int.(Bytes.length nonce = Constants_repr.nonce_length)
  && Nonce_hash.equal (Nonce_hash.hash_bytes [nonce]) hash

let nonce_hash_key_part = Nonce_hash.to_path

let initial_nonce_0 = zero_bytes

let deterministic_seed seed = update_seed seed zero_bytes

let initial_seeds ?initial_seed n =
  let rec loop acc elt i =
    if Compare.Int.(i = 1) then List.rev (elt :: acc)
    else loop (elt :: acc) (deterministic_seed elt) (i - 1)
  in
  let first_seed =
    match initial_seed with
    | Some initial_seed -> update_seed (B initial_seed) initial_nonce_0
    | None -> B (State_hash.hash_bytes [])
  in
  loop [] first_seed n

let nonce_discriminant = Bytes.of_string "Tezos_generating_vdf_discriminant"

let nonce_challenge = Bytes.of_string "Tezos_generating_vdf_challenge"

let generate_vdf_setup ~seed_discriminant ~seed_challenge =
  let size = Vdf.discriminant_size_bytes in
  let seed =
    update_seed seed_discriminant nonce_discriminant |> seed_to_bytes
  in
  let discriminant = Vdf.generate_discriminant ~seed size in
  let input = update_seed seed_challenge nonce_challenge |> seed_to_bytes in
  let challenge = Vdf.generate_challenge discriminant input in
  (discriminant, challenge)

let verify (discriminant, challenge) vdf_difficulty solution =
  (* We return false when getting non group elements as input *)
  let result, proof = solution in
  (* Note: external library call must be wrapped to ensure that
     exceptions are caught. *)
  Option.catch (fun () ->
      Vdf.verify discriminant challenge vdf_difficulty result proof)

let vdf_to_seed seed_challenge solution =
  let result, _ = solution in
  update_seed seed_challenge (Vdf.result_to_bytes result)

type seed_status = RANDAO_seed | VDF_seed

let seed_status_encoding =
  let to_bool = function RANDAO_seed -> false | VDF_seed -> true in
  let of_bool t = if t then VDF_seed else RANDAO_seed in
  Data_encoding.conv to_bool of_bool Data_encoding.bool

let compare_vdf_solution solution solution' =
  let result, _ = solution in
  let result', _ = solution' in
  Compare.Bytes.compare
    (Vdf.result_to_bytes result)
    (Vdf.result_to_bytes result')
