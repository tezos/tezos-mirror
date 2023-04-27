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

type error += No_identity_file of string

type error += Insufficient_proof_of_work of {expected : float}

type error +=
  | Identity_mismatch of {
      filename : string;
      peer_id : Tezos_crypto.Crypto_box.Public_key_hash.t;
    }

type error +=
  | Identity_keys_mismatch of {
      filename : string;
      expected_key : Tezos_crypto.Crypto_box.public_key;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.no_file"
    ~title:"No identity file"
    ~description:"The node identity file cannot be found"
    ~pp:(fun ppf file ->
      Format.fprintf
        ppf
        "Cannot read the identity file: `%s`. See `%s identity --help` on how \
         to generate an identity."
        file
        Sys.argv.(0))
    Data_encoding.(obj1 (req "file" string))
    (function No_identity_file file -> Some file | _ -> None)
    (fun file -> No_identity_file file)

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.insufficient_proof_of_work"
    ~title:"Insufficient proof of work"
    ~description:
      "The proof of work embedded by the current identity is not sufficient"
    ~pp:(fun ppf expected ->
      Format.fprintf
        ppf
        "The current identity does not embed a sufficient stamp of \
         proof-of-work. (expected level: %.2f). See `%s identity --help` on \
         how to generate a new identity."
        expected
        Sys.argv.(0))
    Data_encoding.(obj1 (req "expected" float))
    (function
      | Insufficient_proof_of_work {expected} -> Some expected | _ -> None)
    (fun expected -> Insufficient_proof_of_work {expected})

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.identity_mismatch"
    ~title:"Identity mismatch"
    ~description:
      "The identity (public key hash) does not match the keys provided with it"
    ~pp:(fun ppf (file, public_key_hash) ->
      Format.fprintf
        ppf
        "The current identity (public key hash) does not match the keys in %s.\n\
        \           Expected identity %a."
        file
        Tezos_crypto.Crypto_box.Public_key_hash.pp
        public_key_hash)
    Data_encoding.(
      obj2
        (req "file" string)
        (req "public_key_hash" Tezos_crypto.Crypto_box.Public_key_hash.encoding))
    (function
      | Identity_mismatch {filename; peer_id} -> Some (filename, peer_id)
      | _ -> None)
    (fun (filename, peer_id) -> Identity_mismatch {filename; peer_id})

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.identity_keys_mismatch"
    ~title:"Identity keys mismatch"
    ~description:
      "The current identity file has non-matching keys (secret key/ public key \
       pair is not valid)"
    ~pp:(fun ppf (file, public_key) ->
      Format.fprintf
        ppf
        "The current identity file %s has non-matching keys (secret key/ \
         public key pair is not valid).\n\
        \           Expected public key %a."
        file
        Tezos_crypto.Crypto_box.pp_pk
        public_key)
    Data_encoding.(
      obj2
        (req "file" string)
        (req "public_key" Tezos_crypto.Crypto_box.public_key_encoding))
    (function
      | Identity_keys_mismatch {filename; expected_key} ->
          Some (filename, expected_key)
      | _ -> None)
    (fun (filename, expected_key) ->
      Identity_keys_mismatch {filename; expected_key})

let read ?expected_pow filename =
  let open Lwt_result_syntax in
  let*! file_exists = Lwt_unix.file_exists filename in
  if not file_exists then tzfail (No_identity_file filename)
  else
    let* json = Lwt_utils_unix.Json.read_file filename in
    let id = Data_encoding.Json.destruct P2p_identity.encoding json in
    let pkh = Tezos_crypto.Crypto_box.hash id.public_key in
    (* check public_key hash *)
    if not (Tezos_crypto.Crypto_box.Public_key_hash.equal pkh id.peer_id) then
      tzfail (Identity_mismatch {filename; peer_id = pkh})
      (* check public/private keys correspondence *)
    else if
      not
        Tezos_crypto.Crypto_box.(equal (neuterize id.secret_key) id.public_key)
    then
      tzfail (Identity_keys_mismatch {filename; expected_key = id.public_key})
    else
      (* check PoW level *)
      match expected_pow with
      | None -> return id
      | Some expected ->
          let target = Tezos_crypto.Crypto_box.make_pow_target expected in
          if
            not
              (Tezos_crypto.Crypto_box.check_proof_of_work
                 id.public_key
                 id.proof_of_work_stamp
                 target)
          then tzfail (Insufficient_proof_of_work {expected})
          else return id

type error += Existent_identity_file of string

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.existent_file"
    ~title:"Cannot overwrite identity file"
    ~description:"Cannot implicitly overwrite the current identity file"
    ~pp:(fun ppf file ->
      Format.fprintf
        ppf
        "Cannot implicitly overwrite the current identity file: '%s'. See `%s \
         identity --help` on how to generate a new identity."
        file
        Sys.argv.(0))
    Data_encoding.(obj1 (req "file" string))
    (function Existent_identity_file file -> Some file | _ -> None)
    (fun file -> Existent_identity_file file)

let write ~check_data_dir file identity =
  let open Lwt_result_syntax in
  if Sys.file_exists file then tzfail (Existent_identity_file file)
  else
    let* () = check_data_dir ~data_dir:(Filename.dirname file) in
    Lwt_utils_unix.Json.write_file
      file
      (Data_encoding.Json.construct P2p_identity.encoding identity)

let generate_with_animation ppf target =
  let open Lwt_syntax in
  let duration = 1200 / Animation.number_of_frames in
  Animation.make_with_animation
    ppf
    ~make:(fun count ->
      Lwt.catch
        (fun () ->
          let+ id = P2p_identity.generate_with_bound ~max:count target in
          Ok id)
        (function
          | Not_found -> Lwt.return @@ Error count | exc -> Lwt.fail exc))
    ~on_retry:(fun time count ->
      let ms = int_of_float (Mtime.Span.to_ms time) in
      let count =
        if ms <= 1 then max 10 (count * 10) else count * duration / ms
      in
      let* () = Lwt.pause () in
      Lwt.return count)
    10000

let generate ~check_data_dir identity_file expected_pow =
  let open Lwt_result_syntax in
  if Sys.file_exists identity_file then
    tzfail (Existent_identity_file identity_file)
  else
    let target = Tezos_crypto.Crypto_box.make_pow_target expected_pow in
    Format.eprintf "Generating a new identity... (level: %.2f) " expected_pow ;
    let*! id = generate_with_animation Format.err_formatter target in
    let* () = write ~check_data_dir identity_file id in
    Format.eprintf
      "Stored the new identity (%a) into '%s'.@."
      P2p_peer.Id.pp
      id.peer_id
      identity_file ;
    return id
