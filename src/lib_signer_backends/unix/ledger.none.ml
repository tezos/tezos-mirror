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

open Client_keys

type error += NoLedgerSupport

let () =
  register_error_kind
    `Permanent
    ~id:"signer.ledger"
    ~title:"No Ledger support"
    ~description:
      "This client has been compiled without support for Ledger Nano device"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "This client has been compiled without support for Ledger Nano device")
    Data_encoding.empty
    (function NoLedgerSupport -> Some () | _ -> None)
    (fun () -> NoLedgerSupport)

(** The implementation of the “signer-plugin.” *)
module Signer_implementation : Client_keys.SIGNER = struct
  include Client_keys.Signature_type

  let scheme = "no_ledger"

  let title = "Fake signer when Ledger Nano device support is disabled"

  let description =
    "In order to communicate with a Ledger Nano, recompile with \
     ledgerwallet-tezos library installed"

  let neuterize _sk = Lwt_tzresult_syntax.fail NoLedgerSupport

  let public_key _sk_uri = Lwt_tzresult_syntax.fail NoLedgerSupport

  let public_key_hash _sk_uri = Lwt_tzresult_syntax.fail NoLedgerSupport

  let import_secret_key ~io:_ _pk_uri = Lwt_tzresult_syntax.fail NoLedgerSupport

  let sign ?watermark:_k _sk_uri _msg = Lwt_tzresult_syntax.fail NoLedgerSupport

  let deterministic_nonce _sk_uri _msg =
    Lwt_tzresult_syntax.fail NoLedgerSupport

  let deterministic_nonce_hash _sk_uri _msg =
    Lwt_tzresult_syntax.fail NoLedgerSupport

  let supports_deterministic_nonces _ = Lwt_tzresult_syntax.return_false
end

let commands () = []
