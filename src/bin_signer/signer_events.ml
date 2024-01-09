(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <marcin.pastudzki@tqtezos.com> *)
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

module Http_daemon = struct
  include Internal_event.Simple

  let section = ["signer"; "http"]

  let listening =
    declare_1
      ~section
      ~level:Notice
      ~name:"signer_listening"
      ~msg:"listening on address: {address}"
      ("address", P2p_addr.encoding)

  let accepting_requests =
    declare_2
      ~section
      ~level:Notice
      ~name:"accepting_requests"
      ~msg:"accepting {transport_protocol} requests on port {port}"
      ("transport_protocol", Data_encoding.string)
      ("port", Data_encoding.int31)
end

module Handler = struct
  include Internal_event.Simple

  let level = Internal_event.Notice

  let section = ["signer"; "handler"]

  let request_for_signing =
    declare_3
      ~section
      ~level
      ~name:"request_for_signing"
      ~pp3:(fun fmt -> Format.fprintf fmt "%02X")
      ~msg:
        "request for signing {bytes} bytes of data for key {key}, magic byte = \
         {magic}"
      ("bytes", Data_encoding.int31)
      ("key", Tezos_crypto.Signature.Public_key_hash.encoding)
      ("magic", Data_encoding.uint8)

  let signing_data =
    declare_1
      ~section
      ~level
      ~name:"signing_data"
      ~msg:"signing data for key {key}"
      ("key", Data_encoding.string)

  let signing_data_failure =
    declare_2
      ~section
      ~level
      ~name:"signing_data_failure"
      ~msg:"Failed to sign data for key {key}: {failure}"
      ("key", Data_encoding.string)
      ("failure", Data_encoding.string)

  let request_for_deterministic_nonce =
    declare_2
      ~section
      ~level
      ~name:"request_for_deterministic_nonce"
      ~msg:"request for creating a nonce from {bytes} input bytes for key {key}"
      ("bytes", Data_encoding.int31)
      ("key", Tezos_crypto.Signature.Public_key_hash.encoding)

  let creating_nonce =
    declare_1
      ~section
      ~level
      ~name:"creating_nonce"
      ~msg:"creating nonce for key {key}"
      ("key", Data_encoding.string)

  let request_for_deterministic_nonce_hash =
    declare_2
      ~section
      ~level
      ~name:"request_for_deterministic_nonce_hash"
      ~msg:
        "request for creating a nonce hash from {bytes} input bytes for key \
         {key}"
      ("bytes", Data_encoding.int31)
      ("key", Tezos_crypto.Signature.Public_key_hash.encoding)

  let creating_nonce_hash =
    declare_1
      ~section
      ~level
      ~name:"creating_nonce_hash"
      ~msg:"creating nonce hash for key {key}"
      ("key", Data_encoding.string)

  let request_for_supports_deterministic_nonces =
    declare_1
      ~section
      ~level
      ~name:"request_for_supports_deterministic_nonces"
      ~msg:
        "request for checking whether the signer supports deterministic nonces \
         for key {key}"
      ("key", Tezos_crypto.Signature.Public_key_hash.encoding)

  let supports_deterministic_nonces =
    declare_1
      ~section
      ~level
      ~name:"supports_deterministic_nonces"
      ~msg:
        "returns true if and only if signer can generate deterministic nonces \
         for key {key}"
      ("key", Data_encoding.string)

  let request_for_public_key =
    declare_1
      ~section
      ~level
      ~name:"request_for_public_key"
      ~msg:"request for public key {key}"
      ("key", Tezos_crypto.Signature.Public_key_hash.encoding)

  let not_found_public_key =
    declare_1
      ~section
      ~level
      ~name:"not_found_public_key"
      ~msg:"no public key found for hash {hash}"
      ("hash", Tezos_crypto.Signature.Public_key_hash.encoding)

  let found_public_key =
    declare_2
      ~section
      ~level
      ~name:"found_public_key"
      ~msg:"found public key for hash {hash} (name: {name})"
      ("hash", Tezos_crypto.Signature.Public_key_hash.encoding)
      ("name", Data_encoding.string)
end

module Socket_daemon = struct
  include Internal_event.Simple

  let section = ["signer"; "socket"]

  let level = Internal_event.Notice

  let accepting_tcp_requests =
    declare_2
      ~section
      ~level
      ~name:"accepting_tcp_requests"
      ~msg:"accepting TCP requests on {host}:{service}"
      ("host", Data_encoding.string)
      ("service", Data_encoding.string)

  let accepting_unix_requests =
    declare_1
      ~section
      ~level
      ~name:"accepting_unix_requests"
      ~msg:"accepting UNIX requests on {path}"
      ("path", Data_encoding.string)
end
