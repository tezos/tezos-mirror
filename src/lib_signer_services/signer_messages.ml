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

module type Authenticated_request = sig
  type t = {
    pkh : Signature.Public_key_hash.t;
    data : Bytes.t;
    signature : Signature.t option;
  }

  val to_sign : pkh:Signature.Public_key_hash.t -> data:Bytes.t -> Bytes.t

  val encoding : t Data_encoding.t
end

module type Tag = sig
  val tag : int
end

module Make_authenticated_request (T : Tag) : Authenticated_request = struct
  type t = {
    pkh : Signature.Public_key_hash.t;
    data : Bytes.t;
    signature : Signature.t option;
  }

  let x04 = Bytes.of_string "\x04"

  let to_sign ~pkh ~data =
    let tag = Bytes.make 1 '0' in
    TzEndian.set_int8 tag 0 T.tag ;
    Bytes.concat
      Bytes.empty
      [x04; tag; Signature.Public_key_hash.to_bytes pkh; data]

  let encoding =
    let open Data_encoding in
    conv
      (fun {pkh; data; signature} -> (pkh, data, signature))
      (fun (pkh, data, signature) -> {pkh; data; signature})
      (obj3
         (req "pkh" Signature.Public_key_hash.encoding)
         (req "data" bytes)
         (opt "signature" Signature.encoding))
end

module Sign = struct
  module Request = Make_authenticated_request (struct
    let tag = 1
  end)

  module Response = struct
    type t = Signature.t

    let encoding =
      let open Data_encoding in
      def "signer_messages.sign.response"
      @@ obj1 (req "signature" Signature.encoding)
  end
end

module Deterministic_nonce = struct
  module Request = Make_authenticated_request (struct
    let tag = 2
  end)

  module Response = struct
    type t = Bytes.t

    let encoding =
      let open Data_encoding in
      def "signer_messages.deterministic_nonce.response"
      @@ obj1 (req "deterministic_nonce" bytes)
  end
end

module Deterministic_nonce_hash = struct
  module Request = Make_authenticated_request (struct
    let tag = 3
  end)

  module Response = struct
    type t = Bytes.t

    let encoding =
      let open Data_encoding in
      def "signer_messages.deterministic_nonce_hash.response"
      @@ obj1 (req "deterministic_nonce_hash" bytes)
  end
end

module Supports_deterministic_nonces = struct
  module Request = struct
    type t = Signature.Public_key_hash.t

    let encoding =
      let open Data_encoding in
      def "signer_messages.supports_deterministic_nonces.request"
      @@ obj1 (req "pkh" Signature.Public_key_hash.encoding)
  end

  module Response = struct
    type t = bool

    let encoding =
      let open Data_encoding in
      def "signer_messages.supports_deterministic_nonces.response"
      @@ obj1 (req "bool" bool)
  end
end

module Public_key = struct
  module Request = struct
    type t = Signature.Public_key_hash.t

    let encoding =
      let open Data_encoding in
      def "signer_messages.public_key.request"
      @@ obj1 (req "pkh" Signature.Public_key_hash.encoding)
  end

  module Response = struct
    type t = Signature.Public_key.t

    let encoding =
      let open Data_encoding in
      def "signer_messages.public_key.response"
      @@ obj1 (req "pubkey" Signature.Public_key.encoding)
  end
end

module Authorized_keys = struct
  module Response = struct
    type t =
      | No_authentication
      | Authorized_keys of Signature.Public_key_hash.t list

    let encoding =
      let open Data_encoding in
      union
        [
          case
            (Tag 0)
            ~title:"No_authentication"
            (constant "no_authentication_required")
            (function No_authentication -> Some () | _ -> None)
            (fun () -> No_authentication);
          case
            (Tag 1)
            ~title:"Authorized_keys"
            (list Signature.Public_key_hash.encoding)
            (function Authorized_keys l -> Some l | _ -> None)
            (fun l -> Authorized_keys l);
        ]
  end
end

module Request = struct
  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t
    | Authorized_keys
    | Deterministic_nonce of Deterministic_nonce.Request.t
    | Deterministic_nonce_hash of Deterministic_nonce_hash.Request.t
    | Supports_deterministic_nonces of Supports_deterministic_nonces.Request.t

  let encoding =
    let open Data_encoding in
    def "signer_messages.request"
    @@ union
         [
           case
             (Tag 0)
             ~title:"Sign"
             (merge_objs
                (obj1 (req "kind" (constant "sign")))
                Sign.Request.encoding)
             (function Sign req -> Some ((), req) | _ -> None)
             (fun ((), req) -> Sign req);
           case
             (Tag 1)
             ~title:"Public_key"
             (merge_objs
                (obj1 (req "kind" (constant "public_key")))
                Public_key.Request.encoding)
             (function Public_key req -> Some ((), req) | _ -> None)
             (fun ((), req) -> Public_key req);
           case
             (Tag 2)
             ~title:"Authorized_keys"
             (obj1 (req "kind" (constant "authorized_keys")))
             (function Authorized_keys -> Some () | _ -> None)
             (fun () -> Authorized_keys);
           case
             (Tag 3)
             ~title:"Deterministic_nonce"
             (merge_objs
                (obj1 (req "kind" (constant "deterministic_nonce")))
                Deterministic_nonce.Request.encoding)
             (function Deterministic_nonce req -> Some ((), req) | _ -> None)
             (fun ((), req) -> Deterministic_nonce req);
           case
             (Tag 4)
             ~title:"Deterministic_nonce_hash"
             (merge_objs
                (obj1 (req "kind" (constant "deterministic_nonce_hash")))
                Deterministic_nonce_hash.Request.encoding)
             (function
               | Deterministic_nonce_hash req -> Some ((), req) | _ -> None)
             (fun ((), req) -> Deterministic_nonce_hash req);
           case
             (Tag 5)
             ~title:"Supports_deterministic_nonces"
             (merge_objs
                (obj1 (req "kind" (constant "supports_deterministic_nonces")))
                Supports_deterministic_nonces.Request.encoding)
             (function
               | Supports_deterministic_nonces req -> Some ((), req) | _ -> None)
             (fun ((), req) -> Supports_deterministic_nonces req);
         ]
end

let () =
  let open Data_encoding.Registration in
  register Request.encoding ;
  register Sign.Response.encoding ;
  register Deterministic_nonce.Response.encoding ;
  register Deterministic_nonce_hash.Response.encoding ;
  register Supports_deterministic_nonces.Response.encoding ;
  register Public_key.Response.encoding
