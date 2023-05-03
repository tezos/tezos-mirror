(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Lwt.Syntax

let base = 58

let zbase = Z.of_int base

module Alphabet = struct
  type t = {encode : string; decode : string}

  let make alphabet =
    if String.length alphabet <> base then
      invalid_arg "Base58: invalid alphabet (length)" ;
    let bytes = Bytes.make 256 '\255' in
    for i = 0 to String.length alphabet - 1 do
      let char = int_of_char alphabet.[i] in
      if Bytes.get bytes char <> '\255' then
        Format.kasprintf
          invalid_arg
          "Base58: invalid alphabet (dup '%c' %d %d)"
          (char_of_int char)
          (int_of_char @@ Bytes.get bytes char)
          i ;
      Bytes.set bytes char (char_of_int i)
    done ;
    let decode = Bytes.unsafe_to_string bytes in
    {encode = alphabet; decode}

  let bitcoin =
    make "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let ripple = make "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  let flickr = make "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

  let default = bitcoin

  let all_in_alphabet ?(ignore_case = false) alphabet string =
    let ok = Array.make 256 false in
    String.iter
      (fun x ->
        if ignore_case then ok.(Char.code x) <- true
        else (
          ok.(Char.code (Char.lowercase_ascii x)) <- true ;
          ok.(Char.code (Char.uppercase_ascii x)) <- true))
      alphabet.encode ;
    let res = ref true in
    for i = 0 to String.length string - 1 do
      res := !res && ok.(Char.code string.[i])
    done ;
    !res

  let pp ppf {encode; _} = Format.fprintf ppf "%s" encode
end

let count_trailing_char s c =
  let len = String.length s in
  let rec loop i =
    if i < 0 then len else if s.[i] <> c then len - i - 1 else loop (i - 1)
  in
  loop (len - 1)

let count_leading_char s c =
  let len = String.length s in
  let rec loop i =
    if i = len then len else if s.[i] <> c then i else loop (i + 1)
  in
  loop 0

let of_char ?(alphabet = Alphabet.default) x =
  let pos = alphabet.decode.[int_of_char x] in
  match pos with '\255' -> None | _ -> Some (int_of_char pos)

let to_char ?(alphabet = Alphabet.default) x = alphabet.encode.[x]

let raw_encode ?(alphabet = Alphabet.default) s =
  let len = String.length s in
  let s = String.init len (fun i -> s.[len - i - 1]) in
  let zero = alphabet.encode.[0] in
  let zeros = count_trailing_char s '\000' in
  let res_len = ((len * 8) + 4) / 5 in
  let res = Bytes.make res_len '\000' in
  let s = Z.of_bits s in
  let rec loop s i =
    if s = Z.zero then i
    else
      let s, r = Z.div_rem s zbase in
      Bytes.set res i (to_char ~alphabet (Z.to_int r)) ;
      loop s (i - 1)
  in
  let i = loop s (res_len - 1) in
  let ress = Bytes.sub_string res (i + 1) (res_len - i - 1) in
  String.make zeros zero ^ ress

let raw_decode ?(alphabet = Alphabet.default) s =
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Option_syntax in
  let slen = String.length s in
  let rec decode acc index =
    if index >= slen then return acc
    else
      match of_char ~alphabet s.[index] with
      | Some i ->
          let acc = Z.(add (of_int i) (mul acc zbase)) in
          (decode [@ocaml.tailcall]) acc (index + 1)
      | _ -> fail
  in
  let* res = decode Z.zero 0 in
  let res = Z.to_bits res in
  let res_tzeros = count_trailing_char res '\000' in
  let len = String.length res - res_tzeros in
  let zeros = count_leading_char s alphabet.encode.[0] in
  let v =
    String.make zeros '\000' ^ String.init len (fun i -> res.[len - i - 1])
  in
  return v

let checksum s =
  let hash = Hacl.Hash.SHA256.(digest (digest (Bytes.of_string s))) in
  Bytes.sub_string hash 0 4

(* Append a 4-bytes cryptographic checksum before encoding string s *)
let safe_encode ?alphabet s = raw_encode ?alphabet (s ^ checksum s)

let safe_decode ?alphabet s =
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Option_syntax in
  let* s = raw_decode ?alphabet s in
  let len = String.length s in
  if len < 4 then fail
  else
    (* only if the string is long enough to extract a checksum do we check it *)
    let msg = String.sub s 0 (len - 4) in
    let msg_hash = String.sub s (len - 4) 4 in
    if msg_hash <> checksum msg then fail else return msg

type data = ..

type 'a encoding = {
  prefix : string;
  length : int;
  encoded_prefix : string;
  encoded_length : int;
  to_raw : 'a -> string;
  of_raw : string -> 'a option;
  wrap : 'a -> data;
}

let prefix {prefix; _} = prefix

let simple_decode ?alphabet {prefix; of_raw; _} s =
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Option_syntax in
  let* s = safe_decode ?alphabet s in
  let* s = TzString.remove_prefix ~prefix s in
  of_raw s

let simple_encode ?alphabet {prefix; to_raw; _} d =
  safe_encode ?alphabet (prefix ^ to_raw d)

type registered_encoding = Encoding : 'a encoding -> registered_encoding

module MakeEncodings (E : sig
  val encodings : registered_encoding list
end) =
struct
  let encodings = ref E.encodings

  let check_ambiguous_prefix prefix length encodings =
    List.iter
      (fun (Encoding {encoded_prefix = s; length = l; _}) ->
        if
          length = l
          && (TzString.remove_prefix ~prefix:s prefix <> None
             || TzString.remove_prefix ~prefix s <> None)
        then
          Format.ksprintf
            invalid_arg
            "Base58.register_encoding: duplicate prefix: %S, %S."
            s
            prefix)
      encodings

  let make_encoded_prefix prefix len =
    let zeros = safe_encode (prefix ^ String.make len '\000')
    and ones = safe_encode (prefix ^ String.make len '\255') in
    let len = String.length zeros in
    if String.length ones <> len then
      Format.ksprintf
        invalid_arg
        "Base58.registered_encoding: variable length encoding." ;
    let rec loop i =
      if i = len then len else if zeros.[i] = ones.[i] then loop (i + 1) else i
    in
    let len = loop 0 in
    if len = 0 then invalid_arg "Base58.register_encoding: not a unique prefix." ;
    (String.sub zeros 0 len, String.length zeros)

  let register_encoding ~prefix ~length ~to_raw ~of_raw ~wrap =
    let to_raw x =
      let s = to_raw x in
      assert (String.length s = length) ;
      s
    in
    let of_raw s =
      assert (String.length s = length) ;
      of_raw s
    in
    let encoded_prefix, encoded_length = make_encoded_prefix prefix length in
    check_ambiguous_prefix encoded_prefix encoded_length !encodings ;
    let encoding =
      {prefix; length; encoded_prefix; encoded_length; to_raw; of_raw; wrap}
    in
    encodings := Encoding encoding :: !encodings ;
    encoding

  let check_encoded_prefix enc p l =
    if enc.encoded_prefix <> p then
      Format.kasprintf
        Stdlib.failwith
        "Unexpected prefix %s (expected %s)"
        p
        enc.encoded_prefix ;
    if enc.encoded_length <> l then
      Format.kasprintf
        Stdlib.failwith
        "Unexpected encoded length %d for %s (expected %d)"
        l
        p
        enc.encoded_length

  let decode ?alphabet s =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Option_syntax in
    let* s = safe_decode ?alphabet s in
    List.find_map
      (fun (Encoding {prefix; of_raw; wrap; _}) ->
        let* msg = TzString.remove_prefix ~prefix s in
        let+ v = of_raw msg in
        wrap v)
      !encodings
end

type 'a resolver =
  | Resolver : {
      encoding : 'h encoding;
      resolver : 'a -> string -> 'h list Lwt.t;
    }
      -> 'a resolver

module MakeResolvers (R : sig
  type context
end) =
struct
  let resolvers = ref []

  let register_resolver (type a) (encoding : a encoding)
      (resolver : R.context -> string -> a list Lwt.t) =
    resolvers := Resolver {encoding; resolver} :: !resolvers

  let partial_decode ?(alphabet = Alphabet.default) request len =
    let zero = alphabet.encode.[0] in
    let last = alphabet.encode.[base - 1] in
    let n = String.length request in
    let min = raw_decode ~alphabet (request ^ String.make (len - n) zero) in
    let max = raw_decode ~alphabet (request ^ String.make (len - n) last) in
    match (min, max) with
    | Some min, Some max ->
        let prefix_len = TzString.common_prefix min max in
        Some (String.sub min 0 prefix_len)
    | _ -> None

  let complete ?alphabet context request =
    let rec find s = function
      | [] -> Lwt.return_nil
      | Resolver {encoding; resolver} :: resolvers -> (
          if not (TzString.has_prefix ~prefix:encoding.encoded_prefix s) then
            find s resolvers
          else
            match partial_decode ?alphabet request encoding.encoded_length with
            | None -> find s resolvers
            | Some prefix ->
                let len = String.length prefix in
                let ignored = String.length encoding.prefix in
                let msg =
                  if len <= ignored then ""
                  else (
                    assert (String.sub prefix 0 ignored = encoding.prefix) ;
                    String.sub prefix ignored (len - ignored))
                in
                let+ msgs = resolver context msg in
                List.filter_map
                  (fun msg ->
                    let res = simple_encode encoding ?alphabet msg in
                    TzString.remove_prefix ~prefix:request res
                    |> Option.map (fun _ -> res))
                  msgs)
    in
    find request !resolvers
end

include MakeEncodings (struct
  let encodings = []
end)

include MakeResolvers (struct
  type context = unit
end)

let register_resolver enc f = register_resolver enc (fun () s -> f s)

let complete ?alphabet s = complete ?alphabet () s

module Make (C : sig
  type context
end) =
struct
  include MakeEncodings (struct
    let encodings = !encodings
  end)

  include MakeResolvers (struct
    type context = C.context
  end)
end

module Prefix = struct
  (* These encoded prefixes are computed using scripts/base58_prefix.py
     $ ./scripts/base58_prefix.py tz1 20
     36 434591 [6L, 161L, 159L]
     $ dune utop src/lib_crypto
     utop # Tezos_crypto.Base58.make_encoded_prefix "\006\161\159" 20 ;;
     - : string * int = ("tz1", 36)
  *)

  (* 32 *)
  let block_hash = "\001\052" (* B(51) *)

  let operation_hash = "\005\116" (* o(51) *)

  let operation_list_hash = "\133\233" (* Lo(52) *)

  let operation_list_list_hash = "\029\159\109" (* LLo(53) *)

  let protocol_hash = "\002\170" (* P(51) *)

  let context_hash = "\079\199" (* Co(52) *)

  let block_metadata_hash = "\234\249" (* bm(52) *)

  let operation_metadata_hash = "\005\183" (* r(51) *)

  let operation_metadata_list_hash = "\134\039" (* Lr(52) *)

  let operation_metadata_list_list_hash = "\029\159\182" (* LLr(53) *)

  (* 20 *)
  let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

  let secp256k1_public_key_hash = "\006\161\161" (* tz2(36) *)

  let p256_public_key_hash = "\006\161\164" (* tz3(36) *)

  let bls12_381_public_key_hash = "\006\161\166" (* tz4(36) *)

  let smart_rollup_address = "\006\124\117" (* sr1(36) *)

  (* 16 *)
  let cryptobox_public_key_hash = "\153\103" (* id(30) *)

  (* 32 *)
  let ed25519_seed = "\013\015\058\007" (* edsk(54) *)

  let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

  let secp256k1_secret_key = "\017\162\224\201" (* spsk(54) *)

  let p256_secret_key = "\016\081\238\189" (* p2sk(54) *)

  let smart_rollup_commitment = "\017\165\134\138" (* src1(54) *)

  let smart_rollup_state = "\017\165\235\240" (* srs1(54) *)

  let smart_rollup_merkelized_payload = "\003\255\138\145\140" (* srib2(55) *)

  (* 56 *)
  let ed25519_encrypted_seed = "\007\090\060\179\041" (* edesk(88) *)

  let secp256k1_encrypted_secret_key = "\009\237\241\174\150" (* spesk(88) *)

  let p256_encrypted_secret_key = "\009\048\057\115\171" (* p2esk(88) *)

  (* 60 *)
  let secp256k1_encrypted_scalar = "\001\131\036\086\248" (* seesk(93) *)

  (* 33 *)
  let secp256k1_public_key = "\003\254\226\086" (* sppk(55) *)

  let p256_public_key = "\003\178\139\127" (* p2pk(55) *)

  let secp256k1_scalar = "\038\248\136" (* SSp(53) *)

  let secp256k1_element = "\005\092\000" (* GSp(54) *)

  (* 64 *)
  let ed25519_secret_key = "\043\246\078\007" (* edsk(98) *)

  let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

  let secp256k1_signature = "\013\115\101\019\063" (* spsig1(99) *)

  let p256_signature = "\054\240\044\052" (* p2sig(98) *)

  let generic_signature = "\004\130\043" (* sig(96) *)

  (* 4 *)
  let chain_id = "\087\082\000" (* Net(15) *)

  (* 169 *)
  let sapling_spending_key = "\011\237\020\092" (* sask(241) *)

  (* 43 *)
  let sapling_address = "\018\071\040\223" (* zet1(69) *)

  (* 96 *)
  let generic_aggregate_signature = "\002\075\234\101" (* asig(141) *)

  (* 96 *)
  let bls12_381_signature = "\040\171\064\207" (* BLsig(142) *)

  (* 48 *)
  let bls12_381_public_key = "\006\149\135\204" (* BLpk(76) *)

  (* 32 *)
  let bls12_381_secret_key = "\003\150\192\040" (* BLsk(54) *)

  (* 56 *)
  let bls12_381_encrypted_secret_key = "\002\005\030\053\025" (* BLesk(88) *)

  (* 48 *)
  let slot_header = "\002\116\180" (* sh(74) *)
end
