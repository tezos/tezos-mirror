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

type error += Unregistered_key_scheme of string

type error += Invalid_uri of Uri.t

let () =
  register_error_kind
    `Permanent
    ~id:"cli.unregistered_key_scheme"
    ~title:"Unregistered key scheme"
    ~description:
      "A key has been provided with an unregistered scheme (no corresponding \
       plugin)"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "No matching plugin for key scheme %s" s)
    Data_encoding.(obj1 (req "value" string))
    (function Unregistered_key_scheme s -> Some s | _ -> None)
    (fun s -> Unregistered_key_scheme s) ;
  register_error_kind
    `Permanent
    ~id:"cli.key.invalid_uri"
    ~title:"Invalid key uri"
    ~description:"A key has been provided with an invalid uri."
    ~pp:(fun ppf s -> Format.fprintf ppf "Cannot parse the key uri: %s" s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_uri s -> Some (Uri.to_string s) | _ -> None)
    (fun s -> Invalid_uri (Uri.of_string s))

module Public_key_hash = struct
  include Client_aliases.Alias (struct
    (* includes t, Compare, encoding *)
    include Signature.Public_key_hash

    let of_source s = Lwt.return (Signature.Public_key_hash.of_b58check s)

    let to_source p = Lwt.return_ok (Signature.Public_key_hash.to_b58check p)

    let name = "public key hash"
  end)
end

module Logging = struct
  let tag = Tag.def ~doc:"Identity" "pk_alias" Format.pp_print_text
end

let uri_encoding =
  let to_uri s =
    let o = Uri.of_string s in
    match Uri.scheme o with
    | None -> Stdlib.failwith "Key URI needs a scheme"
    | Some _ -> o
  in
  Data_encoding.(conv Uri.to_string to_uri string)

type error += Unexisting_scheme of Uri.t

let () =
  register_error_kind
    `Permanent
    ~id:"cli.unexisting_scheme"
    ~title:"Unexisting scheme"
    ~description:"The requested scheme does not exist"
    ~pp:(fun ppf uri ->
      Format.fprintf
        ppf
        "The uri %a does specify a scheme to use"
        Uri.pp_hum
        uri)
    Data_encoding.(obj1 (req "uri" uri_encoding))
    (function Unexisting_scheme uri -> Some uri | _ -> None)
    (fun uri -> Unexisting_scheme uri)

type pk_uri = Uri.t

module Pk_uri_hashtbl = Hashtbl.Make (struct
  type t = pk_uri

  let equal = Uri.equal

  let hash = Hashtbl.hash
end)

let make_pk_uri (x : Uri.t) : pk_uri tzresult =
  let open Tzresult_syntax in
  match Uri.scheme x with
  | None ->
      fail (Exn (Failure "Error while parsing URI: PK_URI needs a scheme"))
  | Some _ -> return x

type sk_uri = Uri.t

module CompareUri = Compare.Make (struct
  type t = Uri.t

  let compare = Uri.compare
end)

let make_sk_uri (x : Uri.t) : sk_uri tzresult =
  let open Tzresult_syntax in
  match Uri.scheme x with
  | None ->
      fail (Exn (Failure "Error while parsing URI: SK_URI needs a scheme"))
  | Some _ -> return x

type sapling_uri = Uri.t

let make_sapling_uri (x : Uri.t) : sapling_uri tzresult =
  let open Tzresult_syntax in
  match Uri.scheme x with
  | None -> fail (Exn (Failure "SAPLING_URI needs a scheme"))
  | Some _ -> return x

type pvss_sk_uri = Uri.t

let make_pvss_sk_uri (x : Uri.t) : pvss_sk_uri tzresult =
  let open Tzresult_syntax in
  match Uri.scheme x with
  | None ->
      fail (Exn (Failure "Error while parsing URI: PVSS_URI needs a scheme"))
  | Some _ -> return x

let pk_uri_parameter () =
  Clic.parameter (fun _ s -> Lwt.return @@ make_pk_uri (Uri.of_string s))

let pk_uri_param ?name ?desc params =
  let name = Option.value ~default:"uri" name in
  let desc =
    Option.value
      ~default:
        "public key\n\
         Varies from one scheme to the other.\n\
         Use command `list signing schemes` for more information."
      desc
  in
  Clic.param ~name ~desc (pk_uri_parameter ()) params

let sk_uri_parameter () =
  Clic.parameter (fun _ s -> Lwt.return (make_sk_uri @@ Uri.of_string s))

let sk_uri_param ?name ?desc params =
  let name = Option.value ~default:"uri" name in
  let desc =
    Option.value
      ~default:
        "secret key\n\
         Varies from one scheme to the other.\n\
         Use command `list signing schemes` for more information."
      desc
  in
  Clic.param ~name ~desc (sk_uri_parameter ()) params

module Secret_key = Client_aliases.Alias (struct
  let name = "secret_key"

  type t = sk_uri

  include (CompareUri : Compare.S with type t := t)

  let of_source s = Lwt.return (make_sk_uri @@ Uri.of_string s)

  let to_source t = Lwt.return_ok (Uri.to_string t)

  let encoding = uri_encoding
end)

module Public_key = Client_aliases.Alias (struct
  let name = "public_key"

  type t = pk_uri * Signature.Public_key.t option

  include Compare.Make (struct
    type nonrec t = t

    let compare (apk, aso) (bpk, bso) =
      Compare.or_else (CompareUri.compare apk bpk) (fun () ->
          Option.compare Signature.Public_key.compare aso bso)
  end)

  let of_source s =
    let open Lwt_result_syntax in
    let*? pk_uri = make_pk_uri @@ Uri.of_string s in
    return (pk_uri, None)

  let to_source (t, _) = Lwt.return_ok (Uri.to_string t)

  let encoding =
    let open Data_encoding in
    union
      [
        case
          Json_only
          ~title:"Locator_only"
          uri_encoding
          (function (uri, None) -> Some uri | (_, Some _) -> None)
          (fun uri -> (uri, None));
        case
          Json_only
          ~title:"Locator_and_full_key"
          (obj2
             (req "locator" uri_encoding)
             (req "key" Signature.Public_key.encoding))
          (function (uri, Some key) -> Some (uri, key) | (_, None) -> None)
          (fun (uri, key) -> (uri, Some key));
      ]
end)

type sapling_key = {
  sk : sapling_uri;
  (* zip32 derivation path *)
  path : int32 list;
  (* index of the next address to generate *)
  address_index : Tezos_sapling.Core.Client.Viewing_key.index;
}

module Sapling_key = Client_aliases.Alias (struct
  module S = Tezos_sapling.Core.Client

  let name = "sapling_key"

  type t = sapling_key

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      Compare.or_else (CompareUri.compare a.sk b.sk) (fun () ->
          Compare.or_else (Stdlib.compare a.path b.path) (fun () ->
              Tezos_sapling.Core.Client.Viewing_key.compare_index
                a.address_index
                b.address_index))
  end)

  let encoding =
    let open Data_encoding in
    conv
      (fun k -> (k.sk, k.path, k.address_index))
      (fun (sk, path, address_index) -> {sk; path; address_index})
      (obj3
         (req "sk" uri_encoding)
         (req "path" (list int32))
         (req "address_index" S.Viewing_key.index_encoding))

  let of_source s =
    let open Lwt_tzresult_syntax in
    let open Data_encoding in
    match Json.from_string s with
    | Error _ -> failwith "corrupted wallet"
    | Ok s -> return (Json.destruct encoding s)

  let to_source k =
    let open Data_encoding in
    Lwt.return_ok @@ Json.to_string (Json.construct encoding k)
end)

module PVSS_public_key = Client_aliases.Alias (struct
  include Pvss_secp256k1.Public_key (* t, Compare, encoding *)

  let name = "PVSS public key"

  let of_source s = Lwt.return (Pvss_secp256k1.Public_key.of_b58check s)

  let to_source t = Lwt.return_ok (Pvss_secp256k1.Public_key.to_b58check t)
end)

module PVSS_secret_key = Client_aliases.Alias (struct
  let name = "PVSS secret key"

  type t = pvss_sk_uri

  include CompareUri

  let encoding = uri_encoding

  let of_source s = Lwt.return (make_pvss_sk_uri @@ Uri.of_string s)

  let to_source t = Lwt.return_ok (Uri.to_string t)
end)

module Make_common_type (S : sig
  include S.COMMON_SIGNATURE

  type pk_uri

  type sk_uri
end) =
struct
  type pk_uri = S.pk_uri

  type sk_uri = S.sk_uri

  type public_key_hash = S.Public_key_hash.t

  type public_key = S.Public_key.t

  type secret_key = S.Secret_key.t
end

module Signature_type = Make_common_type (struct
  include Signature

  type nonrec pk_uri = pk_uri

  type nonrec sk_uri = sk_uri
end)

module type COMMON_SIGNER = sig
  val scheme : string

  val title : string

  val description : string

  type pk_uri = private Uri.t

  type sk_uri = private Uri.t

  type public_key_hash

  type public_key

  type secret_key

  val neuterize : sk_uri -> pk_uri tzresult Lwt.t

  val import_secret_key :
    io:Client_context.io_wallet ->
    pk_uri ->
    (public_key_hash * public_key option) tzresult Lwt.t

  val public_key : pk_uri -> public_key tzresult Lwt.t

  val public_key_hash :
    pk_uri -> (public_key_hash * public_key option) tzresult Lwt.t
end

module type SIGNER = sig
  include
    COMMON_SIGNER
      with type public_key_hash = Signature.Public_key_hash.t
       and type public_key = Signature.Public_key.t
       and type secret_key = Signature.Secret_key.t
       and type pk_uri = pk_uri
       and type sk_uri = sk_uri

  val sign :
    ?watermark:Signature.watermark ->
    sk_uri ->
    Bytes.t ->
    Signature.t tzresult Lwt.t

  val deterministic_nonce : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  val deterministic_nonce_hash : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  val supports_deterministic_nonces : sk_uri -> bool tzresult Lwt.t
end

type signer = Simple of (module SIGNER)

let signers_table : signer String.Hashtbl.t = String.Hashtbl.create 13

let register_signer signer =
  let module Signer = (val signer : SIGNER) in
  String.Hashtbl.replace signers_table Signer.scheme (Simple signer)

let find_signer_for_key ~scheme : signer tzresult =
  let open Tzresult_syntax in
  match String.Hashtbl.find signers_table scheme with
  | None -> fail (Unregistered_key_scheme scheme)
  | Some signer -> return signer

let find_simple_signer_for_key ~scheme =
  let open Tzresult_syntax in
  let* signer = find_signer_for_key ~scheme in
  match signer with Simple signer -> return signer

let registered_signers () : (string * signer) list =
  String.Hashtbl.fold (fun k v acc -> (k, v) :: acc) signers_table []

type error += Signature_mismatch of sk_uri

let () =
  register_error_kind
    `Permanent
    ~id:"cli.signature_mismatch"
    ~title:"Signature mismatch"
    ~description:"The signer produced an invalid signature"
    ~pp:(fun ppf sk ->
      Format.fprintf
        ppf
        "The signer for %a produced an invalid signature"
        Uri.pp_hum
        sk)
    Data_encoding.(obj1 (req "locator" uri_encoding))
    (function Signature_mismatch sk -> Some sk | _ -> None)
    (fun sk -> Signature_mismatch sk)

let with_scheme_signer (uri : Uri.t) (f : signer -> 'a tzresult Lwt.t) :
    'a tzresult Lwt.t =
  let open Lwt_tzresult_syntax in
  match Uri.scheme uri with
  | None -> fail @@ Unexisting_scheme uri
  | Some scheme ->
      let*? signer = find_signer_for_key ~scheme in
      f signer

let with_scheme_simple_signer (uri : Uri.t)
    (f : (module SIGNER) -> 'a tzresult Lwt.t) : 'a tzresult Lwt.t =
  let open Lwt_tzresult_syntax in
  match Uri.scheme uri with
  | None -> fail @@ Unexisting_scheme uri
  | Some scheme ->
      let*? signer = find_simple_signer_for_key ~scheme in
      f signer

let neuterize (sk_uri : sk_uri) : pk_uri tzresult Lwt.t =
  with_scheme_simple_signer sk_uri (fun (module Signer : SIGNER) ->
      Signer.neuterize sk_uri)

let public_key pk_uri =
  with_scheme_simple_signer pk_uri (fun (module Signer : SIGNER) ->
      Signer.public_key pk_uri)

let public_key_hash pk_uri =
  with_scheme_simple_signer pk_uri (fun (module Signer : SIGNER) ->
      Signer.public_key_hash pk_uri)

let import_secret_key ~io pk_uri =
  with_scheme_simple_signer pk_uri (fun (module Signer : SIGNER) ->
      Signer.import_secret_key ~io pk_uri)

let sign cctxt ?watermark sk_uri buf =
  let open Lwt_tzresult_syntax in
  with_scheme_simple_signer sk_uri (fun (module Signer : SIGNER) ->
      let* signature = Signer.sign ?watermark sk_uri buf in
      let* pk_uri = Signer.neuterize sk_uri in
      let* pubkey =
        let* o = Secret_key.rev_find cctxt sk_uri in
        match o with
        | None -> public_key pk_uri
        | Some name -> (
            let* r = Public_key.find cctxt name in
            match r with
            | (_, None) ->
                let* pk = public_key pk_uri in
                let* () = Public_key.update cctxt name (pk_uri, Some pk) in
                return pk
            | (_, Some pubkey) -> return pubkey)
      in
      let* () =
        fail_unless
          (Signature.check ?watermark pubkey signature buf)
          (Signature_mismatch sk_uri)
      in
      return signature)

let append cctxt ?watermark loc buf =
  let open Lwt_result_syntax in
  let+ signature = sign cctxt ?watermark loc buf in
  Signature.concat buf signature

let check ?watermark pk_uri signature buf =
  let open Lwt_result_syntax in
  let* pk = public_key pk_uri in
  return (Signature.check ?watermark pk signature buf)

let deterministic_nonce sk_uri data =
  with_scheme_simple_signer sk_uri (fun (module Signer : SIGNER) ->
      Signer.deterministic_nonce sk_uri data)

let deterministic_nonce_hash sk_uri data =
  with_scheme_simple_signer sk_uri (fun (module Signer : SIGNER) ->
      Signer.deterministic_nonce_hash sk_uri data)

let supports_deterministic_nonces sk_uri =
  with_scheme_signer sk_uri (function Simple (module Signer : SIGNER) ->
      Signer.supports_deterministic_nonces sk_uri)

let register_key cctxt ?(force = false) (public_key_hash, pk_uri, sk_uri)
    ?public_key name =
  let open Lwt_result_syntax in
  let* () = Public_key.add ~force cctxt name (pk_uri, public_key) in
  let* () = Secret_key.add ~force cctxt name sk_uri in
  Public_key_hash.add ~force cctxt name public_key_hash

let register_keys cctxt xs =
  let open Lwt_result_syntax in
  let* () =
    Public_key.add_many
      cctxt
      (List.map (fun (name, _, pk, pk_uri, _) -> (name, (pk_uri, Some pk))) xs)
  in
  let* () =
    Secret_key.add_many
      cctxt
      (List.map (fun (name, _, _, _, sk_uri) -> (name, sk_uri)) xs)
  in
  let* () =
    Public_key_hash.add_many
      cctxt
      (List.map
         (fun (name, public_key_hash, _, _, _) -> (name, public_key_hash))
         xs)
  in
  return_unit

(* This function is used to chose between two aliases associated
   to the same key hash; if we know the secret key for one of them
   we take it, otherwise if we know the public key for one of them
   we take it. *)
let join_keys keys1_opt keys2 =
  match (keys1_opt, keys2) with
  | (Some (_, Some _, None), (_, None, None)) -> keys1_opt
  | (Some (_, _, Some _), _) -> keys1_opt
  | _ -> Some keys2

(* For efficiency, this function avoids loading the wallet, except for
   the call to [Public_key.update]. Indeed the arguments [pkhs],
   [pks], [sks] represent the already loaded list of public key
   hashes, public keys, and secret keys. *)
let raw_get_key_aux (cctxt : #Client_context.wallet) pkhs pks sks pkh =
  let open Lwt_tzresult_syntax in
  let rev_find_all list pkh =
    List.filter_map
      (fun (name, pkh') ->
        if Signature.Public_key_hash.equal pkh pkh' then Some name else None)
      list
  in
  let*! r =
    let names = rev_find_all pkhs pkh in
    let* o =
      List.fold_left_es
        (fun keys_opt name ->
          let sk_uri_opt = List.assoc ~equal:String.equal name sks in
          let* pk_opt =
            match List.assoc ~equal:String.equal name pks with
            | None -> return_none
            | Some (_, Some pk) -> return_some pk
            | Some (pk_uri, None) ->
                let* pk = public_key pk_uri in
                let* () = Public_key.update cctxt name (pk_uri, Some pk) in
                return_some pk
          in
          return @@ join_keys keys_opt (name, pk_opt, sk_uri_opt))
        None
        names
    in
    match o with
    | None ->
        failwith
          "no keys for the source contract %a"
          Signature.Public_key_hash.pp
          pkh
    | Some keys -> return keys
  in
  match r with
  | (Ok (_, _, None) | Error _) as initial_result -> (
      (* try to lookup for a remote key *)
      let*! r =
        let*? signer = find_simple_signer_for_key ~scheme:"remote" in
        let module Signer = (val signer : SIGNER) in
        let path = Signature.Public_key_hash.to_b58check pkh in
        let uri = Uri.make ~scheme:Signer.scheme ~path () in
        let* pk = Signer.public_key uri in
        return (path, Some pk, Some uri)
      in
      match r with
      | Error _ -> Lwt.return initial_result
      | Ok _ as success -> Lwt.return success)
  | Ok _ as success -> Lwt.return success

let raw_get_key (cctxt : #Client_context.wallet) pkh =
  let open Lwt_result_syntax in
  let* pkhs = Public_key_hash.load cctxt in
  let* pks = Public_key.load cctxt in
  let* sks = Secret_key.load cctxt in
  raw_get_key_aux cctxt pkhs pks sks pkh

let get_key cctxt pkh =
  let open Lwt_tzresult_syntax in
  let* r = raw_get_key cctxt pkh in
  match r with
  | (pkh, Some pk, Some sk) -> return (pkh, pk, sk)
  | (_pkh, _pk, None) ->
      failwith "Unknown secret key for %a" Signature.Public_key_hash.pp pkh
  | (_pkh, None, _sk) ->
      failwith "Unknown public key for %a" Signature.Public_key_hash.pp pkh

let get_public_key cctxt pkh =
  let open Lwt_tzresult_syntax in
  let* r = raw_get_key cctxt pkh in
  match r with
  | (pkh, Some pk, _sk) -> return (pkh, pk)
  | (_pkh, None, _sk) ->
      failwith "Unknown public key for %a" Signature.Public_key_hash.pp pkh

let get_keys (cctxt : #Client_context.wallet) =
  let open Lwt_tzresult_syntax in
  let* sks = Secret_key.load cctxt in
  let* pkhs = Public_key_hash.load cctxt in
  let* pks = Public_key.load cctxt in
  let*! keys =
    List.filter_map_s
      (fun (name, sk_uri) ->
        let*! r =
          match List.assoc ~equal:String.equal name pkhs with
          | Some pkh ->
              let* pk =
                match List.assoc ~equal:String.equal name pks with
                | Some (_, Some pk) -> return pk
                | Some (pk_uri, None) ->
                    let* pk = public_key pk_uri in
                    let* () = Public_key.update cctxt name (pk_uri, Some pk) in
                    return pk
                | None -> failwith "no public key alias named %s" name
              in
              return (name, pkh, pk, sk_uri)
          | None -> failwith "no public key hash alias named %s" name
        in
        match r with Ok r -> Lwt.return_some r | Error _ -> Lwt.return_none)
      sks
  in
  return keys

let list_keys cctxt =
  let open Lwt_result_syntax in
  let* pkhs = Public_key_hash.load cctxt in
  let* pks = Public_key.load cctxt in
  let* sks = Secret_key.load cctxt in
  List.map_es
    (fun (name, pkh) ->
      let*! r = raw_get_key_aux cctxt pkhs pks sks pkh in
      match r with
      | Ok (_name, pk, sk_uri) -> return (name, pkh, pk, sk_uri)
      | Error _ -> return (name, pkh, None, None))
    pkhs

let alias_keys cctxt name =
  let open Lwt_result_syntax in
  let* pkh = Public_key_hash.find cctxt name in
  let*! r = raw_get_key cctxt pkh in
  match r with
  | Ok (_name, pk, sk_uri) -> return_some (pkh, pk, sk_uri)
  | Error _ -> return_none

let force_switch () =
  Clic.switch ~long:"force" ~short:'f' ~doc:"overwrite existing keys" ()
