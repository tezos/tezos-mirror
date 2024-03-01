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

type error += Wrong_key_scheme of (string * string)

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
    (fun s -> Invalid_uri (Uri.of_string s)) ;
  register_error_kind
    `Permanent
    ~id:"cli.wrong_key_scheme"
    ~title:"Wrong key scheme"
    ~description:
      "A certain scheme type has been requested but another one was found"
    ~pp:(fun ppf (expected, found) ->
      Format.fprintf ppf "Expected a %s scheme found a %s one" expected found)
    Data_encoding.(obj2 (req "expected" string) (req "found" string))
    (function
      | Wrong_key_scheme (expected, found) -> Some (expected, found) | _ -> None)
    (fun (expected, found) -> Wrong_key_scheme (expected, found))

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
  let open Result_syntax in
  match Uri.scheme x with
  | None ->
      tzfail (Exn (Failure "Error while parsing URI: PK_URI needs a scheme"))
  | Some _ -> return x

type sk_uri = Uri.t

module CompareUri = Compare.Make (struct
  type t = Uri.t

  let compare = Uri.compare
end)

let make_sk_uri (x : Uri.t) : sk_uri tzresult =
  let open Result_syntax in
  match Uri.scheme x with
  | None ->
      tzfail (Exn (Failure "Error while parsing URI: SK_URI needs a scheme"))
  | Some _ -> return x

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

type sapling_uri = Uri.t

let make_sapling_uri (x : Uri.t) : sapling_uri tzresult =
  let open Result_syntax in
  match Uri.scheme x with
  | None -> tzfail (Exn (Failure "SAPLING_URI needs a scheme"))
  | Some _ -> return x

type aggregate_pk_uri = Uri.t

type aggregate_sk_uri = Uri.t

let make_aggregate_pk_uri (x : Uri.t) : aggregate_pk_uri tzresult =
  let open Result_syntax in
  match Uri.scheme x with
  | None ->
      tzfail
        (Exn
           (Failure "Error while parsing URI: AGGREGATE_PK_URI needs a scheme"))
  (* because it's possible to make an aggregate pk uri without having the signer
     in the client we can't check that scheme is linked to a known signer *)
  | Some _ -> return x

let make_aggregate_sk_uri (x : Uri.t) : aggregate_sk_uri tzresult =
  let open Result_syntax in
  match Uri.scheme x with
  | None ->
      tzfail
        (Exn
           (Failure "Error while parsing URI: AGGREGATE_SK_URI needs a scheme"))
  | Some _ -> return x

let pk_uri_parameter () =
  Tezos_clic.parameter (fun _ s -> Lwt.return @@ make_pk_uri (Uri.of_string s))

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
  Tezos_clic.param ~name ~desc (pk_uri_parameter ()) params

let sk_uri_parameter () =
  Tezos_clic.parameter (fun _ s -> Lwt.return (make_sk_uri @@ Uri.of_string s))

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
  Tezos_clic.param ~name ~desc (sk_uri_parameter ()) params

let aggregate_sk_uri_parameter () =
  Tezos_clic.parameter (fun _ s ->
      make_aggregate_sk_uri @@ Uri.of_string s |> Lwt.return)

let aggregate_sk_uri_param ?name ?desc params =
  let name = Option.value ~default:"uri" name in
  let desc =
    Option.value
      ~default:
        "secret key\n\
         Varies from one scheme to the other.\n\
         Use command `list signing schemes` for more information."
      desc
  in
  Tezos_clic.param ~name ~desc (aggregate_sk_uri_parameter ()) params

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
    let open Lwt_result_syntax in
    let open Data_encoding in
    match Json.from_string s with
    | Error _ -> failwith "corrupted wallet"
    | Ok s -> return (Json.destruct encoding s)

  let to_source k =
    let open Data_encoding in
    Lwt.return_ok @@ Json.to_string (Json.construct encoding k)
end)

module Aggregate_alias = struct
  module Public_key_hash = struct
    include Client_aliases.Alias (struct
      (* includes t, Compare, encoding, of/to_b58check *)
      include Tezos_crypto.Aggregate_signature.Public_key_hash

      let of_source s = Lwt.return (of_b58check s)

      let to_source p = Lwt_result_syntax.return (to_b58check p)

      let name = "Aggregate_public_key_hash"
    end)
  end

  type pk_uri = Uri.t

  let make_pk_uri (x : Uri.t) : pk_uri tzresult =
    let open Result_syntax in
    match Uri.scheme x with
    | None ->
        tzfail
          (Exn
             (Failure "Error while parsing URI: AGGREGATE_PK_URI needs a scheme"))
    | Some _ -> return x

  module Public_key = Client_aliases.Alias (struct
    let name = "Aggregate_public_key"

    type t = pk_uri * Tezos_crypto.Aggregate_signature.Public_key.t option

    include Compare.Make (struct
      type nonrec t = t

      let compare (apk, aso) (bpk, bso) =
        Compare.or_else (CompareUri.compare apk bpk) (fun () ->
            Option.compare
              Tezos_crypto.Aggregate_signature.Public_key.compare
              aso
              bso)
    end)

    let of_source s =
      let open Lwt_result_syntax in
      let*? pk_uri = make_pk_uri @@ Uri.of_string s in
      return (pk_uri, None)

    let to_source (t, _) = Lwt_result_syntax.return (Uri.to_string t)

    let encoding =
      let open Data_encoding in
      union
        [
          case
            Json_only
            uri_encoding
            ~title:"Locator_only"
            (function uri, None -> Some uri | _, Some _ -> None)
            (fun uri -> (uri, None));
          case
            Json_only
            ~title:"Locator_and_full_key"
            (obj2
               (req "locator" uri_encoding)
               (req "key" Tezos_crypto.Aggregate_signature.Public_key.encoding))
            (function uri, Some key -> Some (uri, key) | _, None -> None)
            (fun (uri, key) -> (uri, Some key));
        ]
  end)

  type sk_uri = Uri.t

  let make_sk_uri (x : Uri.t) : sk_uri tzresult Lwt.t =
    let open Lwt_result_syntax in
    match Uri.scheme x with
    | None ->
        failwith "Error while parsing URI: AGGREGATE_SK_URI needs a scheme"
    | Some _ -> return x

  module Secret_key = Client_aliases.Alias (struct
    let name = "Aggregate_secret_key"

    type t = sk_uri

    include CompareUri

    let encoding = uri_encoding

    let of_source s = make_sk_uri @@ Uri.of_string s

    let to_source t = Lwt_result_syntax.return (Uri.to_string t)
  end)
end

module type COMMON_SIGNER = sig
  val scheme : string

  val title : string

  val description : string

  type pk_uri = private Uri.t

  type sk_uri = private Uri.t

  type public_key_hash

  type public_key

  type secret_key

  type signature

  val neuterize : sk_uri -> pk_uri tzresult Lwt.t

  val import_secret_key :
    io:Client_context.io_wallet ->
    pk_uri ->
    (public_key_hash * public_key option) tzresult Lwt.t

  val public_key : pk_uri -> public_key tzresult Lwt.t

  val public_key_hash :
    pk_uri -> (public_key_hash * public_key option) tzresult Lwt.t
end

module type AGGREGATE_SIGNER = sig
  include
    COMMON_SIGNER
      with type public_key_hash =
        Tezos_crypto.Aggregate_signature.Public_key_hash.t
       and type public_key = Tezos_crypto.Aggregate_signature.Public_key.t
       and type secret_key = Tezos_crypto.Aggregate_signature.Secret_key.t
       and type pk_uri = aggregate_pk_uri
       and type sk_uri = aggregate_sk_uri

  val sign :
    aggregate_sk_uri ->
    Bytes.t ->
    Tezos_crypto.Aggregate_signature.t tzresult Lwt.t
end

module Make_common_type (S : sig
  include Tezos_crypto.Intfs.COMMON_SIGNATURE

  type pk_uri

  type sk_uri
end) =
struct
  type pk_uri = S.pk_uri

  type sk_uri = S.sk_uri

  type public_key_hash = S.Public_key_hash.t

  type public_key = S.Public_key.t

  type secret_key = S.Secret_key.t

  type signature = S.t
end

module Aggregate_type = Make_common_type (struct
  include Tezos_crypto.Aggregate_signature

  type pk_uri = aggregate_pk_uri

  type sk_uri = aggregate_sk_uri
end)

module type Signature_S = sig
  include
    Tezos_crypto.Intfs.SIGNATURE
      with type watermark = Tezos_crypto.Signature.watermark

  val concat : Bytes.t -> t -> Bytes.t

  module Adapter : sig
    val public_key_hash :
      Tezos_crypto.Signature.Public_key_hash.t -> Public_key_hash.t tzresult

    val public_key :
      Tezos_crypto.Signature.Public_key.t -> Public_key.t tzresult

    val signature : Tezos_crypto.Signature.t -> t tzresult
  end
end

module type SIMPLE_SIGNER = sig
  include COMMON_SIGNER with type pk_uri = pk_uri and type sk_uri = sk_uri

  val sign :
    ?watermark:Tezos_crypto.Signature.watermark ->
    sk_uri ->
    Bytes.t ->
    signature tzresult Lwt.t

  val deterministic_nonce : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  val deterministic_nonce_hash : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  val supports_deterministic_nonces : sk_uri -> bool tzresult Lwt.t
end

module type S = sig
  type public_key_hash

  type public_key

  type secret_key

  type watermark

  type signature

  module Signature_type : sig
    type nonrec public_key_hash = public_key_hash

    type nonrec public_key = public_key

    type nonrec secret_key = secret_key

    type nonrec signature = signature

    type nonrec pk_uri = pk_uri

    type nonrec sk_uri = sk_uri
  end

  module Public_key_hash : Client_aliases.Alias with type t = public_key_hash

  module Public_key :
    Client_aliases.Alias with type t = pk_uri * public_key option

  module Secret_key : Client_aliases.Alias with type t = sk_uri

  val import_secret_key :
    io:Client_context.io_wallet ->
    pk_uri ->
    (public_key_hash * public_key option) tzresult Lwt.t

  val public_key : pk_uri -> public_key tzresult Lwt.t

  val public_key_hash :
    pk_uri -> (public_key_hash * public_key option) tzresult Lwt.t

  val neuterize : sk_uri -> pk_uri tzresult Lwt.t

  val sign :
    #Client_context.wallet ->
    ?watermark:watermark ->
    sk_uri ->
    Bytes.t ->
    signature tzresult Lwt.t

  val append :
    #Client_context.wallet ->
    ?watermark:watermark ->
    sk_uri ->
    Bytes.t ->
    Bytes.t tzresult Lwt.t

  val check :
    ?watermark:watermark ->
    pk_uri ->
    signature ->
    Bytes.t ->
    bool tzresult Lwt.t

  val deterministic_nonce : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  val deterministic_nonce_hash : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  val supports_deterministic_nonces : sk_uri -> bool tzresult Lwt.t

  val register_key :
    #Client_context.wallet ->
    ?force:bool ->
    public_key_hash * pk_uri * sk_uri ->
    ?public_key:public_key ->
    string ->
    unit tzresult Lwt.t

  val register_keys :
    #Client_context.wallet ->
    (string * public_key_hash * public_key * pk_uri * sk_uri) list ->
    unit tzresult Lwt.t

  val list_keys :
    #Client_context.wallet ->
    (string * public_key_hash * public_key option * sk_uri option) list tzresult
    Lwt.t

  val alias_keys :
    #Client_context.wallet ->
    string ->
    (public_key_hash * public_key option * sk_uri option) option tzresult Lwt.t

  val get_key :
    #Client_context.wallet ->
    public_key_hash ->
    (string * public_key * sk_uri) tzresult Lwt.t

  val get_public_key :
    #Client_context.wallet ->
    public_key_hash ->
    (string * public_key) tzresult Lwt.t

  val get_keys :
    #Client_context.wallet ->
    (string * public_key_hash * public_key * sk_uri) list tzresult Lwt.t

  val force_switch : unit -> (bool, 'ctx) Tezos_clic.arg
end

module type SIGNER =
  SIMPLE_SIGNER
    with type public_key_hash = Tezos_crypto.Signature.Public_key_hash.t
     and type public_key = Tezos_crypto.Signature.Public_key.t
     and type secret_key = Tezos_crypto.Signature.Secret_key.t
     and type signature = Tezos_crypto.Signature.t

type signer =
  | Simple of (module SIGNER)
  | Aggregate of (module AGGREGATE_SIGNER)

let signers_table : signer String.Hashtbl.t = String.Hashtbl.create 13

let register_signer signer =
  let module Signer = (val signer : SIGNER) in
  String.Hashtbl.replace signers_table Signer.scheme (Simple signer)

let register_aggregate_signer signer =
  let module Signer = (val signer : AGGREGATE_SIGNER) in
  String.Hashtbl.replace signers_table Signer.scheme (Aggregate signer)

let registered_signers () : (string * signer) list =
  String.Hashtbl.fold (fun k v acc -> (k, v) :: acc) signers_table []

let find_signer_for_key ~scheme : signer tzresult =
  let open Result_syntax in
  match String.Hashtbl.find signers_table scheme with
  | None -> tzfail (Unregistered_key_scheme scheme)
  | Some signer -> return signer

let find_aggregate_signer_for_key ~scheme =
  let open Result_syntax in
  let* signer = find_signer_for_key ~scheme in
  match signer with
  | Simple _signer -> tzfail (Wrong_key_scheme ("aggregate", "standard"))
  | Aggregate signer -> return signer

let with_scheme_aggregate_signer (uri : Uri.t)
    (f : (module AGGREGATE_SIGNER) -> 'a tzresult Lwt.t) : 'a tzresult Lwt.t =
  let open Lwt_result_syntax in
  match Uri.scheme uri with
  | None -> tzfail @@ Unexisting_scheme uri
  | Some scheme ->
      let*? signer = find_aggregate_signer_for_key ~scheme in
      f signer

let register_aggregate_key cctxt ?(force = false)
    (public_key_hash, pk_uri, sk_uri) ?public_key name =
  let open Lwt_result_syntax in
  let* () =
    Aggregate_alias.Public_key.add ~force cctxt name (pk_uri, public_key)
  in
  let* () = Aggregate_alias.Secret_key.add ~force cctxt name sk_uri in
  Aggregate_alias.Public_key_hash.add ~force cctxt name public_key_hash

let aggregate_neuterize (sk_uri : sk_uri) : pk_uri tzresult Lwt.t =
  with_scheme_aggregate_signer sk_uri (fun (module Signer : AGGREGATE_SIGNER) ->
      Signer.neuterize sk_uri)

let aggregate_public_key pk_uri =
  with_scheme_aggregate_signer pk_uri (fun (module Signer : AGGREGATE_SIGNER) ->
      Signer.public_key pk_uri)

(* This function is used to chose between two aliases associated
   to the same key hash; if we know the secret key for one of them
   we take it, otherwise if we know the public key for one of them
   we take it. *)
let join_keys keys1_opt keys2 =
  match (keys1_opt, keys2) with
  | Some (_, Some _, None), (_, None, None) -> keys1_opt
  | Some (_, _, Some _), _ -> keys1_opt
  | _ -> Some keys2

(* For efficiency, this function avoids loading the wallet, except for
   the call to [Public_key.update]. Indeed the arguments [pkhs],
   [pks], [sks] represent the already loaded list of public key
   hashes, public keys, and secret keys. *)
let raw_get_aggregate_key_aux (cctxt : #Client_context.wallet) pkhs pks sks pkh
    =
  let open Lwt_result_syntax in
  let rev_find_all list pkh =
    List.filter_map
      (fun (name, pkh') ->
        if Tezos_crypto.Aggregate_signature.Public_key_hash.equal pkh pkh' then
          Some name
        else None)
      list
  in
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
              let* pk = aggregate_public_key pk_uri in
              let* () =
                Aggregate_alias.Public_key.update cctxt name (pk_uri, Some pk)
              in
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
        Tezos_crypto.Aggregate_signature.Public_key_hash.pp
        pkh
  | Some keys -> return keys

let raw_get_aggregate_key (cctxt : #Client_context.wallet) pkh =
  let open Lwt_result_syntax in
  let* pkhs = Aggregate_alias.Public_key_hash.load cctxt in
  let* pks = Aggregate_alias.Public_key.load cctxt in
  let* sks = Aggregate_alias.Secret_key.load cctxt in
  raw_get_aggregate_key_aux cctxt pkhs pks sks pkh

let list_aggregate_keys cctxt =
  let open Lwt_result_syntax in
  let* pkhs = Aggregate_alias.Public_key_hash.load cctxt in
  let* pks = Aggregate_alias.Public_key.load cctxt in
  let* sks = Aggregate_alias.Secret_key.load cctxt in
  List.map_es
    (fun (name, pkh) ->
      let*! r = raw_get_aggregate_key_aux cctxt pkhs pks sks pkh in
      match r with
      | Ok (_name, pk, sk_uri) -> return (name, pkh, pk, sk_uri)
      | Error _ -> return (name, pkh, None, None))
    pkhs

let import_aggregate_secret_key ~io pk_uri =
  with_scheme_aggregate_signer pk_uri (fun (module Signer : AGGREGATE_SIGNER) ->
      Signer.import_secret_key ~io pk_uri)

let alias_aggregate_keys cctxt name =
  let open Lwt_result_syntax in
  let* pkh = Aggregate_alias.Public_key_hash.find cctxt name in
  let*! r = raw_get_aggregate_key cctxt pkh in
  match r with
  | Ok (_name, pk, sk_uri) -> return_some (pkh, pk, sk_uri)
  | Error _ -> return_none

let aggregate_sign cctxt sk_uri buf =
  let open Lwt_result_syntax in
  with_scheme_aggregate_signer sk_uri (fun (module Signer : AGGREGATE_SIGNER) ->
      let* signature = Signer.sign sk_uri buf in
      let* pk_uri = Signer.neuterize sk_uri in
      let* pubkey =
        let* o = Aggregate_alias.Secret_key.rev_find cctxt sk_uri in
        match o with
        | None -> aggregate_public_key pk_uri
        | Some name -> (
            let* r = Aggregate_alias.Public_key.find cctxt name in
            match r with
            | _, None ->
                let* pk = aggregate_public_key pk_uri in
                let* () =
                  Aggregate_alias.Public_key.update cctxt name (pk_uri, Some pk)
                in
                return pk
            | _, Some pubkey -> return pubkey)
      in
      let* () =
        fail_unless
          (Tezos_crypto.Aggregate_signature.check pubkey signature buf)
          (Signature_mismatch sk_uri)
      in
      return signature)

module Make (Signature : Signature_S) :
  S
    with type public_key_hash := Signature.Public_key_hash.t
     and type public_key := Signature.Public_key.t
     and type secret_key := Signature.Secret_key.t
     and type watermark := Signature.watermark
     and type signature := Signature.t = struct
  module Public_key_hash = struct
    include Client_aliases.Alias (struct
      (* includes t, Compare, encoding *)
      include Signature.Public_key_hash

      let of_source s = Lwt.return (Signature.Public_key_hash.of_b58check s)

      let to_source p = Lwt.return_ok (Signature.Public_key_hash.to_b58check p)

      let name = "public key hash"
    end)
  end

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
      let public_key = Signature.Public_key.of_b58check_opt (Uri.path pk_uri) in
      return (pk_uri, public_key)

    let to_source (t, _) = Lwt.return_ok (Uri.to_string t)

    let encoding =
      let open Data_encoding in
      union
        [
          case
            Json_only
            ~title:"Locator_only"
            uri_encoding
            (function uri, None -> Some uri | _, Some _ -> None)
            (fun uri -> (uri, None));
          case
            Json_only
            ~title:"Locator_and_full_key"
            (obj2
               (req "locator" uri_encoding)
               (req "key" Signature.Public_key.encoding))
            (function uri, Some key -> Some (uri, key) | _, None -> None)
            (fun (uri, key) -> (uri, Some key));
        ]
  end)

  module Signature_type = Make_common_type (struct
    include Signature

    type nonrec pk_uri = pk_uri

    type nonrec sk_uri = sk_uri
  end)

  module type V_SIGNER =
    SIMPLE_SIGNER
      with type public_key_hash = Signature.Public_key_hash.t
       and type public_key = Signature.Public_key.t
       and type secret_key = Signature.Secret_key.t
       and type signature = Signature.t

  module Adapt (S : SIGNER) : V_SIGNER = struct
    let scheme = S.scheme

    let title = S.title

    let description = S.description

    type pk_uri = Uri.t

    type sk_uri = Uri.t

    type public_key_hash = Signature.Public_key_hash.t

    type public_key = Signature.Public_key.t

    type secret_key = Signature.Secret_key.t

    type signature = Signature.t

    let neuterize = S.neuterize

    let import_secret_key ~io sk =
      let open Lwt_result_syntax in
      let* pkh, pk = S.import_secret_key ~io sk in
      let*? pkh = Signature.Adapter.public_key_hash pkh in
      let*? pk = Option.map_e Signature.Adapter.public_key pk in
      return (pkh, pk)

    let public_key pk =
      let open Lwt_result_syntax in
      let* pk = S.public_key pk in
      let*? pk = Signature.Adapter.public_key pk in
      return pk

    let public_key_hash pk =
      let open Lwt_result_syntax in
      let* pkh, pk = S.public_key_hash pk in
      let*? pkh = Signature.Adapter.public_key_hash pkh in
      let*? pk = Option.map_e Signature.Adapter.public_key pk in
      return (pkh, pk)

    let sign ?watermark sk msg =
      let open Lwt_result_syntax in
      let* signature = S.sign ?watermark sk msg in
      let*? signature = Signature.Adapter.signature signature in
      return signature

    let deterministic_nonce = S.deterministic_nonce

    let deterministic_nonce_hash = S.deterministic_nonce_hash

    let supports_deterministic_nonces = S.supports_deterministic_nonces
  end

  let adapt_signer (module Signer : SIGNER) =
    let module V_Signer = Adapt (Signer) in
    (module V_Signer : V_SIGNER)

  let with_scheme_signer (uri : Uri.t) (f : signer -> 'a tzresult Lwt.t) :
      'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    match Uri.scheme uri with
    | None -> tzfail @@ Unexisting_scheme uri
    | Some scheme ->
        let*? signer = find_signer_for_key ~scheme in
        f signer

  let find_simple_signer_for_key ~scheme =
    let open Result_syntax in
    let* signer = find_signer_for_key ~scheme in
    match signer with
    | Simple signer -> return (adapt_signer signer)
    | Aggregate _signer -> tzfail (Wrong_key_scheme ("simple", "aggregate"))

  let with_scheme_simple_signer (uri : Uri.t)
      (f : (module V_SIGNER) -> 'a tzresult Lwt.t) : 'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    match Uri.scheme uri with
    | None -> tzfail @@ Unexisting_scheme uri
    | Some scheme ->
        let*? signer = find_simple_signer_for_key ~scheme in
        f signer

  let neuterize (sk_uri : sk_uri) : pk_uri tzresult Lwt.t =
    with_scheme_simple_signer sk_uri (fun (module Signer) ->
        Signer.neuterize sk_uri)

  let public_key pk_uri =
    with_scheme_simple_signer pk_uri (fun (module Signer) ->
        Signer.public_key pk_uri)

  let public_key_hash pk_uri =
    with_scheme_simple_signer pk_uri (fun (module Signer) ->
        Signer.public_key_hash pk_uri)

  let import_secret_key ~io pk_uri =
    with_scheme_simple_signer pk_uri (fun (module Signer) ->
        Signer.import_secret_key ~io pk_uri)

  let sign cctxt ?watermark sk_uri buf =
    let open Lwt_result_syntax in
    with_scheme_simple_signer sk_uri (fun (module Signer) ->
        let* signature = Signer.sign ?watermark sk_uri buf in
        let* pk_uri = Signer.neuterize sk_uri in
        let* pubkey =
          let* o = Secret_key.rev_find cctxt sk_uri in
          match o with
          | None -> public_key pk_uri
          | Some name -> (
              let* r = Public_key.find cctxt name in
              match r with
              | _, None ->
                  let* pk = public_key pk_uri in
                  let* () = Public_key.update cctxt name (pk_uri, Some pk) in
                  return pk
              | _, Some pubkey -> return pubkey)
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
    with_scheme_simple_signer sk_uri (fun (module Signer) ->
        Signer.deterministic_nonce sk_uri data)

  let deterministic_nonce_hash sk_uri data =
    with_scheme_simple_signer sk_uri (fun (module Signer) ->
        Signer.deterministic_nonce_hash sk_uri data)

  let supports_deterministic_nonces sk_uri =
    let open Lwt_result_syntax in
    with_scheme_signer sk_uri (function
        | Simple (module Signer : SIGNER) ->
            Signer.supports_deterministic_nonces sk_uri
        | Aggregate _ -> return_false)

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
        (List.map
           (fun (name, _, pk, pk_uri, _) -> (name, (pk_uri, Some pk)))
           xs)
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

  (* For efficiency, this function avoids loading the wallet, except for
     the call to [Public_key.update]. Indeed the arguments [pkhs],
     [pks], [sks] represent the already loaded list of public key
     hashes, public keys, and secret keys. *)
  let raw_get_key_aux (cctxt : #Client_context.wallet) pkhs pks sks pkh =
    let open Lwt_result_syntax in
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
          let module Signer = (val signer) in
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
    let open Lwt_result_syntax in
    let* r = raw_get_key cctxt pkh in
    match r with
    | pkh, Some pk, Some sk -> return (pkh, pk, sk)
    | _pkh, _pk, None ->
        failwith "Unknown secret key for %a" Signature.Public_key_hash.pp pkh
    | _pkh, None, _sk ->
        failwith "Unknown public key for %a" Signature.Public_key_hash.pp pkh

  let get_public_key cctxt pkh =
    let open Lwt_result_syntax in
    let* r = raw_get_key cctxt pkh in
    match r with
    | pkh, Some pk, _sk -> return (pkh, pk)
    | _pkh, None, _sk ->
        failwith "Unknown public key for %a" Signature.Public_key_hash.pp pkh

  let get_keys (cctxt : #Client_context.wallet) =
    let open Lwt_result_syntax in
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
                      let* () =
                        Public_key.update cctxt name (pk_uri, Some pk)
                      in
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
    Tezos_clic.switch ~long:"force" ~short:'f' ~doc:"overwrite existing keys" ()
end

module V0 = Make (struct
  include Tezos_crypto.Signature.V0

  let generate_key = generate_key ?algo:None

  module Adapter = struct
    let public_key_hash :
        Tezos_crypto.Signature.Public_key_hash.t -> Public_key_hash.t tzresult =
      let open Result_syntax in
      function
      | Bls _ ->
          tzfail (Exn (Failure "BLS public key hash not supported by V0"))
      | Ed25519 k -> return (Ed25519 k : Public_key_hash.t)
      | Secp256k1 k -> return (Secp256k1 k : Public_key_hash.t)
      | P256 k -> return (P256 k : Public_key_hash.t)

    let public_key :
        Tezos_crypto.Signature.Public_key.t -> Public_key.t tzresult =
      let open Result_syntax in
      function
      | Bls _ -> tzfail (Exn (Failure "BLS public key not supported by V0"))
      | Ed25519 k -> return (Ed25519 k : Public_key.t)
      | Secp256k1 k -> return (Secp256k1 k : Public_key.t)
      | P256 k -> return (P256 k : Public_key.t)

    let signature : Tezos_crypto.Signature.t -> t tzresult =
      let open Result_syntax in
      function
      | Bls _ -> tzfail (Exn (Failure "BLS signature not supported by V0"))
      | Ed25519 k -> return (Ed25519 k : t)
      | Secp256k1 k -> return (Secp256k1 k : t)
      | P256 k -> return (P256 k : t)
      | Unknown k -> return (Unknown k : t)
  end
end)

module V1 = Make (struct
  include Tezos_crypto.Signature.V1

  let generate_key = generate_key ?algo:None

  module Adapter = struct
    let identity x = Ok x

    let public_key_hash = identity

    let public_key = identity

    let signature = identity
  end
end)

module V_latest = Make (struct
  include Tezos_crypto.Signature.V_latest

  let generate_key = generate_key ?algo:None

  module Adapter = struct
    let identity x = Ok x

    let public_key_hash = identity

    let public_key = identity

    let signature = identity
  end
end)

include V_latest

module Mnemonic = struct
  let new_random = Bip39.of_entropy (Tezos_crypto.Hacl.Rand.gen 32)

  let to_32_bytes mnemonic =
    let seed_64_to_seed_32 (seed_64 : bytes) : bytes =
      assert (Bytes.length seed_64 = 64) ;
      let first_32 = Bytes.sub seed_64 0 32 in
      let second_32 = Bytes.sub seed_64 32 32 in
      let seed_32 = Bytes.create 32 in
      for i = 0 to 31 do
        Bytes.set
          seed_32
          i
          (Char.chr
             (Char.code (Bytes.get first_32 i)
             lxor Char.code (Bytes.get second_32 i)))
      done ;
      seed_32
    in
    seed_64_to_seed_32 (Bip39.to_seed mnemonic)

  let words_pp = Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
end
