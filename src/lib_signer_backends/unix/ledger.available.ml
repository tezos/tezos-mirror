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

module Events = Signer_events
open Client_keys

module Bip32_path = struct
  let hard = Int32.logor 0x8000_0000l

  let unhard = Int32.logand 0x7fff_ffffl

  let is_hard n = Int32.logand 0x8000_0000l n <> 0l

  let tezos_root = [hard 44l; hard 1729l]

  let pp_node ppf node =
    match is_hard node with
    | true -> Fmt.pf ppf "%ldh" (unhard node)
    | false -> Fmt.pf ppf "%ld" node

  let pp_path = Fmt.(list ~sep:(const char '/') pp_node)

  let string_of_path = Fmt.to_to_string pp_path
end

type error +=
  | LedgerError of Ledgerwallet.Transport.error
  | Ledger_signing_hash_mismatch of string * string
  | Ledger_msg_chunk_too_long of string

let error_encoding =
  let open Data_encoding in
  conv
    (fun e -> Format.asprintf "%a" Ledgerwallet.Transport.pp_error e)
    (fun _ -> invalid_arg "Ledger error is not deserializable")
    (obj1 (req "ledger-error" string))

let () =
  register_error_kind
    `Permanent
    ~id:"signer.ledger"
    ~title:"Ledger error"
    ~description:"Error communicating with a Ledger Nano device"
    ~pp:(fun ppf e ->
      Format.fprintf ppf "@[Ledger %a@]" Ledgerwallet.Transport.pp_error e)
    error_encoding
    (function LedgerError e -> Some e | _ -> None)
    (fun e -> LedgerError e)

let () =
  register_error_kind
    `Permanent
    ~id:"signer.ledger.msg-chunk-too-big"
    ~title:"Too long message chunk sent to a ledger"
    ~description:"Too long message chunk sent to a ledger."
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Chunk of ledger message %s too long, the ledger app may not be up to \
         date."
        msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Ledger_msg_chunk_too_long msg -> Some msg | _ -> None)
    (fun msg -> Ledger_msg_chunk_too_long msg)

let () =
  let description ledger_hash computed_hash =
    let paren fmt hash_opt =
      match Base.Option.bind ~f:Tezos_crypto.Blake2B.of_string_opt hash_opt with
      | None -> ()
      | Some hash ->
          Format.fprintf fmt " (%a)" Tezos_crypto.Blake2B.pp_short hash
    in
    Format.asprintf
      "The ledger returned a hash%a which doesn't match the independently \
       computed hash%a."
      paren
      ledger_hash
      paren
      computed_hash
  in
  register_error_kind
    `Permanent
    ~id:"signer.ledger.signing-hash-mismatch"
    ~title:"Ledger signing-hash mismatch"
    ~description:(description None None)
    ~pp:(fun ppf (lh, ch) ->
      Format.pp_print_string ppf (description (Some lh) (Some ch)))
    Data_encoding.(obj2 (req "ledger-hash" string) (req "computed-hash" string))
    (function
      | Ledger_signing_hash_mismatch (lh, ch) -> Some (lh, ch) | _ -> None)
    (fun (lh, ch) -> Ledger_signing_hash_mismatch (lh, ch))

let pp_round_opt fmt = function
  | None -> ()
  | Some x -> Format.fprintf fmt " (round: %ld)" x

(** Wrappers around Ledger APDUs. *)
module Ledger_commands = struct
  let wrap_ledger_cmd f =
    let open Lwt_result_syntax in
    let buf = Buffer.create 100 in
    let pp =
      Format.make_formatter
        (fun s ofs lgth -> Buffer.add_substring buf s ofs lgth)
        (fun () ->
          Events.(emit__dont_wait__use_with_care Ledger.communication)
            (Buffer.contents buf) ;
          Buffer.clear buf)
    in
    let res = f pp in
    match res with
    | Error
        (Ledgerwallet.Transport.AppError
          {status = Ledgerwallet.Status.Incorrect_length_for_ins; msg}) ->
        tzfail (Ledger_msg_chunk_too_long msg)
    | Error err -> tzfail (LedgerError err)
    | Ok v -> return v

  let get_version ~device_name h =
    let open Lwt_result_syntax in
    let buf = Buffer.create 100 in
    let pp = Format.formatter_of_buffer buf in
    let version = Ledgerwallet_tezos.get_version ~pp h in
    let*! () = Events.(emit Ledger.communication) (Buffer.contents buf) in
    match version with
    | Error e ->
        let*! () =
          Events.(emit Ledger.not_tezos)
            ( device_name,
              Format.asprintf "@[Ledger %a@]" Ledgerwallet.Transport.pp_error e
            )
        in
        return_none
    | Ok version ->
        let* () =
          if (version.major, version.minor) < (1, 4) then
            failwith
              "Version %a of the ledger apps is not supported by this client"
              Ledgerwallet_tezos.Version.pp
              version
          else return_unit
        in
        let* git_commit =
          wrap_ledger_cmd (fun pp -> Ledgerwallet_tezos.get_git_commit ~pp h)
        in
        let*! () =
          Events.(emit Ledger.found_application)
            ( Format.asprintf "%a" Ledgerwallet_tezos.Version.pp version,
              device_name,
              git_commit )
        in
        let cleaned_up =
          (* The ledger sends a NUL-terminated C-String: *)
          if git_commit.[String.length git_commit - 1] = '\x00' then
            String.sub git_commit 0 (String.length git_commit - 1)
          else git_commit
        in
        return_some (version, cleaned_up)

  let secp256k1_ctx =
    Libsecp256k1.External.Context.create ~sign:false ~verify:false ()

  let public_key_returning_instruction which ?(prompt = false) hidapi curve path
      =
    let open Lwt_result_syntax in
    let path = Bip32_path.tezos_root @ path in
    let+ pk =
      match which with
      | `Get_public_key ->
          wrap_ledger_cmd (fun pp ->
              Ledgerwallet_tezos.get_public_key ~prompt ~pp hidapi curve path)
      | `Authorize_baking ->
          wrap_ledger_cmd (fun pp ->
              Ledgerwallet_tezos.authorize_baking ~pp hidapi curve path)
      | `Setup (main_chain_id, main_hwm, test_hwm) ->
          wrap_ledger_cmd (fun pp ->
              Ledgerwallet_tezos.setup_baking
                ~pp
                hidapi
                curve
                path
                ~main_chain_id
                ~main_hwm
                ~test_hwm)
    in
    match curve with
    | Ed25519 | Bip32_ed25519 ->
        let pk = Cstruct.to_bytes pk in
        TzEndian.set_int8 pk 0 0 ;
        (* hackish, but works. *)
        Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding pk
    | Secp256k1 ->
        let open Libsecp256k1.External in
        let buf = Bigstring.create (Key.compressed_pk_bytes + 1) in
        let pk = Key.read_pk_exn secp256k1_ctx (Cstruct.to_bigarray pk) in
        EndianBigstring.BigEndian.set_int8 buf 0 1 ;
        let _nb_written = Key.write secp256k1_ctx ~pos:1 buf pk in
        Data_encoding.Binary.of_bytes_exn
          Signature.Public_key.encoding
          (Bigstring.to_bytes buf)
    | Secp256r1 -> (
        let open Tezos_crypto.Hacl.P256 in
        let buf = Bytes.create (pk_size + 1) in
        match pk_of_bytes (Cstruct.to_bytes pk) with
        | None ->
            Stdlib.failwith "Impossible to read P256 public key from Ledger"
        | Some pk ->
            TzEndian.set_int8 buf 0 2 ;
            blit_to_bytes pk ~pos:1 buf ;
            Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding buf)

  let get_public_key = public_key_returning_instruction `Get_public_key

  let pkh_of_pk = Signature.Public_key.hash

  let public_key ?(first_import : Client_context.io_wallet option) hid curve
      path =
    let open Lwt_result_syntax in
    match first_import with
    | Some cctxt ->
        let* pk = get_public_key ~prompt:false hid curve path in
        let pkh = pkh_of_pk pk in
        let*! () =
          cctxt#message
            "Please validate@ (and write down)@ the public key hash@ \
             displayed@ on the Ledger,@ it should be equal@ to `%a`:"
            Signature.Public_key_hash.pp
            pkh
        in
        get_public_key ~prompt:true hid curve path
    | None -> get_public_key ~prompt:false hid curve path

  let public_key_hash ?first_import hid curve path =
    let open Lwt_result_syntax in
    let* pk = public_key ?first_import hid curve path in
    return (pkh_of_pk pk, pk)

  let get_authorized_path hid version =
    let open Lwt_result_syntax in
    let open Ledgerwallet_tezos.Version in
    if version.major < 2 then
      let+ path =
        wrap_ledger_cmd (fun pp ->
            Ledgerwallet_tezos.get_authorized_key ~pp hid)
      in
      `Legacy_path path
    else
      let*! r =
        wrap_ledger_cmd (fun pp ->
            Ledgerwallet_tezos.get_authorized_path_and_curve ~pp hid)
      in
      match r with
      | Error
          (LedgerError
             (AppError
               {status = Ledgerwallet.Status.Referenced_data_not_found; _})
          :: _) ->
          return `No_baking_authorized
      | Error _ as e -> Lwt.return e
      | Ok (path, curve) -> return (`Path_curve (path, curve))

  let sign ?watermark ~version hid curve path (base_msg : Bytes.t) =
    let open Lwt_result_syntax in
    let msg =
      Option.fold watermark ~none:base_msg ~some:(fun watermark ->
          Bytes.cat (Signature.bytes_of_watermark watermark) base_msg)
    in
    let path = Bip32_path.tezos_root @ path in
    let* hash_opt, signature =
      wrap_ledger_cmd (fun pp ->
          let {Ledgerwallet_tezos.Version.major; minor; patch; _} = version in
          let open Result_syntax in
          if (major, minor, patch) <= (2, 0, 0) then
            let* s =
              Ledgerwallet_tezos.sign ~pp hid curve path (Cstruct.of_bytes msg)
            in
            Ok (None, s)
          else
            let* h, s =
              Ledgerwallet_tezos.sign_and_hash
                ~pp
                hid
                curve
                path
                (Cstruct.of_bytes msg)
            in
            Ok (Some h, s))
    in
    let* () =
      match hash_opt with
      | None -> return_unit
      | Some hsh ->
          let hash_msg = Tezos_crypto.Blake2B.hash_bytes [msg] in
          let ledger_one =
            Tezos_crypto.Blake2B.of_bytes_exn (Cstruct.to_bytes hsh)
          in
          if Tezos_crypto.Blake2B.equal hash_msg ledger_one then return_unit
          else
            tzfail
              (Ledger_signing_hash_mismatch
                 ( Tezos_crypto.Blake2B.to_string ledger_one,
                   Tezos_crypto.Blake2B.to_string hash_msg ))
    in
    match curve with
    | Ed25519 | Bip32_ed25519 ->
        let signature =
          Signature.Ed25519.of_bytes_exn (Cstruct.to_bytes signature)
        in
        return (Signature.of_ed25519 signature)
    | Secp256k1 ->
        (* Remove parity info *)
        Cstruct.(set_uint8 signature 0 (get_uint8 signature 0 land 0xfe)) ;
        let signature = Cstruct.to_bigarray signature in
        let open Libsecp256k1.External in
        let signature = Sign.read_der_exn secp256k1_ctx signature in
        let bytes = Sign.to_bytes secp256k1_ctx signature in
        let signature =
          Signature.Secp256k1.of_bytes_exn (Bigstring.to_bytes bytes)
        in
        return (Signature.of_secp256k1 signature)
    | Secp256r1 ->
        (* Remove parity info *)
        Cstruct.(set_uint8 signature 0 (get_uint8 signature 0 land 0xfe)) ;
        let signature = Cstruct.to_bigarray signature in
        let open Libsecp256k1.External in
        (* We use secp256r1 library to extract P256 DER signature. *)
        let signature = Sign.read_der_exn secp256k1_ctx signature in
        let buf = Sign.to_bytes secp256k1_ctx signature in
        let signature = Signature.P256.of_bytes_exn (Bigstring.to_bytes buf) in
        return (Signature.of_p256 signature)

  let get_deterministic_nonce hid curve path msg =
    let open Lwt_result_syntax in
    let path = Bip32_path.tezos_root @ path in
    let* nonce =
      wrap_ledger_cmd (fun pp ->
          Ledgerwallet_tezos.get_deterministic_nonce
            ~pp
            hid
            curve
            path
            (Cstruct.of_bytes msg))
    in
    return (Bigstring.of_bytes (Cstruct.to_bytes nonce))
end

(** Identification of a ledger's root key through crouching-tigers
    (not the keys used for an account). *)
module Ledger_id = struct
  (**
     The “ID” of the ledger is the animals (or pkh) corresponding to
     ["/ed25519/"] (first curve, no path).
  *)
  type t = Animals of Ledger_names.t | Pkh of Signature.public_key_hash

  let animals_of_pkh pkh =
    pkh |> Signature.Public_key_hash.to_string |> Ledger_names.crouching_tiger

  let curve = Ledgerwallet_tezos.Ed25519

  let get hidapi =
    let open Lwt_result_syntax in
    let* pk = Ledger_commands.get_public_key hidapi curve [] in
    let pkh = Signature.Public_key.hash pk in
    let animals = animals_of_pkh pkh in
    return (Animals animals)

  let pp ppf = function
    | Animals a -> Ledger_names.pp ppf a
    | Pkh pkh -> Signature.Public_key_hash.pp ppf pkh

  let to_animals = function Animals a -> a | Pkh pkh -> animals_of_pkh pkh

  let equal a b = to_animals a = to_animals b

  let hash = Hashtbl.hash
end

(** An account is a given key-pair corresponding to a
    [ledger + curve + derivation-path]. *)
module Ledger_account = struct
  type t = {
    ledger : Ledger_id.t;
    curve : Ledgerwallet_tezos.curve;
    path : int32 list;
  }
end

(** {!Ledger_uri.t} represents a parsed ["ledger://..."] URI which may
    refer to a {!Ledger_id.t} or a full blown {!Ledger_account.t}. *)
module Ledger_uri = struct
  type t = [`Ledger of Ledger_id.t | `Ledger_account of Ledger_account.t]

  let int32_of_path_element_exn ~allow_weak x =
    let failf ppf = Printf.ksprintf Stdlib.failwith ppf in
    let len = String.length x in
    if len = 0 then failf "Empty path element"
    else
      match x.[len - 1] with
      | '\'' | 'h' -> (
          let intpart = String.sub x 0 (len - 1) in
          match Int32.of_string_opt intpart with
          | Some i -> Bip32_path.hard i
          | None -> failf "Path is not an integer: %S" intpart)
      | _ when allow_weak -> (
          match Int32.of_string_opt x with
          | Some i -> i
          | None -> failf "Path is not a non-hardened integer: %S" x)
      | _ ->
          failf
            "Non-hardened paths are not allowed for this derivation scheme (%S)"
            x

  let parse_animals animals =
    match String.split_no_empty '-' animals with
    | [c; t; h; d] -> Some {Ledger_names.c; t; h; d}
    | _ -> None

  let derivation_supports_weak_paths = function
    | Ledgerwallet_tezos.Ed25519 -> false
    | Ledgerwallet_tezos.Secp256k1 -> true
    | Ledgerwallet_tezos.Secp256r1 -> true
    | Ledgerwallet_tezos.Bip32_ed25519 -> true

  let parse ?allow_weak uri : t tzresult Lwt.t =
    let open Lwt_result_syntax in
    let host = Uri.host uri in
    let* ledger =
      match Option.bind host Signature.Public_key_hash.of_b58check_opt with
      | Some pkh -> return (Ledger_id.Pkh pkh)
      | None -> (
          match Option.bind host parse_animals with
          | Some animals -> return (Ledger_id.Animals animals)
          | None -> failwith "Cannot parse host of URI: %s" (Uri.to_string uri))
    in
    let components = String.split_no_empty '/' (Uri.path uri) in
    match components with
    | s :: tl ->
        let curve, more_path =
          match Ledgerwallet_tezos.curve_of_string s with
          | Some curve -> (curve, tl)
          | None -> (Ledger_id.curve, s :: tl)
        in
        let actually_allow_weak =
          match allow_weak with
          | None -> derivation_supports_weak_paths curve
          | Some x -> x
        in
        let* bip32 =
          try
            return
              (List.map
                 (int32_of_path_element_exn ~allow_weak:actually_allow_weak)
                 more_path)
          with Failure s ->
            failwith
              "Failed to parse Curve/BIP32 path from %s (%s): %s"
              (Uri.path uri)
              (Uri.to_string uri)
              s
        in
        return (`Ledger_account Ledger_account.{ledger; curve; path = bip32})
    | [] -> return (`Ledger ledger)

  let ledger_uri_or_alias_param next =
    let open Lwt_result_syntax in
    let name = "account-alias-or-ledger-uri" in
    let desc =
      "An imported ledger alias or a ledger URI (e.g. \
       \"ledger://animal/curve/path\")."
    in
    let open Tezos_clic in
    param
      ~name
      ~desc
      (parameter (fun cctxt str ->
           let* uri =
             let* o = Public_key.find_opt cctxt str in
             match o with
             | Some ((x : pk_uri), _) -> return (x :> Uri.t)
             | None -> (
                 try return (Uri.of_string str)
                 with e ->
                   failwith "Error while parsing URI: %s" (Printexc.to_string e)
                 )
           in
           parse uri))
      next

  let pp : _ -> t -> unit =
   fun ppf ->
    Format.(
      function
      | `Ledger lid -> fprintf ppf "ledger://%a" Ledger_id.pp lid
      | `Ledger_account {Ledger_account.ledger; curve; path} ->
          fprintf
            ppf
            "ledger://%a/%a/%a"
            Ledger_id.pp
            ledger
            Ledgerwallet_tezos.pp_curve
            curve
            Bip32_path.pp_path
            path)

  let if_matches (meta_uri : t) ledger_id cont =
    let open Lwt_result_syntax in
    match meta_uri with
    | `Ledger l -> if Ledger_id.equal l ledger_id then cont () else return_none
    | `Ledger_account {Ledger_account.ledger; _} ->
        if Ledger_id.equal ledger ledger_id then cont () else return_none

  let full_account (ledger_uri : t) =
    let open Lwt_result_syntax in
    match ledger_uri with
    | `Ledger_account acc -> return acc
    | `Ledger ledger_id ->
        failwith
          "Insufficient information: you need to provide a curve & BIP32 path \
           (%a)."
          Ledger_id.pp
          ledger_id
end

(** Filters allow early dismissal of HID devices/ledgers which
    searching for a ledger. *)
module Filter = struct
  type version_filter = Ledgerwallet_tezos.Version.t * string -> bool

  type t = [`None | `Hid_path of string | `Version of string * version_filter]

  let version_matches (t : t) version_commit =
    match t with `Version (_, f) -> f version_commit | _ -> true

  let is_app : _ -> _ -> t =
   fun msg app ->
    `Version
      ( msg,
        fun ({Ledgerwallet_tezos.Version.app_class; _}, _) -> app = app_class )

  let is_baking = is_app "App = Baking" Ledgerwallet_tezos.Version.TezBake

  let pp ppf (f : t) =
    let open Format in
    match f with
    | `None -> fprintf ppf "None"
    | `Hid_path s -> fprintf ppf "HID-path: %s" s
    | `Version (s, _) -> fprintf ppf "%s" s
end

module Request_cache =
  Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong) (Ledger_id)

type request_info = {
  version : Ledgerwallet_tezos.Version.t;
  git_commit : string;
  ledger_id : Ledger_id.t;
  path : Ledgerwallet.Transport.path;
}

let cache = Request_cache.create 7

let path_name (path : Ledgerwallet.Transport.path) =
  match path with
  | Hidapi_path device_info -> device_info.path
  | Proxy_path _ -> "proxy"

type 'a ledger_function =
  Ledgerwallet.Transport.t ->
  Ledgerwallet_tezos.Version.t * string ->
  device_path:Ledgerwallet.Transport.path ->
  Ledger_id.t ->
  ('a option, tztrace) result Lwt.t

let use_ledger ?(filter : Filter.t = `None) (f : 'a ledger_function) =
  let open Lwt_result_syntax in
  let ledgers = Ledgerwallet.Transport.enumerate () in
  let*! () =
    Events.(emit Ledger.found)
      ( List.length ledgers,
        String.concat
          " -- "
          (List.map
             (fun (path : Ledgerwallet.Transport.path) ->
               match path with
               | Proxy_path _ -> "Proxy"
               | Hidapi_path l ->
                   Printf.sprintf "(%04x, %04x)" l.vendor_id l.product_id)
             ledgers) )
  in
  let selected (path : Ledgerwallet.Transport.path) =
    let id_ok =
      match path with
      | Proxy_path _ -> true
      | Hidapi_path device_info ->
          (* HID interfaces get the number 0
             (cf. https://github.com/LedgerHQ/ledger-nano-s/issues/48)
             *BUT* on MacOSX the Hidapi library does not report the interface-number
             so we look at the usage-page (which is even more unspecified but used by
             prominent Ledger users:
             https://github.com/LedgerHQ/ledgerjs/commit/333ade0d55dc9c59bcc4b451cf7c976e78629681).
          *)
          device_info.Hidapi.interface_number = 0
          || device_info.Hidapi.interface_number = -1
             && device_info.Hidapi.usage_page = 0xffa0
    in
    id_ok
    &&
    match filter with
    | `None -> true
    | `Version _ -> true
    | `Hid_path hp -> (
        match path with
        | Proxy_path _ -> false
        | Hidapi_path device_info -> device_info.path = hp)
  in

  let process_device (path : Ledgerwallet.Transport.path) =
    let device_name = path_name path in
    if not (selected path) then return_none
    else
      let*! () = Events.(emit Ledger.processing) device_name in
      match Ledgerwallet.Transport.open_path path with
      | None -> return_none
      | Some h ->
          Lwt.finalize
            (fun () ->
              let* o = Ledger_commands.get_version ~device_name h in
              match o with
              | Some version_git when Filter.version_matches filter version_git
                ->
                  let* ledger_id = Ledger_id.get h in
                  f h version_git ~device_path:path ledger_id
              | None | Some _ -> return_none)
            (fun () ->
              Ledgerwallet.Transport.close h ;
              Lwt.return_unit)
  in
  let rec go = function
    | [] -> return_none
    | h :: t -> (
        let* o = process_device h in
        match o with Some x -> return_some x | None -> go t)
  in
  go ledgers

let min_version_of_derivation_scheme = function
  | Ledgerwallet_tezos.Ed25519 -> (1, 3, 0)
  | Ledgerwallet_tezos.Secp256k1 -> (1, 3, 0)
  | Ledgerwallet_tezos.Secp256r1 -> (1, 3, 0)
  | Ledgerwallet_tezos.Bip32_ed25519 -> (2, 1, 0)

let is_derivation_scheme_supported version curve =
  Ledgerwallet_tezos.Version.(
    let {major; minor; patch; _} = version in
    (major, minor, patch) >= min_version_of_derivation_scheme curve)

let use_ledger_or_fail ~ledger_uri ?(filter = `None) ?msg
    (f : 'a ledger_function) =
  let open Lwt_result_syntax in
  let f hidapi (version, git_commit) ~device_path ledger_id =
    let go () = f hidapi (version, git_commit) ~device_path ledger_id in
    match ledger_uri with
    | `Ledger_account {Ledger_account.curve; _} ->
        if is_derivation_scheme_supported version curve then go ()
        else
          Ledgerwallet_tezos.(
            failwith
              "To use derivation scheme %a you need %a or later but you're \
               using %a."
              pp_curve
              curve
              Version.pp
              (let a, b, c = min_version_of_derivation_scheme curve in
               {version with major = a; minor = b; patch = c})
              Version.pp
              version)
    | _ -> go ()
  in
  let* o =
    let check_filter (transport_path, version, git_commit) =
      match (filter : Filter.t) with
      | `None -> true
      | `Hid_path path -> (
          match (transport_path : Ledgerwallet.Transport.path) with
          | Hidapi_path device_info -> device_info.path = path
          | Proxy_path _ -> false)
      | `Version (_, f) -> f (version, git_commit)
    in
    let ledger_id =
      match ledger_uri with
      | `Ledger id -> id
      | `Ledger_account {ledger; _} -> ledger
    in
    match Request_cache.find_opt cache ledger_id with
    | Some {path; version; git_commit; ledger_id}
      when check_filter (path, version, git_commit) -> (
        match Ledgerwallet.Transport.(open_path path) with
        | None -> return_none
        | Some hidapi ->
            Lwt.finalize
              (fun () ->
                f hidapi (version, git_commit) ~device_path:path ledger_id)
              (fun () ->
                Ledgerwallet.Transport.close hidapi ;
                Lwt.return_unit))
    | Some _ | None ->
        use_ledger
          ~filter
          (fun hidapi (version, git_commit) ~device_path ledger_id ->
            Ledger_uri.if_matches ledger_uri ledger_id (fun () ->
                Request_cache.replace
                  cache
                  ledger_id
                  {path = device_path; version; git_commit; ledger_id} ;
                f hidapi (version, git_commit) ~device_path ledger_id))
  in
  match o with
  | Some o -> return o
  | None ->
      failwith
        "%aFound no ledger corresponding to %a%t."
        (Format.pp_print_option (fun fmt s -> Format.fprintf fmt "%s: " s))
        msg
        Ledger_uri.pp
        ledger_uri
        (fun ppf -> Format.fprintf ppf " with filter \"%a\"" Filter.pp filter)

(** A global {!Hashtbl.t} which allows us to avoid calling
    {!Signer_implementation.get_public_key} too often. *)
module Global_cache : sig
  val record :
    pk_uri -> pk:Signature.public_key -> pkh:Signature.public_key_hash -> unit

  val get : pk_uri -> (Signature.public_key_hash * Signature.public_key) option
end = struct
  let cache :
      (Signature.Public_key_hash.t * Signature.Public_key.t)
      Client_keys.Pk_uri_hashtbl.t =
    Client_keys.Pk_uri_hashtbl.create 13

  let record pk_uri ~pk ~pkh =
    Client_keys.Pk_uri_hashtbl.replace cache pk_uri (pkh, pk)

  let get pk_uri = Client_keys.Pk_uri_hashtbl.find cache pk_uri
end

(** The implementation of the “signer-plugin.” *)
module Signer_implementation : Client_keys.SIGNER = struct
  let scheme = "ledger"

  let title = "Built-in signer using a Ledger Nano device."

  let description =
    Printf.sprintf
      "Valid URIs are of the form\n\
      \ - ledger://<animals>/<curve>[/<path>]\n\
       where:\n\
      \ - <animals> is the identifier of the ledger of the form \
       'crouching-tiger-hidden-dragon' and can be obtained with the command \
       `octez-client list connected ledgers` (which also provides full \
       examples).\n\
       - <curve> is the signing curve, e.g. `ed1551`\n\
       - <path> is a BIP32 path anchored at m/%s. The ledger does not yet \
       support non-hardened paths, so each node of the path must be hardened."
      Bip32_path.(string_of_path tezos_root)

  include Client_keys.Signature_type

  let neuterize (sk : sk_uri) =
    let open Lwt_result_syntax in
    let*? v = make_pk_uri (sk :> Uri.t) in
    return v

  let pkh_of_pk = Signature.Public_key.hash

  let public_key_maybe_prompt ?(first_import : Client_context.io_wallet option)
      (pk_uri : pk_uri) =
    let open Lwt_result_syntax in
    match Global_cache.get pk_uri with
    | Some (_, pk) -> return pk
    | None -> (
        let*! r =
          let* ledger_uri = Ledger_uri.parse (pk_uri :> Uri.t) in
          let* {curve; path; _} = Ledger_uri.full_account ledger_uri in
          use_ledger_or_fail
            ~ledger_uri
            (fun hidapi (_version, _git_commit) ~device_path:_ _ledger_id ->
              let* pk =
                Ledger_commands.public_key ?first_import hidapi curve path
              in
              let pkh = pkh_of_pk pk in
              Global_cache.record pk_uri ~pkh ~pk ;
              return_some pk)
        in
        match r with
        | Error err -> failwith "%a" pp_print_trace err
        | Ok v -> return v)

  let public_key_hash_maybe_prompt ?first_import pk_uri =
    let open Lwt_result_syntax in
    match Global_cache.get pk_uri with
    | Some (pkh, pk) -> return (pkh, Some pk)
    | None ->
        let* pk = public_key_maybe_prompt ?first_import pk_uri in
        return (pkh_of_pk pk, Some pk)

  let public_key = public_key_maybe_prompt ?first_import:None

  let public_key_hash = public_key_hash_maybe_prompt ?first_import:None

  let import_secret_key ~io pk_uri =
    public_key_hash_maybe_prompt ~first_import:io pk_uri

  let sign ?watermark (sk_uri : sk_uri) msg =
    let open Lwt_result_syntax in
    let* ledger_uri = Ledger_uri.parse (sk_uri :> Uri.t) in
    let* {curve; path; _} = Ledger_uri.full_account ledger_uri in
    use_ledger_or_fail
      ~ledger_uri
      (fun hidapi (version, _git_commit) ~device_path:_ _ledger_id ->
        let* bytes =
          Ledger_commands.sign ?watermark ~version hidapi curve path msg
        in
        return_some bytes)

  let deterministic_nonce (sk_uri : sk_uri) msg =
    let open Lwt_result_syntax in
    let* ledger_uri = Ledger_uri.parse (sk_uri :> Uri.t) in
    let* {curve; path; _} = Ledger_uri.full_account ledger_uri in
    use_ledger_or_fail
      ~ledger_uri
      (fun hidapi (_version, _git_commit) ~device_path:_ _ledger_id ->
        let* bytes =
          Ledger_commands.get_deterministic_nonce hidapi curve path msg
        in
        return_some (Bigstring.to_bytes bytes))

  let deterministic_nonce_hash (sk : sk_uri) msg =
    let open Lwt_result_syntax in
    let* nonce = deterministic_nonce sk msg in
    return
      (Tezos_crypto.Blake2B.to_bytes (Tezos_crypto.Blake2B.hash_bytes [nonce]))

  let supports_deterministic_nonces _ = Lwt_result_syntax.return_true
end

(* The Ledger uses a special value 0x00000000 for the “any” chain-id: *)
let pp_ledger_chain_id fmt s =
  match s with
  | "\x00\x00\x00\x00" -> Format.fprintf fmt "'Unspecified'"
  | other -> Format.fprintf fmt "%a" Chain_id.pp (Chain_id.of_string_exn other)

(** Commands for both ledger applications. *)
let generic_commands group =
  let open Lwt_result_syntax in
  Tezos_clic.
    [
      command
        ~group
        ~desc:"List supported Ledger Nano devices connected."
        no_options
        (fixed ["list"; "connected"; "ledgers"])
        (fun () (cctxt : Client_context.full) ->
          let* _ =
            use_ledger
              (fun _hidapi (version, git_commit) ~device_path ledger_id ->
                let open Hidapi in
                let*! () =
                  cctxt#message
                    "%t"
                    Format.(
                      fun ppf ->
                        let intro =
                          match device_path with
                          | Proxy_path _ ->
                              asprintf
                                "Found a %a (git-description: %S) application \
                                 running on proxy server."
                                Ledgerwallet_tezos.Version.pp
                                version
                                git_commit
                          | Hidapi_path device_info ->
                              asprintf
                                "Found a %a (git-description: %S) application \
                                 running on %s %s at [%s]."
                                Ledgerwallet_tezos.Version.pp
                                version
                                git_commit
                                (device_info.manufacturer_string
                                |> Option.value ~default:"NO-MANUFACTURER")
                                (device_info.product_string
                                |> Option.value ~default:"NO-PRODUCT")
                                device_info.path
                        in
                        pp_open_vbox ppf 0 ;
                        fprintf ppf "## Ledger `%a`@," Ledger_id.pp ledger_id ;
                        pp_open_hovbox ppf 0 ;
                        pp_print_text ppf intro ;
                        pp_close_box ppf () ;
                        pp_print_cut ppf () ;
                        pp_print_cut ppf () ;
                        pp_open_hovbox ppf 0 ;
                        pp_print_text
                          ppf
                          "To use keys at BIP32 path m/44'/1729'/0'/0' \
                           (default Tezos key path), use one of:" ;
                        pp_close_box ppf () ;
                        pp_print_cut ppf () ;
                        List.iter
                          (fun curve ->
                            fprintf
                              ppf
                              "  octez-client import secret key ledger_%s \
                               \"ledger://%a/%a/0h/0h\""
                              (Sys.getenv_opt "USER"
                              |> Option.value ~default:"user")
                              Ledger_id.pp
                              ledger_id
                              Ledgerwallet_tezos.pp_curve
                              curve ;
                            pp_print_cut ppf ())
                          (List.filter
                             (is_derivation_scheme_supported version)
                             [Ed25519; Secp256k1; Secp256r1; Bip32_ed25519]) ;
                        pp_close_box ppf () ;
                        pp_print_newline ppf ())
                in
                return_none)
          in
          return_unit);
      Tezos_clic.command
        ~group
        ~desc:"Display version/public-key/address information for a Ledger URI"
        (args1 (switch ~doc:"Test signing operation" ~long:"test-sign" ()))
        (prefixes ["show"; "ledger"]
        @@ Ledger_uri.ledger_uri_or_alias_param @@ stop)
        (fun test_sign ledger_uri (cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            (fun hidapi (version, git_commit) ~device_path _ledger_id ->
              let*! () =
                cctxt#message
                  "Found ledger corresponding to %a:"
                  Ledger_uri.pp
                  ledger_uri
              in
              let*! () =
                match device_path with
                | Proxy_path _ -> Lwt.return_unit
                | Hidapi_path device_info ->
                    let*! () =
                      cctxt#message
                        "* Manufacturer: %s"
                        (Option.value
                           device_info.manufacturer_string
                           ~default:"NONE")
                    in
                    let*! () =
                      cctxt#message
                        "* Product: %s"
                        (Option.value
                           device_info.product_string
                           ~default:"NONE")
                    in
                    let*! () =
                      cctxt#message
                        "* Application: %a (git-description: %S)"
                        Ledgerwallet_tezos.Version.pp
                        version
                        git_commit
                    in
                    Lwt.return_unit
              in
              let* () =
                match ledger_uri with
                | `Ledger_account {curve; path; _} -> (
                    let*! () =
                      cctxt#message
                        "* Curve: `%a`"
                        Ledgerwallet_tezos.pp_curve
                        curve
                    in
                    let full_path = Bip32_path.tezos_root @ path in
                    let*! () =
                      cctxt#message
                        "* Path: `%s` [%s]"
                        (Bip32_path.string_of_path full_path)
                        (String.concat
                           "; "
                           (List.map (Printf.sprintf "0x%lX") full_path))
                    in
                    let* pkh, pk =
                      Ledger_commands.public_key_hash hidapi curve path
                    in
                    let*! () =
                      cctxt#message
                        "* Public Key: %a"
                        Signature.Public_key.pp
                        pk
                    in
                    let*! () =
                      cctxt#message
                        "* Public Key Hash: %a@\n"
                        Signature.Public_key_hash.pp
                        pkh
                    in
                    match (test_sign, version.app_class) with
                    | true, Tezos -> (
                        let pkh_bytes =
                          Signature.Public_key_hash.to_bytes pkh
                        in
                        (* Signing requires validation on the device.  *)
                        let*! () =
                          cctxt#message
                            "@[Attempting a signature@ (of `%a`),@ please@ \
                             validate on@ the ledger.@]"
                            Hex.pp
                            (Hex.of_bytes pkh_bytes)
                        in
                        let* signature =
                          Ledger_commands.sign
                            ~version
                            ~watermark:Generic_operation
                            hidapi
                            curve
                            path
                            pkh_bytes
                        in
                        match
                          Signature.check
                            ~watermark:Generic_operation
                            pk
                            signature
                            pkh_bytes
                        with
                        | false ->
                            failwith
                              "Fatal: Ledger cannot sign with %a"
                              Signature.Public_key_hash.pp
                              pkh
                        | true ->
                            let*! () =
                              cctxt#message
                                "Tezos Wallet successfully signed:@ %a."
                                Signature.pp
                                signature
                            in
                            return_unit)
                    | true, TezBake ->
                        failwith
                          "Option --test-sign only works for the Tezos Wallet \
                           app."
                    | false, _ -> return_unit)
                | `Ledger _ when test_sign ->
                    failwith
                      "Option --test-sign only works with a full ledger \
                       URI/account (with curve/path)."
                | `Ledger _ ->
                    let*! () = cctxt#message "* This is just a ledger URI." in
                    return_unit
              in
              return_some ()));
    ]

(** Commands specific to the Baking app minus the high-water-mark ones
    which get a specific treatment in {!high_water_mark_commands}. *)
let baking_commands group =
  let open Lwt_result_syntax in
  Tezos_clic.
    [
      Tezos_clic.command
        ~group
        ~desc:"Query the path of the authorized key"
        no_options
        (prefixes ["get"; "ledger"; "authorized"; "path"; "for"]
        @@ Ledger_uri.ledger_uri_or_alias_param @@ stop)
        (fun () ledger_uri (cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            ~filter:Filter.is_baking
            (fun hidapi (version, _git_commit) ~device_path:_ _ledger_id ->
              let* authorized =
                Ledger_commands.get_authorized_path hidapi version
              in
              match authorized with
              | `Legacy_path p ->
                  let*! () =
                    cctxt#message
                      "@[<v 0>Authorized baking path (Legacy < 2.x.y): %a@]"
                      Bip32_path.pp_path
                      p
                  in
                  return_some ()
              | `No_baking_authorized ->
                  let*! () = cctxt#message "No baking key authorized at all." in
                  return_some ()
              | `Path_curve (ledger_path, ledger_curve) -> (
                  let*! () =
                    cctxt#message
                      "@[<v 0>Authorized baking path: %a@]"
                      Bip32_path.pp_path
                      ledger_path
                  in
                  let*! () =
                    cctxt#message
                      "@[<v 0>Authorized baking curve: %a@]"
                      Ledgerwallet_tezos.pp_curve
                      ledger_curve
                  in
                  match ledger_uri with
                  | `Ledger _ -> return_some ()
                  | `Ledger_account {curve; path; _}
                    when curve = ledger_curve
                         && Bip32_path.tezos_root @ path = ledger_path ->
                      let*! () =
                        cctxt#message
                          "@[<v 0>Authorized baking URI: %a@]"
                          Ledger_uri.pp
                          ledger_uri
                      in
                      return_some ()
                  | `Ledger_account {curve; path; _} ->
                      failwith
                        "Path and curve do not match the ones specified in the \
                         command line: %a & %a"
                        Ledgerwallet_tezos.pp_curve
                        curve
                        Bip32_path.pp_path
                        (Bip32_path.tezos_root @ path))));
      Tezos_clic.command
        ~group
        ~desc:
          "Authorize a Ledger to bake for a key (deprecated, use `setup ledger \
           ...` with recent versions of the Baking app)"
        no_options
        (prefixes ["authorize"; "ledger"; "to"; "bake"; "for"]
        @@ Ledger_uri.ledger_uri_or_alias_param @@ stop)
        (fun () ledger_uri (cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            ~filter:Filter.is_baking
            (fun hidapi (version, _git_commit) ~device_path:_ _ledger_id ->
              let* () =
                match version with
                | {Ledgerwallet_tezos.Version.app_class = Tezos; _} ->
                    failwith
                      "This command (`authorize ledger ...`) only works with \
                       the Tezos Baking app"
                | {Ledgerwallet_tezos.Version.app_class = TezBake; major; _}
                  when major >= 2 ->
                    failwith
                      "This command (`authorize ledger ...`) is@ not \
                       compatible with@ this version of the Ledger@ Baking app \
                       (%a >= 2.0.0),@ please use the command@ `setup ledger \
                       to bake for ...`@ from now on."
                      Ledgerwallet_tezos.Version.pp
                      version
                | _ ->
                    let*! () =
                      cctxt#message
                        "This Ledger Baking app is outdated (%a)@ running@ in \
                         backwards@ compatibility mode."
                        Ledgerwallet_tezos.Version.pp
                        version
                    in
                    return_unit
              in
              let* {Ledger_account.curve; path; _} =
                Ledger_uri.full_account ledger_uri
              in
              let* pk =
                Ledger_commands.public_key_returning_instruction
                  `Authorize_baking
                  hidapi
                  curve
                  path
              in
              let pkh = Signature.Public_key.hash pk in
              let*! () =
                cctxt#message
                  "@[<v 0>Authorized baking for address: %a@,\
                   Corresponding full public key: %a@]"
                  Signature.Public_key_hash.pp
                  pkh
                  Signature.Public_key.pp
                  pk
              in
              return_some ()));
      Tezos_clic.command
        ~group
        ~desc:"Setup a Ledger to bake for a key"
        (let hwm_arg kind =
           let doc =
             Printf.sprintf
               "Use <HWM> as %s chain high watermark instead of asking the \
                ledger."
               kind
           in
           let long = kind ^ "-hwm" in
           default_arg
             ~doc
             ~long
             ~placeholder:"HWM"
             ~default:"ASK-LEDGER"
             (parameter (fun _ -> function
                | "ASK-LEDGER" -> return_none
                | s -> (
                    try return_some (Int32.of_string s)
                    with _ ->
                      failwith "Parameter %S should be a 32-bits integer" s)))
         in
         args3
           (default_arg
              ~doc:"Use <ID> as main chain-id instead of asking the node."
              ~long:"main-chain-id"
              ~placeholder:"ID"
              ~default:"ASK-NODE"
              (parameter (fun _ -> function
                 | "ASK-NODE" -> return `Ask_node
                 | s -> (
                     try return (`Int32 (Int32.of_string s))
                     with _ -> (
                       try return (`Chain_id (Chain_id.of_b58check_exn s))
                       with _ ->
                         failwith
                           "Parameter %S should be a 32-bits integer or a \
                            Tezos_crypto.Base58 chain-id"
                           s)))))
           (hwm_arg "main")
           (hwm_arg "test"))
        (prefixes ["setup"; "ledger"; "to"; "bake"; "for"]
        @@ Ledger_uri.ledger_uri_or_alias_param @@ stop)
        (fun (chain_id_opt, main_hwm_opt, test_hwm_opt)
             ledger_uri
             (cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            ~filter:Filter.is_baking
            (fun hidapi (version, _git_commit) ~device_path:_ _ledger_id ->
              let* () =
                let open Ledgerwallet_tezos.Version in
                match version with
                | {app_class = Tezos; _} ->
                    failwith
                      "This command (`setup ledger ...`) only works with the \
                       Tezos Baking app"
                | {app_class = TezBake; major; _} when major < 2 ->
                    failwith
                      "This command (`setup ledger ...`)@ is not@ compatible@ \
                       with this version@ of the Ledger Baking app@ (%a < \
                       2.0.0),@ please upgrade@ your ledger@ or use the \
                       command@ `authorize ledger to bake for ...`"
                      pp
                      version
                | _ -> return_unit
              in
              let* {Ledger_account.curve; path; _} =
                Ledger_uri.full_account ledger_uri
              in
              let chain_id_of_int32 i32 =
                let open Int32 in
                let byte n =
                  logand 0xFFl (shift_right i32 (n * 8))
                  |> Int32.to_int |> char_of_int
                in
                Chain_id.of_string_exn
                  (Stringext.of_array (Array.init 4 (fun i -> byte (3 - i))))
              in
              let* main_chain_id =
                match chain_id_opt with
                | `Ask_node -> Chain_services.chain_id cctxt ()
                | `Int32 s -> return (chain_id_of_int32 s)
                | `Chain_id chid -> return chid
              in
              let* ( `Main_hwm (current_mh, current_mhr_opt),
                     `Test_hwm (current_th, current_thr_opt),
                     `Chain_id current_ci ) =
                Ledger_commands.wrap_ledger_cmd (fun pp ->
                    Ledgerwallet_tezos.get_all_high_watermarks ~pp hidapi)
              in
              let main_hwm = Option.value main_hwm_opt ~default:current_mh in
              let test_hwm = Option.value test_hwm_opt ~default:current_th in
              let*! () =
                cctxt#message
                  "Setting up the ledger:@.* Main chain ID: %a -> %a@.* Main \
                   chain High Watermark: %ld%a -> %ld%a@.* Test chain High \
                   Watermark: %ld%a -> %ld%a"
                  pp_ledger_chain_id
                  current_ci
                  Chain_id.pp
                  main_chain_id
                  current_mh
                  pp_round_opt
                  current_mhr_opt
                  main_hwm
                  pp_round_opt
                  (Option.map (fun _ -> 0l) current_mhr_opt)
                  current_th
                  pp_round_opt
                  current_thr_opt
                  test_hwm
                  pp_round_opt
                  (Option.map (fun _ -> 0l) current_thr_opt)
              in
              let* pk =
                Ledger_commands.public_key_returning_instruction
                  (`Setup
                    (Chain_id.to_string main_chain_id, main_hwm, test_hwm))
                  hidapi
                  curve
                  path
              in
              let pkh = Signature.Public_key.hash pk in
              let*! () =
                cctxt#message
                  "@[<v 0>Authorized baking for address: %a@,\
                   Corresponding full public key: %a@]"
                  Signature.Public_key_hash.pp
                  pkh
                  Signature.Public_key.pp
                  pk
              in
              return_some ()));
      Tezos_clic.command
        ~group
        ~desc:"Deauthorize Ledger from baking"
        no_options
        (prefixes ["deauthorize"; "ledger"; "baking"; "for"]
        @@ Ledger_uri.ledger_uri_or_alias_param @@ stop)
        (fun () ledger_uri (_cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            ~filter:Filter.is_baking
            (fun hidapi (_version, _git_commit) ~device_path:_ _ledger_id ->
              let* () =
                Ledger_commands.wrap_ledger_cmd (fun pp ->
                    Ledgerwallet_tezos.deauthorize_baking ~pp hidapi)
              in
              return_some ()));
    ]

(** Commands for high water mark of the Baking app. The
    [watermark_spelling] argument is used to make 2 sets of commands: with
    the old/wrong spelling “watermark” for backwards compatibility and
    with the correct one “high water mark” (it's a mark of the highest
    water level). *)
let high_water_mark_commands group watermark_spelling =
  let open Lwt_result_syntax in
  let make_desc desc =
    if Compare.List_length_with.(watermark_spelling = 1) then
      desc ^ " (legacy/deprecated spelling)"
    else desc
  in
  Tezos_clic.
    [
      Tezos_clic.command
        ~group
        ~desc:(make_desc "Get high water mark of a Ledger")
        (args1
           (switch
              ~doc:
                "Prevent the fallback to the (deprecated) Ledger instructions \
                 (for 1.x.y versions of the Baking app)"
              ~long:"no-legacy-instructions"
              ()))
        (prefixes (["get"; "ledger"; "high"] @ watermark_spelling @ ["for"])
        @@ Ledger_uri.ledger_uri_or_alias_param @@ stop)
        (fun no_legacy_apdu ledger_uri (cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            ~filter:Filter.is_baking
            (fun hidapi (version, _git_commit) ~device_path:_ _ledger_id ->
              match version.app_class with
              | Tezos ->
                  failwith
                    "Fatal: this operation is only valid with the Tezos Baking \
                     application"
              | TezBake when (not no_legacy_apdu) && version.major < 2 ->
                  let* hwm, hwm_round_opt =
                    Ledger_commands.wrap_ledger_cmd (fun pp ->
                        Ledgerwallet_tezos.get_high_watermark ~pp hidapi)
                  in
                  let*! () =
                    cctxt#message
                      "The high water mark for@ %a@ is %ld%a."
                      Ledger_uri.pp
                      ledger_uri
                      hwm
                      pp_round_opt
                      hwm_round_opt
                  in
                  return_some ()
              | TezBake when no_legacy_apdu && version.major < 2 ->
                  failwith
                    "Cannot get the high water mark with@ \
                     `--no-legacy-instructions` and version %a"
                    Ledgerwallet_tezos.Version.pp
                    version
              | TezBake ->
                  let* `Main_hwm (mh, mr), `Test_hwm (th, tr), `Chain_id ci =
                    Ledger_commands.wrap_ledger_cmd (fun pp ->
                        Ledgerwallet_tezos.get_all_high_watermarks ~pp hidapi)
                  in
                  let*! () =
                    cctxt#message
                      "The high water mark values for@ %a@ are@ %ld%a for the \
                       main-chain@ (%a)@ and@ %ld%a for the test-chain."
                      Ledger_uri.pp
                      ledger_uri
                      mh
                      pp_round_opt
                      mr
                      pp_ledger_chain_id
                      ci
                      th
                      pp_round_opt
                      tr
                  in
                  return_some ()));
      Tezos_clic.command
        ~group
        ~desc:(make_desc "Set high water mark of a Ledger")
        no_options
        (prefixes (["set"; "ledger"; "high"] @ watermark_spelling @ ["for"])
        @@ Ledger_uri.ledger_uri_or_alias_param @@ prefix "to"
        @@ param
             ~name:"high watermark"
             ~desc:"High watermark"
             (parameter (fun _ctx s ->
                  try return (Int32.of_string s)
                  with _ -> failwith "%s is not an int32 value" s))
        @@ stop)
        (fun () ledger_uri hwm (cctxt : Client_context.full) ->
          use_ledger_or_fail
            ~ledger_uri
            ~filter:Filter.is_baking
            (fun hidapi (version, _git_commit) ~device_path:_ _ledger_id ->
              match version.app_class with
              | Tezos ->
                  failwith "Fatal: this operation is only valid with TezBake"
              | TezBake ->
                  let* () =
                    Ledger_commands.wrap_ledger_cmd (fun pp ->
                        Ledgerwallet_tezos.set_high_watermark ~pp hidapi hwm)
                  in
                  let* new_hwm, new_hwm_round_opt =
                    Ledger_commands.wrap_ledger_cmd (fun pp ->
                        Ledgerwallet_tezos.get_high_watermark ~pp hidapi)
                  in
                  let*! () =
                    cctxt#message
                      "@[<v 0>%a has now high water mark: %ld%a@]"
                      Ledger_uri.pp
                      ledger_uri
                      new_hwm
                      pp_round_opt
                      new_hwm_round_opt
                  in
                  return_some ()));
    ]

let commands =
  let group =
    {
      Tezos_clic.name = "ledger";
      title = "Commands for managing the connected Ledger Nano devices";
    }
  in
  fun () ->
    generic_commands group @ baking_commands group
    @ high_water_mark_commands group ["water"; "mark"]
    @ high_water_mark_commands group ["watermark"]
