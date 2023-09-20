(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

type error +=
  | Reveal_data_path_not_a_directory of string
  | Cannot_create_reveal_data_dir of string
  | Cannot_write_page_to_page_storage of {hash : string; content : bytes}
  | Cannot_read_page_from_page_storage of string
  | Incorrect_page_hash of {
      expected : Dac_plugin.raw_hash;
      actual : Dac_plugin.raw_hash;
    }
  | Cannot_fetch_remote_page_with_flooding_strategy of {
      hash : Dac_plugin.raw_hash;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.dac.reveal_data_path_not_a_dir"
    ~title:"Reveal data path is not a directory"
    ~description:"Reveal data path is not a directory"
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Reveal data path %s is not a directory"
        reveal_data_path)
    Data_encoding.(obj1 (req "path" string))
    (function Reveal_data_path_not_a_directory path -> Some path | _ -> None)
    (fun path -> Reveal_data_path_not_a_directory path) ;
  register_error_kind
    `Permanent
    ~id:"dac.node.dac.cannot_create_directory"
    ~title:"Cannot create directory to store reveal data"
    ~description:"Cannot create directory to store reveal data"
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Cannot create a directory \"%s\" to store reveal data"
        reveal_data_path)
    Data_encoding.(obj1 (req "path" string))
    (function Cannot_create_reveal_data_dir path -> Some path | _ -> None)
    (fun path -> Cannot_create_reveal_data_dir path) ;
  register_error_kind
    `Permanent
    ~id:"cannot_write_page_to_page_storage"
    ~title:"Cannot write DAC page to page store"
    ~description:"Persisting DAC page with given key and content failed"
    ~pp:(fun ppf (key, content) ->
      Format.fprintf
        ppf
        "Unable to write the following DAC page to the page storage: {key=%s; \
         content=%s}"
        key
        (Bytes.to_string content))
    Data_encoding.(obj2 (req "key" Data_encoding.string) (req "content" bytes))
    (function
      | Cannot_write_page_to_page_storage {hash; content} -> Some (hash, content)
      | _ -> None)
    (fun (hash, content) -> Cannot_write_page_to_page_storage {hash; content}) ;
  register_error_kind
    `Permanent
    ~id:"cannot_read_dac_page_from_page_storage"
    ~title:"Cannot read DAC page from the page storage"
    ~description:"Reading DAC page with given key from the page storage failed"
    ~pp:(fun ppf key ->
      Format.fprintf
        ppf
        "Unable to read DAC page with {key=%s} from the page storage"
        key)
    Data_encoding.(obj1 (req "hash" Data_encoding.string))
    (function
      | Cannot_read_page_from_page_storage hash -> Some hash | _ -> None)
    (fun hash -> Cannot_read_page_from_page_storage hash) ;
  register_error_kind
    `Permanent
    ~id:"hash_of_page_is_invalid"
    ~title:"Hash of page contents is different from the expected hash"
    ~description:"Hash of page content is different from the expected hash"
    ~pp:(fun ppf (expected, actual) ->
      Format.fprintf
        ppf
        "Hash of page content is %a, while %a was expected."
        Format.pp_print_bytes
        (Dac_plugin.raw_hash_to_bytes expected)
        Format.pp_print_bytes
        (Dac_plugin.raw_hash_to_bytes actual))
    Data_encoding.(
      obj2
        (req "expected" Dac_plugin.raw_hash_encoding)
        (req "acual" Dac_plugin.raw_hash_encoding))
    (function
      | Incorrect_page_hash {expected; actual} -> Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Incorrect_page_hash {expected; actual}) ;

  register_error_kind
    `Permanent
    ~id:"remote_with_flooding_load_failed"
    ~title:"Cannot load page content from remote store using flooding strategy"
    ~description:
      "Cannot load page content from remote store using flooding strategy."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Cannot load page content from remote store using flooding strategy \
         for hash %a."
        Dac_plugin.pp_raw_hash
        hash)
    Data_encoding.(obj1 (req "hash" Dac_plugin.raw_hash_encoding))
    (function
      | Cannot_fetch_remote_page_with_flooding_strategy {hash} -> Some hash
      | _ -> None)
    (fun hash -> Cannot_fetch_remote_page_with_flooding_strategy {hash})

let ensure_reveal_data_dir_exists reveal_data_dir =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! () = Lwt_utils_unix.create_dir ~perm:0o744 reveal_data_dir in
      return ())
    (function
      | Failure s ->
          if String.equal s "Not a directory" then
            tzfail @@ Reveal_data_path_not_a_directory reveal_data_dir
          else tzfail @@ Cannot_create_reveal_data_dir reveal_data_dir
      | _ -> tzfail @@ Cannot_create_reveal_data_dir reveal_data_dir)

module type S = sig
  type t

  type configuration

  val init : configuration -> t

  val save :
    Dac_plugin.t ->
    t ->
    hash:Dac_plugin.hash ->
    content:bytes ->
    unit tzresult Lwt.t

  val mem : Dac_plugin.t -> t -> Dac_plugin.hash -> bool tzresult Lwt.t

  val load : Dac_plugin.t -> t -> Dac_plugin.hash -> bytes tzresult Lwt.t
end

(** Implementation of dac pages storage using filesystem. *)
module Filesystem : S with type configuration = string = struct
  (** [t] represents directory path *)
  type t = string

  type configuration = string

  let init data_dir = data_dir

  let path data_dir hash_string_key =
    Filename.(concat data_dir @@ hash_string_key)

  let save ((module Plugin) : Dac_plugin.t) data_dir ~hash ~content =
    let open Lwt_result_syntax in
    let hash_string = Plugin.to_hex hash in
    let path = path data_dir hash_string in
    let*! result =
      Lwt_utils_unix.with_atomic_open_out path @@ fun chan ->
      Lwt_utils_unix.write_bytes chan content
    in
    match result with
    | Ok () -> return ()
    | Error _ ->
        tzfail
        @@ Cannot_write_page_to_page_storage {hash = hash_string; content}

  let mem ((module Plugin) : Dac_plugin.t) data_dir hash =
    let open Lwt_result_syntax in
    let hash_string = Plugin.to_hex hash in
    let path = path data_dir hash_string in
    let*! result =
      Lwt_utils_unix.with_open_in path (fun _fd -> Lwt.return ())
    in
    match result with
    | Ok () -> return true
    | Error {unix_code = Unix.ENOENT; _} -> return false
    | Error _ -> tzfail @@ Cannot_read_page_from_page_storage hash_string

  let load ((module Plugin) : Dac_plugin.t) data_dir hash =
    let open Lwt_result_syntax in
    let hash_string = Plugin.to_hex hash in
    let path = path data_dir hash_string in
    Lwt.catch
      (fun () ->
        let*! result = Lwt_utils_unix.read_file path in
        return @@ String.to_bytes result)
      (fun _exn -> tzfail @@ Cannot_read_page_from_page_storage hash_string)
end

module With_data_integrity_check (P : S) :
  S with type configuration = P.configuration and type t = P.t = struct
  type t = P.t

  type configuration = P.configuration

  let init config = P.init config

  let save plugin page_store ~hash ~content =
    let open Lwt_result_syntax in
    let ((module Plugin) : Dac_plugin.t) = plugin in
    let scheme = Plugin.scheme_of_hash hash in
    let content_hash = Plugin.hash_bytes [content] ~scheme in
    if not @@ Plugin.equal hash content_hash then
      tzfail
      @@ Incorrect_page_hash
           {
             expected = Dac_plugin.hash_to_raw hash;
             actual = Dac_plugin.hash_to_raw content_hash;
           }
    else P.save plugin page_store ~hash ~content

  let mem = P.mem

  let load = P.load
end

module Filesystem_with_integrity_check :
  S with type configuration = string and type t = Filesystem.t =
  With_data_integrity_check (Filesystem)

module With_remote_fetch (R : sig
  type remote_context

  val fetch :
    Dac_plugin.t -> remote_context -> Dac_plugin.hash -> bytes tzresult Lwt.t
end)
(P : S) :
  S
    with type configuration = R.remote_context * P.t
     and type t = R.remote_context * P.t = struct
  type t = R.remote_context * P.t

  type configuration = R.remote_context * P.t

  let init (remote_ctxt, page_store) = (remote_ctxt, page_store)

  let save plugin (_remote_ctxt, page_store) ~hash ~content =
    P.save plugin page_store ~hash ~content

  let mem plugin (_remote_ctxt, page_store) hash = P.mem plugin page_store hash

  let load plugin (remote_ctxt, page_store) hash =
    let open Lwt_result_syntax in
    let* page_exists_in_store = mem plugin (remote_ctxt, page_store) hash in
    if page_exists_in_store then P.load plugin page_store hash
    else
      let* content = R.fetch plugin remote_ctxt hash in
      let+ () = P.save plugin page_store ~hash ~content in
      content
end

type remote_configuration = {
  cctxt : Dac_node_client.cctxt;
  page_store : Filesystem.t;
}

module Remote : S with type configuration = remote_configuration = struct
  module F = Filesystem_with_integrity_check

  module Internal :
    S
      with type configuration = Dac_node_client.cctxt * Filesystem.t
       and type t = Dac_node_client.cctxt * Filesystem.t =
    With_remote_fetch
      (struct
        type remote_context = Dac_node_client.cctxt

        let fetch _dac_plugin remote_context hash =
          Dac_node_client.V0.get_preimage
            remote_context
            ~page_hash:(Dac_plugin.hash_to_raw hash)
      end)
      (F)

  include Internal

  type t = Internal.t

  type configuration = remote_configuration

  let init {cctxt; page_store} = Internal.init (cctxt, page_store)
end

type remote_with_flooding_configuration = {
  timeout : float;
  cctxts : Dac_node_client.cctxt list;
  page_store : Filesystem.t;
}

module Remote_with_flooding :
  S with type configuration = remote_with_flooding_configuration = struct
  module F = Filesystem_with_integrity_check

  module Internal :
    S
      with type configuration =
        (float * Dac_node_client.cctxt list) * Filesystem.t
       and type t = (float * Dac_node_client.cctxt list) * Filesystem.t =
    With_remote_fetch
      (struct
        type remote_context = float * Dac_node_client.cctxt list

        (** TODO: https://gitlab.com/tezos/tezos/-/issues/5673
            Optimize flooding.
        *)
        let fetch _dac_plugin (timeout, remote_cctxts) hash =
          let open Lwt_result_syntax in
          let page_hash = Dac_plugin.hash_to_raw hash in
          let fetch_page cctxt =
            Lwt_unix.with_timeout timeout (fun () ->
                Dac_node_client.V0.get_preimage cctxt ~page_hash)
          in
          let*! results =
            List.filter_map_p
              (fun committee_member_cctxt ->
                Lwt.catch
                  (fun () ->
                    let*! page_data = fetch_page committee_member_cctxt in
                    match page_data with
                    | Ok page_data -> Lwt.return (Some page_data)
                    | Error _ -> Lwt.return_none)
                  (fun _ -> Lwt.return_none))
              remote_cctxts
          in
          match List.hd results with
          | Some a -> return a
          | None ->
              tzfail
                (Cannot_fetch_remote_page_with_flooding_strategy
                   {hash = Dac_plugin.hash_to_raw hash})
      end)
      (F)

  include Internal

  type t = Internal.t

  type configuration = remote_with_flooding_configuration

  let init {timeout; cctxts; page_store} =
    Internal.init ((timeout, cctxts), page_store)
end

module Internal_for_tests = struct
  module With_data_integrity_check (P : S) :
    S with type configuration = P.configuration and type t = P.t =
    With_data_integrity_check (P)

  module With_remote_fetch (R : sig
    type remote_context

    val fetch :
      Dac_plugin.t -> remote_context -> Dac_plugin.hash -> bytes tzresult Lwt.t
  end)
  (P : S) :
    S
      with type configuration = R.remote_context * P.t
       and type t = R.remote_context * P.t =
    With_remote_fetch (R) (P)
end
