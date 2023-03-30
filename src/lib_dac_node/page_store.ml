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
  | Cannot_write_page_to_page_storage of {hash : string; content : bytes}
  | Cannot_read_page_from_page_storage of string
  | Incorrect_page_hash of {
      expected : Dac_plugin.hash;
      actual : Dac_plugin.hash;
    }

let () =
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
        (Dac_plugin.hash_to_bytes expected)
        Format.pp_print_bytes
        (Dac_plugin.hash_to_bytes actual))
    Data_encoding.(
      obj2
        (req "expected" Dac_plugin.non_proto_encoding_unsafe)
        (req "acual" Dac_plugin.non_proto_encoding_unsafe))
    (function
      | Incorrect_page_hash {expected; actual} -> Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Incorrect_page_hash {expected; actual})

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

  let save ((module P) : Dac_plugin.t) data_dir ~hash ~content =
    let open Lwt_result_syntax in
    let hash_string = P.to_hex hash in
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

  let mem ((module P) : Dac_plugin.t) data_dir hash =
    let open Lwt_result_syntax in
    let hash_string = P.to_hex hash in
    let path = path data_dir hash_string in
    let*! result =
      Lwt_utils_unix.with_open_in path (fun _fd -> Lwt.return ())
    in
    match result with
    | Ok () -> return true
    | Error {unix_code = Unix.ENOENT; _} -> return false
    | Error _ -> tzfail @@ Cannot_read_page_from_page_storage hash_string

  let load ((module P) : Dac_plugin.t) data_dir hash =
    let open Lwt_result_syntax in
    let hash_string = P.to_hex hash in
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
      tzfail @@ Incorrect_page_hash {expected = hash; actual = content_hash}
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
    let (module Plugin : Dac_plugin.T) = plugin in
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

        let fetch dac_plugin remote_context hash =
          Dac_node_client.get_preimage dac_plugin remote_context ~page_hash:hash
      end)
      (F)

  include Internal

  type t = Internal.t

  type configuration = remote_configuration

  let init {cctxt; page_store} = Internal.init (cctxt, page_store)
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
