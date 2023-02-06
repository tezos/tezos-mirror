(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Protocol
open Environment.Error_monad

type error +=
  | Cannot_write_page_to_page_storage of {
      hash : Sc_rollup_reveal_hash.t;
      content : bytes;
    }
  | Cannot_read_page_from_page_storage of Sc_rollup_reveal_hash.t

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
        (Sc_rollup_reveal_hash.to_hex key)
        (Bytes.to_string content))
    Data_encoding.(
      obj2 (req "key" Sc_rollup_reveal_hash.encoding) (req "content" bytes))
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
        (Sc_rollup_reveal_hash.to_hex key))
    Data_encoding.(obj1 (req "hash" Sc_rollup_reveal_hash.encoding))
    (function
      | Cannot_read_page_from_page_storage hash -> Some hash | _ -> None)
    (fun hash -> Cannot_read_page_from_page_storage hash)

module type S = sig
  type t

  type hash

  type configuration

  val init : configuration -> t

  val save : t -> hash:hash -> content:bytes -> unit tzresult Lwt.t

  val load : t -> hash:hash -> bytes tzresult Lwt.t
end

(** Implementation of dac pages storage using filesystem. *)
module Filesystem :
  S with type configuration = string and type hash = Sc_rollup_reveal_hash.t =
struct
  (** [t] represents directory path *)
  type t = string

  type configuration = string

  type hash = Protocol.Sc_rollup_reveal_hash.t

  let init data_dir = data_dir

  let path data_dir key =
    Filename.(concat data_dir @@ Protocol.Sc_rollup_reveal_hash.to_hex key)

  let save data_dir ~hash ~content =
    let open Lwt_result_syntax in
    let path = path data_dir hash in
    let*! result =
      Lwt_utils_unix.with_atomic_open_out path @@ fun chan ->
      Lwt_utils_unix.write_bytes chan content
    in
    match result with
    | Ok () -> return ()
    | Error _ -> tzfail @@ Cannot_write_page_to_page_storage {hash; content}

  let load data_dir ~hash =
    let open Lwt_result_syntax in
    let path = path data_dir hash in
    Lwt.catch
      (fun () ->
        let*! result = Lwt_utils_unix.read_file path in
        return @@ String.to_bytes result)
      (fun _exn -> tzfail @@ Cannot_read_page_from_page_storage hash)
end
