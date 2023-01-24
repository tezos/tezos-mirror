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

type error +=
  | Cannot_write_dac_page_to_page_storage of {key : string; contents : bytes}
  | Cannot_read_dac_page_from_page_storage of string

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_write_dac_page_to_page_storage"
    ~title:"Cannot write DAC page to page storage"
    ~description:"Persisting DAC page with given key and contents failed"
    ~pp:(fun ppf (key, contents) ->
      Format.fprintf
        ppf
        "Unable to write the following DAC page to the page storage: {key=%s; \
         contents=%s}"
        key
        (Bytes.to_string contents))
    Data_encoding.(obj2 (req "key" string) (req "contents" bytes))
    (function
      | Cannot_write_dac_page_to_page_storage {key; contents} ->
          Some (key, contents)
      | _ -> None)
    (fun (key, contents) ->
      Cannot_write_dac_page_to_page_storage {key; contents}) ;
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
    Data_encoding.(obj1 (req "key" string))
    (function
      | Cannot_read_dac_page_from_page_storage key -> Some key | _ -> None)
    (fun key -> Cannot_read_dac_page_from_page_storage key)

module type S = sig
  type t

  type key

  val write_page : key:key -> contents:bytes -> t -> unit tzresult Lwt.t

  val read_page : key:key -> t -> bytes tzresult Lwt.t
end

(** Implementation of dac pages storage using filesystem. *)
module Default = struct
  (** [t] represents directory path *)
  type t = string

  type key = string

  let path data_dir key = Filename.(concat data_dir key)

  let write_page ~key ~contents data_dir =
    let open Lwt_result_syntax in
    let path = path data_dir key in
    lwt_map_error
      (fun _err ->
        TzTrace.make @@ Cannot_write_dac_page_to_page_storage {key; contents})
      ( Lwt_utils_unix.with_atomic_open_out path @@ fun chan ->
        Lwt_utils_unix.write_bytes chan contents )

  let read_page ~key data_dir =
    let open Lwt_result_syntax in
    let path = path data_dir key in
    Lwt.catch
      (fun () ->
        let*! result = Lwt_utils_unix.read_file path in
        return @@ String.to_bytes result)
      (fun _exn -> tzfail @@ Cannot_read_dac_page_from_page_storage key)
end
