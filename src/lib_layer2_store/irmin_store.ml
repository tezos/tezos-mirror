(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Store_sigs

module Make (N : sig
  val name : string
end) =
struct
  module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)
  include Maker.Make (Tezos_context_encoding.Context.Schema)

  let make_key_path path key = path @ [key]

  type nonrec +'a t = t

  let name = N.name

  let load : type a. a mode -> string -> a t tzresult Lwt.t =
   fun mode data_dir ->
    let open Lwt_result_syntax in
    trace (Store_errors.Cannot_load_store {name = N.name; path = data_dir})
    @@ protect
    @@ fun () ->
    let readonly = match mode with Read_only -> true | Read_write -> false in
    let*! repo = Repo.v (Irmin_pack.config ~readonly data_dir) in
    let*! main = main repo in
    return main

  let flush store =
    try Ok (flush (repo store))
    with exn ->
      Error
        TzTrace.(
          cons (Store_errors.Cannot_write_to_store N.name) @@ make (Exn exn))

  let close store =
    let open Lwt_result_syntax in
    trace (Store_errors.Cannot_close_store N.name)
    @@ protect
    @@ fun () ->
    let*! () = Repo.close (repo store) in
    return_unit

  let info message =
    let date =
      Tezos_base.Time.(
        System.now () |> System.to_protocol |> Protocol.to_seconds)
    in
    Irmin.Info.Default.v ~author:N.name ~message date

  let path_to_string path = String.concat "/" path

  let set store path bytes =
    let open Lwt_result_syntax in
    trace (Store_errors.Cannot_write_to_store N.name)
    @@ protect
    @@ fun () ->
    let full_path = path_to_string path in
    let info () = info full_path in
    let*! () = set_exn ~info store path bytes in
    return_unit

  let get store path =
    let open Lwt_result_syntax in
    trace (Store_errors.Cannot_read_from_store N.name)
    @@ protect
    @@ fun () ->
    let*! bytes = get store path in
    return bytes

  let readonly = Fun.id

  let mem store path =
    let open Lwt_result_syntax in
    trace (Store_errors.Cannot_read_from_store N.name)
    @@ protect
    @@ fun () ->
    let*! is_present = mem store path in
    return is_present

  let find store path =
    let open Lwt_result_syntax in
    trace (Store_errors.Cannot_read_from_store N.name)
    @@ protect
    @@ fun () ->
    let*! bytes_opt = find store path in
    return bytes_opt
end
