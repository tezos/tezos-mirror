(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs <contact@nomadic-labs.com>                     *)
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

(** This module provides a client contexts that stores information (such as the
    wallet, etc.) in memory and that cannot do RPCs but which can be used in
    unit tests.

    It is largely taken from the mockup simulator of the baker.
 *)

open Tezos_client_base

class dummy_prompter : Client_context.prompter =
  object
    method prompt : type a. (a, string tzresult) Client_context.lwt_format -> a
        =
      fun _msg -> assert false

    method prompt_password : type a.
        (a, Bytes.t tzresult) Client_context.lwt_format -> a =
      fun _msg -> assert false

    method multiple_password_retries = false
  end

class faked_ctxt : Tezos_rpc.Context.generic =
  let local_ctxt =
    Tezos_mockup_proxy.RPC_client.local_ctxt Tezos_rpc.Directory.empty
  in
  object
    method base = local_ctxt#base

    method generic_media_type_call meth ?body uri =
      local_ctxt#generic_media_type_call meth ?body uri

    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          'p ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      fun service params query body ->
        local_ctxt#call_service service params query body

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          on_chunk:('o -> unit) ->
          on_close:(unit -> unit) ->
          'p ->
          'q ->
          'i ->
          (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        local_ctxt#call_streamed_service
          service
          ~on_chunk
          ~on_close
          params
          query
          body
  end

class faked_wallet ~base_dir ~filesystem : Client_context.wallet =
  object (self)
    method load_passwords = None

    method read_file fname =
      match String.Hashtbl.find filesystem fname with
      | None -> failwith "faked_wallet: cannot read file (%s)" fname
      | Some (content, _mtime) -> return content

    method private filename alias_name =
      Filename.concat
        base_dir
        (String.map (function ' ' -> '_' | c -> c) alias_name ^ "s")

    val lock_mutex = Lwt_mutex.create ()

    method with_lock : type a. (unit -> a Lwt.t) -> a Lwt.t =
      fun f -> Lwt_mutex.with_lock lock_mutex f

    method get_base_dir = base_dir

    method load : type a.
        string -> default:a -> a Data_encoding.encoding -> a tzresult Lwt.t =
      let open Lwt_result_syntax in
      fun alias_name ~default encoding ->
        let filename = self#filename alias_name in
        if not (String.Hashtbl.mem filesystem filename) then return default
        else
          let* content = self#read_file filename in
          let json = (Ezjsonm.from_string content :> Data_encoding.json) in
          match Data_encoding.Json.destruct encoding json with
          | exception e ->
              failwith
                "did not understand the %s alias file %s : %s"
                alias_name
                filename
                (Printexc.to_string e)
          | data -> return data

    method write : type a.
        string -> a -> a Data_encoding.encoding -> unit tzresult Lwt.t =
      fun alias_name list encoding ->
        let filename = self#filename alias_name in
        let json = Data_encoding.Json.construct encoding list in
        let str = Ezjsonm.value_to_string (json :> Ezjsonm.value) in
        String.Hashtbl.replace
          filesystem
          filename
          (str, Some (Ptime.to_float_s (Ptime_clock.now ()))) ;
        return_unit

    method last_modification_time : string -> float option tzresult Lwt.t =
      let open Lwt_result_syntax in
      fun alias_name ->
        let filename = self#filename alias_name in
        let file = String.Hashtbl.find_opt filesystem filename in
        match file with
        | None -> return_none
        | Some (_content, mtime) -> return mtime
  end

class faked_io_wallet ~base_dir ~filesystem : Client_context.io_wallet =
  let log _channel msg = Logs_lwt.info (fun m -> m "%s" msg) in
  object
    inherit Client_context.simple_printer log

    inherit dummy_prompter

    inherit faked_wallet ~base_dir ~filesystem
  end

class unix_faked ~base_dir ~filesystem : Client_context.full =
  object
    inherit faked_io_wallet ~base_dir ~filesystem

    inherit faked_ctxt

    inherit Tezos_client_base_unix.Client_context_unix.unix_ui

    method chain = `Main

    method block = `Head 0

    method confirmations = None

    method verbose_rpc_error_diagnostics = false
  end
