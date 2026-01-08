(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {input : Bytes.t Eio.Stream.t; output : Bytes.t Eio.Stream.t}

let input_channel t = t.input

let output_channel t = t.output

let read_message ic = Eio.Stream.take ic

let write_message oc (msg : Bytes.t) = Eio.Stream.add oc msg

(* This value allows to limit the number of pending requests. If the limit is
   reached, sending messages to the process will be blocking. *)
let process_stream_limit = 1024

let run f args =
  let in_child = Eio.Stream.create process_stream_limit in
  let out_child = Eio.Stream.create process_stream_limit in
  let (_promise : 'a Eio.Promise.t) =
    let mgr = Eio.Stdenv.domain_mgr (Tezos_base_unix.Event_loop.env_exn ()) in
    let sw = Tezos_base_unix.Event_loop.main_switch_exn () in
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Domain_manager.run mgr (fun () -> f in_child out_child args))
  in
  {input = in_child; output = out_child}
