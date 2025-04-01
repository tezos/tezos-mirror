(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type close_status = Normal_closure | Going_away | Policy | Message_too_big

(* https://datatracker.ietf.org/doc/html/rfc6455#section-7.4.1 *)
let code_of_close_status = function
  | Normal_closure -> 1000
  | Going_away -> 1001
  | Policy -> 1008
  | Message_too_big -> 1009

let opcode_encoding : Websocket.Frame.Opcode.t Data_encoding.t =
  let open Data_encoding in
  let open Websocket.Frame.Opcode in
  union
    [
      case
        (Tag 0)
        ~title:"continuation"
        (constant "continuation")
        (function Continuation -> Some () | _ -> None)
        (fun () -> Continuation);
      case
        (Tag 1)
        ~title:"text"
        (constant "text")
        (function Text -> Some () | _ -> None)
        (fun () -> Text);
      case
        (Tag 2)
        ~title:"binary"
        (constant "binary")
        (function Binary -> Some () | _ -> None)
        (fun () -> Binary);
      case
        (Tag 8)
        ~title:"close"
        (constant "close")
        (function Close -> Some () | _ -> None)
        (fun () -> Close);
      case
        (Tag 9)
        ~title:"ping"
        (constant "ping")
        (function Ping -> Some () | _ -> None)
        (fun () -> Ping);
      case
        (Tag 10)
        ~title:"pong"
        (constant "pong")
        (function Pong -> Some () | _ -> None)
        (fun () -> Pong);
      case
        (Tag 15)
        ~title:"ctrl"
        (obj1 (req "ctrl" int31))
        (function Ctrl i -> Some i | _ -> None)
        (fun i -> Ctrl i);
      case
        (Tag 255)
        ~title:"nonctrl"
        (obj1 (req "nonctrl" int31))
        (function Nonctrl i -> Some i | _ -> None)
        (fun i -> Nonctrl i);
    ]

let frame_encoding : Websocket.Frame.t Data_encoding.t =
  let open Data_encoding in
  let open Websocket.Frame in
  conv
    (fun {opcode; extension; final; content} ->
      (opcode, extension, final, content))
    (fun (opcode, extension, final, content) ->
      {opcode; extension; final; content})
  @@ obj4
       (req "opcode" opcode_encoding)
       (req "extension" int31)
       (req "final" bool)
       (req "content" string)
