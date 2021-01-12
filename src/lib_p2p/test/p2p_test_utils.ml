(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides functions used for tests. *)

type error += Timeout_exceed of string

let () =
  register_error_kind
    `Permanent
    ~id:"test_p2p.utils.timeout_exceed"
    ~title:"Timeout exceed"
    ~description:"The timeout used to wait connections has been exceed."
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "The timeout has been exceed : %s." msg)
    Data_encoding.(obj1 (req "msg" Data_encoding.string))
    (function Timeout_exceed msg -> Some msg | _ -> None)
    (fun msg -> Timeout_exceed msg)

let connect_all ?timeout connect_handler points =
  List.map_ep
    (fun point -> P2p_connect_handler.connect ?timeout connect_handler point)
    points

type 'a timeout_t = {time : float; msg : 'a -> string}

(* [wait_pred] wait until [pred] [arg] is true or [timeout] is exceed. If
     [timeout] is exceed an error is raised. There is a Lwt cooperation
     point. *)
let wait_pred ?timeout ~pred ~arg () =
  let rec inner_wait_pred () =
    Lwt.pause ()
    >>= fun () -> if not (pred arg) then inner_wait_pred () else return_unit
  in
  match timeout with
  | None ->
      inner_wait_pred ()
  | Some timeout ->
      Lwt.pick
        [ ( Lwt_unix.sleep timeout.time
          >>= fun () -> Error_monad.fail (Timeout_exceed (timeout.msg arg)) );
          inner_wait_pred () ]

let wait_pred_s ?timeout ~pred ~arg () =
  let rec inner_wait_pred () =
    Lwt.pause ()
    >>= fun () ->
    pred arg
    >>= fun cond -> if not cond then inner_wait_pred () else return_unit
  in
  match timeout with
  | None ->
      inner_wait_pred ()
  | Some timeout ->
      Lwt.pick
        [ ( Lwt_unix.sleep timeout.time
          >>= fun () -> Error_monad.fail (Timeout_exceed (timeout.msg arg)) );
          inner_wait_pred () ]

let wait_conns ?timeout ~pool n =
  let timeout =
    Option.map
      (fun timeout ->
        {
          time = timeout;
          msg =
            (fun (pool, n) ->
              Format.asprintf
                "in wait_conns with n = %d and conns = %d"
                n
                (P2p_pool.active_connections pool));
        })
      timeout
  in
  wait_pred
    ?timeout
    ~pred:(fun (pool, n) -> P2p_pool.active_connections pool = n)
    ~arg:(pool, n)
    ()

let close_active_conns pool =
  P2p_pool.Connection.fold
    ~init:Lwt.return_unit
    ~f:(fun _ conn _ -> P2p_conn.disconnect ~wait:true conn)
    pool
