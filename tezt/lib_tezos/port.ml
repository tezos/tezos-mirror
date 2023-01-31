(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* The trick for generating a fresh port relies on a non-POSIX but
   standard behavior implemented by many operating systems, including
   Linux and macOS.

   [Unix.bind] binds the address (IP with port) given in parameters to
   the socket. If the port of the address given is [0], a fresh port
   is used, then the operating system actually replace the port with a
   fresh port which is known not used by the operating
   system. Consequently, to generate a fresh port, we open a socket,
   bind it to a fake address with port [0]. Ask for the address of the
   socket and return the port given by the operating system. Finally,
   we close the socket.
*)
let fresh () =
  let dummy_socket = Unix.(socket PF_INET SOCK_STREAM 0) in
  Fun.protect ~finally:(fun () -> Unix.close dummy_socket) @@ fun () ->
  Unix.bind dummy_socket Unix.(ADDR_INET (inet_addr_loopback, 0)) ;
  let addr = Unix.getsockname dummy_socket in
  match addr with ADDR_INET (_, port) -> port | _ -> assert false
