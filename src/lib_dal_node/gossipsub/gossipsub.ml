(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

include Gs_interface

module Worker = struct
  module Config = Gs_interface.Worker_config
  module Default_parameters = Gs_default_parameters
  module Logging = Gs_logging
  include Gs_interface.Worker_instance
  module Validate_message_hook = Validate_message_hook
end

module Transport_layer = struct
  module Interface = Transport_layer_interface
  module Default_parameters = Transport_layer_default_parameters

  type t =
    ( Interface.p2p_message,
      Interface.peer_metadata,
      Interface.connection_metadata )
    P2p.t

  let create ~network_name config limits =
    P2p.create
      ~config
      ~limits
      Interface.peer_meta_config
      Interface.conn_meta_config
    @@ Interface.message_config ~network_name

  let activate ?(additional_points = []) p2p =
    let () = P2p.activate p2p in
    P2p.pool p2p
    |> Option.iter (fun pool ->
           List.iter
             (fun point ->
               P2p_pool.register_point ~trusted:true pool point |> ignore)
             additional_points)
end

module Transport_layer_hooks = Gs_transport_connection
