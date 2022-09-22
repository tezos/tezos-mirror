(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Protocol Signature Instance

    This module is the entrypoint to the protocol for shells and other
    embedders. This signature is an instance of
    {{!Tezos_protocol_environment_sigs.V7.T.Updater.PROTOCOL} the
    [Updater.PROTOCOL] signature} from the
    {{:https://tezos.gitlab.io/shell/the_big_picture.html#the-economic-protocol-environment-and-compiler}
    Protocol Environment}.

    Each Protocol depends on a version of the Protocol Environment. For the
    currently developed protocol, this is normally the latest version.  You can
    see {{!Tezos_protocol_environment_sigs} the full list of versions here}.

    For details on how Protocol and Environment interact, see
    {{:https://tezos.gitlab.io/shell/the_big_picture.html} this overview}.
 *)

type operation_data = Alpha_context.packed_protocol_data

type operation = Alpha_context.packed_operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

include
  Updater.PROTOCOL
    with type block_header_data = Alpha_context.Block_header.protocol_data
     and type block_header_metadata = Apply_results.block_metadata
     and type block_header = Alpha_context.Block_header.t
     and type operation_data := operation_data
     and type operation_receipt = Apply_results.packed_operation_metadata
     and type operation := operation
     and type validation_state = Validate.validation_state
     and type application_state = Apply.application_state
