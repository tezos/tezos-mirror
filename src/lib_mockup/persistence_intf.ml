(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type S = sig
  val get_registered_mockup :
    Protocol_hash.t option ->
    #Tezos_client_base.Client_context.printer ->
    Registration.mockup_environment tzresult Lwt.t

  (** Returns a mockup environment for the default protocol (which is the first
    in the list of registered protocol, cf [Registration] module). *)
  val default_mockup_context :
    Tezos_client_base.Client_context.printer ->
    (Registration.mockup_environment * Registration.mockup_context) tzresult
    Lwt.t

  (**  Returns a mockup environment for the specified protocol hash. *)
  val init_mockup_context_by_protocol_hash :
    cctxt:Tezos_client_base.Client_context.printer ->
    protocol_hash:Protocol_hash.t ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    (Registration.mockup_environment * Registration.mockup_context) tzresult
    Lwt.t

  (** Load a mockup environment and initializes a protocol RPC context from
    a mockup base directory. If the protocol is specified, check that the
    loaded environment agrees with it. *)
  val get_mockup_context_from_disk :
    base_dir:string ->
    protocol_hash:Protocol_hash.t option ->
    #Tezos_client_base.Client_context.printer ->
    (Registration.mockup_environment * Registration.mockup_context) tzresult
    Lwt.t

  (** Initializes an on-disk mockup environment in [base_dir] for the specified
    protocol. *)
  val create_mockup :
    cctxt:Tezos_client_base.Client_context.full ->
    protocol_hash:Protocol_hash.t ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    asynchronous:bool ->
    unit tzresult Lwt.t

  (** Overwrites an on-disk mockup environment. *)
  val overwrite_mockup :
    protocol_hash:Protocol_hash.t ->
    chain_id:Chain_id.t ->
    rpc_context:Tezos_protocol_environment.rpc_context ->
    protocol_data:bytes ->
    base_dir:string ->
    unit tzresult Lwt.t

  (** {2 Base directory states} *)

  type base_dir_class =
    | Base_dir_does_not_exist
    | Base_dir_is_file
    | Base_dir_is_mockup
    | Base_dir_is_nonempty
    | Base_dir_is_empty

  val pp_base_dir_class : Format.formatter -> base_dir_class -> unit

  (** Test whether base directory is a valid target for loading or creating
    a mockup environment. *)
  val classify_base_dir : string -> base_dir_class tzresult Lwt.t
end

module type T = sig
  include S

  module type S = S

  module Internal_for_tests : sig
    module Make (Registration : Registration.S) : S
  end
end
