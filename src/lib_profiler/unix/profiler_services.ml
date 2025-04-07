(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc.Context

module type REGISTERED = sig
  type t = {
    registered_backend : Profiler.view list;
    backends : (string * Profiler.view) list;
  }

  val encoding : t Data_encoding.t

  module S : sig
    val registered :
      ([`GET], unit, unit, unit, unit, t) Tezos_rpc.Service.service
  end

  val registered : #simple -> t tzresult Lwt.t
end

module MakeRegistered () : REGISTERED = struct
  type t = {
    registered_backend : Profiler.view list;
    backends : (string * Profiler.view) list;
  }

  let backends_encoding =
    let open Data_encoding in
    def
      "backends_encoding"
      ~description:"Backends encoding"
      ~title:"Backends encoding"
      (list (tup2 string Profiler_kind.kind_encoding))

  let encoding =
    let open Data_encoding in
    def "profiler registered backend" ~description:"Registered backend."
    @@ conv
         (fun {registered_backend; backends} -> (registered_backend, backends))
         (fun (registered_backend, backends) -> {registered_backend; backends})
         (obj2
            (req "registered_backend" (list Profiler_kind.kind_encoding))
            (req "backends" backends_encoding))

  module S = struct
    let registered =
      Tezos_rpc.Service.get_service
        ~description:"Registered backend."
        ~query:Tezos_rpc.Query.empty
        ~output:encoding
        Tezos_rpc.Path.(root / "profiler" / "registered_backend")
  end

  let registered ctxt = Tezos_rpc.Context.make_call S.registered ctxt () () ()
end

let registered_module () = (module MakeRegistered () : REGISTERED)
