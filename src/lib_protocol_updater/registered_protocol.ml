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

module type T = sig
  val hash : Protocol_hash.t

  include Tezos_protocol_environment.PROTOCOL

  val complete_b58prefix :
    Tezos_protocol_environment.Context.t -> string -> string list Lwt.t
end

type t = (module T)

let build hash =
  match Tezos_protocol_registerer.get hash with
  | None -> None
  | Some (V0 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V0.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V1 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V1.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V2 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V2.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V3 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V3.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V4 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V4.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V5 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V5.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V6 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V6.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V7 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V7.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V8 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V8.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V9 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V9.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)
  | Some (V10 protocol) ->
      let (module F) = protocol in
      let module Name = struct
        let name = Protocol_hash.to_b58check hash
      end in
      let module Env = Tezos_protocol_environment.V10.Make (Name) () in
      Some
        (module struct
          module Raw = F (Env)

          module P = struct
            let hash = hash

            include Env.Lift (Raw)
          end

          include P

          let complete_b58prefix = Env.Context.complete
        end : T)

module VersionTable = Protocol_hash.Table

let versions : (module T) VersionTable.t = VersionTable.create 20

let sources : Protocol.t VersionTable.t = VersionTable.create 20

let mem hash =
  VersionTable.mem versions hash || Tezos_protocol_registerer.mem hash

let get hash =
  let set_logger proto =
    let consumer = Protocol_logging.make_log_message_consumer () in
    let (module Proto : T) = proto in
    Proto.set_log_message_consumer consumer
  in
  match VersionTable.find versions hash with
  | Some proto ->
      set_logger proto ;
      Some proto
  | None -> (
      match build hash with
      | Some proto ->
          VersionTable.add versions hash proto ;
          set_logger proto ;
          Some proto
      | None -> None)

type error += Unregistered_protocol of Protocol_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"registered_protocol.unregistered_protocol"
    ~title:"Unregistered protocol"
    ~description:"No protocol was registered with the requested hash."
    ~pp:(fun fmt hash ->
      Format.fprintf
        fmt
        "@[<hov>No registered protocol with hash:@ %a@]"
        Protocol_hash.pp
        hash)
    Data_encoding.(obj1 (req "protocol_hash" Protocol_hash.encoding))
    (function Unregistered_protocol hash -> Some hash | _ -> None)
    (fun hash -> Unregistered_protocol hash)

let get_result hash =
  let open Lwt_result_syntax in
  match get hash with
  | Some t -> return t
  | None -> tzfail (Unregistered_protocol hash)

let seq () = VersionTable.to_seq_values versions

let seq_embedded () = VersionTable.to_seq_keys sources

let get_embedded_sources hash = VersionTable.find sources hash

module type Source_sig = sig
  val hash : Protocol_hash.t option

  val sources : Protocol.t
end

module Register_embedded_V0
    (Env : Tezos_protocol_environment.V0.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V1
    (Env : Tezos_protocol_environment.V1.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V2
    (Env : Tezos_protocol_environment.V2.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V3
    (Env : Tezos_protocol_environment.V3.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V4
    (Env : Tezos_protocol_environment.V4.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V5
    (Env : Tezos_protocol_environment.V5.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V6
    (Env : Tezos_protocol_environment.V6.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V7
    (Env : Tezos_protocol_environment.V7.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V8
    (Env : Tezos_protocol_environment.V8.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V9
    (Env : Tezos_protocol_environment.V9.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end

module Register_embedded_V10
    (Env : Tezos_protocol_environment.V10.T)
    (Proto : Env.Updater.PROTOCOL)
    (Source : Source_sig) =
struct
  let hash =
    match Source.hash with
    | None -> Protocol.hash Source.sources
    | Some hash -> hash

  module Self = struct
    module P = struct
      let hash = hash

      include Env.Lift (Proto)
    end

    include P

    let complete_b58prefix = Env.Context.complete
  end

  let () =
    VersionTable.add sources hash Source.sources ;
    VersionTable.add versions hash (module Self : T)

  include Self
end
