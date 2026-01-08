(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Id = struct
  (* A net point (address x port). *)
  type t = P2p_addr.t * P2p_addr.port

  let compare (a1, p1) (a2, p2) =
    match Ipaddr.V6.compare a1 a2 with 0 -> p1 - p2 | x -> x

  let equal p1 p2 = compare p1 p2 = 0

  let pp ppf (addr, port) =
    match Ipaddr.v4_of_v6 addr with
    | Some addr -> Format.fprintf ppf "%a:%d" Ipaddr.V4.pp addr port
    | None -> Format.fprintf ppf "[%a]:%d" Ipaddr.V6.pp addr port

  let pp_opt ppf = function
    | None -> Format.pp_print_string ppf "none"
    | Some point -> pp ppf point

  let pp_list ppf point_list =
    Format.pp_print_list ~pp_sep:Format.pp_print_space pp ppf point_list

  let is_local (addr, _) = Ipaddr.V6.is_private addr

  let is_global (addr, _) = not @@ Ipaddr.V6.is_private addr

  include Point_parser

  let parse_addr_port_id addr = parse_full_addr (Lexing.from_string addr)

  let pp_addr_port_id fmt {addr; port; peer_id} =
    let open Format in
    (* Protects the resulting network resource identifier from
       conflictual colon (:).
       See https://en.wikipedia.org/wiki/IPv6_address#Literal_IPv6_addresses_in_network_resource_identifiers *)
    let unconflictual_addr =
      if String.contains addr ':' then Format.sprintf "[%s]" addr else addr
    in
    pp_print_string fmt unconflictual_addr ;
    Option.iter (fprintf fmt ":%d") port ;
    Option.iter
      (fun peer -> fprintf fmt "#%s" (P2p_peer_id.to_b58check peer))
      peer_id

  let addr_port_id_to_string addr = Format.asprintf "%a" pp_addr_port_id addr

  let addr_port_id_of_string_exn str =
    match parse_addr_port_id str with
    | Ok r -> r
    | Error err ->
        invalid_arg
          (Format.asprintf
             "Parsing of '%s' failed: %s.@."
             str
             (string_of_parsing_error err))

  let of_string_exn ?default_port str =
    let {addr; port; _} = addr_port_id_of_string_exn str in
    let port =
      match (port, default_port) with
      | Some port, _ -> port
      | None, Some port -> port
      | None, None -> invalid_arg "P2p_point.of_string_exn: no port"
    in
    match Ipaddr.of_string_exn addr with
    | V4 addr -> (Ipaddr.v6_of_v4 addr, port)
    | V6 addr -> (addr, port)

  let of_string ?default_port str =
    try Ok (of_string_exn ?default_port str) with
    | Invalid_argument s -> Error s
    | Failure s -> Error s
    | _ -> Error "P2p_point.of_string"

  let to_string saddr = Format.asprintf "%a" pp saddr

  let encoding =
    let open Data_encoding in
    check_size
      (4 (* Uint30 that gives the size of the encoded string *)
      + 8 (*number of IPv6 chunks *)
        *
        (*size of IPv6 chunks*)
        4
      +
      (*IPv6 chunk separators*)
      7
      +
      (*optional enclosing bracket*)
      2
      +
      (*port separator*)
      1
      +
      (*size of port number*)
      5)
    @@ def "p2p_point.id" ~description:"Identifier for a peer point"
    @@ conv to_string of_string_exn string

  let addr_port_id_encoding =
    let open Data_encoding in
    conv addr_port_id_to_string addr_port_id_of_string_exn string

  let rpc_arg =
    Tezos_rpc.Arg.make
      ~name:"point"
      ~descr:"A network point (ipv4:port or [ipv6]:port)."
      ~destruct:of_string
      ~construct:to_string
      ()

  let hash = Hashtbl.hash
end

module Map = Map.Make (Id)
module Set = Set.Make (Id)

module Table = Hashtbl.MakeSeeded (struct
  type t = Id.t

  let equal = Id.equal

  (* See [src/lib_base/tzPervasives.ml] for an explanation *)
  [@@@ocaml.warning "-32"]

  let hash = Hashtbl.seeded_hash

  let seeded_hash = Hashtbl.seeded_hash

  [@@@ocaml.warning "+32"]
end)

module Filter = struct
  type t = Requested | Accepted | Running | Disconnected

  let rpc_arg =
    Tezos_rpc.Arg.make
      ~name:"p2p.point.state_filter"
      ~destruct:(function
        | "requested" -> Ok Requested
        | "accepted" -> Ok Accepted
        | "running" -> Ok Running
        | "disconnected" -> Ok Disconnected
        | s -> Error (Format.asprintf "Invalid state: %s" s))
      ~construct:(function
        | Requested -> "requested"
        | Accepted -> "accepted"
        | Running -> "running"
        | Disconnected -> "disconnected")
      ()
end

module State = struct
  type t =
    | Requested
    | Accepted of P2p_peer_id.t
    | Running of P2p_peer_id.t
    | Disconnected

  let of_p2p_peer_id = function
    | Requested -> None
    | Accepted pi -> Some pi
    | Running pi -> Some pi
    | Disconnected -> None

  let of_peerid_state state pi =
    match (state, pi) with
    | Requested, _ -> Requested
    | Accepted _, Some pi -> Accepted pi
    | Running _, Some pi -> Running pi
    | Disconnected, _ -> Disconnected
    | _ -> invalid_arg "state_of_state_peerid"

  let pp_digram ppf = function
    | Requested -> Format.fprintf ppf "⚎"
    | Accepted _ -> Format.fprintf ppf "⚍"
    | Running _ -> Format.fprintf ppf "⚌"
    | Disconnected -> Format.fprintf ppf "⚏"

  let encoding =
    let open Data_encoding in
    let branch_encoding name obj =
      conv
        (fun x -> ((), x))
        (fun ((), x) -> x)
        (merge_objs (obj1 (req "event_kind" (constant name))) obj)
    in
    def
      "p2p_point.state"
      ~description:
        "The state a connection to a peer point can be in: requested \
         (connection open from here), accepted (handshake), running \
         (connection already established), disconnected (no connection)."
    @@ union
         ~tag_size:`Uint8
         [
           case
             (Tag 0)
             ~title:"Requested"
             (branch_encoding "requested" empty)
             (function Requested -> Some () | _ -> None)
             (fun () -> Requested);
           case
             (Tag 1)
             ~title:"Accepted"
             (branch_encoding
                "accepted"
                (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
             (function Accepted p2p_peer_id -> Some p2p_peer_id | _ -> None)
             (fun p2p_peer_id -> Accepted p2p_peer_id);
           case
             (Tag 2)
             ~title:"Running"
             (branch_encoding
                "running"
                (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
             (function Running p2p_peer_id -> Some p2p_peer_id | _ -> None)
             (fun p2p_peer_id -> Running p2p_peer_id);
           case
             (Tag 3)
             ~title:"Disconnected"
             (branch_encoding "disconnected" empty)
             (function Disconnected -> Some () | _ -> None)
             (fun () -> Disconnected);
         ]

  let raw_filter (f : Filter.t) (s : t) =
    match (f, s) with
    | Requested, Requested -> true
    | Requested, (Accepted _ | Running _ | Disconnected)
    | (Accepted | Running | Disconnected), Requested ->
        false
    | Accepted, Accepted _ -> true
    | Accepted, (Running _ | Disconnected)
    | (Running | Disconnected), Accepted _ ->
        false
    | Running, Running _ -> true
    | Disconnected, Disconnected -> true
    | Running, Disconnected | Disconnected, Running _ -> false

  let filter filters state = List.exists (fun f -> raw_filter f state) filters
end

module Info = struct
  type t = {
    trusted : bool;
    reconnection_time : Time.System.t option;
    state : State.t;
    last_failed_connection : Time.System.t option;
    last_rejected_connection : (P2p_peer_id.t * Time.System.t) option;
    last_established_connection : (P2p_peer_id.t * Time.System.t) option;
    last_disconnection : (P2p_peer_id.t * Time.System.t) option;
    last_seen : (P2p_peer_id.t * Time.System.t) option;
    last_miss : Time.System.t option;
    expected_peer_id : P2p_peer_id.t option;
  }

  let encoding =
    let open Data_encoding in
    def
      "p2p_point.info"
      ~description:
        "Information about a peer point. Includes flags, state, and records \
         about past events."
    @@ conv
         (fun {
                trusted;
                reconnection_time;
                state;
                last_failed_connection;
                last_rejected_connection;
                last_established_connection;
                last_disconnection;
                last_seen;
                last_miss;
                expected_peer_id;
              }
            ->
           let p2p_peer_id = State.of_p2p_peer_id state in
           ( ( trusted,
               reconnection_time,
               state,
               p2p_peer_id,
               last_failed_connection,
               last_rejected_connection,
               last_established_connection,
               last_disconnection,
               last_seen,
               last_miss ),
             expected_peer_id ))
         (fun ( ( trusted,
                  reconnection_time,
                  state,
                  p2p_peer_id,
                  last_failed_connection,
                  last_rejected_connection,
                  last_established_connection,
                  last_disconnection,
                  last_seen,
                  last_miss ),
                expected_peer_id )
            ->
           let state = State.of_peerid_state state p2p_peer_id in
           {
             trusted;
             reconnection_time;
             state;
             last_failed_connection;
             last_rejected_connection;
             last_established_connection;
             last_disconnection;
             last_seen;
             last_miss;
             expected_peer_id;
           })
         (merge_objs
            (obj10
               (req "trusted" bool)
               (opt "greylisted_until" Time.System.encoding)
               (req "state" State.encoding)
               (opt "p2p_peer_id" P2p_peer_id.encoding)
               (opt "last_failed_connection" Time.System.encoding)
               (opt
                  "last_rejected_connection"
                  (tup2 P2p_peer_id.encoding Time.System.encoding))
               (opt
                  "last_established_connection"
                  (tup2 P2p_peer_id.encoding Time.System.encoding))
               (opt
                  "last_disconnection"
                  (tup2 P2p_peer_id.encoding Time.System.encoding))
               (opt
                  "last_seen"
                  (tup2 P2p_peer_id.encoding Time.System.encoding))
               (opt "last_miss" Time.System.encoding))
            (obj1 (opt "expected_peer_id" P2p_peer_id.encoding)))
end

module Pool_event = struct
  type kind =
    | Outgoing_request
    | Accepting_request of P2p_peer_id.t
    | Rejecting_request of P2p_peer_id.t
    | Request_rejected of P2p_peer_id.t option
    | Connection_established of P2p_peer_id.t
    | Disconnection of P2p_peer_id.t
    | External_disconnection of P2p_peer_id.t

  let kind_encoding =
    let open Data_encoding in
    let branch_encoding name obj =
      conv
        (fun x -> ((), x))
        (fun ((), x) -> x)
        (merge_objs (obj1 (req "event_kind" (constant name))) obj)
    in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Outgoing_request"
          (branch_encoding "outgoing_request" empty)
          (function Outgoing_request -> Some () | _ -> None)
          (fun () -> Outgoing_request);
        case
          (Tag 1)
          ~title:"Accepting_request"
          (branch_encoding
             "accepting_request"
             (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
          (function
            | Accepting_request p2p_peer_id -> Some p2p_peer_id | _ -> None)
          (fun p2p_peer_id -> Accepting_request p2p_peer_id);
        case
          (Tag 2)
          ~title:"Rejecting_request"
          (branch_encoding
             "rejecting_request"
             (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
          (function
            | Rejecting_request p2p_peer_id -> Some p2p_peer_id | _ -> None)
          (fun p2p_peer_id -> Rejecting_request p2p_peer_id);
        case
          (Tag 3)
          ~title:"Rejecting_rejected"
          (branch_encoding
             "request_rejected"
             (obj1 (opt "p2p_peer_id" P2p_peer_id.encoding)))
          (function
            | Request_rejected p2p_peer_id -> Some p2p_peer_id | _ -> None)
          (fun p2p_peer_id -> Request_rejected p2p_peer_id);
        case
          (Tag 4)
          ~title:"Connection_established"
          (branch_encoding
             "rejecting_request"
             (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
          (function
            | Connection_established p2p_peer_id -> Some p2p_peer_id | _ -> None)
          (fun p2p_peer_id -> Connection_established p2p_peer_id);
        case
          (Tag 5)
          ~title:"Disconnection"
          (branch_encoding
             "rejecting_request"
             (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
          (function Disconnection p2p_peer_id -> Some p2p_peer_id | _ -> None)
          (fun p2p_peer_id -> Disconnection p2p_peer_id);
        case
          (Tag 6)
          ~title:"External_disconnection"
          (branch_encoding
             "rejecting_request"
             (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
          (function
            | External_disconnection p2p_peer_id -> Some p2p_peer_id | _ -> None)
          (fun p2p_peer_id -> External_disconnection p2p_peer_id);
      ]

  type t = kind Time.System.stamped

  let encoding =
    Data_encoding.def
      "p2p_point.pool_event"
      ~description:
        "Events happening during maintenance of and operations on a peer point \
         pool (such as connections, disconnections, connection requests)."
    @@ Time.System.stamped_encoding kind_encoding
end

let () =
  Data_encoding.Registration.register ~pp:Id.pp Id.encoding ;
  Data_encoding.Registration.register ~pp:State.pp_digram State.encoding ;
  Data_encoding.Registration.register Info.encoding ;
  Data_encoding.Registration.register Pool_event.encoding
