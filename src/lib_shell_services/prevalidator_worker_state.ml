(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

module Request = struct
  type ('a, 'b) t =
    | Flush :
        Block_hash.t
        * Chain_validator_worker_state.update
        * Block_hash.Set.t
        * Operation_hash.Set.t
        -> (unit, error trace) t
    | Notify : P2p_peer.Id.t * Mempool.t -> (unit, Empty.t) t
    | Leftover : (unit, Empty.t) t
    | Inject : {op : Operation.t; force : bool} -> (unit, error trace) t
    | Arrived : Operation_hash.t * Operation.t -> (unit, Empty.t) t
    | Advertise : (unit, Empty.t) t
    | Ban : Operation_hash.t -> (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Flush"
          (obj3
             (req "request" (constant "flush"))
             (req "block" Block_hash.encoding)
             (req "event" Chain_validator_worker_state.update_encoding))
          (function
            | View (Flush (hash, event, _, _)) -> Some ((), hash, event)
            | _ -> None)
          (fun ((), hash, event) ->
            View
              (Flush
                 (hash, event, Block_hash.Set.empty, Operation_hash.Set.empty)));
        case
          (Tag 1)
          ~title:"Notify"
          (obj3
             (req "request" (constant "notify"))
             (req "peer" P2p_peer.Id.encoding)
             (req "mempool" Mempool.encoding))
          (function
            | View (Notify (peer, mempool)) -> Some ((), peer, mempool)
            | _ -> None)
          (fun ((), peer, mempool) -> View (Notify (peer, mempool)));
        case
          (Tag 2)
          ~title:"Inject"
          (obj3
             (req "request" (constant "inject"))
             (req "operation" Operation.encoding)
             (req "force" bool))
          (function
            | View (Inject {op; force}) -> Some ((), op, force) | _ -> None)
          (fun ((), op, force) -> View (Inject {op; force}));
        case
          (Tag 3)
          ~title:"Arrived"
          (obj3
             (req "request" (constant "arrived"))
             (req "operation_hash" Operation_hash.encoding)
             (req "operation" Operation.encoding))
          (function
            | View (Arrived (oph, op)) -> Some ((), oph, op) | _ -> None)
          (fun ((), oph, op) -> View (Arrived (oph, op)));
        case
          (Tag 4)
          ~title:"Advertise"
          (obj1 (req "request" (constant "advertise")))
          (function View Advertise -> Some () | _ -> None)
          (fun () -> View Advertise);
        case
          (Tag 5)
          ~title:"Leftover"
          (obj1 (req "request" (constant "leftover")))
          (function View Leftover -> Some () | _ -> None)
          (fun () -> View Leftover);
        case
          (Tag 6)
          ~title:"Ban"
          (obj2
             (req "request" (constant "ban"))
             (req "operation_hash" Operation_hash.encoding))
          (function View (Ban oph) -> Some ((), oph) | _ -> None)
          (fun ((), oph) -> View (Ban oph));
      ]

  let pp ppf (View r) =
    match r with
    | Flush (hash, _, _, _) ->
        Format.fprintf ppf "switching to new head %a" Block_hash.pp hash
    | Notify (id, {Mempool.known_valid; pending}) ->
        Format.fprintf
          ppf
          "@[<v 2>notified by %a of operations"
          P2p_peer.Id.pp
          id ;
        Operation_hash.Set.iter
          (fun oph ->
            Format.fprintf ppf "@,%a (known_valid)" Operation_hash.pp oph)
          known_valid ;
        Operation_hash.Set.iter
          (fun oph -> Format.fprintf ppf "@,%a (pending)" Operation_hash.pp oph)
          pending ;
        Format.fprintf ppf "@]"
    | Leftover -> Format.fprintf ppf "process next batch of operation"
    | Inject {op; force} ->
        Format.fprintf
          ppf
          "injecting operation %a (force:%b)"
          Operation_hash.pp
          (Operation.hash op)
          force
    | Arrived (oph, _) ->
        Format.fprintf ppf "operation %a arrived" Operation_hash.pp oph
    | Advertise -> Format.fprintf ppf "advertising pending operations"
    | Ban oph -> Format.fprintf ppf "banning operation %a" Operation_hash.pp oph
end

module Operation_encountered = struct
  type situation =
    | Injected
    | Arrived
    | Notified of P2p_peer_id.t option
    | Other

  type t = situation * Operation_hash.t

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"injected"
          (obj2
             (req "situation" (constant "injected"))
             (req "operation" Operation_hash.encoding))
          (function Injected, oph -> Some ((), oph) | _ -> None)
          (fun ((), oph) -> (Injected, oph));
        case
          (Tag 1)
          ~title:"arrived"
          (obj2
             (req "situation" (constant "arrived"))
             (req "operation" Operation_hash.encoding))
          (function Arrived, oph -> Some ((), oph) | _ -> None)
          (fun ((), oph) -> (Arrived, oph));
        case
          (Tag 2)
          ~title:"notified"
          (obj3
             (req "situation" (constant "notified"))
             (req "operation" Operation_hash.encoding)
             (req "peer" (option P2p_peer_id.encoding)))
          (function Notified peer, oph -> Some ((), oph, peer) | _ -> None)
          (fun ((), oph, peer) -> (Notified peer, oph));
        case
          (Tag 3)
          ~title:"other"
          (obj2
             (req "situation" (constant "other"))
             (req "operation" Operation_hash.encoding))
          (function Other, hash -> Some ((), hash) | _ -> None)
          (fun ((), oph) -> (Other, oph));
      ]

  let situation_pp ppf = function
    | Injected -> Format.fprintf ppf "injected"
    | Arrived -> Format.fprintf ppf "arrived"
    | Notified None -> Format.fprintf ppf "notified from the previous run"
    | Notified (Some pid) ->
        Format.fprintf ppf "notified from %a" P2p_peer_id.pp pid
    | Other -> Format.fprintf ppf "encountered"

  let pp ppf (situation, oph) =
    Format.fprintf
      ppf
      "operation %a: %a"
      situation_pp
      situation
      Operation_hash.pp
      oph
end
