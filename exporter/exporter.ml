(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let rec exec db ?cb q =
  let when_locked () =
    let delay = 1. in
    Format.eprintf
      "Failed to exec: %s. Retrying in %fs@."
      (Sqlite3.errmsg db)
      delay ;
    Unix.sleepf delay ;
    exec db ?cb q
  in
  match Sqlite3.exec db ?cb q with
  | Sqlite3.Rc.OK -> ()
  | Sqlite3.Rc.BUSY ->
      (* confusingly, the error message is "database is locked" *)
      when_locked ()
  | _ -> Format.eprintf "Failed to exec \'%s\': %s@." q (Sqlite3.errmsg db)

let parse_block_row r =
  let hash = Block_hash.of_hex_exn (`Hex (Stdlib.Option.get r.(0))) in
  let delegate =
    Signature.Public_key_hash.of_hex_exn (`Hex (Stdlib.Option.get r.(1)))
  in
  let round = Int32.of_string (Stdlib.Option.get r.(3)) in
  let timestamp =
    Time.Protocol.of_seconds (Int64.of_string (Stdlib.Option.get r.(5)))
  in
  let reception_time =
    match Time.System.of_notation_opt (Stdlib.Option.get r.(4)) with
    | Some t -> t
    | None ->
        Format.eprintf "failed to parse date (%s)" (Stdlib.Option.get r.(4)) ;
        Time.System.of_protocol_exn timestamp
  in
  let nonce = None in
  Data.Block.
    {
      hash;
      delegate;
      delegate_alias = r.(2);
      round;
      reception_time;
      timestamp;
      nonce;
    }

let select_blocks db level =
  let q =
    Format.asprintf
      "SELECT hex(b.hash), hex(d.address), d.alias, b.round, r.timestamp, \
       b.timestamp FROM blocks b, blocks_reception r, delegates d ON r.block = \
       b.id AND d.id = b.baker WHERE b.level = %d"
      level
  in
  let blocks = ref [] in
  let cb r _headers =
    let block = parse_block_row r in
    blocks := block :: !blocks
  in
  exec db ~cb q ;
  !blocks

module Op_key = struct
  type t = {kind : Consensus_ops.operation_kind; round : int}

  let compare op1 op2 =
    let c = op1.round - op2.round in
    if c = 0 then
      match (op1.kind, op2.kind) with
      | (Preendorsement, Endorsement) -> -1
      | (Endorsement, Preendorsement) -> 1
      | _ -> 0
    else c
end

module Pkh_ops = Map.Make (Op_key)

type received = {reception_time : Time.System.t; errors : string}

type op_info = {included : Block_hash.t list; received : received list}

let select_ops db level =
  (* We make 3 queries:
     - one to detect "missing" ops (not included, not received)
     - one to detect included ops
     - one to detect received ops
     We then combine the results.
  *)
  let q_missing =
    Format.asprintf
      "SELECT hex(d.address), d.alias FROM endorsing_rights e, delegates d ON \
       e.delegate = d.id WHERE e.level = %d AND e.delegate NOT IN (SELECT \
       o.endorser FROM operations o WHERE level = o.level = %d)"
      level
      level
  in
  let parse_missing_row row =
    ( `Hex (Stdlib.Option.get row.(0)) |> Signature.Public_key_hash.of_hex_exn,
      row.(1) )
  in
  let q_included =
    Format.asprintf
      "SELECT hex(d.address), d.alias, o.endorsement, o.round, hex(b.hash) \
       FROM operations o, operations_inclusion i, blocks b, delegates d ON \
       i.operation = o.id AND i.block = b.id AND o.endorser = d.id WHERE \
       o.level = %d"
      level
  in
  let parse_included_row row =
    ( `Hex (Stdlib.Option.get row.(0)) |> Signature.Public_key_hash.of_hex_exn,
      row.(1),
      (match row.(2) with
      | Some "0" -> Consensus_ops.Preendorsement
      | Some "1" -> Consensus_ops.Endorsement
      | _ -> Stdlib.failwith "[parse_included_row] unknown operation kind"),
      int_of_string (Stdlib.Option.get row.(3)),
      `Hex (Stdlib.Option.get row.(4)) |> Block_hash.of_hex_exn )
  in
  let q_received =
    Format.asprintf
      "SELECT hex(d.address), d.alias, r.timestamp, r.errors, o.endorsement, \
       o.round FROM operations o, operations_reception r, delegates d ON \
       r.operation = o.id AND o.endorser = d.id WHERE o.level = %d"
      level
  in
  let parse_received_row row =
    ( `Hex (Stdlib.Option.get row.(0)) |> Signature.Public_key_hash.of_hex_exn,
      row.(1),
      Time.System.of_notation_exn (Stdlib.Option.get row.(2)),
      Option.value ~default:"" row.(3),
      (match row.(4) with
      | Some "0" -> Consensus_ops.Preendorsement
      | Some "1" -> Consensus_ops.Endorsement
      | _ -> Stdlib.failwith "[parse_included_row] unknown operation kind"),
      int_of_string (Stdlib.Option.get row.(5)) )
  in
  let module Ops = Signature.Public_key_hash.Map in
  let info = ref Ops.empty in
  let cb_missing r _headers =
    let (delegate, alias) = parse_missing_row r in
    info := Ops.add delegate (alias, Pkh_ops.empty) !info
  in
  let cb_included r _headers =
    let (delegate, alias, kind, round, block_hash) = parse_included_row r in
    match Ops.find_opt delegate !info with
    | Some (alias, ops) ->
        let op_key = Op_key.{kind; round} in
        let op =
          match Pkh_ops.find_opt op_key ops with
          | Some op_info ->
              {op_info with included = block_hash :: op_info.included}
          | None -> {included = [block_hash]; received = []}
        in
        let ops' = Pkh_ops.add op_key op ops in
        info := Ops.add delegate (alias, ops') !info
    | None ->
        let op_key = Op_key.{kind; round} in
        let op_info = {included = [block_hash]; received = []} in
        let ops = Pkh_ops.singleton op_key op_info in
        info := Ops.add delegate (alias, ops) !info
  in
  let cb_received r _headers =
    let (delegate, alias, reception_time, errors, kind, round) =
      parse_received_row r
    in
    let received_info = {reception_time; errors} in
    match Ops.find_opt delegate !info with
    | Some (alias, ops) ->
        let op_key = Op_key.{kind; round} in
        let op =
          match Pkh_ops.find_opt op_key ops with
          | Some op_info ->
              {op_info with received = received_info :: op_info.received}
          | None -> {included = []; received = [received_info]}
        in
        let ops' = Pkh_ops.add op_key op ops in
        info := Ops.add delegate (alias, ops') !info
    | None ->
        let op_key = Op_key.{kind; round} in
        let op_info = {included = []; received = [received_info]} in
        let ops = Pkh_ops.singleton op_key op_info in
        info := Ops.add delegate (alias, ops) !info
  in
  exec db ~cb:cb_missing q_missing ;
  exec db ~cb:cb_included q_included ;
  exec db ~cb:cb_received q_received ;
  let translate_ops pkh_ops =
    List.map
      (fun (Op_key.{kind; round}, op_info) ->
        let reception_time =
          if op_info.received = [] then None
          else
            let ordered =
              List.sort
                (fun op1 op2 ->
                  Ptime.compare op1.reception_time op2.reception_time)
                op_info.received
            in
            let r = Stdlib.List.hd ordered in
            Some r.reception_time
        in
        Data.Delegate_operations.
          {
            kind;
            round = Some (Int32.of_int round);
            reception_time;
            errors = None (* TODO *);
            block_inclusion = op_info.included;
          })
      (Pkh_ops.bindings pkh_ops)
  in
  Ops.fold
    (fun pkh (alias, pkh_ops) acc ->
      Data.Delegate_operations.
        {
          delegate = pkh;
          delegate_alias = alias;
          operations = translate_ops pkh_ops;
        }
      :: acc)
    !info
    []

let data_at_level db level =
  let blocks = select_blocks db level in
  let delegate_operations = select_ops db level in
  let unaccurate = (* FIXME: we should check the node's head *) false in
  Data.{blocks; delegate_operations; unaccurate}
