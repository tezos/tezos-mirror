(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Make (Backend : Simulator.SimulationBackend) : Tezlink_backend_sig.S =
struct
  type block_param =
    [ `Head of int32
    | `Level of int32
    | `Hash of Ethereum_types.block_hash * int32 ]

  let constants _chain (_block : block_param) =
    failwith "Not Implemented Yet (%s)" __LOC__

  let current_level _ _ ~offset:_ = failwith "Not Implemented Yet (%s)" __LOC__

  let balance _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let bootstrap_accounts () = failwith "Not Implemented Yet (%s)" __LOC__

  let get_storage _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let get_code _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let get_script _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let manager_key _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let counter _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let block _chain _block = failwith "Not Implemented Yet (%s)" __LOC__

  let monitor_heads _chain _query =
    Stdlib.failwith (Format.sprintf "Not Implemented Yet (%s)" __LOC__)

  let bootstrapped () = failwith "Not Implemented Yet (%s)" __LOC__

  let block_hash _chain _block = failwith "Not Implemented Yet (%s)" __LOC__

  let simulate_operation ~chain_id:_ ~skip_signature:_ _op _hash _block =
    failwith "Not Implemented Yet (%s)" __LOC__
end
