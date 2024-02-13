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

module Get_delegates = struct
  open Tezos_protocol_alpha
  open Protocol

  type context = Alpha_context.t

  type contract = Alpha_context.Contract.t

  let hash = hash

  module Tez = struct
    include Alpha_context.Tez

    let ( +? ) a b = Environment.wrap_tzresult (a +? b)
  end

  module Signature = struct
    include Tezos_crypto.Signature.V1
    module To_latest = Tezos_crypto.Signature.Of_V1
  end

  module Contract = struct
    open Alpha_context.Contract

    let fold context ~init ~f =
      let open Lwt_syntax in
      let* l = list context in
      Lwt_list.fold_left_s f init l

    let balance ctxt t = get_balance ctxt t |> Lwt.map Environment.wrap_tzresult

    let frozen_bonds ctxt t =
      get_frozen_bonds ctxt t |> Lwt.map Environment.wrap_tzresult

    let get_staked_balance ctxt t =
      For_RPC.get_staked_balance ctxt t |> Lwt.map Environment.wrap_tzresult

    let get_unstaked_frozen_balance ctxt t =
      For_RPC.get_unstaked_frozen_balance ctxt t
      |> Lwt.map Environment.wrap_tzresult

    let get_unstaked_finalizable_balance ctxt t =
      For_RPC.get_unstaked_finalizable_balance ctxt t
      |> Lwt.map Environment.wrap_tzresult

    let get_full_balance ctxt t =
      For_RPC.get_full_balance ctxt t |> Lwt.map Environment.wrap_tzresult

    let contract_address contract = Alpha_context.Contract.to_b58check contract

    let total_supply ctxt =
      Alpha_context.Contract.get_total_supply ctxt
      |> Lwt.map Environment.wrap_tzresult
  end

  module Commitment = struct
    include Alpha_context.Commitment

    type t = Blinded_public_key_hash.t

    let fold ctxt ~order ~init ~f =
      fold ctxt ~order ~init ~f:(fun c t acc -> f c (Tez_repr.to_mutez t) acc)
  end

  module Delegate = struct
    open Alpha_context.Delegate

    let fold ctxt ~order ~init ~f = fold ctxt ~order ~init ~f

    let pubkey ctxt pkh =
      Alpha_context.Contract.get_manager_key ctxt pkh
      |> Lwt.map Environment.wrap_tzresult

    let staking_balance ctxt pkh =
      For_RPC.staking_balance ctxt pkh |> Lwt.map Environment.wrap_tzresult

    let current_frozen_deposits ctxt pkh =
      current_frozen_deposits ctxt pkh |> Lwt.map Environment.wrap_tzresult

    let unstaked_frozen_deposits ctxt pkh =
      Alpha_context.Unstaked_frozen_deposits.balance
        ctxt
        pkh
        Alpha_context.Level.(current ctxt).cycle
      |> Lwt.map Environment.wrap_tzresult

    let deactivated ctxt pkh =
      deactivated ctxt pkh |> Lwt.map Environment.wrap_tzresult
  end

  let prepare_context ctxt ~level ~predecessor_timestamp ~timestamp =
    let open Lwt_result_syntax in
    let+ ctxt, _, _ =
      Alpha_context.prepare ctxt ~level ~predecessor_timestamp ~timestamp
      |> Lwt.map Environment.wrap_tzresult
    in
    ctxt
end

let () = Known_protocols.register (module Get_delegates)
