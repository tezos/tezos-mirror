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

open Storage_functors
open Storage_sigs

module Encoding = struct
  module UInt16 = struct
    type t = int

    let encoding = Data_encoding.uint16
  end

  module Int32 = struct
    type t = Int32.t

    let encoding = Data_encoding.int32
  end

  module Int64 = struct
    type t = Int64.t

    let encoding = Data_encoding.int64
  end

  module Z = struct
    type t = Z.t

    let encoding = Data_encoding.z
  end
end

module Int31_index : INDEX with type t = int = struct
  type t = int

  let path_length = 1

  let to_path c l = string_of_int c :: l

  let of_path = function [] | _ :: _ :: _ -> None | [c] -> int_of_string_opt c

  type 'a ipath = 'a * t

  let args =
    Storage_description.One
      {
        rpc_arg = RPC_arg.int;
        encoding = Data_encoding.int31;
        compare = Compare.Int.compare;
      }
end

module Make_index (H : Storage_description.INDEX) :
  INDEX with type t = H.t and type 'a ipath = 'a * H.t = struct
  include H

  type 'a ipath = 'a * t

  let args = Storage_description.One {rpc_arg; encoding; compare}
end

module type Simple_single_data_storage = sig
  type value

  val get : Raw_context.t -> value tzresult Lwt.t

  val update : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t

  val init : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t
end

module Block_priority =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["block_priority"]
    end)
    (Encoding.UInt16)

(** Contracts handling *)

module Contract = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["contracts"]
      end)

  module Global_counter =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["global_counter"]
      end)
      (Encoding.Z)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Contract_repr.Index))

  let fold = Indexed_context.fold_keys

  let list = Indexed_context.keys

  module Balance =
    Indexed_context.Make_map
      (struct
        let name = ["balance"]
      end)
      (Tez_repr)

  module Frozen_balance_index =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["frozen_balance"]
         end))
         (Make_index (Cycle_repr.Index))

  module Frozen_deposits =
    Frozen_balance_index.Make_map
      (struct
        let name = ["deposits"]
      end)
      (Tez_repr)

  module Frozen_fees =
    Frozen_balance_index.Make_map
      (struct
        let name = ["fees"]
      end)
      (Tez_repr)

  module Frozen_rewards =
    Frozen_balance_index.Make_map
      (struct
        let name = ["rewards"]
      end)
      (Tez_repr)

  module Manager =
    Indexed_context.Make_map
      (struct
        let name = ["manager"]
      end)
      (Manager_repr)

  module Delegate =
    Indexed_context.Make_map
      (struct
        let name = ["delegate"]
      end)
      (Signature.Public_key_hash)

  module Inactive_delegate =
    Indexed_context.Make_set
      (Registered)
      (struct
        let name = ["inactive_delegate"]
      end)

  module Delegate_desactivation =
    Indexed_context.Make_map
      (struct
        let name = ["delegate_desactivation"]
      end)
      (Cycle_repr)

  module Delegated =
    Make_data_set_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["delegated"]
         end))
         (Make_index (Contract_repr.Index))

  module Counter =
    Indexed_context.Make_map
      (struct
        let name = ["counter"]
      end)
      (Encoding.Z)

  (* Consume gas for serialization and deserialization of expr in this
     module *)
  module Make_carbonated_map_expr (N : Storage_sigs.NAME) :
    Storage_sigs.Non_iterable_indexed_carbonated_data_storage
      with type key = Contract_repr.t
       and type value = Script_repr.lazy_expr
       and type t := Raw_context.t = struct
    module I =
      Indexed_context.Make_carbonated_map
        (N)
        (struct
          type t = Script_repr.lazy_expr

          let encoding = Script_repr.lazy_expr_encoding
        end)

    type context = I.context

    type key = I.key

    type value = I.value

    let mem = I.mem

    let remove_existing = I.remove_existing

    let remove = I.remove

    let consume_deserialize_gas ctxt value =
      Raw_context.consume_gas ctxt (Script_repr.force_decode_cost value)

    let consume_serialize_gas ctxt value =
      Raw_context.consume_gas ctxt (Script_repr.force_bytes_cost value)

    let get ctxt contract =
      I.get ctxt contract >>=? fun (ctxt, value) ->
      Lwt.return
        (consume_deserialize_gas ctxt value >|? fun ctxt -> (ctxt, value))

    let find ctxt contract =
      I.find ctxt contract >>=? fun (ctxt, value_opt) ->
      Lwt.return
      @@
      match value_opt with
      | None -> ok (ctxt, None)
      | Some value ->
          consume_deserialize_gas ctxt value >|? fun ctxt -> (ctxt, value_opt)

    let update ctxt contract value =
      consume_serialize_gas ctxt value >>?= fun ctxt ->
      I.update ctxt contract value

    let add_or_remove ctxt contract value_opt =
      match value_opt with
      | None -> I.add_or_remove ctxt contract None
      | Some value ->
          consume_serialize_gas ctxt value >>?= fun ctxt ->
          I.add_or_remove ctxt contract value_opt

    let init ctxt contract value =
      consume_serialize_gas ctxt value >>?= fun ctxt ->
      I.init ctxt contract value

    let add ctxt contract value =
      consume_serialize_gas ctxt value >>?= fun ctxt ->
      I.add ctxt contract value
  end

  module Code = Make_carbonated_map_expr (struct
    let name = ["code"]
  end)

  module Storage = Make_carbonated_map_expr (struct
    let name = ["storage"]
  end)

  module Paid_storage_space =
    Indexed_context.Make_map
      (struct
        let name = ["paid_bytes"]
      end)
      (Encoding.Z)

  module Used_storage_space =
    Indexed_context.Make_map
      (struct
        let name = ["used_bytes"]
      end)
      (Encoding.Z)

  module Roll_list =
    Indexed_context.Make_map
      (struct
        let name = ["roll_list"]
      end)
      (Roll_repr)

  module Change =
    Indexed_context.Make_map
      (struct
        let name = ["change"]
      end)
      (Tez_repr)
end

module type NEXT = sig
  type id

  val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

  val incr : Raw_context.t -> (Raw_context.t * id) tzresult Lwt.t
end

(** Big maps handling *)

module Big_map = struct
  type id = Lazy_storage_kind.Big_map.Id.t

  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["big_maps"]
      end)

  module Next : NEXT with type id := id = struct
    module Storage =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["next"]
        end)
        (Lazy_storage_kind.Big_map.Id)

    let incr ctxt =
      Storage.get ctxt >>=? fun i ->
      Storage.update ctxt (Lazy_storage_kind.Big_map.Id.next i) >|=? fun ctxt ->
      (ctxt, i)

    let init ctxt = Storage.init ctxt Lazy_storage_kind.Big_map.Id.init
  end

  module Index :
    Storage_description.INDEX with type t = Lazy_storage_kind.Big_map.Id.t =
  struct
    (* After flat storage, just use module Index = Lazy_storage_kind.Big_map.Id *)

    module Id = Lazy_storage_kind.Big_map.Id

    type t = Id.t

    let path_length = 6 + Id.path_length

    let to_path c l =
      let raw_key = Data_encoding.Binary.to_bytes_exn Id.encoding c in
      let (`Hex index_key) = Hex.of_bytes (Raw_hashes.blake2b raw_key) in
      String.sub index_key 0 2
      ::
      String.sub index_key 2 2
      ::
      String.sub index_key 4 2
      ::
      String.sub index_key 6 2
      :: String.sub index_key 8 2 :: String.sub index_key 10 2 :: Id.to_path c l

    let of_path = function
      | []
      | [_]
      | [_; _]
      | [_; _; _]
      | [_; _; _; _]
      | [_; _; _; _; _]
      | [_; _; _; _; _; _]
      | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ ->
          None
      | index1 :: index2 :: index3 :: index4 :: index5 :: index6 :: tail ->
          Id.of_path tail
          |> Option.map (fun c ->
                 let raw_key =
                   Data_encoding.Binary.to_bytes_exn Id.encoding c
                 in
                 let (`Hex index_key) =
                   Hex.of_bytes (Raw_hashes.blake2b raw_key)
                 in
                 assert (Compare.String.(String.sub index_key 0 2 = index1)) ;
                 assert (Compare.String.(String.sub index_key 2 2 = index2)) ;
                 assert (Compare.String.(String.sub index_key 4 2 = index3)) ;
                 assert (Compare.String.(String.sub index_key 6 2 = index4)) ;
                 assert (Compare.String.(String.sub index_key 8 2 = index5)) ;
                 assert (Compare.String.(String.sub index_key 10 2 = index6)) ;
                 c)

    let rpc_arg = Id.rpc_arg

    let encoding = Id.encoding

    let compare = Id.compare
  end

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Index))

  let rpc_arg = Index.rpc_arg

  let fold = Indexed_context.fold_keys

  let list = Indexed_context.keys

  let remove ctxt n = Indexed_context.remove ctxt n

  let copy ctxt ~from ~to_ = Indexed_context.copy ctxt ~from ~to_

  type key = Raw_context.t * Index.t

  module Total_bytes =
    Indexed_context.Make_map
      (struct
        let name = ["total_bytes"]
      end)
      (Encoding.Z)

  module Key_type =
    Indexed_context.Make_map
      (struct
        let name = ["key_type"]
      end)
      (struct
        type t = Script_repr.expr

        let encoding = Script_repr.expr_encoding
      end)

  module Value_type =
    Indexed_context.Make_map
      (struct
        let name = ["value_type"]
      end)
      (struct
        type t = Script_repr.expr

        let encoding = Script_repr.expr_encoding
      end)

  module Contents :
    Non_iterable_indexed_carbonated_data_storage_with_values
      with type key = Script_expr_hash.t
       and type value = Script_repr.expr
       and type t := key = struct
    module I =
      Storage_functors.Make_indexed_carbonated_data_storage
        (Make_subcontext (Registered) (Indexed_context.Raw_context)
           (struct
             let name = ["contents"]
           end))
           (Make_index (Script_expr_hash))
           (struct
             type t = Script_repr.expr

             let encoding = Script_repr.expr_encoding
           end)

    type context = I.context

    type key = I.key

    type value = I.value

    let mem = I.mem

    let remove_existing = I.remove_existing

    let remove = I.remove

    let update = I.update

    let add_or_remove = I.add_or_remove

    let init = I.init

    let add = I.add

    let list_values = I.list_values

    let consume_deserialize_gas ctxt value =
      Raw_context.consume_gas ctxt (Script_repr.deserialized_cost value)

    let get ctxt contract =
      I.get ctxt contract >>=? fun (ctxt, value) ->
      Lwt.return
        (consume_deserialize_gas ctxt value >|? fun ctxt -> (ctxt, value))

    let find ctxt contract =
      I.find ctxt contract >>=? fun (ctxt, value_opt) ->
      Lwt.return
      @@
      match value_opt with
      | None -> ok (ctxt, None)
      | Some value ->
          consume_deserialize_gas ctxt value >|? fun ctxt -> (ctxt, value_opt)
  end
end

module Sapling = struct
  type id = Lazy_storage_kind.Sapling_state.Id.t

  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["sapling"]
      end)

  module Next = struct
    module Storage =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["next"]
        end)
        (Lazy_storage_kind.Sapling_state.Id)

    let incr ctxt =
      Storage.get ctxt >>=? fun i ->
      Storage.update ctxt (Lazy_storage_kind.Sapling_state.Id.next i)
      >|=? fun ctxt -> (ctxt, i)

    let init ctxt = Storage.init ctxt Lazy_storage_kind.Sapling_state.Id.init
  end

  module Index = Lazy_storage_kind.Sapling_state.Id

  let rpc_arg = Index.rpc_arg

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Index))

  let remove ctxt n = Indexed_context.remove ctxt n

  let copy ctxt ~from ~to_ = Indexed_context.copy ctxt ~from ~to_

  module Total_bytes =
    Indexed_context.Make_map
      (struct
        let name = ["total_bytes"]
      end)
      (Encoding.Z)

  module Commitments_size =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["commitments_size"]
      end)
      (Encoding.Int64)

  module Memo_size =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["memo_size"]
      end)
      (Sapling_repr.Memo_size)

  module Commitments :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Hash.t =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitments"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse node position"
             in
             RPC_arg.make
               ~descr:"The position of a node in a sapling commitment tree"
               ~name:"sapling_node_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_node_position"
               ~title:"Sapling node position"
               ~description:
                 "The position of a node in a sapling commitment tree"
               Data_encoding.int64

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (Sapling.Hash)

  let commitments_init ctx id =
    Indexed_context.Raw_context.remove (ctx, id) ["commitments"]
    >|= fun (ctx, _id) -> ctx

  module Ciphertexts :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Ciphertext.t =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["ciphertexts"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse ciphertext position"
             in
             RPC_arg.make
               ~descr:"The position of a sapling ciphertext"
               ~name:"sapling_ciphertext_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_ciphertext_position"
               ~title:"Sapling ciphertext position"
               ~description:"The position of a sapling ciphertext"
               Data_encoding.int64

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (Sapling.Ciphertext)

  let ciphertexts_init ctx id =
    Indexed_context.Raw_context.remove (ctx, id) ["commitments"]
    >|= fun (ctx, _id) -> ctx

  module Nullifiers_size =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["nullifiers_size"]
      end)
      (Encoding.Int64)

  (* For sequential access when building a diff *)
  module Nullifiers_ordered :
    Non_iterable_indexed_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Nullifier.t =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["nullifiers_ordered"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse nullifier position"
             in
             RPC_arg.make
               ~descr:"A sapling nullifier position"
               ~name:"sapling_nullifier_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_nullifier_position"
               ~title:"Sapling nullifier position"
               ~description:"Sapling nullifier position"
               Data_encoding.int64

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (Sapling.Nullifier)

  (* Check membership in O(1) for verify_update *)
  module Nullifiers_hashed =
    Make_carbonated_data_set_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["nullifiers_hashed"]
         end))
         (Make_index (struct
           type t = Sapling.Nullifier.t

           let encoding = Sapling.Nullifier.encoding

           let of_string hexstring =
             let b = Hex.to_bytes (`Hex hexstring) in
             Data_encoding.Binary.of_bytes_opt encoding b
             |> Result.of_option ~error:"Cannot parse sapling nullifier"

           let to_string nf =
             let b = Data_encoding.Binary.to_bytes_exn encoding nf in
             let (`Hex hexstring) = Hex.of_bytes b in
             hexstring

           let rpc_arg =
             RPC_arg.make
               ~descr:"A sapling nullifier"
               ~name:"sapling_nullifier"
               ~construct:to_string
               ~destruct:of_string
               ()

           let compare = Sapling.Nullifier.compare

           let path_length = 1

           let to_path c l = to_string c :: l

           let of_path = function
             | [c] -> Result.to_option (of_string c)
             | _ -> None
         end))

  let nullifiers_init ctx id =
    Nullifiers_size.add (ctx, id) Int64.zero >>= fun ctx ->
    Indexed_context.Raw_context.remove (ctx, id) ["nullifiers_ordered"]
    >>= fun (ctx, id) ->
    Indexed_context.Raw_context.remove (ctx, id) ["nullifiers_hashed"]
    >|= fun (ctx, _id) -> ctx

  module Roots :
    Non_iterable_indexed_data_storage
      with type t := Raw_context.t * id
       and type key = int32
       and type value = Sapling.Hash.t =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["roots"]
         end))
         (Make_index (struct
           type t = int32

           let rpc_arg =
             let construct = Int32.to_string in
             let destruct hash =
               Int32.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse nullifier position"
             in
             RPC_arg.make
               ~descr:"A sapling root"
               ~name:"sapling_root"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_root"
               ~title:"Sapling root"
               ~description:"Sapling root"
               Data_encoding.int32

           let compare = Compare.Int32.compare

           let path_length = 1

           let to_path c l = Int32.to_string c :: l

           let of_path = function [c] -> Int32.of_string_opt c | _ -> None
         end))
      (Sapling.Hash)

  module Roots_pos =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["roots_pos"]
      end)
      (Encoding.Int32)

  module Roots_level =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["roots_level"]
      end)
      (Raw_level_repr)
end

module Delegates =
  Make_data_set_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["delegates"]
       end))
       (Make_index (Signature.Public_key_hash))

module Active_delegates_with_rolls =
  Make_data_set_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["active_delegates_with_rolls"]
       end))
       (Make_index (Signature.Public_key_hash))

module Delegates_with_frozen_balance_index =
  Make_indexed_subcontext
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["delegates_with_frozen_balance"]
       end))
       (Make_index (Cycle_repr.Index))

module Delegates_with_frozen_balance =
  Make_data_set_storage
    (Delegates_with_frozen_balance_index.Raw_context)
    (Make_index (Signature.Public_key_hash))

(** Rolls *)

module Cycle = struct
  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["cycle"]
         end))
         (Make_index (Cycle_repr.Index))

  module Last_roll =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["last_roll"]
         end))
         (Int31_index)
      (Roll_repr)

  module Roll_snapshot =
    Indexed_context.Make_map
      (struct
        let name = ["roll_snapshot"]
      end)
      (Encoding.UInt16)

  type unrevealed_nonce = {
    nonce_hash : Nonce_hash.t;
    delegate : Signature.Public_key_hash.t;
    rewards : Tez_repr.t;
    fees : Tez_repr.t;
  }

  type nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  let nonce_status_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Unrevealed"
          (tup4
             Nonce_hash.encoding
             Signature.Public_key_hash.encoding
             Tez_repr.encoding
             Tez_repr.encoding)
          (function
            | Unrevealed {nonce_hash; delegate; rewards; fees} ->
                Some (nonce_hash, delegate, rewards, fees)
            | _ -> None)
          (fun (nonce_hash, delegate, rewards, fees) ->
            Unrevealed {nonce_hash; delegate; rewards; fees});
        case
          (Tag 1)
          ~title:"Revealed"
          Seed_repr.nonce_encoding
          (function Revealed nonce -> Some nonce | _ -> None)
          (fun nonce -> Revealed nonce);
      ]

  module Nonce =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["nonces"]
         end))
         (Make_index (Raw_level_repr.Index))
         (struct
           type t = nonce_status

           let encoding = nonce_status_encoding
         end)

  module Seed =
    Indexed_context.Make_map
      (struct
        let name = ["random_seed"]
      end)
      (struct
        type t = Seed_repr.seed

        let encoding = Seed_repr.seed_encoding
      end)
end

module Roll = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["rolls"]
      end)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Roll_repr.Index))

  module Next =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["next"]
      end)
      (Roll_repr)

  module Limbo =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["limbo"]
      end)
      (Roll_repr)

  module Delegate_roll_list =
    Wrap_indexed_data_storage
      (Contract.Roll_list)
      (struct
        type t = Signature.Public_key_hash.t

        let wrap = Contract_repr.implicit_contract

        let unwrap = Contract_repr.is_implicit
      end)

  module Successor =
    Indexed_context.Make_map
      (struct
        let name = ["successor"]
      end)
      (Roll_repr)

  module Delegate_change =
    Wrap_indexed_data_storage
      (Contract.Change)
      (struct
        type t = Signature.Public_key_hash.t

        let wrap = Contract_repr.implicit_contract

        let unwrap = Contract_repr.is_implicit
      end)

  module Snapshoted_owner_index : INDEX with type t = Cycle_repr.t * int =
  struct
    type t = Cycle_repr.t * int

    let path_length = Cycle_repr.Index.path_length + 1

    let to_path (c, n) s = Cycle_repr.Index.to_path c (string_of_int n :: s)

    let of_path l =
      match Misc.take Cycle_repr.Index.path_length l with
      | None | Some (_, ([] | _ :: _ :: _)) -> None
      | Some (l1, [l2]) -> (
          match (Cycle_repr.Index.of_path l1, int_of_string_opt l2) with
          | (None, _) | (_, None) -> None
          | (Some c, Some i) -> Some (c, i))

    type 'a ipath = ('a * Cycle_repr.t) * int

    let left_args =
      Storage_description.One
        {
          rpc_arg = Cycle_repr.rpc_arg;
          encoding = Cycle_repr.encoding;
          compare = Cycle_repr.compare;
        }

    let right_args =
      Storage_description.One
        {
          rpc_arg = RPC_arg.int;
          encoding = Data_encoding.int31;
          compare = Compare.Int.compare;
        }

    let args = Storage_description.(Pair (left_args, right_args))
  end

  module Owner =
    Make_indexed_data_snapshotable_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["owner"]
         end))
         (Snapshoted_owner_index)
      (Make_index (Roll_repr.Index))
      (Signature.Public_key)

  module Snapshot_for_cycle = Cycle.Roll_snapshot
  module Last_for_snapshot = Cycle.Last_roll

  let clear = Indexed_context.clear
end

(** Votes *)

module Vote = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["votes"]
      end)

  module Pred_period_kind =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["pred_period_kind"]
      end)
      (struct
        type t = Voting_period_repr.kind

        let encoding = Voting_period_repr.kind_encoding
      end)

  module Current_period =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["current_period"]
      end)
      (struct
        type t = Voting_period_repr.t

        let encoding = Voting_period_repr.encoding
      end)

  module Participation_ema =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["participation_ema"]
      end)
      (Encoding.Int32)

  module Current_proposal =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["current_proposal"]
      end)
      (Protocol_hash)

  module Listings_size =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["listings_size"]
      end)
      (Encoding.Int32)

  module Listings =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["listings"]
         end))
         (Make_index (Signature.Public_key_hash))
         (Encoding.Int32)

  module Proposals =
    Make_data_set_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["proposals"]
         end))
         (Pair
            (Make_index
               (Protocol_hash))
               (Make_index (Signature.Public_key_hash)))

  module Proposals_count =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["proposals_count"]
         end))
         (Make_index (Signature.Public_key_hash))
         (Encoding.UInt16)

  module Ballots =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["ballots"]
         end))
         (Make_index (Signature.Public_key_hash))
         (struct
           type t = Vote_repr.ballot

           let encoding = Vote_repr.ballot_encoding
         end)
end

module type FOR_CYCLE = sig
  val init :
    Raw_context.t ->
    Cycle_repr.t ->
    Seed_repr.seed ->
    Raw_context.t tzresult Lwt.t

  val get : Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

  val remove_existing :
    Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
end

(** Seed *)

module Seed = struct
  type unrevealed_nonce = Cycle.unrevealed_nonce = {
    nonce_hash : Nonce_hash.t;
    delegate : Signature.Public_key_hash.t;
    rewards : Tez_repr.t;
    fees : Tez_repr.t;
  }

  type nonce_status = Cycle.nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  module Nonce :
    Non_iterable_indexed_data_storage
      with type key := Level_repr.t
       and type value := nonce_status
       and type t := Raw_context.t = struct
    open Level_repr

    type context = Raw_context.t

    let mem ctxt (l : Level_repr.t) = Cycle.Nonce.mem (ctxt, l.cycle) l.level

    let get ctxt (l : Level_repr.t) = Cycle.Nonce.get (ctxt, l.cycle) l.level

    let find ctxt (l : Level_repr.t) = Cycle.Nonce.find (ctxt, l.cycle) l.level

    let update ctxt (l : Level_repr.t) v =
      Cycle.Nonce.update (ctxt, l.cycle) l.level v

    let init ctxt (l : Level_repr.t) v =
      Cycle.Nonce.init (ctxt, l.cycle) l.level v

    let add ctxt (l : Level_repr.t) v =
      Cycle.Nonce.add (ctxt, l.cycle) l.level v

    let add_or_remove ctxt (l : Level_repr.t) v =
      Cycle.Nonce.add_or_remove (ctxt, l.cycle) l.level v

    let remove_existing ctxt (l : Level_repr.t) =
      Cycle.Nonce.remove_existing (ctxt, l.cycle) l.level

    let remove ctxt (l : Level_repr.t) =
      Cycle.Nonce.remove (ctxt, l.cycle) l.level
  end

  module For_cycle : FOR_CYCLE = Cycle.Seed
end

(** Commitments *)

module Commitments =
  Make_indexed_data_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["commitments"]
       end))
       (Make_index (Blinded_public_key_hash.Index))
       (Tez_repr)

(** Ramp up security deposits... *)

module Ramp_up = struct
  module Rewards =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["ramp_up"; "rewards"]
         end))
         (Make_index (Cycle_repr.Index))
         (struct
           type t = Tez_repr.t list * Tez_repr.t list

           let encoding =
             Data_encoding.(
               obj2
                 (req "baking_reward_per_endorsement" (list Tez_repr.encoding))
                 (req "endorsement_reward" (list Tez_repr.encoding)))
         end)

  module Security_deposits =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["ramp_up"; "deposits"]
         end))
         (Make_index (Cycle_repr.Index))
         (struct
           type t = Tez_repr.t * Tez_repr.t

           let encoding = Data_encoding.tup2 Tez_repr.encoding Tez_repr.encoding
         end)
end

module Pending_migration = struct
  module Balance_updates =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["pending_migration_balance_updates"]
      end)
      (struct
        type t = Receipt_repr.balance_updates

        let encoding = Receipt_repr.balance_updates_encoding
      end)

  module Operation_results =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["pending_migration_operation_results"]
      end)
      (struct
        type t = Migration_repr.origination_result list

        let encoding = Migration_repr.origination_result_list_encoding
      end)

  let remove ctxt =
    let balance_updates ctxt =
      Balance_updates.find ctxt >>=? function
      | Some balance_updates ->
          Balance_updates.remove ctxt >>= fun ctxt ->
          (* When applying balance updates in a migration, we must attach receipts.
             The balance updates returned from here will be applied in the first
             block of the new protocol. *)
          return (ctxt, balance_updates)
      | None -> return (ctxt, [])
    in
    let operation_results ctxt =
      Operation_results.find ctxt >>=? function
      | Some operation_results ->
          Operation_results.remove ctxt >>= fun ctxt ->
          return (ctxt, operation_results)
      | None -> return (ctxt, [])
    in
    balance_updates ctxt >>=? fun (ctxt, balance_updates) ->
    operation_results ctxt >>=? fun (ctxt, operation_results) ->
    return (ctxt, balance_updates, operation_results)
end

module Liquidity_baking = struct
  module Escape_ema =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["liquidity_baking_escape_ema"]
      end)
      (Encoding.Int32)

  module Cpmm_address =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["liquidity_baking_cpmm_address"]
      end)
      (Contract_repr)
end
