open StaTz
open Tezos_benchmark
open Michelson_samplers
open Protocol

module Comparable_type_name = struct
  type t = type_name

  let compare (x : t) (y : t) = Stdlib.compare x y
end

let pp_type_name fmtr (t : type_name) =
  Format.pp_print_string fmtr
  @@
  match t with
  | `TString -> "string"
  | `TNat -> "nat"
  | `TPair -> "pair"
  | `TKey -> "key"
  | `TLambda -> "lambda"
  | `TUnion -> "union"
  | `TOperation -> "operation"
  | `TOption -> "option"
  | `TSapling_state -> "sapling_state"
  | `TBytes -> "bytes"
  | `TChain_id -> "chain_id"
  | `TBool -> "bool"
  | `TBls12_381_g2 -> "bls12_381_g2"
  | `TTicket -> "ticket"
  | `TMap -> "map"
  | `TAddress -> "address"
  | `TContract -> "contract"
  | `TBls12_381_fr -> "bls12_381_fr"
  | `TSapling_transaction -> "sapling_transaction"
  | `TTimestamp -> "timestamp"
  | `TKey_hash -> "key_hash"
  | `TBig_map -> "big_map"
  | `TSet -> "set"
  | `TBls12_381_g1 -> "bls12_381_g1"
  | `TList -> "list"
  | `TMutez -> "mutez"
  | `TSignature -> "signature"
  | `TUnit -> "unit"
  | `TInt -> "int"

let rec tnames_of_type :
    type a. a Script_typed_ir.ty -> type_name list -> type_name list =
 fun t acc ->
  match t with
  | Script_typed_ir.Unit_t _ -> `TUnit :: acc
  | Script_typed_ir.Int_t _ -> `TInt :: acc
  | Script_typed_ir.Nat_t _ -> `TNat :: acc
  | Script_typed_ir.Signature_t _ -> `TSignature :: acc
  | Script_typed_ir.String_t _ -> `TString :: acc
  | Script_typed_ir.Bytes_t _ -> `TBytes :: acc
  | Script_typed_ir.Mutez_t _ -> `TMutez :: acc
  | Script_typed_ir.Key_hash_t _ -> `TKey_hash :: acc
  | Script_typed_ir.Key_t _ -> `TKey :: acc
  | Script_typed_ir.Timestamp_t _ -> `TTimestamp :: acc
  | Script_typed_ir.Address_t _ -> `TAddress :: acc
  | Script_typed_ir.Bool_t _ -> `TBool :: acc
  | Script_typed_ir.Pair_t ((lty, _, _), (rty, _, _), _) ->
      tnames_of_type lty (tnames_of_type rty (`TPair :: acc))
  | Script_typed_ir.Union_t ((lty, _), (rty, _), _) ->
      tnames_of_type lty (tnames_of_type rty (`TUnion :: acc))
  | Script_typed_ir.Lambda_t (dom, range, _) ->
      tnames_of_type dom (tnames_of_type range (`TLambda :: acc))
  | Script_typed_ir.Option_t (ty, _) -> tnames_of_type ty (`TOption :: acc)
  | Script_typed_ir.List_t (ty, _) -> tnames_of_type ty (`TList :: acc)
  | Script_typed_ir.Set_t (ty, _) -> tnames_of_comparable_type ty (`TSet :: acc)
  | Script_typed_ir.Map_t (kty, vty, _) ->
      tnames_of_comparable_type kty (tnames_of_type vty (`TMap :: acc))
  | Script_typed_ir.Big_map_t (kty, vty, _) ->
      tnames_of_comparable_type kty (tnames_of_type vty (`TBig_map :: acc))
  | Script_typed_ir.Contract_t (ty, _) -> tnames_of_type ty (`TContract :: acc)
  | Script_typed_ir.Sapling_transaction_t (_, _) -> `TSapling_transaction :: acc
  | Script_typed_ir.Sapling_state_t (_, _) -> `TSapling_state :: acc
  | Script_typed_ir.Operation_t _ -> `TOperation :: acc
  | Script_typed_ir.Chain_id_t _ -> `TChain_id :: acc
  | Script_typed_ir.Never_t _ -> assert false
  | Script_typed_ir.Bls12_381_g1_t _ -> `TBls12_381_g1 :: acc
  | Script_typed_ir.Bls12_381_g2_t _ -> `TBls12_381_g2 :: acc
  | Script_typed_ir.Bls12_381_fr_t _ -> `TBls12_381_fr :: acc
  | Script_typed_ir.Ticket_t (ty, _) ->
      tnames_of_comparable_type ty (`TTicket :: acc)
  | Script_typed_ir.Chest_key_t _ -> assert false
  | Script_typed_ir.Chest_t _ -> assert false

and tnames_of_comparable_type :
    type a. a Script_typed_ir.comparable_ty -> type_name list -> type_name list
    =
 fun t acc ->
  match t with
  | Script_typed_ir.Unit_key _ -> `TUnit :: acc
  | Script_typed_ir.Never_key _ -> assert false
  | Script_typed_ir.Int_key _ -> `TInt :: acc
  | Script_typed_ir.Nat_key _ -> `TNat :: acc
  | Script_typed_ir.Signature_key _ -> `TSignature :: acc
  | Script_typed_ir.String_key _ -> `TString :: acc
  | Script_typed_ir.Bytes_key _ -> `TBytes :: acc
  | Script_typed_ir.Mutez_key _ -> `TMutez :: acc
  | Script_typed_ir.Bool_key _ -> `TBool :: acc
  | Script_typed_ir.Key_hash_key _ -> `TKey_hash :: acc
  | Script_typed_ir.Key_key _ -> `TKey :: acc
  | Script_typed_ir.Timestamp_key _ -> `TTimestamp :: acc
  | Script_typed_ir.Chain_id_key _ -> `TChain_id :: acc
  | Script_typed_ir.Address_key _ -> `TAddress :: acc
  | Script_typed_ir.Pair_key ((lty, _), (rty, _), _) ->
      tnames_of_comparable_type
        lty
        (tnames_of_comparable_type rty (`TPair :: acc))
  | Script_typed_ir.Union_key ((lty, _), (rty, _), _) ->
      tnames_of_comparable_type
        lty
        (tnames_of_comparable_type rty (`TUnion :: acc))
  | Script_typed_ir.Option_key (ty, _) ->
      tnames_of_comparable_type ty (`TOption :: acc)

module Config = struct
  open Michelson_samplers_parameters

  let parameters =
    {
      int_size = {min = 8; max = 32};
      string_size = {min = 8; max = 128};
      bytes_size = {min = 8; max = 128};
      stack_size = {min = 3; max = 8};
      type_size = {min = 1; max = 15};
      list_size = {min = 0; max = 1000};
      set_size = {min = 0; max = 1000};
      map_size = {min = 0; max = 1000};
    }

  let size = 16

  let algo = `Default
end

module Sampler = Michelson_samplers.Make (Config)

let pp_stats = Stats.pp_fin_fun pp_type_name

let tnames_dist : type_name list -> type_name Stats.fin_prb =
 fun tnames ->
  Stats.empirical_of_raw_data (Array.of_list tnames)
  |> Stats.fin_prb_of_empirical (module Comparable_type_name)

let rec sample nsamples acc =
  let open Sampling_helpers.M in
  if nsamples = 0 then return acc
  else
    let* size =
      Base_samplers.(sample_in_interval ~range:{min = 1; max = 1000})
    in
    let* (Ex_ty ty) = Sampler.Random_type.m_type ~size in
    let* acc = sample (nsamples - 1) acc in
    return (tnames_of_type ty acc)

let sample nsamples = sample nsamples []

let dist nsamples =
  let open Sampling_helpers.M in
  let* samples = sample nsamples in
  return (tnames_dist samples)

let () =
  Format.printf
    "stats:@.%a@."
    pp_stats
    (Obj.magic (dist 500 (Random.State.make [|0x1337; 0x533D|])))
