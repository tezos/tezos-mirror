open Michelson_samplers
open Protocol
open Internal_for_tests

let pp_type_name fmtr (t : type_name) =
  Format.pp_print_string fmtr
  @@
  match t with
  | `TString -> "string"
  | `TNat -> "nat"
  | `TPair -> "pair"
  | `TKey -> "key"
  | `TLambda -> "lambda"
  | `TOr -> "or"
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
  | `TSapling_transaction_deprecated -> "sapling_transaction_deprecated"
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

module Type_name = struct
  type t = type_name

  let equal (x : t) (y : t) = x = y

  let pp = pp_type_name

  let hash = Stdlib.Hashtbl.hash
end

module Type_name_hashtbl = Stdlib.Hashtbl.Make (Type_name)

let rec tnames_of_type :
    type a ac. (a, ac) Script_typed_ir.ty -> type_name list -> type_name list =
 fun t acc ->
  match t with
  | Script_typed_ir.Unit_t -> `TUnit :: acc
  | Script_typed_ir.Int_t -> `TInt :: acc
  | Script_typed_ir.Nat_t -> `TNat :: acc
  | Script_typed_ir.Signature_t -> `TSignature :: acc
  | Script_typed_ir.String_t -> `TString :: acc
  | Script_typed_ir.Bytes_t -> `TBytes :: acc
  | Script_typed_ir.Mutez_t -> `TMutez :: acc
  | Script_typed_ir.Key_hash_t -> `TKey_hash :: acc
  | Script_typed_ir.Key_t -> `TKey :: acc
  | Script_typed_ir.Timestamp_t -> `TTimestamp :: acc
  | Script_typed_ir.Address_t -> `TAddress :: acc
  | Script_typed_ir.Bool_t -> `TBool :: acc
  | Script_typed_ir.Pair_t (lty, rty, _, _) ->
      tnames_of_type lty (tnames_of_type rty (`TPair :: acc))
  | Script_typed_ir.Or_t (lty, rty, _, _) ->
      tnames_of_type lty (tnames_of_type rty (`TOr :: acc))
  | Script_typed_ir.Lambda_t (dom, range, _) ->
      tnames_of_type dom (tnames_of_type range (`TLambda :: acc))
  | Script_typed_ir.Option_t (ty, _, _) -> tnames_of_type ty (`TOption :: acc)
  | Script_typed_ir.List_t (ty, _) -> tnames_of_type ty (`TList :: acc)
  | Script_typed_ir.Set_t (ty, _) -> tnames_of_type ty (`TSet :: acc)
  | Script_typed_ir.Map_t (kty, vty, _) ->
      tnames_of_type kty (tnames_of_type vty (`TMap :: acc))
  | Script_typed_ir.Big_map_t (kty, vty, _) ->
      tnames_of_type kty (tnames_of_type vty (`TBig_map :: acc))
  | Script_typed_ir.Contract_t (ty, _) -> tnames_of_type ty (`TContract :: acc)
  | Script_typed_ir.Sapling_transaction_t _ -> `TSapling_transaction :: acc
  | Script_typed_ir.Sapling_transaction_deprecated_t _ ->
      `TSapling_transaction_deprecated :: acc
  | Script_typed_ir.Sapling_state_t _ -> `TSapling_state :: acc
  | Script_typed_ir.Operation_t -> `TOperation :: acc
  | Script_typed_ir.Chain_id_t -> `TChain_id :: acc
  | Script_typed_ir.Never_t -> assert false
  | Script_typed_ir.Bls12_381_g1_t -> `TBls12_381_g1 :: acc
  | Script_typed_ir.Bls12_381_g2_t -> `TBls12_381_g2 :: acc
  | Script_typed_ir.Bls12_381_fr_t -> `TBls12_381_fr :: acc
  | Script_typed_ir.Ticket_t (ty, _) -> tnames_of_type ty (`TTicket :: acc)
  | Script_typed_ir.Chest_key_t -> assert false
  | Script_typed_ir.Chest_t -> assert false

module Crypto_samplers = Crypto_samplers.Make_finite_key_pool (struct
  let algo = `Default

  let size = 16
end)

module Sampler =
  Michelson_samplers.Make
    (struct
      let parameters =
        {
          base_parameters =
            {
              Michelson_samplers_base.int_size = {min = 8; max = 32};
              string_size = {min = 8; max = 128};
              bytes_size = {min = 8; max = 128};
            };
          list_size = {min = 10; max = 1000};
          set_size = {min = 10; max = 1000};
          map_size = {min = 10; max = 1000};
        }
    end)
    (Crypto_samplers)

open Stats

let tnames_dist : type_name list -> type_name Fin.Float.prb =
 fun tnames ->
  Emp.of_raw_data (Array.of_list tnames)
  |> Fin.Float.counts_of_empirical (module Type_name_hashtbl)
  |> Fin.Float.normalize

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
    (Fin.Float.pp_fin_mes Type_name.pp)
    (Fin.Float.as_measure (dist 500 (Random.State.make [|0x1337; 0x533D|])))
