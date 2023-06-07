(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol

exception Term_contains_holes

module Mikhailsky_signature :
  Algebraic_signature.S with type t = Mikhailsky_prim.prim = struct
  type t = Mikhailsky_prim.prim

  let compare (x : t) (y : t) = Stdlib.compare x y

  let hash (x : t) = Hashtbl.hash x

  let pp = Mikhailsky_prim.pp
end

include
  Micheline_with_hash_consing.Make
    (Mikhailsky_signature)
    (struct
      let initial_size = None
    end)

module Path = Path.With_hash_consing (struct
  let initial_size = None
end)

(* Prints a Mikhailsky term. *)
let pp fmt node =
  let canonical = Micheline.strip_locations node in
  let printable =
    Micheline_printer.printable Mikhailsky_prim.string_of_prim canonical
  in
  Micheline_printer.print_expr fmt printable

let to_string node =
  pp Format.str_formatter node ;
  Format.flush_str_formatter ()

(* Adapted from Script_ir_translator.parse_ty *)
let rec parse_ty :
    allow_big_map:bool ->
    allow_operation:bool ->
    allow_contract:bool ->
    node ->
    Type.Base.t =
 fun ~allow_big_map ~allow_operation ~allow_contract node ->
  match node with
  | Prim (_loc, T_unit, [], _annot) -> Type.unit
  | Prim (_loc, T_int, [], _annot) -> Type.int
  | Prim (_loc, T_nat, [], _annot) -> Type.nat
  | Prim (_loc, T_string, [], _annot) -> Type.string
  | Prim (_loc, T_bytes, [], _annot) -> Type.bytes
  | Prim (_loc, T_bool, [], _annot) -> Type.bool
  | Prim (_loc, T_key_hash, [], _annot) -> Type.key_hash
  | Prim (_loc, T_timestamp, [], _annot) -> Type.timestamp
  | Prim (_loc, T_mutez, [], _annot) -> Type.mutez
  | Prim (_loc, T_option, [ut], _annot) ->
      let ty = parse_ty ~allow_big_map ~allow_operation ~allow_contract ut in
      Type.option ty
  | Prim (_loc, T_pair, [utl; utr], _annot) ->
      let lty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utl in
      let rty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utr in
      Type.pair lty rty
  | Prim (_loc, T_or, [utl; utr], _annot) ->
      let lty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utl in
      let rty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utr in
      Type.or_ lty rty
  | Prim (_loc, T_set, [ut], _annot) ->
      let ut = parse_ty ~allow_big_map ~allow_operation ~allow_contract ut in
      Type.set ut
  | Prim (_loc, T_map, [uta; utb], _annot) ->
      let uta = parse_ty ~allow_big_map ~allow_operation ~allow_contract uta in
      let utb = parse_ty ~allow_big_map ~allow_operation ~allow_contract utb in
      Type.map uta utb
  | Prim (_loc, T_lambda, [dom; range], _annot) ->
      let dom = parse_ty ~allow_big_map ~allow_operation ~allow_contract dom in
      let range =
        parse_ty ~allow_big_map ~allow_operation ~allow_contract range
      in
      Type.lambda dom range
  | Prim (_loc, T_list, [elt], _annot) ->
      let elt = parse_ty ~allow_big_map ~allow_operation ~allow_contract elt in
      Type.list elt
  | _ ->
      let s = to_string node in
      Stdlib.failwith ("Mikhailsky.parse_ty: could not parse " ^ s)

exception Term_has_variables

exception Ill_formed_mikhailsky

let rec map_var f (x : Type.Base.t) =
  match x.node with
  | Unit_t -> prim T_unit [] []
  | Var_t v -> f v
  | Int_t -> prim T_int [] []
  | Nat_t -> prim T_nat [] []
  | Bool_t -> prim T_bool [] []
  | String_t -> prim T_string [] []
  | Bytes_t -> prim T_bytes [] []
  | Key_hash_t -> prim T_key_hash [] []
  | Timestamp_t -> prim T_timestamp [] []
  | Mutez_t -> prim T_mutez [] []
  | Key_t -> prim T_key [] []
  | Option_t ty ->
      let mty = map_var f ty in
      prim T_option [mty] []
  | Pair_t (lty, rty) ->
      let lty = map_var f lty in
      let rty = map_var f rty in
      prim T_pair [lty; rty] []
  | Or_t (lty, rty) ->
      let lty = map_var f lty in
      let rty = map_var f rty in
      prim T_or [lty; rty] []
  | List_t ty ->
      let mty = map_var f ty in
      prim T_list [mty] []
  | Set_t ty ->
      let mty = map_var f ty in
      prim T_set [mty] []
  | Map_t (kty, vty) ->
      let mkty = map_var f kty in
      let mvty = map_var f vty in
      prim T_map [mkty; mvty] []
  | Lambda_t (dom, range) ->
      let dom = map_var f dom in
      let range = map_var f range in
      prim T_lambda [dom; range] []

let unparse_ty_exn (x : Type.Base.t) =
  map_var (fun _ -> raise Term_has_variables) x

let unparse_ty (x : Type.Base.t) =
  try Some (unparse_ty_exn x) with Term_has_variables -> None

(* Exports a Mikhailsky term to Michelson. Fails if term contains holes.
   Erases annotations, introduces types where missing. *)
let rec to_michelson (n : node) =
  match n with
  | Micheline.Int (_, i) -> Micheline.Int (0, i)
  | Micheline.Prim (_, head, [term], _)
    when Mikhailsky_prim.kind head = Annot_kind && head <> A_Lambda ->
      to_michelson term
  | Micheline.Prim (_, I_Hole, _, _) -> raise Term_contains_holes
  | Micheline.Prim (_, D_Hole, _, _) -> raise Term_contains_holes
  | Micheline.Prim (_, head, subterms, annots) ->
      let head = Mikhailsky_prim.to_michelson head in
      Micheline.Prim (0, head, List.map to_michelson subterms, annots)
  | Micheline.String (_, s) -> Micheline.String (0, s)
  | Micheline.Bytes (_, b) -> Micheline.Bytes (0, b)
  | Micheline.Seq (_, subterms) ->
      Micheline.Seq (0, List.map to_michelson subterms)

let to_michelson (n : node) : Script_repr.expr =
  Micheline.strip_locations (to_michelson n)

let rec size : node -> int =
 fun node ->
  match node with
  | Micheline.Int (_, _) -> 1
  | Micheline.String (_, _) -> 1
  | Micheline.Bytes (_, _) -> 1
  | Micheline.Prim (_, _, subterms, _) ->
      List.fold_left (fun acc n -> acc + size n) 1 subterms
  | Micheline.Seq (_, subterms) ->
      List.fold_left (fun acc n -> acc + size n) 1 subterms

let instr_hole = prim I_Hole [] []

let data_hole = prim D_Hole [] []

(* types *)
let unit_ty = prim T_unit [] []

let bool_ty = prim T_bool [] []

let int_ty = prim T_int [] []

let nat_ty = prim T_nat [] []

let string_ty = prim T_string [] []

let bytes_ty = prim T_bytes [] []

let key_hash_ty = prim T_key_hash [] []

let option_ty x = prim T_option [x] []

let list_ty x = prim T_list [x] []

(* Unique identifier provided by hash-consing Micheline terms. *)
let tag node =
  let l = label node in
  l.tag

(* hash of term *)
let hash node =
  let l = label node in
  l.hash

module Instructions = struct
  (* arithmetic *)

  let add ty1 ty2 = prim I_ADD [ty1; ty2] []

  let sub ty1 ty2 = prim I_SUB [ty1; ty2] []

  let mul ty1 ty2 = prim I_MUL [ty1; ty2] []

  let ediv ty1 ty2 = prim I_EDIV [ty1; ty2] []

  let abs = prim I_ABS [] []

  let gt = prim I_GT [] []

  (* stack ops *)
  let push ty v = prim I_PUSH [ty; v] []

  let dip code = prim I_DIP [seq [code]] []

  let dup = prim I_DUP [] []

  let drop = prim I_DROP [] []

  let dropn n = prim I_DROP [int (Z.of_int n)] []

  let swap = prim I_SWAP [] []

  (* crypto *)
  let blake2b = prim I_BLAKE2B [] []

  let sha256 = prim I_SHA256 [] []

  let sha512 = prim I_SHA512 [] []

  let hash_key = prim I_HASH_KEY [] []

  (* control *)
  let if_ bt bf = prim I_IF [seq [bt]; seq [bf]] []

  let if_left bt bf = prim I_IF_LEFT [seq [bt]; seq [bf]] []

  let if_none bt bf = prim I_IF_NONE [seq [bt]; seq [bf]] []

  let loop b = prim I_LOOP [seq [b]] []

  let loop_left b = prim I_LOOP_LEFT [seq [b]] []

  (* pairs *)
  let car = prim I_CAR [] []

  let cdr = prim I_CDR [] []

  let pair = prim I_PAIR [] []

  (* ors *)

  let left = prim I_LEFT [] []

  let right = prim I_RIGHT [] []

  (* boolean *)
  let and_ = prim I_AND [] []

  (* compare *)
  let compare = prim I_COMPARE [] []

  (* map/set *)
  let empty_set = prim I_EMPTY_SET [] []

  let update_set = prim I_UPDATE_SET [] []

  let size_set = prim I_SIZE_SET [] []

  let iter_set code = prim I_ITER_SET [seq code] []

  let mem_set = prim I_MEM_SET [] []

  let empty_map = prim I_EMPTY_MAP [] []

  let update_map = prim I_UPDATE_MAP [] []

  let size_map = prim I_SIZE_MAP [] []

  let iter_map code = prim I_ITER_MAP [seq code] []

  let map_map code = prim I_MAP_MAP [seq code] []

  let get_map = prim I_GET_MAP [] []

  let mem_map = prim I_MEM_MAP [] []

  (* lists*)
  let nil = prim I_NIL [] []

  let cons = prim I_CONS [] []

  let size_list = prim I_SIZE_LIST [] []

  let iter_list code = prim I_ITER_LIST [seq code] []

  let map_list code = prim I_MAP_LIST [seq code] []

  (* strings *)
  let concat = prim I_CONCAT [] []

  let size_string = prim I_SIZE_STRING [] []

  let size_bytes = prim I_SIZE_BYTES [] []

  (* Lambdas *)
  let lambda code = prim I_LAMBDA [seq code] []

  let exec = prim I_EXEC [] []

  let apply = prim I_APPLY [] []

  (* pack/unpack *)
  let pack = prim I_PACK [] []

  let unpack = prim I_UNPACK [] []

  (* hole *)
  let hole = instr_hole
end

(* value constructors *)
module Data = struct
  let unit = prim D_Unit [] []

  let false_ = prim D_False [] []

  let true_ = prim D_True [] []

  let none = prim D_None [] []

  let some x = prim D_Some [x] []

  let pair x y = prim D_Pair [x; y] []

  let left x = prim D_Left [x] []

  let right x = prim D_Right [x] []

  let list elts = prim A_List [seq elts] []

  let set elts = prim A_Set [seq elts] []

  let map_elt k v = prim D_Elt [k; v] []

  let map elts = prim A_Map [seq elts] []

  let timestamp ts =
    let z = Protocol.Script_timestamp.to_zint ts in
    prim A_Timestamp [int z] []

  let mutez (tz : Protocol.Alpha_context.Tez.tez) =
    let i = Protocol.Alpha_context.Tez.to_mutez tz in
    prim A_Mutez [int (Z.of_int64 i)] []

  let key_hash kh =
    let b =
      Data_encoding.Binary.to_bytes_exn
        Environment.Signature.Public_key_hash.encoding
        kh
    in
    prim A_Key_hash [bytes b] []

  let key k =
    let b =
      Data_encoding.Binary.to_bytes_exn
        Environment.Signature.Public_key.encoding
        k
    in
    prim A_Key [bytes b] []

  let integer (i : int) = prim A_Int [int (Z.of_int i)] []

  let natural (i : int) =
    assert (i >= 0) ;
    prim A_Nat [int (Z.of_int i)] []

  let big_integer (i : Z.t) = prim A_Int [int i] []

  let big_natural (i : Z.t) =
    assert (Z.geq i Z.zero) ;
    prim A_Nat [int i] []

  let string = string

  let bytes = bytes

  let lambda code = prim A_Lambda [seq code] []

  let hole = data_hole
end
