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

(** Testing
    -------
    Component:  Protocol (script typed IR size)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test "^script typed ir size$"
    Subject:    Script_typed_ir computes good approximation of values' sizes
*)

open Protocol
open Alpha_context
open Script_ir_translator
open Script_typed_ir

(*
   Helpers
   -------
*)

exception Script_typed_ir_test_error of string

let err x = Exn (Script_typed_ir_test_error x)

let dummy_loc = Micheline.dummy_location

let get = Stdlib.Option.get

let is_ok m = match m with Ok x -> x | _ -> assert false

let footprint v =
  (* This is to turn every statically allocated data into
     heap-allocated data, to consider the worst-case in-memory
     representation of values. Note that it does NOT remove sharing.*)
  let v' =
    try Marshal.(from_bytes (to_bytes v [Closures]) 0)
    with _ -> (* Custom blocks are problematic. *) v
  in
  let size v = Obj.(reachable_words (repr v) * 8) in
  max (size v) (size v')

(** [gen_string s] returns a heap-allocated string. Notice that a
    string literal ["foo"] written in the code is statically allocated
    and is therefore not counted by [Obj.reachable_words]. *)
let gen_string s =
  let s = Bytes.of_string s |> Bytes.to_string in
  is_ok @@ Script_string_repr.of_string s

let boxed_set_elements s = Script_set.fold (fun x s -> x :: s) s []

let boxed_map_bindings s = Script_map.fold (fun k v s -> (k, v) :: s) s []

let big_map_bindings (Big_map s) = Big_map_overlay.bindings s.diff.map

let show_script_int fmt x = Z.pp_print fmt (Script_int.to_zint x)

let show_bool fmt b = Format.fprintf fmt "%B" b

let show_script_string fmt x =
  Format.fprintf fmt "%s" (Script_string.to_string x)

let show_address fmt Script_typed_ir.{destination; entrypoint} =
  Format.fprintf
    fmt
    "%a(%d):%a(%d)"
    Destination.pp
    destination
    (footprint destination)
    Entrypoint.pp
    entrypoint
    (footprint entrypoint)

let dont_show _fmt _ = ()

let size = {Tezos_benchmark.Base_samplers.min = 4; max = 32}

module Crypto_samplers =
Tezos_benchmark.Crypto_samplers.Make_finite_key_pool (struct
  let size = 10

  let algo = `Default
end)

include
  Michelson_samplers.Make
    (struct
      let parameters : Michelson_samplers.parameters =
        {
          base_parameters =
            {
              Michelson_samplers_base.int_size = size;
              string_size = size;
              bytes_size = size;
            };
          list_size = size;
          set_size = size;
          map_size = size;
        }
    end)
    (Crypto_samplers)

let random_state = Random.State.make [|37; 73; 17; 71; 42|]

let sample_ty size = Random_type.m_type ~size random_state

let sample_value ty = Random_value.value ty random_state

type ex = Ex : string * ('a, _) Script_typed_ir.ty * 'a * int -> ex [@@boxed]

let ex ?(error = 0) label ty v = Ex (label, ty, v, error)

let ex_random ?(error = 0) show ty ?(sample = fun () -> sample_value ty) label =
  let v = sample () in
  let label = Format.asprintf "@[%a%s@]@." show v label in
  ex ~error label ty v

let exs ?(error = 0) n show ty ?(sample = fun () -> sample_value ty) label =
  List.map (fun _ -> ex_random ~error show ty label ~sample) (1 -- n)

let nsample = 100

(** [check_value_size ()] covers a finite number of cases of Michelson
   values, checking that the cost model is sound with respect to their
   memory footprint.

   One could wonder why we do not simply use a single value generator
   based on a randomly chosen type. We actually implemented such a
   strategy in a previous version of this test but this results in a
   flaky test. Indeed, for some types, the values are overapproximated
   and it was difficult to correctly handle the accumulation of errors
   when types were randomly composed.

   The current strategy requires more code but, in exchange, it
   provides a finer control over the overapproximation. As a
   consequence, we can check for example that there is no
   overapproximation for values for which the model is exact.  We can
   also check that the overapproximation is at least well understood
   on the values for which size model is not exact. *)
let check_value_size () =
  let check (Ex (what, ty, v, error)) =
    let expected_size = footprint v in
    let (_, size) = Script_typed_ir_size.value_size ty v in
    let size = Saturation_repr.to_int size in
    fail_when
      (expected_size + error < size || size < expected_size)
      (err
         (Printf.sprintf
            "%s was expected to have size %d while the size model answered %d \
             (with +%d accepted over approximation error)"
            what
            expected_size
            size
            error))
  in
  List.iter_es
    check
    ((*
        Unit_t
        ======
     *)
     [ex "() : unit" Unit_t ()]
    (*
        Int_t
        =====
    *)
    @ (let error = 8 in
       [
         ex ~error "0 : int" Int_t Script_int.zero;
         ex ~error "2^63 : int" Int_t (Script_int.of_int max_int);
         ex
           ~error
           "37^73 : int"
           Int_t
           (Script_int.of_zint Z.(pow (of_int 37) 73));
         ex
           ~error
           "-37^73 : int"
           Int_t
           (Script_int.of_zint Z.(neg (pow (of_int 37) 73)));
         ex
           ~error
           "13270006022583112970 : int"
           Int_t
           (get @@ Script_int.of_string "13270006022583112970");
       ]
       @ exs ~error nsample show_script_int Int_t ": int")
    (*
        Nat_t
        =====
    *)
    @ (let error = 8 in
       [
         ex ~error "0 : nat" Nat_t Script_int.zero_n;
         ex
           ~error
           "2^63 : nat"
           Nat_t
           (get Script_int.(is_nat @@ of_int max_int));
         ex
           ~error
           "37^73 : int"
           Nat_t
           (get Script_int.(is_nat @@ of_zint Z.(pow (of_int 37) 73)));
       ]
       @ exs ~error nsample show_script_int Nat_t ": nat")
    (*
       Signature_t
       ===========
    *)
    @ (let show fmt (Script_typed_ir.Script_signature.Signature_tag s) =
         Signature.pp fmt s
       in
       exs ~error:8 nsample show Signature_t ": signature")
    (*
       String_t
       ========
    *)
    @ (let show fmt s =
         Format.fprintf fmt "%s" (Script_string_repr.to_string s)
       in
       exs nsample show String_t ": string")
    (*
       Bytes_t
       =======
    *)
    @ (let show fmt s = Format.fprintf fmt "%s" (Bytes.to_string s) in
       exs nsample show Bytes_t ": bytes")
    (*
       Mutez_t
       =======
    *)
    @ (let show fmt t = Format.fprintf fmt "%s" (Tez.to_string t) in
       exs nsample show Mutez_t ": mutez")
    (*
       Key_hash_t
       ==========
    *)
    @ (let show = Signature.Public_key_hash.pp in
       exs nsample show Key_hash_t ": key_hash")
    (*
       Key_t
       =====
    *)
    @ (let show = Signature.Public_key.pp in
       exs nsample show Key_t ": key_t")
    (*
       Timestamp_t
       ===========
    *)
    @ (let show fmt s =
         Format.fprintf fmt "%s" (Script_timestamp.to_string s)
       in
       exs ~error:8 nsample show Timestamp_t ": timestamp_t")
    (*
       Address_t
       =========
    *)
    @ exs nsample show_address Address_t ": address_t"
    (*
       Tx_rollup_l2_address_t
       ======================
    *)
    @ (let show = Indexable.pp Tx_rollup_l2_address.pp in
       exs nsample show Tx_rollup_l2_address_t ": tx_rollup_l2_t")
    (*
       Bool_t
       ======
    *)
    @ [ex "true : bool" Bool_t true; ex "false : bool" Bool_t false]
    (*
       Pair_t
       ======
    *)
    @ (let module P = struct
         type ('a, 'b) f = {apply : 'c. ('a * 'b, 'c) ty -> ex}
       end in
      let on_pair : type a b. (a, _) ty -> (b, _) ty -> (a, b) P.f -> ex =
       fun ty1 ty2 f ->
        let (Ty_ex_c ty) = is_ok @@ pair_t dummy_loc ty1 ty2 in
        f.apply ty
      in
      let open Script_int in
      [
        (* "int * int" *)
        on_pair
          int_t
          int_t
          {apply = (fun ty -> ex "(0, 0) : int * int" ty (of_int 0, of_int 0))};
        (* "string * string" *)
        on_pair
          string_t
          string_t
          {
            apply =
              (fun ty ->
                let foo = gen_string "foo" in
                let bar = gen_string "bar" in
                ex "(foo, bar) : string * string" ty (foo, bar));
          };
        (* "string * int" *)
        on_pair
          string_t
          int_t
          {
            apply =
              (fun ty ->
                let foo = gen_string "foo" in
                ex "(foo, 0) : string * int" ty (foo, of_int 0));
          };
        (* "int * int * int" *)
        on_pair
          int_t
          int_t
          {
            apply =
              (fun ty ->
                on_pair int_t ty
                @@ {
                     apply =
                       (fun ty ->
                         ex
                           "(0, (1, 2)) : int * int * int"
                           ty
                           (of_int 0, (of_int 1, of_int 2)));
                   });
          };
      ])
    (*
        Union_t
        =======
    *)
    @ (let module P = struct
         type ('a, 'b) f = {apply : 'c. (('a, 'b) union, 'c) ty -> ex}
       end in
      let on_union : type a b. (a, _) ty -> (b, _) ty -> (a, b) P.f -> ex =
       fun ty1 ty2 f ->
        let (Ty_ex_c ty) = is_ok @@ union_t dummy_loc ty1 ty2 in
        f.apply ty
      in
      let open Script_int in
      [
        (* "int + int" *)
        on_union
          int_t
          int_t
          {apply = (fun ty -> ex "L 0 : int + int" ty (L (of_int 0)))};
        on_union
          int_t
          int_t
          {apply = (fun ty -> ex "R 0 : int + int" ty (R (of_int 0)))};
        (* "string + string" *)
        on_union
          string_t
          string_t
          {
            apply =
              (fun ty ->
                let foo = gen_string "foo" in
                ex "L foo : string * string" ty (L foo));
          };
        on_union
          string_t
          string_t
          {
            apply =
              (fun ty ->
                let foo = gen_string "foo" in
                ex "R foo : string * string" ty (R foo));
          };
        (* "string + int" *)
        on_union
          string_t
          int_t
          {
            apply =
              (fun ty ->
                let foo = gen_string "foo" in
                ex "L foo : string * int" ty (L foo));
          };
        (* "int + int + int" *)
        on_union
          int_t
          int_t
          {
            apply =
              (fun ty ->
                on_union
                  int_t
                  ty
                  {
                    apply =
                      (fun ty -> ex "L 0 : int + int + int" ty (L (of_int 0)));
                  });
          };
        on_union
          int_t
          int_t
          {
            apply =
              (fun ty ->
                on_union
                  int_t
                  ty
                  {
                    apply =
                      (fun ty ->
                        ex "R (L 0) : int + int + int" ty (R (L (of_int 0))));
                  });
          };
        on_union
          int_t
          int_t
          {
            apply =
              (fun ty ->
                on_union
                  int_t
                  ty
                  {
                    apply =
                      (fun ty ->
                        ex "R (R 0) : int + int + int" ty (R (R (of_int 0))));
                  });
          };
      ])
    (*
        Option_t
        ========
    *)
    @ (let module P = struct
         type 'a f = {apply : 'c. ('a option, 'c) ty -> ex}
       end in
      let on_option : type a. (a, _) ty -> a P.f -> ex =
       fun ty f -> f.apply @@ is_ok @@ option_t dummy_loc ty
      in
      let open Script_int in
      [
        (* "option int" *)
        on_option int_t {apply = (fun ty -> ex "None : option int" ty None)};
        on_option
          int_t
          {apply = (fun ty -> ex "Some 0 : option int" ty (Some (of_int 0)))};
        (* "option string" *)
        on_option
          string_t
          {apply = (fun ty -> ex "None : option string" ty None)};
        on_option
          string_t
          {
            apply =
              (fun ty ->
                ex "Some \"foo\" : option string" ty (Some (gen_string "foo")));
          };
      ])
    (*
        List_t
        ======
    *)
    @ (let module P = struct
         type 'a f = {apply : 'c. ('a boxed_list, 'c) ty -> ex list}
       end in
      let on_list : type a. (a, _) ty -> a P.f -> ex list =
       fun ty f -> f.apply @@ is_ok @@ list_t dummy_loc ty
      in
      let check ty show_elt =
        on_list
          ty
          {
            apply =
              (fun ty ->
                let show fmt l = Format.pp_print_list show_elt fmt l.elements in
                exs nsample show ty ": list _");
          }
      in
      check string_t show_script_string)
    (*
        Set_t
        ======
    *)
    @ (let module P = struct
         type 'a f = {apply : 'c. ('a set, 'c) ty -> ex list}
       end in
      let on_set : type a. (a, _) ty -> a P.f -> ex list =
       fun ty f -> f.apply @@ is_ok @@ set_t dummy_loc ty
      in
      let check ty show_elt =
        on_set
          ty
          {
            apply =
              (fun ty ->
                let show fmt s =
                  Format.fprintf
                    fmt
                    "%a / %a"
                    show_script_int
                    (Script_set.size s)
                    (Format.pp_print_list show_elt)
                    (boxed_set_elements s)
                in
                exs nsample show ty ": set _");
          }
      in
      check string_t show_script_string)
    (*
        Map_t
        ======
    *)
    @ (let module P = struct
         type ('k, 'v) f = {apply : 'c. (('k, 'v) map, 'c) ty -> ex list}
       end in
      let on_map : type k v. (k, _) ty -> (v, _) ty -> (k, v) P.f -> ex list =
       fun kty vty f -> f.apply @@ is_ok @@ map_t dummy_loc kty vty
      in
      let check kty vty show_key show_value =
        on_map
          kty
          vty
          {
            apply =
              (fun ty ->
                let show_binding fmt (k, v) =
                  Format.fprintf fmt "(%a -> %a)" show_key k show_value v
                in
                let show fmt s =
                  Format.pp_print_list show_binding fmt (boxed_map_bindings s)
                in
                exs nsample show ty ": map _");
          }
      in
      check string_t string_t show_script_string show_script_string)
    (*
        Big_map_t
        ======
    *)
    @ (let module P = struct
         type ('k, 'v) f = {apply : 'c. (('k, 'v) big_map, 'c) ty -> ex list}
       end in
      let on_big_map : type k v. (k, _) ty -> (v, _) ty -> (k, v) P.f -> ex list
          =
       fun kty vty f -> f.apply @@ is_ok @@ big_map_t dummy_loc kty vty
      in
      let check kty vty show_key show_value =
        on_big_map
          kty
          vty
          {
            apply =
              (fun ty ->
                let show_binding fmt (_, (k, v)) =
                  match v with
                  | Some v ->
                      Format.fprintf fmt "(%a -> %a)" show_key k show_value v
                  | None -> Format.fprintf fmt "(%a?)" show_key k
                in
                let show fmt s =
                  Format.pp_print_list show_binding fmt (big_map_bindings s)
                in
                exs nsample show ty ": big_map _");
          }
      in
      check bool_t bool_t show_bool show_bool)
    (*
       Contract_t
       =========
    *)
    @ (let show fmt (Typed_contract {arg_ty = _; address}) =
         show_address fmt address
       in
       exs
         nsample
         show
         (is_ok @@ contract_t dummy_loc string_t)
         ": contract string")
    (*
       Chain_t
       =========
    *)
    @ exs nsample dont_show chain_id_t ": chain_id"
    (*
       Bls12_381_g1_t
       ==============
    *)
    @ exs nsample dont_show bls12_381_g1_t ": bls12_381_g1_t"
    (*
       Bls12_381_g2_t
       ==============
    *)
    @ exs nsample dont_show bls12_381_g2_t ": bls12_381_g2_t"
    (*
       Bls12_381_fr_t
       ==============
    *)
    @ exs nsample dont_show bls12_381_fr_t ": bls12_381_fr_t"
    (*
       Ticket_t
       ========
    *)
    @ exs
        ~error:8
        nsample
        dont_show
        (is_ok @@ ticket_t dummy_loc bool_t)
        ": ticket bool"
      (*
          Missing by lack of fully functional samplers:
          - Sapling_transaction_t ;
          - Sapling_transaction_deprecated_t ;
          - Sapling_state ;
          - Operation_t ;
          - Chest_key_t ;
          - Chest_t ;
          - Lambda_t.
    *)
    )

let check_ty_size () =
  let check () =
    match (sample_ty (Random.int 10 + 1) : ex_ty) with
    | Ex_ty ty ->
        let expected_size = footprint ty in
        let (_, size) = Script_typed_ir_size.Internal_for_tests.ty_size ty in
        let size = Saturation_repr.to_int size in
        let what = "some type" in
        fail_when
          (size <> expected_size)
          (err
             (Printf.sprintf
                "%s was expected to have size %d while the size model answered \
                 %d."
                what
                expected_size
                size))
  in
  List.iter_es (fun _ -> check ()) (1 -- nsample)

let tests =
  let open Tztest in
  [
    tztest "check value size" `Quick check_value_size;
    tztest "check ty size" `Quick check_ty_size;
  ]
