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
                src/proto_012_Psithaca/lib_protocol/test/integration/michelson/main.exe \
                -- test "^script typed ir size$"
    Subject:    Script_typed_ir computes good approximation of values' sizes
*)

open Protocol

(*
   Helpers
   -------
*)

exception Script_typed_ir_test_error of string

let err x = Exn (Script_typed_ir_test_error x)

let wrap e = Lwt.return (Environment.wrap_tzresult e)

let iter_n_es n f =
  let rec aux k = if k = n then return () else f k >>=? fun () -> aux (k + 1) in
  aux 0

(*

   We use Snoop's samplers to test that our good approximation of
   Michelson values' sizes is indeed a good approximation and that it
   is never too far from the reality.

   Architecture:
   -------------

   - module [Samplers] provides samplers for values of type [ty],
     [comparable_ty], [code] and any value whose types is dynamically
     representable with a [ty].

   - module [Tests] defines the actual testing procedures and reports
     error using [Printers].

*)

module Samplers = struct
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

  let sample_ty () = Random_type.m_type ~size:10 random_state

  let sample_cty () = Random_type.m_comparable_type ~size:10 random_state

  let sample_value ty = Random_value.value ty random_state

  module Gen =
    Michelson_mcmc_samplers.Make_code_sampler (Michelson_base) (Crypto_samplers)
      (struct
        let rng_state = random_state

        let target_size = 500

        let verbosity = `Silent
      end)

  (* Delay and cache the generator as it's expensive to create. *)
  let generator = lazy (Gen.generator ~burn_in:(500 * 7) random_state)

  type exdescr =
    | Ex_descr : ('a, 's, 'r, 'f) Script_ir_translator.descr -> exdescr

  let sample_ir_code () =
    let Michelson_mcmc_samplers.{term = sample; bef = stack; aft = _} =
      (Lazy.force generator) random_state
    in
    let accounts = Account.generate_accounts 1 in
    Block.alpha_context accounts >>=? fun ctxt ->
    let code = Micheline.root sample in
    let (Ex_stack_ty bef) =
      Type_helpers.michelson_type_list_to_ex_stack_ty stack ctxt
    in
    Script_ir_translator.(parse_instr Lambda ctxt ~legacy:true code bef)
    >>= wrap
    >>=? fun (ir_code, _) ->
    match ir_code with
    | Script_ir_translator.Typed ir_code -> return (sample, Ex_descr ir_code)
    | _ -> assert false
end

module Printers = struct
  let string_of_something f =
    Lwt_main.run
      (let accounts = Account.generate_accounts 1 in
       Block.alpha_context accounts >>=? fun ctxt ->
       f ctxt >>= wrap >>=? fun node ->
       let printable =
         Micheline_printer.printable
           Protocol.Michelson_v1_primitives.string_of_prim
           node
       in
       let b = Buffer.create 13 in
       let fmt = Format.formatter_of_buffer b in
       Format.fprintf fmt "@[%a@]@." Micheline_printer.print_expr printable ;
       return @@ Buffer.contents b)
    |> function
    | Ok s -> s
    | Error (errs : tztrace) ->
        Format.eprintf "@[Error: %a@]" pp_print_trace errs ;
        exit 1

  let string_of_value : type a. a Script_typed_ir.ty -> a -> string =
   fun ty v ->
    string_of_something @@ fun ctxt ->
    Script_ir_translator.(
      unparse_data ctxt Readable ty v >>=? fun (node, _) ->
      return @@ Micheline.strip_locations node)

  let string_of_ty ty =
    string_of_something @@ fun ctxt ->
    Lwt.return
    @@ Script_ir_translator.(
         unparse_ty ~loc:() ctxt ty >>? fun (node, _) ->
         Ok (Micheline.strip_locations node))

  let string_of_code code = string_of_something @@ fun _ -> return code

  let string_of_comparable_ty cty =
    string_of_ty (Script_ir_translator.ty_of_comparable_ty cty)
end

module Tests = struct
  let footprint v = 8 * Obj.(reachable_words (repr v))

  let stats = Stdlib.Hashtbl.create 13

  let remember what vsize rsize = Stdlib.Hashtbl.add stats what (vsize, rsize)

  let check_good_approximation kind ratio what vsize x =
    let rsize = footprint x in
    let vsize = Saturation_repr.to_int vsize in
    remember kind vsize rsize ;
    fail_unless
      (rsize = 0 || (vsize >= rsize / ratio && vsize <= rsize * ratio))
      (err
         (Printf.sprintf
            "Computed size for %s is not a good approximation (%d ~/~ %d)"
            what
            vsize
            rsize))

  let check_in_range what w (v, error) =
    let lower_bound = v -. error and upper_bound = v +. error in
    fail_unless
      (lower_bound <= w && w <= upper_bound)
      (err
         (Printf.sprintf
            "For %s: %f not in [%f; %f]"
            what
            w
            lower_bound
            upper_bound))

  let check_stats what ~expected_mean ~expected_stddev ~expected_ratios =
    let entries = Stdlib.Hashtbl.find_all stats what in
    let nb_entries = List.length entries in
    if nb_entries = 0 then
      (* TODO break dependency on other test's side effects:
         this value is 0 if the generator did not load the values *)
      return_unit
    else
      let nentries = float_of_int @@ nb_entries in
      let ratios =
        List.map
          (fun (vsize, rsize) ->
            (1. +. float_of_int vsize) /. (1. +. float_of_int rsize))
          entries
      in
      let sum = List.fold_left (fun accu r -> accu +. r) 0. ratios in
      let mean = sum /. nentries in
      Format.printf "mean: %f@." mean ;
      let sqr x = x *. x in
      let stddev =
        sqrt
          (List.fold_left (fun accu r -> accu +. sqr (r -. mean)) 0. ratios
          /. nentries)
      in
      let entries_min = List.fold_left min max_float ratios in
      let entries_max = List.fold_left max min_float ratios in
      check_in_range (what ^ ":mean") mean expected_mean >>=? fun () ->
      check_in_range (what ^ ":stddev") stddev expected_stddev >>=? fun () ->
      check_in_range (what ^ ":min") entries_min expected_ratios >>=? fun () ->
      check_in_range (what ^ ":max") entries_max expected_ratios

  let ty_size nsamples =
    iter_n_es nsamples @@ fun i ->
    match Samplers.sample_ty () with
    | Ex_ty ty ->
        check_good_approximation
          "ty_size"
          2
          (Printf.sprintf "type #%d `%s'" i (Printers.string_of_ty ty))
          (snd (Script_typed_ir_size.ty_size ty))
          ty
    | exception _ -> return ()

  let check_ty_size_stats () =
    check_stats
      "ty_size"
      ~expected_mean:(1., 0.01)
      ~expected_stddev:(0., 0.01)
      ~expected_ratios:(1., 0.05)

  let comparable_ty_size nsamples =
    iter_n_es nsamples @@ fun i ->
    match Samplers.sample_cty () with
    | Ex_comparable_ty cty ->
        check_good_approximation
          "comparable_ty_size"
          2
          (Printf.sprintf
             "comparable type #%d `%s'"
             i
             (Printers.string_of_comparable_ty cty))
          (snd (Script_typed_ir_size.comparable_ty_size cty))
          cty
    | exception _ -> return ()

  let check_comparable_ty_size_stats () =
    check_stats
      "comparable_ty_size"
      ~expected_mean:(1., 0.01)
      ~expected_stddev:(0., 0.01)
      ~expected_ratios:(1., 0.05)

  let contains_exceptions ty =
    let apply : type a. bool -> a Script_typed_ir.ty -> bool =
     fun accu -> function
      (* Boxed sets and maps point to a shared first class module.
         This is an overapproximation that we want to ignore in
         the tests. *)
      | Set_t _ | Map_t _
      (* We also want to avoid interferences between testing
         [lambda_size] and [value_size]. *)
      | Lambda_t _ | Contract_t _ ->
          true
      | _ -> accu
    in
    Script_typed_ir.ty_traverse
      ty
      false
      {apply; apply_comparable = (fun accu _ -> accu)}

  let value_size nsamples =
    iter_n_es nsamples @@ fun i ->
    match Samplers.sample_ty () with
    | Ex_ty ty when not (contains_exceptions ty) -> (
        match Samplers.sample_value ty with
        | v ->
            check_good_approximation
              "value_size"
              (* Used to be 3 but leads to flaky tests. Revert when
                 determinism is restored and the protocol is more precise
                 about value sizes.
                 FIXME: https://gitlab.com/tezos/tezos/-/issues/1784
                 FIXME: https://gitlab.com/tezos/tezos/-/issues/1834 *)
              10
              (Printf.sprintf
                 "value #%d `%s' of type `%s'"
                 i
                 (Printers.string_of_value ty v)
                 (Printers.string_of_ty ty))
              (snd (Script_typed_ir_size.value_size ty v))
              v
        | exception _ -> return ())
    | _ | (exception _) -> return ()

  let check_value_size_stats () =
    (* Stddev set to 0.5, used to be 0.2 but leads to flaky tests.
       Revert when determinism is restored and the protocol is more
       precise about value sizes.

       FIXME: https://gitlab.com/tezos/tezos/-/issues/1784
       FIXME: https://gitlab.com/tezos/tezos/-/issues/1834

       Expected_ratios set to 9, used to be 3.
       Revert when the following issue is implemented:
       FIXME: https://gitlab.com/dannywillems/ocaml-bls12-381/-/issues/55
    *)
    check_stats
      "value_size"
      ~expected_mean:(1., 0.2)
      ~expected_stddev:(0., 0.7)
      ~expected_ratios:(1., 9.)

  let lambda_size nsamples =
    iter_n_es nsamples @@ fun i ->
    Samplers.sample_ir_code () >>=? fun (code, Samplers.Ex_descr icode) ->
    let kinstr = (Script_ir_translator.close_descr icode).kinstr in
    check_good_approximation
      "lambda_size"
      3
      (Printf.sprintf "code #%d `%s'" i (Printers.string_of_code code))
      (snd (Script_typed_ir_size.kinstr_size kinstr))
      kinstr

  let check_lambda_size_stats () =
    check_stats
      "lambda_size"
      ~expected_mean:(1., 0.2)
      ~expected_stddev:(0., 0.1)
      ~expected_ratios:(1., 0.4)
end

let tests =
  let open Tztest in
  Tests.
    [
      tztest "lambda size is a good approximation (fast)" `Quick (fun () ->
          lambda_size 50);
      tztest "ty size is a good approximation (fast)" `Quick (fun () ->
          ty_size 50);
      tztest "value size is a good approximation (fast)" `Quick (fun () ->
          value_size 50);
      tztest
        "comparable ty size is a good approximation (fast)"
        `Quick
        (fun () -> comparable_ty_size 50);
      tztest "lambda size is a good approximation" `Slow (fun () ->
          lambda_size 2000);
      tztest "value size is a good approximation" `Slow (fun () ->
          value_size 1000);
      tztest "ty size is a good approximation" `Slow (fun () -> ty_size 1000);
      tztest "comparable ty size is a good approximation" `Slow (fun () ->
          comparable_ty_size 1000);
      tztest
        "statistics about ty size are satisfying"
        `Quick
        check_ty_size_stats;
      tztest
        "statistics about comparable ty size are satisfying"
        `Quick
        check_comparable_ty_size_stats;
      tztest
        "statistics about value size are satisfying"
        `Quick
        check_value_size_stats;
      tztest
        "statistics about lambda size are satisfying"
        `Quick
        check_lambda_size_stats;
    ]
