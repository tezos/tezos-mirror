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

type generator_config = {
  target_size : Base_samplers.range;
  burn_in_multiplier : int;
}

let default_generator_config =
  {target_size = {Base_samplers.min = 100; max = 1000}; burn_in_multiplier = 5}

let generator_config_encoding =
  let open Data_encoding in
  conv
    (fun {target_size; burn_in_multiplier} -> (target_size, burn_in_multiplier))
    (fun (target_size, burn_in_multiplier) -> {target_size; burn_in_multiplier})
    (obj2
       (req "target_size" Base_samplers.range_encoding)
       (req "burn_in_multiplier" int31))

(* ----------------------------------------------------------------------- *)

type michelson_data =
  | Code of {term : Script_repr.expr; bef : Script_repr.expr list}
  | Data of {term : Script_repr.expr; typ : Script_repr.expr}

let michelson_data_list_encoding =
  let open Data_encoding in
  let e = Script_repr.expr_encoding in
  list
  @@ union
       [
         case
           ~title:"Code"
           (Tag 0)
           (tup2 e (list e))
           (function Code {term; bef} -> Some (term, bef) | _ -> None)
           (fun (term, bef) -> Code {term; bef});
         case
           ~title:"Data"
           (Tag 1)
           (tup2 e e)
           (function Data {term; typ} -> Some (term, typ) | _ -> None)
           (fun (term, typ) -> Data {term; typ});
       ]

let save ~filename ~terms =
  let bytes =
    match Data_encoding.Binary.to_bytes michelson_data_list_encoding terms with
    | Error err ->
        Format.eprintf
          "Michelson_generation.save: encoding failed (%a); exiting"
          Data_encoding.Binary.pp_write_error
          err ;
        exit 1
    | Ok res -> res
  in
  ignore (* TODO handle error *)
    (Lwt_main.run
    @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file
         filename
         (Bytes.unsafe_to_string bytes))

let load ~filename =
  Lwt_main.run
  @@ ( Tezos_stdlib_unix.Lwt_utils_unix.read_file filename >>= fun str ->
       Format.eprintf "Michelson_generation.load: loaded %s@." filename ;
       let bytes = Bytes.unsafe_of_string str in
       match
         Data_encoding.Binary.of_bytes michelson_data_list_encoding bytes
       with
       | Ok result -> Lwt.return result
       | Error err ->
           Format.eprintf
             "Michelson_generation.load: can't load file (%a); exiting"
             Data_encoding.Binary.pp_read_error
             err ;
           exit 1 )

(* ----------------------------------------------------------------------- *)

let michelson_samplers =
  let module Config = struct
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
  end in
  let module Samplers = Michelson_samplers.Make (Config) in
  (module Samplers : Michelson_samplers.S)

module Samplers = (val michelson_samplers)

(* ----------------------------------------------------------------------- *)

let base_type_to_michelson_type (typ : Type.Base.t) =
  let typ = Mikhailsky.map_var (fun _ -> Mikhailsky.unit_ty) typ in
  Mikhailsky.to_michelson typ

(* Convert a Micheline-encoded type to its internal GADT format. *)
let michelson_type_to_ex_ty (typ : Alpha_context.Script.expr)
    (ctxt : Alpha_context.t) =
  Script_ir_translator.parse_ty
    ctxt
    ~legacy:false
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:false
    ~allow_ticket:false
    (Micheline.root typ)
  |> Environment.wrap_tzresult
  |> function
  | Ok t -> t
  | Error trace ->
      Format.eprintf "%a@." Error_monad.pp_print_trace trace ;
      Stdlib.failwith "Michelson_generation.michelson_type_to_ex_ty: error"

let base_type_to_ex_ty ty =
  michelson_type_to_ex_ty (base_type_to_michelson_type ty)

(* Convert a Mikhailsky stack to a list of Micheline-encoded types *)
let rec stack_type_to_michelson_type_list (typ : Type.Stack.t) =
  let node = typ.node in
  match node with
  | Type.Stack.Stack_var_t _ ->
      Stdlib.failwith "stack_type_to_michelson_type_list: bug found"
  | Type.Stack.Empty_t -> []
  | Type.Stack.Item_t (ty, tl) ->
      base_type_to_michelson_type ty :: stack_type_to_michelson_type_list tl

(* Convert a list of Micheline-encoded Michelson types to the
     internal GADT format. *)
let rec michelson_type_list_to_ex_stack_ty
    (stack_ty : Alpha_context.Script.expr list) ctxt =
  let open Script_ir_translator in
  let open Script_typed_ir in
  match stack_ty with
  | [] -> (Ex_stack_ty Bot_t, ctxt)
  | hd :: tl -> (
      let (ex_ty, ctxt) = michelson_type_to_ex_ty hd ctxt in
      match ex_ty with
      | Ex_ty ty -> (
          let (ex_stack_ty, ctxt) =
            michelson_type_list_to_ex_stack_ty tl ctxt
          in
          match ex_stack_ty with
          | Ex_stack_ty tl -> (Ex_stack_ty (Item_t (ty, tl, None)), ctxt)))

let stack_type_to_ex_stack_ty ty =
  michelson_type_list_to_ex_stack_ty (stack_type_to_michelson_type_list ty)

let make_data_sampler rng_state config =
  let target_size =
    Base_samplers.sample_in_interval rng_state ~range:config.target_size
  in
  let module Gen = Generators.Data (struct
    module Samplers = Samplers

    let rng_state = rng_state

    let target_size = target_size

    let verbosity = `Silent
  end) in
  let burn_in = target_size * config.burn_in_multiplier in
  let generator = Gen.generator ~burn_in in
  let (term, typ) = StaTz.Stats.sample_gen generator in
  Data {term; typ = base_type_to_michelson_type typ}

let make_code_sampler rng_state config =
  let target_size =
    Base_samplers.sample_in_interval rng_state ~range:config.target_size
  in
  let module Gen = Generators.Code (struct
    module Samplers = Samplers

    let rng_state = rng_state

    let target_size = target_size

    let verbosity = `Silent
  end) in
  let burn_in = target_size * config.burn_in_multiplier in
  let generator = Gen.generator ~burn_in in
  let (term, (bef, _aft)) = StaTz.Stats.sample_gen generator in
  let bef = stack_type_to_michelson_type_list bef in
  Code {term; bef}
