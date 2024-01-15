(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.com>                        *)
(* Copyright (c) 2023  Marigold <contact@marigold.dev>                       *)
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

let ns = Namespace.make Registration_helpers.ns "dal"

let fv s = Free_variable.of_namespace (ns s)

module Publish_slot_header : Benchmark.S = struct
  let name = ns "Dal_publish_slot_header"

  let info = "Estimating the cost of publishing a slot header"

  let module_filename = __FILE__

  let purpose = Benchmark.Generate_code "dal"

  let tags = ["dal"]

  type config = Environment.Dal.parameters

  let default_config =
    Default_parameters.constants_mainnet.dal.cryptobox_parameters

  let config_encoding = Tezos_crypto_dal.Cryptobox.parameters_encoding

  type workload = unit

  let workload_encoding = Data_encoding.unit

  let workload_to_vector _ = Sparse_vec.String.of_list []

  let model =
    Model.make
      ~conv:(fun () -> ())
      (Model.unknown_const1 ~name ~const:(fv "publish_slot_header_const"))

  let models = [("dal", model)]

  let operation_generator cryptobox rng_state =
    let open Result_syntax in
    let open Alpha_context in
    let module Crypto = Tezos_crypto_dal.Cryptobox in
    let config = Crypto.parameters cryptobox in
    let slot_index = Dal.Slot_index.zero in
    let slot = Base_samplers.uniform_bytes ~nbytes:config.slot_size rng_state in
    let* polynomial = Crypto.polynomial_from_slot cryptobox slot in
    let* commitment = Crypto.commit cryptobox polynomial in
    let* commitment_proof = Crypto.prove_commitment cryptobox polynomial in
    return
    @@ Dal.Operations.Publish_slot_header.
         {slot_index; commitment; commitment_proof}

  let make_bench rng_state (config : config) () : workload Generator.benchmark =
    let open Lwt_result_syntax in
    let bench_promise =
      let dal =
        {
          Default_parameters.constants_mainnet.dal with
          blocks_per_epoch = 1l;
          feature_enable = true;
        }
      in
      let* ctxt, _ = Execution_context.make ~dal ~rng_state () in
      let* cryptobox =
        match Tezos_crypto_dal.Cryptobox.make config with
        | Ok cryptobox -> return cryptobox
        | Error (`Fail msg) ->
            failwith "Dal_benchmarks: failed to initialize cryptobox (%s)" msg
      in

      let* op =
        match operation_generator cryptobox rng_state with
        | Ok op -> return op
        | Error err ->
            let msg =
              match err with
              | `Slot_wrong_size s -> Format.asprintf "Slot_wrong_size(%s)" s
              | `Invalid_degree_strictly_less_than_expected {given; expected} ->
                  Format.asprintf
                    "Invalid_degree_strictly_less_than_expected {given=%d, \
                     expected=%d}"
                    given
                    expected
              | `Prover_SRS_not_loaded -> "Prover_SRS_not_loaded"
            in
            failwith "Dal_benchmarks: failed to generate operation (%s)" msg
      in
      let workload = () in
      let closure () =
        match Dal_apply.apply_publish_slot_header ctxt op with
        | Error errs ->
            Format.eprintf "%a@." Environment.Error_monad.pp_trace errs ;
            Stdlib.failwith
              "Dal_benchmarks: error raised during closure execution@."
        | exception _ -> assert false
        | Ok _ -> ()
      in
      return (Generator.Plain {workload; closure})
    in
    Lwt_main.run bench_promise |> function
    | Ok closure -> closure
    | Error errs ->
        Format.eprintf "%a@." Error_monad.pp_print_trace errs ;
        Stdlib.failwith "Dal_benchmarks: failed to run benchmark"

  let create_benchmarks ~rng_state ~bench_num config =
    let () =
      Lwt_main.run
      @@ Tezos_crypto_dal.Cryptobox.Config.init_dal
           ~find_srs_files:(Fun.const (Ok ("", "")))
           {
             activated = true;
             use_mock_srs_for_testing = Some config;
             bootstrap_peers = [];
           }
      |> function
      | Ok () -> ()
      | Error errs ->
          Format.eprintf "%a@." Error_monad.pp_print_trace errs ;
          Stdlib.failwith "Dal_benchmarks: failed to initialize"
    in
    Format.printf "Initialized DAL@." ;
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Publish_slot_header)
