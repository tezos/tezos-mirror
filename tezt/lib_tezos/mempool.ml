(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  validated : string list;
  branch_delayed : string list;
  branch_refused : string list;
  refused : string list;
  outdated : string list;
  unprocessed : string list;
}

(* A comparable type for mempool where classification and ordering
   does not matter. *)
let typ : t Check.typ =
  let open Check in
  let sort = List.sort compare in
  convert
    (fun mempool ->
      sort
        (mempool.validated
        @ sort mempool.branch_delayed
        @ sort mempool.branch_refused
        @ sort mempool.refused @ sort mempool.outdated
        @ sort mempool.unprocessed))
    (list string)

(* A comparable type for mempool where ordering does not matter. *)
let classified_typ : t Check.typ =
  let open Check in
  let sort = List.sort compare in
  convert
    (fun mempool ->
      [
        sort mempool.validated;
        sort mempool.branch_delayed;
        sort mempool.branch_refused;
        sort mempool.refused;
        sort mempool.outdated;
        sort mempool.unprocessed;
      ])
    (list (list string))

let empty =
  {
    validated = [];
    branch_delayed = [];
    branch_refused = [];
    refused = [];
    outdated = [];
    unprocessed = [];
  }

let symmetric_diff left right =
  let diff left right =
    List.(
      filter (fun op -> not (mem op right)) left
      @ filter (fun op -> not (mem op left)) right)
  in
  {
    validated = diff left.validated right.validated;
    branch_delayed = diff left.branch_delayed right.branch_delayed;
    branch_refused = diff left.branch_refused right.branch_refused;
    refused = diff left.refused right.refused;
    outdated = diff left.outdated right.outdated;
    unprocessed = diff left.unprocessed right.unprocessed;
  }

let of_json mempool_json =
  let get_hash op = JSON.(op |-> "hash" |> as_string) in
  let get_hashes classification =
    List.map get_hash JSON.(mempool_json |-> classification |> as_list)
  in
  let validated = get_hashes "validated" in
  let branch_delayed = get_hashes "branch_delayed" in
  let branch_refused = get_hashes "branch_refused" in
  let refused = get_hashes "refused" in
  let outdated = get_hashes "outdated" in
  let unprocessed = get_hashes "unprocessed" in
  {validated; branch_delayed; branch_refused; refused; outdated; unprocessed}

let get_mempool ?endpoint ?hooks ?chain ?(validated = true)
    ?(branch_delayed = true) ?(branch_refused = true) ?(refused = true)
    ?(outdated = true) ?(validation_passes = []) client =
  let* mempool_json =
    Client.RPC.call client ?hooks ?endpoint
    @@ RPC.get_chain_mempool_pending_operations
         ?chain
         ~version:"2"
         ~validated
         ~branch_delayed
         ~branch_refused
         ~refused
         ~outdated
         ~validation_passes
         ()
  in
  return (of_json mempool_json)

let check_mempool ?(validated = []) ?(branch_delayed = [])
    ?(branch_refused = []) ?(refused = []) ?(outdated = []) ?(unprocessed = [])
    mempool =
  let expected_mempool =
    {validated; branch_delayed; branch_refused; refused; outdated; unprocessed}
  in
  Check.(
    (expected_mempool = mempool)
      classified_typ
      ~error_msg:"Expected mempool %L, got %R")

module Config = struct
  type t = {
    minimal_fees : int option;
    minimal_nanotez_per_gas_unit : (int * int) option;
    minimal_nanotez_per_byte : (int * int) option;
    replace_by_fee_factor : (int * int) option;
    max_operations : int option;
    max_total_bytes : int option;
  }

  let make ?minimal_fees ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
      ?replace_by_fee_factor ?max_operations ?max_total_bytes () =
    {
      minimal_fees;
      minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte;
      replace_by_fee_factor;
      max_operations;
      max_total_bytes;
    }

  let eq_int_pair (f1, s1) (f2, s2) = Int.equal f1 f2 && Int.equal s1 s2

  let equal x1 x2 =
    Option.equal Int.equal x1.minimal_fees x2.minimal_fees
    && Option.equal
         eq_int_pair
         x1.minimal_nanotez_per_gas_unit
         x2.minimal_nanotez_per_gas_unit
    && Option.equal
         eq_int_pair
         x1.minimal_nanotez_per_byte
         x2.minimal_nanotez_per_byte
    && Option.equal
         eq_int_pair
         x1.replace_by_fee_factor
         x2.replace_by_fee_factor
    && Option.equal Int.equal x1.max_operations x2.max_operations
    && Option.equal Int.equal x1.max_total_bytes x2.max_total_bytes

  let to_json_u config : JSON.u =
    let str_value n = `String (string_of_int n) in
    let int_str_field name = Option.map (fun n -> (name, str_value n)) in
    let pair_field name =
      Option.map (fun (n1, n2) -> (name, `A [str_value n1; str_value n2]))
    in
    let int_field name =
      Option.map (fun n -> (name, `Float (float_of_int n)))
    in
    `O
      (List.filter_map
         Fun.id
         [
           int_str_field "minimal_fees" config.minimal_fees;
           pair_field
             "minimal_nanotez_per_gas_unit"
             config.minimal_nanotez_per_gas_unit;
           pair_field "minimal_nanotez_per_byte" config.minimal_nanotez_per_byte;
           pair_field "replace_by_fee_factor" config.replace_by_fee_factor;
           int_field "max_operations" config.max_operations;
           int_field "max_total_bytes" config.max_total_bytes;
         ])

  let to_string config = Ezjsonm.value_to_string (to_json_u config)

  let pp fmt config = Format.fprintf fmt "%s" (to_string config)

  let check_equal expected actual =
    Check.(
      (expected = actual)
        (equalable pp equal)
        ~error_msg:"Wrong filter configuration: %R.\nExpected: %L.")

  let of_json json =
    let open JSON in
    let as_int_pair_opt t =
      match as_list_opt t with
      | Some [x; y] -> Some (as_int x, as_int y)
      (* A missing field is interpreted as [`Null], from which [as_list_opt]
         produces [Some []]. *)
      | Some [] -> None
      | Some _ | None ->
          Test.fail
            "Constructing a filter_config from json: %s. Expected a list of \
             length 2, found: %s."
            (encode json)
            (encode t)
    in
    {
      minimal_fees = json |-> "minimal_fees" |> as_int_opt;
      minimal_nanotez_per_gas_unit =
        json |-> "minimal_nanotez_per_gas_unit" |> as_int_pair_opt;
      minimal_nanotez_per_byte =
        json |-> "minimal_nanotez_per_byte" |> as_int_pair_opt;
      replace_by_fee_factor =
        json |-> "replace_by_fee_factor" |> as_int_pair_opt;
      max_operations = json |-> "max_operations" |> as_int_opt;
      max_total_bytes = json |-> "max_total_bytes" |> as_int_opt;
    }

  (** Default filter configuration for protocol alpha
      (see src/proto_alpha/lib_plugin/plugin.ml
       and src/lib_shell/prevalidator_bounding.ml). *)

  let default_minimal_fees = 100

  let default_minimal_nanotez_per_gas_unit = (100, 1)

  let default_minimal_nanotez_per_byte = (1000, 1)

  let default_replace_by_fee_factor = (21, 20)

  let default_max_operations = 10_000

  let default_max_total_bytes = 10_000_000

  let default =
    {
      minimal_fees = Some default_minimal_fees;
      minimal_nanotez_per_gas_unit = Some default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = Some default_minimal_nanotez_per_byte;
      replace_by_fee_factor = Some default_replace_by_fee_factor;
      max_operations = Some default_max_operations;
      max_total_bytes = Some default_max_total_bytes;
    }

  let fill_with_default config =
    let aux default v = Some (Option.value v ~default) in
    {
      minimal_fees = aux default_minimal_fees config.minimal_fees;
      minimal_nanotez_per_gas_unit =
        aux
          default_minimal_nanotez_per_gas_unit
          config.minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte =
        aux default_minimal_nanotez_per_byte config.minimal_nanotez_per_byte;
      replace_by_fee_factor =
        aux default_replace_by_fee_factor config.replace_by_fee_factor;
      max_operations = aux default_max_operations config.max_operations;
      max_total_bytes = aux default_max_total_bytes config.max_total_bytes;
    }

  (** Return a copy of the given filter config, where fields equal
      to their default value have been removed (i.e. set to [None]). *)
  let clear_default config =
    let clear_if_default eq_fun default = function
      | Some x when eq_fun default x -> None
      | x -> x
    in
    let aux_int = clear_if_default Int.equal in
    let aux_pair = clear_if_default eq_int_pair in
    {
      minimal_fees = aux_int default_minimal_fees config.minimal_fees;
      minimal_nanotez_per_gas_unit =
        aux_pair
          default_minimal_nanotez_per_gas_unit
          config.minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte =
        aux_pair
          default_minimal_nanotez_per_byte
          config.minimal_nanotez_per_byte;
      replace_by_fee_factor =
        aux_pair default_replace_by_fee_factor config.replace_by_fee_factor;
      max_operations = aux_int default_max_operations config.max_operations;
      max_total_bytes = aux_int default_max_total_bytes config.max_total_bytes;
    }

  let call_get_filter ?endpoint ?hooks ?include_default client =
    Client.RPC.call ?endpoint ?hooks client
    @@ RPC.get_chain_mempool_filter ?include_default ()

  let check_get_filter_all_variations ?(log = false) ?endpoint ?hooks
      expected_config client =
    let expected_full = fill_with_default expected_config in
    let* json = call_get_filter ?endpoint ?hooks client in
    check_equal expected_full (of_json json) ;
    let* json = call_get_filter ?endpoint ?hooks ~include_default:true client in
    check_equal expected_full (of_json json) ;
    let expected_partial = clear_default expected_config in
    let* json =
      call_get_filter ?endpoint ?hooks ~include_default:false client
    in
    check_equal expected_partial (of_json json) ;
    if log then
      Log.info
        "GET /chains/main/mempool/filter returned expected configurations \
         (respectively including/excluding default fields): %s and %s."
        (to_string expected_full)
        (to_string expected_partial) ;
    unit

  let call_post_filter ?endpoint ?hooks json_u client =
    Client.RPC.call ?endpoint ?hooks client
    @@ RPC.post_chain_mempool_filter ~data:(Data json_u) ()

  let post_filter ?(log = false) ?endpoint ?hooks config client =
    let json_u = to_json_u config in
    if log then
      Log.info
        "Set mempool filter config to: %s."
        (Ezjsonm.value_to_string json_u) ;
    call_post_filter ?endpoint ?hooks json_u client

  let post_filter_str ?(log = false) ?endpoint ?hooks config_str client =
    if log then Log.info "Set mempool filter config to: %s." config_str ;
    call_post_filter ?endpoint ?hooks (Ezjsonm.from_string config_str) client

  let set_filter ?log ?endpoint ?hooks ?minimal_fees
      ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
      ?replace_by_fee_factor ?max_operations ?max_total_bytes client =
    let config =
      make
        ?minimal_fees
        ?minimal_nanotez_per_gas_unit
        ?minimal_nanotez_per_byte
        ?replace_by_fee_factor
        ?max_operations
        ?max_total_bytes
        ()
    in
    let* output = post_filter ?endpoint ?hooks ?log config client in
    check_equal (fill_with_default config) (of_json output) ;
    unit
end
