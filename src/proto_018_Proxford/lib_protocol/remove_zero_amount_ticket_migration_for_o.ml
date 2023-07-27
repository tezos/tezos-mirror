(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(* Remove zero tickets from big map with id 5696. This big map is found in the
   storage of [KT1CnygLoKfJA66499U9ZQkL6ykUfzgruGfM]. The id can be looked up in
   https://tzkt.io/KT1CnygLoKfJA66499U9ZQkL6ykUfzgruGfM/storage/

   This procedure removes all keys with zero-amount tickets from the big-map.
   We have compiled a list of hashes corresponding to keys of values with
   zero-amount tickets. We also verify removal by pattern matching on the values
   to make sure they contain zero-amount ticket values.
*)

type error +=
  | Invalid_big_map_key of string
  | Not_zero_ticket of Script_expr_hash.t
  | Invalid_big_map_value of Script_expr_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"migration.invalid_big_map_key"
    ~title:"Invalid big map key as a script expression hash"
    ~description:
      "As part of the protocol migration, a big map key was expected to be \
       parsed successfully into a script expression hash"
    ~pp:(fun ppf key ->
      Format.fprintf ppf "Unable to parse %s as a script expression hash" key)
    Data_encoding.(obj1 (req "key" (string Plain)))
    (function Invalid_big_map_key key -> Some key | _ -> None)
    (fun key -> Invalid_big_map_key key) ;
  register_error_kind
    `Permanent
    ~id:"migration.not_zero_ticket_key"
    ~title:"A big map key is not mapping to any zero ticket"
    ~description:"the value mapped to a big map key has an unexpected shape"
    ~pp:(fun ppf key ->
      Format.fprintf
        ppf
        "%a does not map to a zero ticket, key not found"
        Script_expr_hash.pp
        key)
    Data_encoding.(obj1 (req "key" Script_expr_hash.encoding))
    (function Not_zero_ticket key -> Some key | _ -> None)
    (fun key -> Not_zero_ticket key) ;
  register_error_kind
    `Permanent
    ~id:"migration.zero_ticket_wrong_value"
    ~title:"A big map key is mapped to an unexpected value"
    ~description:
      "As part of the protocol migration, a big map key was expected to be \
       mapping into a value which is of unexpected shape"
    ~pp:(fun ppf key ->
      Format.fprintf
        ppf
        "%a does not map to a zero ticket, unexpected shape"
        Script_expr_hash.pp
        key)
    Data_encoding.(obj1 (req "key" Script_expr_hash.encoding))
    (function Invalid_big_map_value key -> Some key | _ -> None)
    (fun key -> Invalid_big_map_value key)

let remove_zero_ticket_entries ctxt =
  let open Lwt_result_syntax in
  let*? contract_to_clean =
    Contract_repr.of_b58check "KT1CnygLoKfJA66499U9ZQkL6ykUfzgruGfM"
  in
  let big_map_to_clean =
    Lazy_storage_kind.Big_map.Id.parse_z @@ Z.of_int 5696
  in
  let*? keys_to_evict =
    List.map_e
      (fun key ->
        match Script_expr_hash.of_b58check_opt key with
        | Some key -> ok key
        | None -> error (Invalid_big_map_key key))
      [
        "exprtXBtxJxCDEDETueKAFLL7r7vZtNEo1MHajpHba1djtGKqJzWd3";
        "exprtbuRhaGDS942BgZ1qFdD7HAKeBjPEqzRxgLQyWQ6HWxcaiLC2c";
        "exprtePxSLgrhJmTPZEePyFBmESLhaBUN1WodvLYy9xYhEYE6dKPLe";
        "exprtx9GaYz5Fy5ytiuYgSfJqeYqkxGgobust8U6dpCLaeZUMiitmg";
        "expru28t4XoyB61WuRQnExk3Kq8ssGv1ejgdo9XHxpTXoQjXTGw1Dg";
        "expru2fZALknjB4vJjmQBPkrs3dJZ5ytuzfmE9A7ScUk5opJiZQyiJ";
        "expru2riAFKURjHJ1vNpvsZGGw6z4wtTvbstXVuwQPj1MaTqKPeQ6z";
        "expruHoZDr8ioVhaAs495crYTprAYyC87CruEJ6HaS7diYV6qLARqQ";
        "expruMie2gfy5smMd81NtcvvWm4jD7ThUebw9hpF3N3apKVtxkVG9M";
        "expruc3QW7cdxrGurDJQa6k9QqMZjGkRDJahy2XNtBt9WQzC1yavJK";
        "exprud86wYL7inFCVHkF1Jcz8uMXVY7dnbzxVupyyknZjtDVmwoQTJ";
        "exprufYzeBTGn9733Ga8xEEmU4SsrSyDrzEip8V8hTBAG253T5zZQx";
        "exprum9tuHNvisMa3c372AFmCa27rmkbCGrhzMSprrxgJjzXhrKAag";
        "expruokt7oQ6dDHRvL4sURKUzfwJirR8FPHvpXwjgUD4KHhPWhDGbv";
        "expruom5ds2hVgjdTB877Fx3ZuWT5WUnw1H6kUZavVHcJFbCkcgo3x";
        "exprv2DPd1pV3GVSN2CgW7PPrAQUTuZAdeJphwToQrTNrxiJcWzvtX";
        "exprv65Czv5TnKyEWgBHjDztkCkc1FAVEPxZ3V3ocgvGjfXwjPLo8M";
        "exprv6S2KAvqAC18jDLYjaj1w9oc4ESdDGJkUZ63EpkqSTAz88cSYB";
        "exprvNg3VDBnhtTHvc75krAEYzz6vUMr3iU5jtLdxs83FbgTbZ9nFT";
        "exprvS7wNDHYKYZ19nj3ZUo7AAVMCDpTK3NNERFhqe5SJGCBL4pwFA";
      ]
  in
  let* initial_big_map_size =
    Storage.Big_map.Total_bytes.get ctxt big_map_to_clean
  in
  let* initial_storage_size =
    Storage.Contract.Used_storage_space.get ctxt contract_to_clean
  in
  let scan_and_remove (ctxt, storage_freed) key =
    let* ctxt, v = Storage.Big_map.Contents.get (ctxt, big_map_to_clean) key in
    let open Micheline in
    let open Michelson_v1_primitives in
    match root v with
    | Prim
        ( _,
          D_Pair,
          [
            _timestamp;
            Prim
              ( _,
                D_Pair,
                [_data; Prim (_, D_Pair, [_ticketer; Int (_, amount)], _)],
                _ );
          ],
          _ ) ->
        if Z.(equal amount zero) then
          let* ctxt, size_sub, is_evicted =
            Storage.Big_map.Contents.remove (ctxt, big_map_to_clean) key
          in
          if is_evicted then
            return (ctxt, Z.(add storage_freed (Z.of_int size_sub)))
          else tzfail @@ Invalid_big_map_value key
        else tzfail @@ Invalid_big_map_value key
    | _ -> tzfail @@ Invalid_big_map_value key
  in
  let* ctxt, storage_freed =
    List.fold_left_es scan_and_remove (ctxt, Z.zero) keys_to_evict
  in
  let new_big_map_size = Z.sub initial_big_map_size storage_freed in
  let* ctxt =
    Storage.Big_map.Total_bytes.update ctxt big_map_to_clean new_big_map_size
  in
  let new_storage_size = Z.sub initial_storage_size storage_freed in
  Storage.Contract.Used_storage_space.update
    ctxt
    contract_to_clean
    new_storage_size

let remove_zero_ticket_entries orig_ctxt =
  let open Lwt_syntax in
  let* res = remove_zero_ticket_entries orig_ctxt in
  match res with
  | Error e ->
      Logging.(
        log
          Warning
          "Error while removing zero tickets: %a"
          Error_monad.pp_trace
          e) ;
      return orig_ctxt
  | Ok ctxt -> return ctxt
