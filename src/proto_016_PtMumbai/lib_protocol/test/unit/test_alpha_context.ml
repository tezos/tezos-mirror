(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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
open Alpha_context

(** Testing
    -------
    Component:    Alpha_context
    Invocation:   dune exec ./src/proto_alpha/lib_protocol/test/unit/main.exe \
                    -- test Alpha_context
    Dependencies: helpers/block.ml
    Subject:      To test the modules (including the top-level)
                  in alpha_context.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)

(** Creates an Alpha_context without creating a full-fledged block *)
let create () =
  let account = Account.new_account () in
  let bootstrap_account = Account.make_bootstrap_account account in
  Block.alpha_context [bootstrap_account]

let assert_equal_key_values ~loc kvs1 kvs2 =
  let sort_by_key_hash =
    List.sort (fun (k1, _) (k2, _) -> Script_expr_hash.compare k1 k2)
  in
  Assert.assert_equal_list
    ~loc
    (fun (k1, v1) (k2, v2) ->
      Script_expr_hash.equal k1 k2
      && String.equal (Expr.to_string v1) (Expr.to_string v2))
    "Compare key-value list"
    (fun fmt (k, v) ->
      Format.fprintf fmt "(%a, %s)" Script_expr_hash.pp k (Expr.to_string v))
    (sort_by_key_hash kvs1)
    (sort_by_key_hash kvs2)

module Test_Script = struct
  (** Force serialise of lazy [Big_map.t] in a given [alpha_context] *)
  let test_force_bytes_in_context () =
    create () >>=? fun alpha_context ->
    let mbytes_pp ppf t =
      Format.pp_print_string ppf (Environment.Bytes.to_string t)
    in
    let open Alpha_context.Script in
    Environment.wrap_tzresult
    @@ force_bytes_in_context alpha_context
    @@ lazy_expr @@ Micheline.strip_locations
    @@ Prim (0, D_Unit, [], [])
    >>?= fun (bytes, _) ->
    Assert.equal
      ~loc:__LOC__
      Environment.Bytes.equal
      "script serialised incorrectly"
      mbytes_pp
      bytes
      (`Hex "030b" |> Hex.to_bytes_exn)
end

module Test_Big_map = struct
  (** Test failure path: look for a non-existent key in a [Big_map] *)
  let test_mem () =
    ( create () >>=? fun alpha_context ->
      Big_map.fresh ~temporary:true alpha_context >|= Environment.wrap_tzresult
      >>=? fun (alpha_context, big_map_id) ->
      Big_map.mem
        alpha_context
        big_map_id
        (Script_expr_hash.hash_string ["0"; "0"])
      >|= Environment.wrap_tzresult )
    >>=? fun (_alpha_context, is_member) ->
    Assert.equal_bool ~loc:__LOC__ is_member false

  (** Test failure code path of [get_opt] by looking for missing key in a [Big_map.t] *)
  let test_get_opt () =
    ( create () >>=? fun alpha_context ->
      Big_map.fresh ~temporary:true alpha_context >|= Environment.wrap_tzresult
      >>=? fun (alpha_context, big_map_id) ->
      Big_map.get_opt
        alpha_context
        big_map_id
        (Script_expr_hash.hash_string ["0"; "0"])
      >|= Environment.wrap_tzresult )
    >>=? fun (_alpha_context, value) ->
    match value with
    | Some _ ->
        failwith "get_opt should have failed looking for a non-existent key"
    | None -> return_unit

  (** Test existence of a non-existent [Big_map] in an [Alpha_context.t] *)
  let test_exists () =
    ( create () >>=? fun alpha_context ->
      Big_map.fresh ~temporary:true alpha_context >|= Environment.wrap_tzresult
      >>=? fun (alpha_context, big_map_id) ->
      Big_map.exists alpha_context big_map_id >|= Environment.wrap_tzresult )
    >>=? fun (_alpha_context, value) ->
    match value with
    | Some _ ->
        failwith "exists should have failed looking for a non-existent big_map"
    | None -> return_unit

  (** Test that [Big_map.list_key_values] retrieves hashed keys and values. *)
  let test_list_key_values () =
    let open Lwt_result_syntax in
    let* block, source = Context.init1 () in
    let key_values =
      [
        ("1", {|"A"|});
        ("2", {|"B"|});
        ("3", {|"C"|});
        ("4", {|"D"|});
        ("5", {|"E"|});
      ]
      |> List.map (fun (k, v) -> (Expr.from_string k, Expr.from_string v))
    in
    let* big_map_id, ctxt =
      Big_map_helpers.make_big_map
        block
        ~source
        ~key_type:"int"
        ~value_type:"string"
        key_values
    in
    let* _ctxt, retrieved_key_values =
      Big_map.list_key_values ctxt big_map_id >|= Environment.wrap_tzresult
    in
    let expected_key_hash_values =
      List.map
        (fun (key, value) ->
          let bytes =
            Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding key
          in
          let key_hash = Script_expr_hash.hash_bytes [bytes] in
          (key_hash, value))
        key_values
    in
    assert_equal_key_values
      ~loc:__LOC__
      expected_key_hash_values
      retrieved_key_values

  (** Test [Big_map.list_key_values] with [length] and [offset] arguments. *)
  let test_list_key_values_parameters () =
    let open Lwt_result_syntax in
    let* block, source = Context.init1 () in
    let hash_key key =
      let bytes =
        Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding key
      in
      Script_expr_hash.hash_bytes [bytes]
    in
    let check_key_values ~loc ~num_elements ?offset ?length () =
      let key_values =
        WithExceptions.List.init ~loc:__LOC__ num_elements (fun n ->
            (string_of_int n, Printf.sprintf {|"Value %d"|} n))
        |> List.map (fun (k, v) -> (Expr.from_string k, Expr.from_string v))
      in
      let sorted_key_values =
        List.sort
          (fun (k1, _) (k2, _) ->
            Script_expr_hash.compare (hash_key k1) (hash_key k2))
          key_values
      in
      let* big_map_id, ctxt =
        Big_map_helpers.make_big_map
          block
          ~source
          ~key_type:"int"
          ~value_type:"string"
          key_values
      in
      let* _ctxt, retrieved_key_values =
        Big_map.list_key_values ?offset ?length ctxt big_map_id
        >|= Environment.wrap_tzresult
      in
      let expected_key_hash_values =
        (* A negative length is interpreted as 0 *)
        let length =
          match length with
          | Some l -> max l 0
          | None -> List.length sorted_key_values
        in
        let offset = match offset with Some o -> max o 0 | None -> 0 in
        let expected =
          List.take_n length @@ List.drop_n offset sorted_key_values
        in
        List.map
          (fun (key, value) ->
            let bytes =
              Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding key
            in
            let key_hash = Script_expr_hash.hash_bytes [bytes] in
            (key_hash, value))
          expected
      in
      let* () =
        assert_equal_key_values
          ~loc
          retrieved_key_values
          expected_key_hash_values
      in
      return retrieved_key_values
    in
    (* The following combinations should yield the same key-values. *)
    let* kvs1 = check_key_values ~loc:__LOC__ ~num_elements:10 () in
    let* kvs2 = check_key_values ~loc:__LOC__ ~num_elements:10 ~offset:0 () in
    let* kvs3 = check_key_values ~loc:__LOC__ ~num_elements:10 ~length:10 () in
    let* kvs4 =
      check_key_values ~loc:__LOC__ ~num_elements:10 ~offset:0 ~length:10 ()
    in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs1 kvs2 in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs2 kvs3 in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs3 kvs4 in
    (* Attempt to consume more elements then the length. *)
    let* kvs1 = check_key_values ~loc:__LOC__ ~num_elements:20 () in
    let* kvs2 = check_key_values ~loc:__LOC__ ~num_elements:20 ~length:100 () in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs1 kvs2 in
    let* (_ : _ list) =
      check_key_values ~loc:__LOC__ ~num_elements:100 ~offset:100 ~length:1 ()
    in
    (* Offset greater than the length. *)
    let* kvs = check_key_values ~loc:__LOC__ ~num_elements:10 ~offset:100 () in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs [] in
    (* Negative length is treated as zero. *)
    let* kvs = check_key_values ~loc:__LOC__ ~num_elements:10 ~length:(-1) () in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs [] in
    (* Negative offset is treated as zero. *)
    let* kvs1 =
      check_key_values ~loc:__LOC__ ~num_elements:10 ~offset:(-5) ()
    in
    let* kvs2 = check_key_values ~loc:__LOC__ ~num_elements:10 () in
    let* () = assert_equal_key_values ~loc:__LOC__ kvs1 kvs2 in
    return_unit
end

let tests =
  [
    Tztest.tztest
      "Script.force_bytes_in_context: checks if it serialises a simple \
       michelson expression"
      `Quick
      Test_Script.test_force_bytes_in_context;
    Tztest.tztest
      "Big_map.mem: failure case - must return false when starting with an \
       empty map"
      `Quick
      Test_Big_map.test_mem;
    Tztest.tztest
      "Big_map.get_opt: failure case - looking up key that doesn't exist"
      `Quick
      Test_Big_map.test_get_opt;
    Tztest.tztest
      "Big_map.exists: failure case - looking up big_map that doesn't exist"
      `Quick
      Test_Big_map.test_exists;
    Tztest.tztest
      "Big_map.list_key_values basic tests"
      `Quick
      Test_Big_map.test_list_key_values;
    Tztest.tztest
      "Big_map.list_key_values: combinations of parameters"
      `Quick
      Test_Big_map.test_list_key_values_parameters;
  ]
