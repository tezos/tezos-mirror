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

let qcheck_make_result ?count ?print ?pp_error ?check ~name
    ~(gen : 'a QCheck2.Gen.t) (f : 'a -> (bool, 'b) result) =
  let check =
    match check with
    | Some check -> check
    | None -> (
        function
        | Ok b -> b
        | Error err -> (
            match pp_error with
            | Some pp_error ->
                QCheck2.Test.fail_reportf "Test failed:@,%a" pp_error err
            | None ->
                QCheck2.Test.fail_reportf
                  "Test failed but no pretty printer was provided."))
  in
  QCheck2.Test.make ~name ?print ?count gen (fun x -> f x |> check)

let qcheck_make_lwt ?count ?print ~extract ~name ~(gen : 'a QCheck2.Gen.t)
    (f : 'a -> bool Lwt.t) =
  QCheck2.Test.make ~name ?print ?count gen (fun x -> extract (f x))

let qcheck_make_result_lwt ?count ?print ?pp_error ?check ~extract ~name
    ~(gen : 'a QCheck2.Gen.t) (f : 'a -> (bool, 'b) result Lwt.t) =
  let check =
    match check with
    | Some check -> check
    | None -> (
        function
        | Ok b -> b
        | Error err -> (
            match pp_error with
            | Some pp_error ->
                QCheck2.Test.fail_reportf "Test failed:@,%a" pp_error err
            | None ->
                QCheck2.Test.fail_reportf
                  "Test failed but no pretty printer was provided."))
  in
  QCheck2.Test.make ~name ?print ?count gen (fun x -> extract (f x) |> check)

let qcheck_eq ?pp ?cmp ?eq ?__LOC__ expected actual =
  let pass =
    match (eq, cmp) with
    | Some eq, _ -> eq expected actual
    | None, Some cmp -> cmp expected actual = 0
    | None, None -> Stdlib.compare expected actual = 0
  in
  let loc = match __LOC__ with Some s -> s ^ "\n" | None -> "" in
  if pass then true
  else
    match pp with
    | None ->
        QCheck2.Test.fail_reportf
          "@[<h 0>%sValues are not equal, but no pretty printer was provided.@]"
          loc
    | Some pp ->
        QCheck2.Test.fail_reportf
          "@[<v 2>%sEquality check failed!@,expected:@,%a@,actual:@,%a@]"
          loc
          pp
          expected
          pp
          actual

let qcheck_neq ?pp ?cmp ?eq left right =
  let pass =
    match (eq, cmp) with
    | Some eq, _ -> eq left right
    | None, Some cmp -> cmp left right = 0
    | None, None -> Stdlib.compare left right = 0
  in
  if not pass then true
  else
    match pp with
    | None ->
        QCheck.Test.fail_reportf
          "@[<h 0>Values are unexpectedly equal, but no pretty printer was \
           provided.@]"
    | Some pp ->
        QCheck.Test.fail_reportf
          "@[<v 2>Inequality check failed!@,left:@,%a@,right:@,%a@]"
          pp
          left
          pp
          right

let qcheck_eq_tests ~eq ~gen ~eq_name =
  let reflexivity_test =
    QCheck2.Test.make
      ~name:(Printf.sprintf "%s is reflexive: forall t, %s t t" eq_name eq_name)
      gen
      (fun t ->
        if eq t t then true
        else
          QCheck2.Test.fail_reportf
            "@[<v 2>[%s t t] should hold, but it doesn't!@,\
             [t] is printed above if you provided a pretty printer in the \
             generator@]"
            eq_name)
  in
  let symmetry_test =
    QCheck2.Test.make
      ~name:
        (Printf.sprintf
           "%s is symmetric: forall t1 t2, %s t1 t2 = %s t2 t1"
           eq_name
           eq_name
           eq_name)
      QCheck2.Gen.(pair gen gen)
      (fun (t1, t2) ->
        if Bool.equal (eq t1 t2) (eq t2 t1) then true
        else
          QCheck2.Test.fail_reportf
            "@[<v 2>[%s t1 t2 = %s t2 t1] should hold, but it doesn't!@,\
             [t1] and [t2] are printed above if you provided a pretty printer \
             in the generator@]"
            eq_name
            eq_name)
  in
  (* We don't test transitivity (i.e. (t1 = t2 && t2 = t3) ==> t1 = t3),
   * because there is little chance to generate [t1], [t2], and [t3] such
   * that the left-hand side holds. We could generate them such that
   * there are relations between them (for example take [t1 = t2]), but
   * then the test degenerates to reflexivity and symmetry. *)
  [reflexivity_test; symmetry_test]

let qcheck_eq' ?pp ?cmp ?eq ~expected ~actual () =
  qcheck_eq ?pp ?cmp ?eq expected actual

let qcheck_cond ?pp ~cond e () =
  if cond e then true
  else
    match pp with
    | None ->
        QCheck.Test.fail_reportf
          "@[<h 0>The condition check failed, but no pretty printer was \
           provided.@]"
    | Some pp ->
        QCheck.Test.fail_reportf "@[<v 2>The condition check failed!@,%a@]" pp e

let intX_range_gen ~sub ~add ~gen ~shrink a b =
  let gen a b st =
    let range = sub b a in
    let raw_val = gen st range in
    let res = add a raw_val in
    assert (a <= res && res <= b) ;
    res
  in
  let shrink b () = shrink a b () in
  QCheck2.Gen.make_primitive ~gen:(gen a b) ~shrink

let int64_range_gen a b =
  intX_range_gen
    ~sub:Int64.sub
    ~add:Int64.add
    ~gen:Random.State.int64
    ~shrink:QCheck2.Shrink.int64_towards
    a
    b

let int32_range_gen a b =
  intX_range_gen
    ~sub:Int32.sub
    ~add:Int32.add
    ~gen:Random.State.int32
    ~shrink:QCheck2.Shrink.int32_towards
    a
    b

let int64_strictly_positive_gen = int64_range_gen 1L

let int_strictly_positive_gen = QCheck2.Gen.int_range 1

let uint16 = QCheck2.Gen.(0 -- 65535)

let int16 = QCheck2.Gen.(-32768 -- 32767)

let uint8 = QCheck2.Gen.(0 -- 255)

let int8 = QCheck2.Gen.(-128 -- 127)

let string_fixed n = QCheck2.Gen.(string_size (pure n))

let bytes_gen = QCheck2.Gen.(map Bytes.of_string string)

let small_bytes_gen =
  QCheck2.Gen.(map Bytes.of_string @@ small_string ~gen:char)

let bytes_fixed_gen size = QCheck2.Gen.map Bytes.of_string (string_fixed size)

let sublist : 'a list -> 'a list QCheck2.Gen.t =
  (* [take_n n l] returns the first [n] elements of [l].
     We do not reuse the implementation from [Stdlib.TzList] to avoid a
     dependency cycle. *)
  let rec take_n n = function
    | x :: xs when n > 0 -> x :: take_n (n - 1) xs
    | _ -> []
  in
  fun elems ->
    let open QCheck2.Gen in
    match elems with
    | [] -> return []
    | _ ->
        let* res_len = 0 -- List.length elems in
        let+ shuffle = shuffle_l elems in
        take_n res_len shuffle

let holey (l : 'a list) : 'a list QCheck2.Gen.t =
  let open QCheck2.Gen in
  (* Generate as many Booleans as there are elements in [l] *)
  let+ bools = list_repeat (List.length l) bool in
  let rev_result =
    List.fold_left
      (fun acc (elem, pick) -> if pick then elem :: acc else acc)
      []
      (List.combine l bools)
  in
  List.rev rev_result

let rec of_option_gen gen =
  let open QCheck2.Gen in
  gen >>= function None -> of_option_gen gen | Some a -> return a

let endpoint_gen =
  let open QCheck2 in
  let open Gen in
  let protocol_gen = oneofl ["http"; "https"] in
  let path_gen =
    (* Specify the characters to use, to have valid URLs *)
    let+ path_chunks =
      list_size (1 -- 8) (string_size ~gen:(char_range 'a' 'z') (1 -- 8))
    in
    String.concat "." path_chunks
  in
  let port_gen =
    let+ port = 1 -- 32768 in
    ":" ^ Int.to_string port
  in
  let url_string_gen =
    let+ protocol, path, opt_part =
      triple protocol_gen path_gen (opt port_gen)
    in
    String.concat "" [protocol; "://"; path; Option.value ~default:"" opt_part]
  in
  let+ s = url_string_gen in
  Uri.of_string s

module MakeMapGen (Map : sig
  type 'a t

  type key

  val of_seq : (key * 'a) Seq.t -> 'a t
end) =
struct
  open QCheck2

  let gen_of_size (size_gen : int Gen.t) (key_gen : Map.key Gen.t)
      (val_gen : 'v Gen.t) : 'v Map.t Gen.t =
    let open Gen in
    map
      (fun entries -> List.to_seq entries |> Map.of_seq)
      (list_size size_gen @@ pair key_gen val_gen)

  let gen (key_gen : Map.key Gen.t) (val_gen : 'v Gen.t) : 'v Map.t Gen.t =
    gen_of_size Gen.small_nat key_gen val_gen
end

let test_roundtrip ~count ~title ~gen ~eq encoding =
  let pp fmt x =
    Data_encoding.Json.construct encoding x
    |> Data_encoding.Json.to_string |> Format.pp_print_string fmt
  in
  let test rdt input =
    let output =
      try Roundtrip.make encoding rdt input
      with exn ->
        QCheck2.Test.fail_reportf
          "%s %s roundtrip error: error %s on %a"
          title
          (Roundtrip.target rdt)
          (Printexc.to_string exn)
          pp
          input
    in
    let success = eq input output in
    if not success then
      QCheck2.Test.fail_reportf
        "%s %s roundtrip error: %a became %a"
        title
        (Roundtrip.target rdt)
        pp
        input
        pp
        output
  in
  QCheck2.Test.make
    ~count
    ~name:(Format.asprintf "roundtrip %s" title)
    gen
    (fun input ->
      test Roundtrip.binary input ;
      test Roundtrip.json input ;
      true)

let test_roundtrip_through_binary ~count ~title ~gen ~eq encoding1 encoding2 =
  let pp fmt x =
    Data_encoding.Json.construct encoding1 x
    |> Data_encoding.Json.to_string |> Format.pp_print_string fmt
  in
  let test encoding1 encoding2 rdt input =
    let output =
      try Roundtrip.make_with_2_encoding encoding1 encoding2 rdt input
      with exn ->
        QCheck2.Test.fail_reportf
          "%s %s roundtrip error: error %s on %a"
          title
          (Roundtrip.target rdt)
          (Printexc.to_string exn)
          pp
          input
    in
    let success = eq input output in
    if not success then
      QCheck2.Test.fail_reportf
        "%s %s roundtrip error: %a became %a"
        title
        (Roundtrip.target rdt)
        pp
        input
        pp
        output
  in
  QCheck2.Test.make
    ~count
    ~name:(Format.asprintf "roundtrip through binary %s" title)
    gen
    (fun input ->
      test encoding1 encoding2 Roundtrip.binary input ;
      test encoding2 encoding1 Roundtrip.binary input ;
      true)
