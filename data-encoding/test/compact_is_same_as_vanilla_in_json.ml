(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Data_encoding

let rng n = Random.State.make [|231231; n|]

let rec self_zip acc = function
  | [] | [_] -> acc
  | x :: y :: zs -> self_zip ((x, y) :: acc) zs

let rec seq_unfold f x
    (* for compatibility with some OCaml versions, we use our own version of
       Seq.unfold *)
      () =
  match f x with
  | None -> Seq.Nil
  | Some (elt, x) -> Seq.Cons (elt, seq_unfold f x)

let rec seq_append xs ys
    (* for compatibility with some OCaml versions, we use our own version of
       Seq.append *)
      () =
  match xs () with
  | Seq.Cons (x, xs) -> Seq.Cons (x, seq_append xs ys)
  | Seq.Nil -> ys ()

let seq_of_random seed neg mk bound =
  let rng = rng seed in
  seq_unfold
    (fun flip ->
      let i = mk rng bound in
      let i = if flip then neg i else i in
      Some (i, not flip))
    false

let rec seq_alternate s1 s2 () =
  match s1 () with
  | Seq.Nil -> s2 ()
  | Seq.Cons (v, s1) -> Seq.Cons (v, seq_alternate s2 s1)

let rec trunc_exc n s () =
  if n <= 0 then Seq.Nil
  else
    match s () with
    | Seq.Nil -> raise Exit
    | Seq.Cons (v, s) -> Seq.Cons (v, trunc_exc (pred n) s)

let rec trunc n s () =
  if n <= 0 then Seq.Nil
  else
    match s () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (v, s) -> Seq.Cons (v, trunc (pred n) s)

let rec seq_zip s1 s2 () =
  match (s1 (), s2 ()) with
  | Seq.Nil, _ | _, Seq.Nil -> Seq.Nil
  | Seq.Cons (v1, s1), Seq.Cons (v2, s2) -> Seq.Cons ((v1, v2), seq_zip s1 s2)

module Commons = struct
  type 'a t = 'a encoding * 'a compact

  type test_case = Test : 'a t * 'a Seq.t -> test_case

  let unit : unit t = (Encoding.unit, Compact.unit)

  let null : unit t = (Encoding.null, Compact.null)

  let bool : bool t = (Encoding.bool, Compact.bool)

  let int32 : int32 t = (Encoding.int32, Compact.int32)

  let int64 : int64 t = (Encoding.int64, Compact.int64)

  let option : 'a t -> 'a option t =
   fun (e, c) -> (Encoding.option e, Compact.option c)

  let tup1 : 'a t -> 'a t = fun (e, c) -> (Encoding.tup1 e, Compact.tup1 c)

  let obj1 : string -> 'a t -> 'a t =
   fun f (e, c) -> (Encoding.(obj1 (req f e)), Compact.(obj1 (req f c)))

  let list : int -> 'a t -> 'a list t =
   fun bits (e, c) ->
    (Encoding.list e, Compact.(list ~bits (make ~tag_size:`Uint16 c)))

  let tup2 : 'a t -> 'b t -> ('a * 'b) t =
   fun (e1, c1) (e2, c2) -> (Encoding.tup2 e1 e2, Compact.tup2 c1 c2)

  let obj2 : string -> 'a t -> string -> 'b t -> ('a * 'b) t =
   fun f1 (e1, c1) f2 (e2, c2) ->
    ( Encoding.(obj2 (req f1 e1) (req f2 e2)),
      Compact.(obj2 (req f1 c1) (req f2 c2)) )

  (* we only test two-variant union *)
  let either : 'a t -> 'b t -> ('a, 'b) Either.t t =
   fun (ea, ca) (eb, cb) ->
    ( Encoding.(
        union
          [
            case ~title:"left" Json_only ea Either.find_left Either.left;
            case ~title:"right" Json_only eb Either.find_right Either.right;
          ]),
      Compact.(
        union
          [
            case ~title:"left" ca Either.find_left Either.left;
            case ~title:"right" cb Either.find_right Either.right;
          ]) )

  let ground_cases =
    [
      Test (unit, Seq.return ());
      Test (null, Seq.return ());
      Test (bool, Array.to_seq [|true; false|]);
      Test
        ( int32,
          seq_append
            (Array.to_seq [|0l; 1l; 1341234l; Int32.max_int; Int32.min_int|])
            (seq_of_random 32 Int32.neg Random.State.int32 Int32.max_int) );
      Test
        ( int64,
          seq_append
            (Array.to_seq [|0L; 1L; 1341234L; Int64.max_int; Int64.min_int|])
            (seq_of_random 64 Int64.neg Random.State.int64 Int64.max_int) );
    ]

  let comb1 c =
    List.concat_map
      (fun (Test (((vanilla, _) as e), vs)) ->
        (if Data_encoding__Encoding.is_nullable vanilla then []
        else
          [
            Test (option e, Seq.return None);
            Test (option e, Seq.map Option.some vs);
          ])
        @ [
            Test (tup1 e, vs);
            Test (obj1 "lol" e, vs);
            Test
              ( either e e,
                seq_alternate (Seq.map Either.left vs) (Seq.map Either.right vs)
              );
          ]
        @
        let seq_of_lists_of_seq s =
          seq_unfold
            (fun n ->
              match List.of_seq (trunc_exc n s) with
              | exception Exit -> None
              | v -> Some (v, n + 1))
            0
        in
        if
          Data_encoding__Encoding.classify vanilla = `Variable
          || Data_encoding__Encoding.is_zeroable vanilla
        then []
        else
          [
            Test (list 0 e, seq_of_lists_of_seq vs);
            Test (list 1 e, seq_of_lists_of_seq vs);
            Test (list 2 e, seq_of_lists_of_seq vs);
            Test (list 3 e, seq_of_lists_of_seq vs);
            Test (list 4 e, seq_of_lists_of_seq vs);
          ])
      c

  let comb2 cs =
    List.concat_map
      (fun (Test (e1, vs1), Test (e2, vs2)) ->
        [
          Test (tup2 e1 e2, seq_zip vs1 vs2);
          Test (obj2 "lol" e1 "foo" e2, seq_zip vs1 vs2);
          Test
            ( either e1 e2,
              seq_alternate (Seq.map Either.left vs1) (Seq.map Either.right vs2)
            );
        ])
      (self_zip [] cs)

  let all_cases =
    let cs = ground_cases in
    let cs = List.rev_append cs (comb1 cs) in
    let cs = List.rev_append cs (comb2 cs) in
    let cs = List.rev_append cs (comb1 cs) in
    let cs = List.rev_append cs (comb2 cs) in
    cs
end

let test (enc, cmpct) v =
  let json_vanilla = Json.construct enc v in
  let enccmpct = Compact.make ~tag_size:`Uint16 cmpct in
  let json_cmpct = Json.construct enccmpct v in
  if json_vanilla = json_cmpct then ()
  else
    Format.kasprintf
      failwith
      "Disinct JSONs:\nvanilla:\t%a\ncompact:\t%a\n"
      Json.pp
      json_vanilla
      Json.pp
      json_cmpct

let test () =
  List.iter
    (fun (Commons.Test (common, vs)) -> Seq.iter (test common) (trunc 50 vs))
    Commons.all_cases

let () =
  Alcotest.run
    "compact-json"
    [("identical to vanilla", [("identical success", `Quick, test)])]
