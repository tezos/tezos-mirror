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

let rng n = Random.State.make [|231441321234231; n|]

module Commons = struct
  type 'a t = 'a encoding * 'a compact

  type test_case = Test : 'a t * 'a Seq.t -> test_case

  let unit : unit t = (Encoding.unit, Compact.unit)

  let bool : bool t = (Encoding.bool, Compact.bool)

  let int32 : int32 t = (Encoding.int32, Compact.int32)

  let int64 : int64 t = (Encoding.int64, Compact.int64)

  let ground_cases =
    [
      Test (unit, Seq.return ());
      Test (bool, Array.to_seq [|true; false|]);
      Test
        ( int32,
          Seq.append
            (Array.to_seq [|0l; 1l; 1341234l; Int32.max_int; Int32.min_int|])
            (let rng = rng 32 in
             Seq.unfold
               (fun neg ->
                 let i32 = Random.State.int32 rng Int32.max_int in
                 let i32 = if neg then Int32.neg i32 else i32 in
                 Some (i32, not neg))
               false) );
      Test
        ( int64,
          Seq.append
            (Array.to_seq [|0L; 1L; 1341234L; Int64.max_int; Int64.min_int|])
            (let rng = rng 64 in
             Seq.unfold
               (fun neg ->
                 let i64 = Random.State.int64 rng Int64.max_int in
                 let i64 = if neg then Int64.neg i64 else i64 in
                 Some (i64, not neg))
               false) );
    ]

  let option : 'a t -> 'a option t =
   fun (e, c) -> (Encoding.option e, Compact.option c)

  let comb1 c =
    List.concat_map
      (fun (Test (((vanilla, _) as e), vs)) ->
        if Data_encoding__Encoding.is_nullable vanilla then []
        else
          [
            Test (option e, Seq.return None);
            Test (option e, Seq.map Option.some vs);
          ])
      c

  let all_cases = ground_cases @ comb1 ground_cases
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

let rec trunc n s () =
  if n <= 0 then Seq.Nil
  else
    match s () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (v, s) -> Seq.Cons (v, trunc (pred n) s)

let test () =
  List.iter
    (fun (Commons.Test (common, vs)) -> Seq.iter (test common) (trunc 1000 vs))
    Commons.all_cases

let () =
  Alcotest.run
    "compact-json"
    [("identical to vanilla", [("identical success", `Quick, test)])]
