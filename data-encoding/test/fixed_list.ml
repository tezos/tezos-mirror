(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let range n = List.init n Fun.id

let test_roundtrip len () =
  let open Data_encoding in
  let encoding = Fixed.list len int31 in
  let data = range len in
  assert (List.length data = len) ;
  let blob = Data_encoding.Binary.to_string_exn encoding data in
  let data_out = Data_encoding.Binary.of_string_exn encoding blob in
  assert (List.compare_lengths data data_out = 0) ;
  assert (List.for_all2 Int.equal data data_out)

let test_invalid_argument () =
  (match Data_encoding.Fixed.list 0 Data_encoding.int31 with
  | _ -> assert false
  | exception Invalid_argument _ -> ()) ;
  (match Data_encoding.Fixed.list (-1) Data_encoding.int31 with
  | _ -> assert false
  | exception Invalid_argument _ -> ()) ;
  (match Data_encoding.(Fixed.list 1 (Variable.array int31)) with
  | _ -> assert false
  | exception Invalid_argument _ -> ()) ;
  (match Data_encoding.Fixed.list 1 Data_encoding.unit with
  | _ -> assert false
  | exception Invalid_argument _ -> ()) ;
  (match
     Data_encoding.(Fixed.list 1 (obj1 (opt "zeroable" (Variable.array int31))))
   with
  | _ -> assert false
  | exception Invalid_argument _ -> ()) ;
  ()

let test_list_too_long len () =
  let open Data_encoding in
  let encoding = Fixed.list len int31 in
  let encodinglong = Fixed.list (len + 1) int31 in
  let encodingshort = Fixed.list (len - 1) int31 in
  let data = range len in
  assert (List.length data = len) ;
  (match Data_encoding.Binary.to_string_exn encodinglong data with
  | _ -> assert false
  | exception Data_encoding.Binary.Write_error List_invalid_length -> ()) ;
  (match Data_encoding.Binary.to_string_exn encodingshort data with
  | _ -> assert false
  | exception Data_encoding.Binary.Write_error List_invalid_length -> ()) ;
  let blob = Data_encoding.Binary.to_string_exn encoding data in
  (match Data_encoding.Binary.of_string_exn encodinglong blob with
  | _ -> assert false
  | exception Data_encoding.Binary.Read_error Not_enough_data -> ()) ;
  (match Data_encoding.Binary.of_string_exn encodingshort blob with
  | _ -> assert false
  | exception Data_encoding.Binary.Read_error Extra_bytes -> ()) ;
  ()

let tests =
  [
    ("roundtrip1", `Quick, test_roundtrip 1);
    ("roundtrip2", `Quick, test_roundtrip 2);
    ("roundtrip19", `Quick, test_roundtrip 19);
    ("roundtrip1232", `Quick, test_roundtrip 1232);
    ("invalid_arg", `Quick, test_invalid_argument);
    ("too-long 10", `Quick, test_list_too_long 10);
    ("too-long 120", `Quick, test_list_too_long 120);
  ]
