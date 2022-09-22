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

type mu = Cons of (int * mu) | Stop

let dump encoding =
  let open Data_encoding in
  let d = Binary.describe encoding in
  Format.printf "%a\n" Binary_schema.pp d ;
  Format.printf "%a\n" Json.pp (Json.construct Binary_schema.encoding d) ;
  ()

let%expect_test _ =
  dump Data_encoding.empty ;
  [%expect
    {|
      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel":
           { "fields":
               [ { "layout": { "kind": "Zero_width" }, "kind": "anon",
                   "data_kind": { "size": 0, "kind": "Fixed" } } ] },
         "fields": [] } |}] ;
  dump Data_encoding.(uint8) ;
  [%expect
    {|
      +-----------------+--------+------------------------+
      | Name            | Size   | Contents               |
      +=================+========+========================+
      | Unnamed field 0 | 1 byte | unsigned 8-bit integer |
      +-----------------+--------+------------------------+



      { "toplevel":
           { "fields":
               [ { "layout": { "size": "Uint8", "kind": "Int" }, "kind": "anon",
                   "data_kind": { "size": 1, "kind": "Fixed" } } ] },
         "fields": [] } |}] ;
  dump Data_encoding.(Fixed.string 12) ;
  [%expect
    {|
    +-----------------+----------+----------+
    | Name            | Size     | Contents |
    +=================+==========+==========+
    | Unnamed field 0 | 12 bytes | bytes    |
    +-----------------+----------+----------+



    { "toplevel":
         { "fields":
             [ { "layout": { "kind": "String" }, "kind": "anon",
                 "data_kind": { "size": 12, "kind": "Fixed" } } ] },
       "fields": [] } |}] ;
  dump
    Data_encoding.(
      def "this" ~title:"titl" ~description:"des" (Bounded.string 22)) ;
  [%expect
    {|
    +-----------------------+----------+------------------------+
    | Name                  | Size     | Contents               |
    +=======================+==========+========================+
    | # bytes in next field | 1 byte   | unsigned 8-bit integer |
    +-----------------------+----------+------------------------+
    | Unnamed field 0       | Variable | bytes                  |
    +-----------------------+----------+------------------------+



    { "toplevel":
         { "fields":
             [ { "kind": "dyn", "num_fields": 1, "size": "Uint8" },
               { "layout": { "kind": "String" }, "kind": "anon",
                 "data_kind": { "kind": "Variable" } } ] }, "fields": [] } |}] ;
  dump Data_encoding.(constant "foo") ;
  [%expect
    {|
      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel":
           { "fields":
               [ { "layout": { "kind": "Zero_width" }, "kind": "anon",
                   "data_kind": { "size": 0, "kind": "Fixed" } } ] },
         "fields": [] } |}] ;
  dump Data_encoding.(tup1 (constant "foo")) ;
  [%expect
    {|
      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel": { "fields": [] }, "fields": [] } |}] ;
  dump Data_encoding.(tup1 (tup1 (tup1 uint8))) ;
  [%expect
    {|
      +-----------------+--------+------------------------+
      | Name            | Size   | Contents               |
      +=================+========+========================+
      | Unnamed field 0 | 1 byte | unsigned 8-bit integer |
      +-----------------+--------+------------------------+



      { "toplevel":
           { "fields":
               [ { "layout": { "size": "Uint8", "kind": "Int" }, "kind": "anon",
                   "data_kind": { "size": 1, "kind": "Fixed" } } ] },
         "fields": [] } |}] ;
  let tup2_zero_width =
    Data_encoding.(tup2 (constant "foo") (constant "bar"))
  in
  dump tup2_zero_width ;
  [%expect
    {|
      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel": { "fields": [] }, "fields": [] } |}] ;
  dump (Data_encoding.Fixed.add_padding tup2_zero_width 12) ;
  [%expect
    {|
      +---------+----------+----------+
      | Name    | Size     | Contents |
      +=========+==========+==========+
      | padding | 12 bytes | padding  |
      +---------+----------+----------+



      { "toplevel":
           { "fields":
               [ { "name": "padding", "layout": { "kind": "Padding" },
                   "data_kind": { "size": 12, "kind": "Fixed" }, "kind": "named" } ] },
         "fields": [] } |}] ;
  dump (Data_encoding.option tup2_zero_width) ;
  [%expect
    {|
      +-----------------+--------+----------+
      | Name            | Size   | Contents |
      +=================+========+==========+
      | Unnamed field 0 | 1 byte | $X_0     |
      +-----------------+--------+----------+


      X_0 (1 byte, 8-bit tag) ***********************

      None (tag 0)
      ============

      +------+--------+------------------------+
      | Name | Size   | Contents               |
      +======+========+========================+
      | Tag  | 1 byte | unsigned 8-bit integer |
      +------+--------+------------------------+


      Some (tag 1)
      ============

      +------+--------+------------------------+
      | Name | Size   | Contents               |
      +======+========+========================+
      | Tag  | 1 byte | unsigned 8-bit integer |
      +------+--------+------------------------+


      { "toplevel":
           { "fields":
               [ { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "anon",
                   "data_kind": { "size": 1, "kind": "Fixed" } } ] },
         "fields":
           [ { "description": { "title": "X_0" },
               "encoding":
                 { "tag_size": "Uint8", "kind": { "size": 1, "kind": "Fixed" },
                   "cases":
                     [ { "tag": 0,
                         "fields":
                           [ { "name": "Tag",
                               "layout": { "size": "Uint8", "kind": "Int" },
                               "data_kind": { "size": 1, "kind": "Fixed" },
                               "kind": "named" },
                             { "layout": { "kind": "Zero_width" }, "kind": "anon",
                               "data_kind": { "size": 0, "kind": "Fixed" } } ],
                         "name": "None" },
                       { "tag": 1,
                         "fields":
                           [ { "name": "Tag",
                               "layout": { "size": "Uint8", "kind": "Int" },
                               "data_kind": { "size": 1, "kind": "Fixed" },
                               "kind": "named" } ], "name": "Some" } ] } } ] } |}] ;
  dump Data_encoding.(obj2 (req "foo" tup2_zero_width) (req "bar" uint8)) ;
  [%expect
    {|
      +------+---------+------------------------+
      | Name | Size    | Contents               |
      +======+=========+========================+
      | foo  | 0 bytes | $X_0                   |
      +------+---------+------------------------+
      | bar  | 1 byte  | unsigned 8-bit integer |
      +------+---------+------------------------+


      X_0
      ***

      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel":
          { "fields":
              [ { "name": "foo", "layout": { "name": "X_0", "kind": "Ref" },
                  "data_kind": { "size": 0, "kind": "Fixed" }, "kind": "named" },
                { "name": "bar", "layout": { "size": "Uint8", "kind": "Int" },
                  "data_kind": { "size": 1, "kind": "Fixed" }, "kind": "named" } ] },
        "fields":
          [ { "description": { "title": "X_0" }, "encoding": { "fields": [] } } ] } |}] ;
  let obj2_zero_width =
    Data_encoding.(
      obj2 (req "l" (constant "left")) (req "r" (constant "right")))
  in
  dump obj2_zero_width ;
  [%expect
    {|
      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel": { "fields": [] }, "fields": [] } |}] ;
  dump (Data_encoding.option obj2_zero_width) ;
  [%expect
    {|
      +-----------------+--------+----------+
      | Name            | Size   | Contents |
      +=================+========+==========+
      | Unnamed field 0 | 1 byte | $X_0     |
      +-----------------+--------+----------+


      X_0 (1 byte, 8-bit tag) ***********************

      None (tag 0)
      ============

      +------+--------+------------------------+
      | Name | Size   | Contents               |
      +======+========+========================+
      | Tag  | 1 byte | unsigned 8-bit integer |
      +------+--------+------------------------+


      Some (tag 1)
      ============

      +------+--------+------------------------+
      | Name | Size   | Contents               |
      +======+========+========================+
      | Tag  | 1 byte | unsigned 8-bit integer |
      +------+--------+------------------------+


      { "toplevel":
           { "fields":
               [ { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "anon",
                   "data_kind": { "size": 1, "kind": "Fixed" } } ] },
         "fields":
           [ { "description": { "title": "X_0" },
               "encoding":
                 { "tag_size": "Uint8", "kind": { "size": 1, "kind": "Fixed" },
                   "cases":
                     [ { "tag": 0,
                         "fields":
                           [ { "name": "Tag",
                               "layout": { "size": "Uint8", "kind": "Int" },
                               "data_kind": { "size": 1, "kind": "Fixed" },
                               "kind": "named" },
                             { "layout": { "kind": "Zero_width" }, "kind": "anon",
                               "data_kind": { "size": 0, "kind": "Fixed" } } ],
                         "name": "None" },
                       { "tag": 1,
                         "fields":
                           [ { "name": "Tag",
                               "layout": { "size": "Uint8", "kind": "Int" },
                               "data_kind": { "size": 1, "kind": "Fixed" },
                               "kind": "named" } ], "name": "Some" } ] } } ] } |}] ;
  dump Data_encoding.(tup2 obj2_zero_width string) ;
  [%expect
    {|
      +-----------------+----------------------+----------+
      | Name            | Size                 | Contents |
      +=================+======================+==========+
      | Unnamed field 0 | Determined from data | $X_1     |
      +-----------------+----------------------+----------+


      X_0
      ***

      This value's binary representation is empty. It takes zero (0) bytes of output.
      X_1
      ***

      +-----------------------+----------+-------------------------+
      | Name                  | Size     | Contents                |
      +=======================+==========+=========================+
      | # bytes in next field | 4 bytes  | unsigned 30-bit integer |
      +-----------------------+----------+-------------------------+
      | Unnamed field 0       | Variable | bytes                   |
      +-----------------------+----------+-------------------------+


      { "toplevel":
           { "fields":
               [ { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "anon",
                   "data_kind": { "size": 0, "kind": "Fixed" } },
                 { "layout": { "name": "X_1", "kind": "Ref" }, "kind": "anon",
                   "data_kind": { "kind": "Dynamic" } } ] },
         "fields":
           [ { "description": { "title": "X_0" }, "encoding": { "fields": [] } },
             { "description": { "title": "X_1" },
               "encoding":
                 { "fields":
                     [ { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
                       { "layout": { "kind": "String" }, "kind": "anon",
                         "data_kind": { "kind": "Variable" } } ] } } ] } |}] ;
  dump
    Data_encoding.(def "thorn" (check_size 100 (check_size 10 obj2_zero_width))) ;
  [%expect
    {|
      This value's binary representation is empty. It takes zero (0) bytes of output.

      { "toplevel": { "fields": [] }, "fields": [] } |}] ;
  dump Data_encoding.(Fixed.(add_padding (tup2 (string 5) (bytes 10)) 4)) ;
  [%expect
    {|
      +-----------------+----------+----------+
      | Name            | Size     | Contents |
      +=================+==========+==========+
      | Unnamed field 0 | 5 bytes  | bytes    |
      +-----------------+----------+----------+
      | Unnamed field 1 | 10 bytes | bytes    |
      +-----------------+----------+----------+
      | padding         | 4 bytes  | padding  |
      +-----------------+----------+----------+



      { "toplevel":
           { "fields":
               [ { "layout": { "kind": "String" }, "kind": "anon",
                   "data_kind": { "size": 5, "kind": "Fixed" } },
                 { "layout": { "kind": "Bytes" }, "kind": "anon",
                   "data_kind": { "size": 10, "kind": "Fixed" } },
                 { "name": "padding", "layout": { "kind": "Padding" },
                   "data_kind": { "size": 4, "kind": "Fixed" }, "kind": "named" } ] },
         "fields": [] } |}] ;
  dump
    Data_encoding.(
      obj3 (req "a" uint8) (opt "b" uint16) (req "c" (list string))) ;
  [%expect
    {|
    +-------------------------+----------+-------------------------------------+
    | Name                    | Size     | Contents                            |
    +=========================+==========+=====================================+
    | a                       | 1 byte   | unsigned 8-bit integer              |
    +-------------------------+----------+-------------------------------------+
    | ? presence of field "b" | 1 byte   | boolean (0 for false, 255 for true) |
    +-------------------------+----------+-------------------------------------+
    | b                       | 2 bytes  | unsigned 16-bit integer             |
    +-------------------------+----------+-------------------------------------+
    | # bytes in next field   | 4 bytes  | unsigned 30-bit integer             |
    +-------------------------+----------+-------------------------------------+
    | c                       | Variable | sequence of $X_0                    |
    +-------------------------+----------+-------------------------------------+


    X_0
    ***

    +-----------------------+----------+-------------------------+
    | Name                  | Size     | Contents                |
    +=======================+==========+=========================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit integer |
    +-----------------------+----------+-------------------------+
    | Unnamed field 0       | Variable | bytes                   |
    +-----------------------+----------+-------------------------+


    { "toplevel":
         { "fields":
             [ { "name": "a", "layout": { "size": "Uint8", "kind": "Int" },
                 "data_kind": { "size": 1, "kind": "Fixed" }, "kind": "named" },
               { "kind": "option_indicator", "name": "b" },
               { "name": "b", "layout": { "size": "Uint16", "kind": "Int" },
                 "data_kind": { "size": 2, "kind": "Fixed" }, "kind": "named" },
               { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
               { "name": "c",
                 "layout":
                   { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "Seq" },
                 "data_kind": { "kind": "Variable" }, "kind": "named" } ] },
       "fields":
         [ { "description": { "title": "X_0" },
             "encoding":
               { "fields":
                   [ { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
                     { "layout": { "kind": "String" }, "kind": "anon",
                       "data_kind": { "kind": "Variable" } } ] } } ] } |}] ;
  dump
    Data_encoding.(union [case ~title:"a" (Tag 128) string Option.some Fun.id]) ;
  [%expect
    {|
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_0     |
    +-----------------+----------------------+----------+


    X_0 (Determined from data, 8-bit tag)
    *************************************

    a (tag 128)
    ===========

    +-----------------------+----------+-------------------------+
    | Name                  | Size     | Contents                |
    +=======================+==========+=========================+
    | Tag                   | 1 byte   | unsigned 8-bit integer  |
    +-----------------------+----------+-------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit integer |
    +-----------------------+----------+-------------------------+
    | Unnamed field 0       | Variable | bytes                   |
    +-----------------------+----------+-------------------------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "anon",
                 "data_kind": { "kind": "Dynamic" } } ] },
       "fields":
         [ { "description": { "title": "X_0" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 128,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
                           { "layout": { "kind": "String" }, "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "a" } ] } } ] } |}] ;
  dump
    Data_encoding.(
      union
        [
          case ~title:"l" (Tag 128) uint8 Either.find_left Either.left;
          case ~title:"r" (Tag 255) string Either.find_right Either.right;
        ]) ;
  [%expect
    {|
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_0     |
    +-----------------+----------------------+----------+


    X_0 (Determined from data, 8-bit tag)
    *************************************

    l (tag 128)
    ===========

    +-----------------+--------+------------------------+
    | Name            | Size   | Contents               |
    +=================+========+========================+
    | Tag             | 1 byte | unsigned 8-bit integer |
    +-----------------+--------+------------------------+
    | Unnamed field 0 | 1 byte | unsigned 8-bit integer |
    +-----------------+--------+------------------------+


    r (tag 255)
    ===========

    +-----------------------+----------+-------------------------+
    | Name                  | Size     | Contents                |
    +=======================+==========+=========================+
    | Tag                   | 1 byte   | unsigned 8-bit integer  |
    +-----------------------+----------+-------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit integer |
    +-----------------------+----------+-------------------------+
    | Unnamed field 0       | Variable | bytes                   |
    +-----------------------+----------+-------------------------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "anon",
                 "data_kind": { "kind": "Dynamic" } } ] },
       "fields":
         [ { "description": { "title": "X_0" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 128,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } } ],
                       "name": "l" },
                     { "tag": 255,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
                           { "layout": { "kind": "String" }, "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "r" } ] } } ] } |}] ;
  dump
    Data_encoding.(
      mu "weird-list" (fun e ->
          union
            [
              case
                ~title:"c"
                (Tag 128)
                (tup3 obj2_zero_width uint8 e)
                (function Cons (a, m) -> Some (((), ()), a, m) | _ -> None)
                (fun (((), ()), a, m) -> Cons (a, m));
              case
                ~title:"s"
                (Tag 255)
                null
                (function Stop -> Some () | _ -> None)
                (fun () -> Stop);
            ])) ;
  [%expect
    {|
    +-----------------+----------------------+-------------+
    | Name            | Size                 | Contents    |
    +=================+======================+=============+
    | Unnamed field 0 | Determined from data | $weird-list |
    +-----------------+----------------------+-------------+


    X_0
    ***

    This value's binary representation is empty. It takes zero (0) bytes of output.
    weird-list (Determined from data, 8-bit tag)
    ********************************************

    c (tag 128)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weird-list            |
    +-----------------+----------------------+------------------------+


    s (tag 255)
    ===========

    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "weird-list", "kind": "Ref" },
                 "kind": "anon", "data_kind": { "kind": "Dynamic" } } ] },
       "fields":
         [ { "description": { "title": "X_0" }, "encoding": { "fields": [] } },
           { "description": { "title": "weird-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 128,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "X_0", "kind": "Ref" },
                             "kind": "anon",
                             "data_kind": { "size": 0, "kind": "Fixed" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weird-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c" },
                     { "tag": 255,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "kind": "Zero_width" }, "kind": "anon",
                             "data_kind": { "size": 0, "kind": "Fixed" } } ],
                       "name": "s" } ] } } ] } |}] ;
  let obj2_opt_zero =
    Data_encoding.(
      obj2 (opt "l" (constant "left")) (opt "r" (constant "right")))
  in
  dump obj2_opt_zero ;
  [%expect
    {|
      +-------------------------+---------+-------------------------------------+
      | Name                    | Size    | Contents                            |
      +=========================+=========+=====================================+
      | ? presence of field "l" | 1 byte  | boolean (0 for false, 255 for true) |
      +-------------------------+---------+-------------------------------------+
      | l                       | 0 bytes |                                     |
      +-------------------------+---------+-------------------------------------+
      | ? presence of field "r" | 1 byte  | boolean (0 for false, 255 for true) |
      +-------------------------+---------+-------------------------------------+
      | r                       | 0 bytes |                                     |
      +-------------------------+---------+-------------------------------------+



      { "toplevel":
           { "fields":
               [ { "kind": "option_indicator", "name": "l" },
                 { "name": "l", "layout": { "kind": "Zero_width" },
                   "data_kind": { "size": 0, "kind": "Fixed" }, "kind": "named" },
                 { "kind": "option_indicator", "name": "r" },
                 { "name": "r", "layout": { "kind": "Zero_width" },
                   "data_kind": { "size": 0, "kind": "Fixed" }, "kind": "named" } ] },
         "fields": [] } |}] ;
  ()
