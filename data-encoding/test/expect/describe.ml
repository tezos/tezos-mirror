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

type mu =
  | Cons of (int * bu)
  | Snoc of (mu * int)
  | Snocc of (mu * int * int)
  | Stop

and bu = Consb of (int * mu) | Consbb of (int * bu)

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

      { "toplevel": { "fields": [] }, "fields": [] } |}] ;
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

      { "toplevel": { "fields": [] }, "fields": [] } |}] ;
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
                               "kind": "named" } ], "name": "None" },
                       { "tag": 1,
                         "fields":
                           [ { "name": "Tag",
                               "layout": { "size": "Uint8", "kind": "Int" },
                               "data_kind": { "size": 1, "kind": "Fixed" },
                               "kind": "named" } ], "name": "Some" } ] } } ] } |}] ;
  dump Data_encoding.(obj2 (req "foo" tup2_zero_width) (req "bar" uint8)) ;
  [%expect
    {|
      +------+--------+------------------------+
      | Name | Size   | Contents               |
      +======+========+========================+
      | bar  | 1 byte | unsigned 8-bit integer |
      +------+--------+------------------------+



      { "toplevel":
           { "fields":
               [ { "name": "bar", "layout": { "size": "Uint8", "kind": "Int" },
                   "data_kind": { "size": 1, "kind": "Fixed" }, "kind": "named" } ] },
         "fields": [] } |}] ;
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
                               "kind": "named" } ], "name": "None" },
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


      X_1
      ***

      +-----------------------+----------+------------------------------------+
      | Name                  | Size     | Contents                           |
      +=======================+==========+====================================+
      | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
      +-----------------------+----------+------------------------------------+
      | Unnamed field 0       | Variable | bytes                              |
      +-----------------------+----------+------------------------------------+


      { "toplevel":
           { "fields":
               [ { "layout": { "name": "X_1", "kind": "Ref" }, "kind": "anon",
                   "data_kind": { "kind": "Dynamic" } } ] },
         "fields":
           [ { "description": { "title": "X_1" },
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
    | b                       | 2 bytes  | unsigned 16-bit big-endian integer  |
    +-------------------------+----------+-------------------------------------+
    | # bytes in next field   | 4 bytes  | unsigned 30-bit big-endian integer  |
    +-------------------------+----------+-------------------------------------+
    | c                       | Variable | sequence of $X_0                    |
    +-------------------------+----------+-------------------------------------+


    X_0
    ***

    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+


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

    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+


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

    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+


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
      let bu mue =
        mu "weirb-list" (fun e ->
            union
              [
                case
                  ~title:"b"
                  (Tag 64)
                  (tup3 obj2_zero_width uint8 mue)
                  (function
                    | Consb (a, m) -> Some (((), ()), a, m) | Consbb _ -> None)
                  (fun (((), ()), a, m) -> Consb (a, m));
                case
                  ~title:"bb"
                  (Tag 32)
                  (tup2 uint8 e)
                  (function Consbb (a, m) -> Some (a, m) | Consb _ -> None)
                  (fun (a, m) -> Consbb (a, m));
              ])
      in
      mu "weird-list" (fun e ->
          union
            [
              case
                ~title:"c"
                (Tag 128)
                (tup3 obj2_zero_width uint8 (bu e))
                (function Cons (a, m) -> Some (((), ()), a, m) | _ -> None)
                (fun (((), ()), a, m) -> Cons (a, m));
              case
                ~title:"s"
                (Tag 127)
                (tup2 e uint8)
                (function Snoc (m, a) -> Some (m, a) | _ -> None)
                (fun (m, a) -> Snoc (m, a));
              case
                ~title:"t"
                (Tag 126)
                (tup3 e uint8 uint16)
                (function Snocc (m, a, b) -> Some (m, a, b) | _ -> None)
                (fun (m, a, b) -> Snocc (m, a, b));
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


    weirb-list (Determined from data, 8-bit tag)
    ********************************************

    bb (tag 32)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    b (tag 64)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weird-list            |
    +-----------------+----------------------+------------------------+


    weird-list (Determined from data, 8-bit tag)
    ********************************************

    t (tag 126)
    ===========

    +-----------------+----------------------+------------------------------------+
    | Name            | Size                 | Contents                           |
    +=================+======================+====================================+
    | Tag             | 1 byte               | unsigned 8-bit integer             |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 0 | Determined from data | $weird-list                        |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 1 | 1 byte               | unsigned 8-bit integer             |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 2 | 2 bytes              | unsigned 16-bit big-endian integer |
    +-----------------+----------------------+------------------------------------+


    s (tag 127)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weird-list            |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+


    c (tag 128)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirb-list            |
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
         [ { "description": { "title": "weirb-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 32,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "bb" },
                     { "tag": 64,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weird-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "b" } ] } },
           { "description": { "title": "weird-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 126,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weird-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "size": "Uint16", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 2, "kind": "Fixed" } } ],
                       "name": "t" },
                     { "tag": 127,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weird-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } } ],
                       "name": "s" },
                     { "tag": 128,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c" },
                     { "tag": 255,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" } ], "name": "s" } ] } } ] } |}] ;
  dump
    Data_encoding.(
      let bu mue =
        mu "weirb-list" (fun e ->
            union
              [
                case
                  ~title:"b"
                  (Tag 64)
                  (tup3 obj2_zero_width uint8 mue)
                  (function
                    | Consb (a, m) -> Some (((), ()), a, m) | Consbb _ -> None)
                  (fun (((), ()), a, m) -> Consb (a, m));
                case
                  ~title:"bb"
                  (Tag 32)
                  (tup2 uint8 e)
                  (function Consbb (a, m) -> Some (a, m) | Consb _ -> None)
                  (fun (a, m) -> Consbb (a, m));
              ])
      in
      mu "weirder-list" (fun e ->
          Compact.(
            make
              ~tag_size:`Uint8
              (union
                 ~cases_tag_bits:3
                 [
                   case
                     ~title:"c0"
                     (payload Data_encoding.(tup2 obj2_zero_width (bu e)))
                     (function Cons (0, m) -> Some (((), ()), m) | _ -> None)
                     (fun (((), ()), m) -> Cons (0, m));
                   case
                     ~title:"c1"
                     (payload (bu e))
                     (function Cons (1, m) -> Some m | _ -> None)
                     (fun m -> Cons (1, m));
                   case
                     ~title:"c2"
                     (payload (bu e))
                     (function Cons (2, m) -> Some m | _ -> None)
                     (fun m -> Cons (2, m));
                   case
                     ~title:"c3"
                     (payload (bu e))
                     (function Cons (3, m) -> Some m | _ -> None)
                     (fun m -> Cons (3, m));
                   case
                     ~title:"c"
                     (tup2 (payload uint8) (payload (bu e)))
                     (function Cons (x, m) -> Some (x, m) | _ -> None)
                     (fun (x, m) -> Cons (x, m));
                   case
                     ~title:"s"
                     (tup2 (payload e) (payload uint8))
                     (function Snoc (m, a) -> Some (m, a) | _ -> None)
                     (fun (m, a) -> Snoc (m, a));
                   case
                     ~title:"t"
                     (tup3 (payload e) (payload uint8) (payload uint16))
                     (function Snocc (m, a, b) -> Some (m, a, b) | _ -> None)
                     (fun (m, a, b) -> Snocc (m, a, b));
                   case
                     ~title:"s"
                     null
                     (function Stop -> Some () | _ -> None)
                     (fun () -> Stop);
                 ])))) ;
  [%expect
    {|
    +-----------------+----------------------+---------------+
    | Name            | Size                 | Contents      |
    +=================+======================+===============+
    | Unnamed field 0 | Determined from data | $weirder-list |
    +-----------------+----------------------+---------------+


    weirb-list (Determined from data, 8-bit tag)
    ********************************************

    bb (tag 32)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    b (tag 64)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirder-list          |
    +-----------------+----------------------+------------------------+


    weirder-list (Determined from data, 8-bit tag)
    **********************************************

    c0 (tag 0)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c1 (tag 8)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c2 (tag 16)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c3 (tag 24)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c (tag 32)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    s (tag 40)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirder-list          |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+


    t (tag 48)
    ==========

    +-----------------+----------------------+------------------------------------+
    | Name            | Size                 | Contents                           |
    +=================+======================+====================================+
    | Tag             | 1 byte               | unsigned 8-bit integer             |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 0 | Determined from data | $weirder-list                      |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 1 | 1 byte               | unsigned 8-bit integer             |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 2 | 2 bytes              | unsigned 16-bit big-endian integer |
    +-----------------+----------------------+------------------------------------+


    s (tag 56)
    ==========

    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "weirder-list", "kind": "Ref" },
                 "kind": "anon", "data_kind": { "kind": "Dynamic" } } ] },
       "fields":
         [ { "description": { "title": "weirb-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 32,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "bb" },
                     { "tag": 64,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirder-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "b" } ] } },
           { "description": { "title": "weirder-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 0,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c0" },
                     { "tag": 8,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c1" },
                     { "tag": 16,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c2" },
                     { "tag": 24,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c3" },
                     { "tag": 32,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c" },
                     { "tag": 40,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirder-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } } ],
                       "name": "s" },
                     { "tag": 48,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirder-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "size": "Uint16", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 2, "kind": "Fixed" } } ],
                       "name": "t" },
                     { "tag": 56,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" } ], "name": "s" } ] } } ] } |}] ;
  dump
    Data_encoding.(
      let bu mue =
        mu "weirb-list" (fun e ->
            Compact.(
              make
                ~tag_size:`Uint8
                (union
                   ~cases_tag_bits:1
                   [
                     case
                       ~title:"b"
                       (tup2 (payload uint8) (payload mue))
                       (function
                         | Consb (a, m) -> Some (a, m) | Consbb _ -> None)
                       (function a, m -> Consb (a, m));
                     case
                       ~title:"bb"
                       (tup2 (payload uint8) (payload e))
                       (function
                         | Consbb (a, m) -> Some (a, m) | Consb _ -> None)
                       (fun (a, m) -> Consbb (a, m));
                   ])))
      in
      let mu =
        mu "weirder-list" (fun e ->
            Compact.(
              make
                ~tag_size:`Uint8
                (union
                   ~cases_tag_bits:3
                   [
                     case
                       ~title:"c0"
                       (payload Data_encoding.(tup2 obj2_zero_width (bu e)))
                       (function
                         | Cons (0, m) -> Some (((), ()), m) | _ -> None)
                       (fun (((), ()), m) -> Cons (0, m));
                     case
                       ~title:"c1"
                       (payload (bu e))
                       (function Cons (1, m) -> Some m | _ -> None)
                       (fun m -> Cons (1, m));
                     case
                       ~title:"c2"
                       (payload (bu e))
                       (function Cons (2, m) -> Some m | _ -> None)
                       (fun m -> Cons (2, m));
                     case
                       ~title:"c3"
                       (payload (bu e))
                       (function Cons (3, m) -> Some m | _ -> None)
                       (fun m -> Cons (3, m));
                     case
                       ~title:"c"
                       (tup2 (payload uint8) (payload (bu e)))
                       (function Cons (x, m) -> Some (x, m) | _ -> None)
                       (fun (x, m) -> Cons (x, m));
                     case
                       ~title:"s"
                       (tup2 (payload e) (payload uint8))
                       (function Snoc (m, a) -> Some (m, a) | _ -> None)
                       (fun (m, a) -> Snoc (m, a));
                     case
                       ~title:"t"
                       (tup3 (payload e) (payload uint8) (payload uint16))
                       (function
                         | Snocc (m, a, b) -> Some (m, a, b) | _ -> None)
                       (fun (m, a, b) -> Snocc (m, a, b));
                     case
                       ~title:"s"
                       null
                       (function Stop -> Some () | _ -> None)
                       (fun () -> Stop);
                   ])))
      in
      Compact.(make ~tag_size:`Uint8 (list ~bits:3 mu))) ;
  [%expect
    {|
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_49    |
    +-----------------+----------------------+----------+


    weirb-list (Determined from data, 8-bit tag)
    ********************************************

    b (tag 0)
    =========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirder-list          |
    +-----------------+----------------------+------------------------+


    bb (tag 2)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    weirder-list (Determined from data, 8-bit tag)
    **********************************************

    c0 (tag 0)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c1 (tag 8)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c2 (tag 16)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c3 (tag 24)
    ===========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    c (tag 32)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | Determined from data | $weirb-list            |
    +-----------------+----------------------+------------------------+


    s (tag 40)
    ==========

    +-----------------+----------------------+------------------------+
    | Name            | Size                 | Contents               |
    +=================+======================+========================+
    | Tag             | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+
    | Unnamed field 0 | Determined from data | $weirder-list          |
    +-----------------+----------------------+------------------------+
    | Unnamed field 1 | 1 byte               | unsigned 8-bit integer |
    +-----------------+----------------------+------------------------+


    t (tag 48)
    ==========

    +-----------------+----------------------+------------------------------------+
    | Name            | Size                 | Contents                           |
    +=================+======================+====================================+
    | Tag             | 1 byte               | unsigned 8-bit integer             |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 0 | Determined from data | $weirder-list                      |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 1 | 1 byte               | unsigned 8-bit integer             |
    +-----------------+----------------------+------------------------------------+
    | Unnamed field 2 | 2 bytes              | unsigned 16-bit big-endian integer |
    +-----------------+----------------------+------------------------------------+


    s (tag 56)
    ==========

    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+


    X_49 (Determined from data, 8-bit tag)
    **************************************

    case_0 (tag 0)
    ==============

    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+


    case_1 (tag 1)
    ==============

    +-----------------+----------+-------------------------------------+
    | Name            | Size     | Contents                            |
    +=================+==========+=====================================+
    | Tag             | 1 byte   | unsigned 8-bit integer              |
    +-----------------+----------+-------------------------------------+
    | Unnamed field 0 | Variable | sequence of exactly 1 $weirder-list |
    +-----------------+----------+-------------------------------------+


    case_2 (tag 2)
    ==============

    +-----------------+----------+-------------------------------------+
    | Name            | Size     | Contents                            |
    +=================+==========+=====================================+
    | Tag             | 1 byte   | unsigned 8-bit integer              |
    +-----------------+----------+-------------------------------------+
    | Unnamed field 0 | Variable | sequence of exactly 2 $weirder-list |
    +-----------------+----------+-------------------------------------+


    case_3 (tag 3)
    ==============

    +-----------------+----------+-------------------------------------+
    | Name            | Size     | Contents                            |
    +=================+==========+=====================================+
    | Tag             | 1 byte   | unsigned 8-bit integer              |
    +-----------------+----------+-------------------------------------+
    | Unnamed field 0 | Variable | sequence of exactly 3 $weirder-list |
    +-----------------+----------+-------------------------------------+


    case_4 (tag 4)
    ==============

    +-----------------+----------+-------------------------------------+
    | Name            | Size     | Contents                            |
    +=================+==========+=====================================+
    | Tag             | 1 byte   | unsigned 8-bit integer              |
    +-----------------+----------+-------------------------------------+
    | Unnamed field 0 | Variable | sequence of exactly 4 $weirder-list |
    +-----------------+----------+-------------------------------------+


    case_5 (tag 5)
    ==============

    +-----------------+----------+-------------------------------------+
    | Name            | Size     | Contents                            |
    +=================+==========+=====================================+
    | Tag             | 1 byte   | unsigned 8-bit integer              |
    +-----------------+----------+-------------------------------------+
    | Unnamed field 0 | Variable | sequence of exactly 5 $weirder-list |
    +-----------------+----------+-------------------------------------+


    case_6 (tag 6)
    ==============

    +-----------------+----------+-------------------------------------+
    | Name            | Size     | Contents                            |
    +=================+==========+=====================================+
    | Tag             | 1 byte   | unsigned 8-bit integer              |
    +-----------------+----------+-------------------------------------+
    | Unnamed field 0 | Variable | sequence of exactly 6 $weirder-list |
    +-----------------+----------+-------------------------------------+


    case_7 (tag 7)
    ==============

    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | sequence of $weirder-list          |
    +-----------------------+----------+------------------------------------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "X_49", "kind": "Ref" }, "kind": "anon",
                 "data_kind": { "kind": "Dynamic" } } ] },
       "fields":
         [ { "description": { "title": "weirb-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 0,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirder-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "b" },
                     { "tag": 2,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "bb" } ] } },
           { "description": { "title": "weirder-list" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 0,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c0" },
                     { "tag": 8,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c1" },
                     { "tag": 16,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c2" },
                     { "tag": 24,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c3" },
                     { "tag": 32,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "name": "weirb-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } } ],
                       "name": "c" },
                     { "tag": 40,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirder-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } } ],
                       "name": "s" },
                     { "tag": 48,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout": { "name": "weirder-list", "kind": "Ref" },
                             "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                           { "layout": { "size": "Uint8", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 1, "kind": "Fixed" } },
                           { "layout": { "size": "Uint16", "kind": "Int" },
                             "kind": "anon",
                             "data_kind": { "size": 2, "kind": "Fixed" } } ],
                       "name": "t" },
                     { "tag": 56,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" } ], "name": "s" } ] } },
           { "description": { "title": "X_49" },
             "encoding":
               { "tag_size": "Uint8", "kind": { "kind": "Dynamic" },
                 "cases":
                   [ { "tag": 0,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" } ], "name": "case_0" },
                     { "tag": 1,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq",
                                 "length_limit":
                                   { "kind": "exactly", "exactly": 1 } },
                             "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_1" },
                     { "tag": 2,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq",
                                 "length_limit":
                                   { "kind": "exactly", "exactly": 2 } },
                             "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_2" },
                     { "tag": 3,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq",
                                 "length_limit":
                                   { "kind": "exactly", "exactly": 3 } },
                             "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_3" },
                     { "tag": 4,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq",
                                 "length_limit":
                                   { "kind": "exactly", "exactly": 4 } },
                             "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_4" },
                     { "tag": 5,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq",
                                 "length_limit":
                                   { "kind": "exactly", "exactly": 5 } },
                             "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_5" },
                     { "tag": 6,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq",
                                 "length_limit":
                                   { "kind": "exactly", "exactly": 6 } },
                             "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_6" },
                     { "tag": 7,
                       "fields":
                         [ { "name": "Tag",
                             "layout": { "size": "Uint8", "kind": "Int" },
                             "data_kind": { "size": 1, "kind": "Fixed" },
                             "kind": "named" },
                           { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
                           { "layout":
                               { "layout":
                                   { "name": "weirder-list", "kind": "Ref" },
                                 "kind": "Seq" }, "kind": "anon",
                             "data_kind": { "kind": "Variable" } } ],
                       "name": "case_7" } ] } } ] } |}] ;
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
  let list_with_length_16 = Data_encoding.(list_with_length `Uint16 uint8) in
  dump list_with_length_16 ;
  [%expect
    {|
    +-----------------+----------+--------------------------------------------------+
    | Name            | Size     | Contents                                         |
    +=================+==========+==================================================+
    | Unnamed field 0 | 2 bytes  | unsigned 16-bit big-endian integer               |
    +-----------------+----------+--------------------------------------------------+
    | Unnamed field 1 | Variable | sequence of at most 65535 unsigned 8-bit integer |
    +-----------------+----------+--------------------------------------------------+



    { "toplevel":
         { "fields":
             [ { "layout": { "size": "Uint16", "kind": "Int" }, "kind": "anon",
                 "data_kind": { "size": 2, "kind": "Fixed" } },
               { "layout":
                   { "layout": { "size": "Uint8", "kind": "Int" }, "kind": "Seq",
                     "length_limit": { "kind": "at-most", "at_most": 65535 } },
                 "kind": "anon", "data_kind": { "kind": "Variable" } } ] },
       "fields": [] } |}] ;
  let list_with_length_n = Data_encoding.(list_with_length `N uint8) in
  dump list_with_length_n ;
  [%expect
    {|
    +-----------------+----------------------+-------------------------------------------------------+
    | Name            | Size                 | Contents                                              |
    +=================+======================+=======================================================+
    | Unnamed field 0 | Determined from data | $N.t                                                  |
    +-----------------+----------------------+-------------------------------------------------------+
    | Unnamed field 1 | Variable             | sequence of at most 1073741823 unsigned 8-bit integer |
    +-----------------+----------------------+-------------------------------------------------------+


    N.t
    ***

    A variable-length sequence of bytes encoding a Zarith natural number. Each byte has a running unary size bit: the most significant bit of each byte indicates whether this is the last byte in the sequence (0) or whether the sequence continues (1). Size bits ignored, the data is the binary representation of the number in little-endian order.

    +------+----------------------+----------+
    | Name | Size                 | Contents |
    +======+======================+==========+
    | N.t  | Determined from data | bytes    |
    +------+----------------------+----------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "N.t", "kind": "Ref" }, "kind": "anon",
                 "data_kind": { "kind": "Dynamic" } },
               { "layout":
                   { "layout": { "size": "Uint8", "kind": "Int" }, "kind": "Seq",
                     "length_limit": { "kind": "at-most", "at_most": 1073741823 } },
                 "kind": "anon", "data_kind": { "kind": "Variable" } } ] },
       "fields":
         [ { "description":
               { "title": "N.t",
                 "description":
                   "A variable-length sequence of bytes encoding a Zarith natural number. Each byte has a running unary size bit: the most significant bit of each byte indicates whether this is the last byte in the sequence (0) or whether the sequence continues (1). Size bits ignored, the data is the binary representation of the number in little-endian order." },
             "encoding":
               { "fields":
                   [ { "name": "N.t", "layout": { "kind": "Bytes" },
                       "data_kind": { "kind": "Dynamic" }, "kind": "named" } ] } } ] } |}] ;
  let list_with_length_n_2 =
    Data_encoding.(tup2 (list_with_length `N uint8) (list_with_length `N int64))
  in
  dump list_with_length_n_2 ;
  [%expect
    {|
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_0     |
    +-----------------+----------------------+----------+
    | Unnamed field 1 | Determined from data | $X_1     |
    +-----------------+----------------------+----------+


    N.t
    ***

    A variable-length sequence of bytes encoding a Zarith natural number. Each byte has a running unary size bit: the most significant bit of each byte indicates whether this is the last byte in the sequence (0) or whether the sequence continues (1). Size bits ignored, the data is the binary representation of the number in little-endian order.

    +------+----------------------+----------+
    | Name | Size                 | Contents |
    +======+======================+==========+
    | N.t  | Determined from data | bytes    |
    +------+----------------------+----------+


    X_0
    ***

    +-----------------+----------------------+-------------------------------------------------------+
    | Name            | Size                 | Contents                                              |
    +=================+======================+=======================================================+
    | Unnamed field 0 | Determined from data | $N.t                                                  |
    +-----------------+----------------------+-------------------------------------------------------+
    | Unnamed field 1 | Variable             | sequence of at most 1073741823 unsigned 8-bit integer |
    +-----------------+----------------------+-------------------------------------------------------+


    X_1
    ***

    +-----------------+----------------------+-----------------------------------------------------------------+
    | Name            | Size                 | Contents                                                        |
    +=================+======================+=================================================================+
    | Unnamed field 0 | Determined from data | $N.t                                                            |
    +-----------------+----------------------+-----------------------------------------------------------------+
    | Unnamed field 1 | Variable             | sequence of at most 1073741823 signed 64-bit big-endian integer |
    +-----------------+----------------------+-----------------------------------------------------------------+


    { "toplevel":
         { "fields":
             [ { "layout": { "name": "X_0", "kind": "Ref" }, "kind": "anon",
                 "data_kind": { "kind": "Dynamic" } },
               { "layout": { "name": "X_1", "kind": "Ref" }, "kind": "anon",
                 "data_kind": { "kind": "Dynamic" } } ] },
       "fields":
         [ { "description":
               { "title": "N.t",
                 "description":
                   "A variable-length sequence of bytes encoding a Zarith natural number. Each byte has a running unary size bit: the most significant bit of each byte indicates whether this is the last byte in the sequence (0) or whether the sequence continues (1). Size bits ignored, the data is the binary representation of the number in little-endian order." },
             "encoding":
               { "fields":
                   [ { "name": "N.t", "layout": { "kind": "Bytes" },
                       "data_kind": { "kind": "Dynamic" }, "kind": "named" } ] } },
           { "description": { "title": "X_0" },
             "encoding":
               { "fields":
                   [ { "layout": { "name": "N.t", "kind": "Ref" },
                       "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                     { "layout":
                         { "layout": { "size": "Uint8", "kind": "Int" },
                           "kind": "Seq",
                           "length_limit":
                             { "kind": "at-most", "at_most": 1073741823 } },
                       "kind": "anon", "data_kind": { "kind": "Variable" } } ] } },
           { "description": { "title": "X_1" },
             "encoding":
               { "fields":
                   [ { "layout": { "name": "N.t", "kind": "Ref" },
                       "kind": "anon", "data_kind": { "kind": "Dynamic" } },
                     { "layout":
                         { "layout": { "size": "Int64", "kind": "Int" },
                           "kind": "Seq",
                           "length_limit":
                             { "kind": "at-most", "at_most": 1073741823 } },
                       "kind": "anon", "data_kind": { "kind": "Variable" } } ] } } ] } |}] ;
  let dynamic_size_n =
    Data_encoding.(dynamic_size ~kind:`N (Variable.list uint8))
  in
  dump dynamic_size_n ;
  [%expect
    {|
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | # bytes in next field | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | Unnamed field 0       | Variable             | sequence of unsigned 8-bit integer |
    +-----------------------+----------------------+------------------------------------+



    { "toplevel":
         { "fields":
             [ { "kind": "dyn", "num_fields": 1, "size": "N" },
               { "layout":
                   { "layout": { "size": "Uint8", "kind": "Int" },
                     "kind": "Seq" }, "kind": "anon",
                 "data_kind": { "kind": "Variable" } } ] }, "fields": [] } |}] ;
  let lazy_bounded_string =
    Data_encoding.Encoding.(
      lazy_encoding (Bounded.string' ~length_kind:`N Hex 900))
  in
  dump lazy_bounded_string ;
  [%expect
    {|
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+



    { "toplevel":
         { "fields":
             [ { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
               { "layout": { "kind": "Bytes" }, "kind": "anon",
                 "data_kind": { "kind": "Variable" } } ] }, "fields": [] } |}] ;
  let lazy_tup4 =
    Data_encoding.Encoding.(lazy_encoding (tup4 unit uint8 unit uint16))
  in
  dump lazy_tup4 ;
  [%expect
    {|
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+



    { "toplevel":
         { "fields":
             [ { "kind": "dyn", "num_fields": 1, "size": "Uint30" },
               { "layout": { "kind": "Bytes" }, "kind": "anon",
                 "data_kind": { "kind": "Variable" } } ] }, "fields": [] } |}] ;
  dump Data_encoding.(tup4 int31 int64 uint8 uint16) ;
  [%expect
    {|
    +-----------------+---------+-------------------------------------------------------------------------+
    | Name            | Size    | Contents                                                                |
    +=================+=========+=========================================================================+
    | Unnamed field 0 | 4 bytes | signed 31-bit big-endian integer in the range -1073741824 to 1073741823 |
    +-----------------+---------+-------------------------------------------------------------------------+
    | Unnamed field 1 | 8 bytes | signed 64-bit big-endian integer                                        |
    +-----------------+---------+-------------------------------------------------------------------------+
    | Unnamed field 2 | 1 byte  | unsigned 8-bit integer                                                  |
    +-----------------+---------+-------------------------------------------------------------------------+
    | Unnamed field 3 | 2 bytes | unsigned 16-bit big-endian integer                                      |
    +-----------------+---------+-------------------------------------------------------------------------+



    { "toplevel":
         { "fields":
             [ { "layout":
                   { "min": -1073741824, "max": 1073741823, "kind": "RangedInt" },
                 "kind": "anon", "data_kind": { "size": 4, "kind": "Fixed" } },
               { "layout": { "size": "Int64", "kind": "Int" }, "kind": "anon",
                 "data_kind": { "size": 8, "kind": "Fixed" } },
               { "layout": { "size": "Uint8", "kind": "Int" }, "kind": "anon",
                 "data_kind": { "size": 1, "kind": "Fixed" } },
               { "layout": { "size": "Uint16", "kind": "Int" }, "kind": "anon",
                 "data_kind": { "size": 2, "kind": "Fixed" } } ] },
       "fields": [] } |}] ;
  dump
    Data_encoding.(
      tup4 Little_endian.int31 Little_endian.int64 uint16 Little_endian.uint16) ;
  [%expect
    {|
    +-----------------+---------+----------------------------------------------------------------------------+
    | Name            | Size    | Contents                                                                   |
    +=================+=========+============================================================================+
    | Unnamed field 0 | 4 bytes | signed 31-bit little-endian integer in the range -1073741824 to 1073741823 |
    +-----------------+---------+----------------------------------------------------------------------------+
    | Unnamed field 1 | 8 bytes | signed 64-bit little-endian integer                                        |
    +-----------------+---------+----------------------------------------------------------------------------+
    | Unnamed field 2 | 2 bytes | unsigned 16-bit big-endian integer                                         |
    +-----------------+---------+----------------------------------------------------------------------------+
    | Unnamed field 3 | 2 bytes | unsigned 16-bit little-endian integer                                      |
    +-----------------+---------+----------------------------------------------------------------------------+



    { "toplevel":
         { "fields":
             [ { "layout":
                   { "min": -1073741824, "endianness": "Little",
                     "max": 1073741823, "kind": "RangedInt" }, "kind": "anon",
                 "data_kind": { "size": 4, "kind": "Fixed" } },
               { "layout":
                   { "size": "Int64", "endianness": "Little", "kind": "Int" },
                 "kind": "anon", "data_kind": { "size": 8, "kind": "Fixed" } },
               { "layout": { "size": "Uint16", "kind": "Int" }, "kind": "anon",
                 "data_kind": { "size": 2, "kind": "Fixed" } },
               { "layout":
                   { "size": "Uint16", "endianness": "Little", "kind": "Int" },
                 "kind": "anon", "data_kind": { "size": 2, "kind": "Fixed" } } ] },
       "fields": [] } |}] ;
  dump Data_encoding.(list_with_length `Uint30 bool) ;
  [%expect
    {|
    +-----------------+----------+--------------------------------------------------------------------+
    | Name            | Size     | Contents                                                           |
    +=================+==========+====================================================================+
    | Unnamed field 0 | 4 bytes  | unsigned 30-bit big-endian integer in the range 0 to 1073741823    |
    +-----------------+----------+--------------------------------------------------------------------+
    | Unnamed field 1 | Variable | sequence of at most 1073741823 boolean (0 for false, 255 for true) |
    +-----------------+----------+--------------------------------------------------------------------+



    { "toplevel":
         { "fields":
             [ { "layout": { "min": 0, "max": 1073741823, "kind": "RangedInt" },
                 "kind": "anon", "data_kind": { "size": 4, "kind": "Fixed" } },
               { "layout":
                   { "layout": { "kind": "Bool" }, "kind": "Seq",
                     "length_limit": { "kind": "at-most", "at_most": 1073741823 } },
                 "kind": "anon", "data_kind": { "kind": "Variable" } } ] },
       "fields": [] } |}] ;
  ()
