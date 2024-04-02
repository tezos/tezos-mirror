open Data_encoding

let hex code : char =
  assert (code >= 0 && code < 16) ;
  if code <= 10 then Char.chr (code + Char.code '0')
  else Char.chr (code - 10 + Char.code 'A')

let string_of_bin (s : string) : string =
  let b = Bytes.create (String.length s * 2) in
  String.iteri
    (fun i c ->
      let c = Char.code c in
      let h = c / 16 and l = c mod 16 in

      Bytes.set b (i * 2) (hex h) ;
      Bytes.set b ((i * 2) + 1) (hex l))
    s ;
  Bytes.to_string b

let binary encoding to_string value =
  let bin = Binary.to_string_exn encoding value in
  let result = Binary.of_string_exn encoding bin in
  Printf.printf "%s => %s" (to_string value) (string_of_bin bin) ;
  if result <> value then
    Printf.printf
      "<> %s => %s Roundtrip error"
      (to_string result)
      (string_of_bin (Binary.to_string_exn encoding result)) ;
  Printf.printf "\n%!"

let max_int size = (1 lsl (size - 1)) - 1

let min_int size = ~-(1 lsl (size - 1))

let min_uint _size = 0

let max_uint size = (1 lsl size) - 1

let string_int pad x =
  let s = string_of_int x in
  let space = pad - String.length s in
  if space < 0 then s
  else
    String.init pad (fun i ->
        if i < space then ' ' else String.get s (i - space))

let string_z pad x =
  let x = Z.to_int x in
  let s = string_of_int x in
  let space = pad - String.length s in
  if space < 0 then s
  else
    String.init pad (fun i ->
        if i < space then ' ' else String.get s (i - space))

let all_uint encoding size =
  binary encoding (string_int 12) (min_uint size) ;
  binary encoding (string_int 12) (max_uint size / 2) ;
  binary encoding (string_int 12) (max_uint size - 1) ;
  binary encoding (string_int 12) (max_uint size)

let all_int encoding size =
  binary encoding (string_int 12) (min_int size) ;
  binary encoding (string_int 12) 0 ;
  binary encoding (string_int 12) (max_int size / 2) ;
  binary encoding (string_int 12) (max_int size - 1) ;
  binary encoding (string_int 12) (max_int size)

let test_uint_like_n () =
  binary (uint_like_n ()) (string_int 12) 0 ;
  binary (uint_like_n ()) (string_int 12) 1 ;
  binary (uint_like_n ()) (string_int 12) 2 ;
  binary (uint_like_n ()) (string_int 12) 6 ;
  binary (uint_like_n ()) (string_int 12) 7 ;
  binary (uint_like_n ()) (string_int 12) 8 ;
  binary (uint_like_n ()) (string_int 12) 127 ;
  binary (uint_like_n ()) (string_int 12) 128 ;
  binary (uint_like_n ()) (string_int 12) 341234 ;
  binary (uint_like_n ()) (string_int 12) 34341234

let test_int_like_z () =
  binary (int_like_z ()) (string_int 12) 0 ;
  binary (int_like_z ()) (string_int 12) 1 ;
  binary (int_like_z ()) (string_int 12) 2 ;
  binary (int_like_z ()) (string_int 12) 7 ;
  binary (int_like_z ()) (string_int 12) 8 ;
  binary (int_like_z ()) (string_int 12) 63 ;
  binary (int_like_z ()) (string_int 12) 64 ;
  binary (int_like_z ()) (string_int 12) 127 ;
  binary (int_like_z ()) (string_int 12) 128 ;
  binary (int_like_z ()) (string_int 12) 341234 ;
  binary (int_like_z ()) (string_int 12) 34341234 ;
  binary (int_like_z ()) (string_int 12) (-1) ;
  binary (int_like_z ()) (string_int 12) (-2) ;
  binary (int_like_z ()) (string_int 12) (-7) ;
  binary (int_like_z ()) (string_int 12) (-8) ;
  binary (int_like_z ()) (string_int 12) (-63) ;
  binary (int_like_z ()) (string_int 12) (-64) ;
  binary (int_like_z ()) (string_int 12) (-127) ;
  binary (int_like_z ()) (string_int 12) (-256) ;
  binary (int_like_z ()) (string_int 12) (-341234) ;
  binary (int_like_z ()) (string_int 12) (-34341234)

let test_n () =
  binary n (string_z 12) (Z.of_int 0) ;
  binary n (string_z 12) (Z.of_int 1) ;
  binary n (string_z 12) (Z.of_int 2) ;
  binary n (string_z 12) (Z.of_int 6) ;
  binary n (string_z 12) (Z.of_int 7) ;
  binary n (string_z 12) (Z.of_int 8) ;
  binary n (string_z 12) (Z.of_int 127) ;
  binary n (string_z 12) (Z.of_int 128) ;
  binary n (string_z 12) (Z.of_int 341234) ;
  binary n (string_z 12) (Z.of_int 34341234)

let test_z () =
  binary z (string_z 12) (Z.of_int 0) ;
  binary z (string_z 12) (Z.of_int 1) ;
  binary z (string_z 12) (Z.of_int 2) ;
  binary z (string_z 12) (Z.of_int 7) ;
  binary z (string_z 12) (Z.of_int 8) ;
  binary z (string_z 12) (Z.of_int 63) ;
  binary z (string_z 12) (Z.of_int 64) ;
  binary z (string_z 12) (Z.of_int 127) ;
  binary z (string_z 12) (Z.of_int 128) ;
  binary z (string_z 12) (Z.of_int 341234) ;
  binary n (string_z 12) (Z.of_int 34341234) ;
  binary z (string_z 12) (Z.of_int (-1)) ;
  binary z (string_z 12) (Z.of_int (-2)) ;
  binary z (string_z 12) (Z.of_int (-7)) ;
  binary z (string_z 12) (Z.of_int (-8)) ;
  binary z (string_z 12) (Z.of_int (-63)) ;
  binary z (string_z 12) (Z.of_int (-64)) ;
  binary z (string_z 12) (Z.of_int (-127)) ;
  binary z (string_z 12) (Z.of_int (-256)) ;
  binary z (string_z 12) (Z.of_int (-341234)) ;
  binary z (string_z 12) (Z.of_int (-34341234))

let%expect_test _ =
  all_uint uint8 8 ;
  [%expect {|
      0 => 00
    127 => 7F
    254 => FE
    255 => FF |}] ;
  all_int int8 8 ;
  [%expect
    {|
     -128 => 80
        0 => 00
       63 => 3F
      126 => 7E
      127 => 7F |}] ;
  all_uint uint16 16 ;
  [%expect
    {|
        0 => 0000
    32767 => 7FFF
    65534 => FFFE
    65535 => FFFF |}] ;
  all_int int16 16 ;
  [%expect
    {|
    -32768 => 8000
         0 => 0000
     16383 => 3FFF
     32766 => 7FFE
     32767 => 7FFF |}] ;
  all_int int31 31 ;
  [%expect
    {|
    -1073741824 => C0000000
              0 => 00000000
      536870911 => 1FFFFFFF
     1073741822 => 3FFFFFFE
     1073741823 => 3FFFFFFF |}] ;
  test_uint_like_n () ;
  [%expect
    {|
           0 => 00
           1 => 01
           2 => 02
           6 => 06
           7 => 07
           8 => 08
         127 => 7F
         128 => 8001
      341234 => F2E914
    34341234 => F282B010 |}] ;
  test_int_like_z () ;
  [%expect
    {|
            0 => 00
            1 => 01
            2 => 02
            7 => 07
            8 => 08
           63 => 3F
           64 => 8001
          127 => BF01
          128 => 8002
       341234 => B2D329
     34341234 => B285E020
           -1 => 41
           -2 => 42
           -7 => 47
           -8 => 48
          -63 => 7F
          -64 => C001
         -127 => FF01
         -256 => C004
      -341234 => F2D329
    -34341234 => F285E020 |}] ;
  test_n () ;
  [%expect
    {|
             0 => 00
             1 => 01
             2 => 02
             6 => 06
             7 => 07
             8 => 08
           127 => 7F
           128 => 8001
        341234 => F2E914
      34341234 => F282B010
     |}] ;
  test_z () ;
  [%expect
    {|
            0 => 00
            1 => 01
            2 => 02
            7 => 07
            8 => 08
           63 => 3F
           64 => 8001
          127 => BF01
          128 => 8002
       341234 => B2D329
     34341234 => F282B010
           -1 => 41
           -2 => 42
           -7 => 47
           -8 => 48
          -63 => 7F
          -64 => C001
         -127 => FF01
         -256 => C004
      -341234 => F2D329
    -34341234 => F285E020 |}]
