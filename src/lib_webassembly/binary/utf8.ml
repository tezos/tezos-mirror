open Binary_exn

let con n = 0x80 lor (n land 0x3f)

let encode_int = function
  | n when n < 0 -> raise Utf8
  | n when n < 0x80 -> [n]
  | n when n < 0x800 -> [0xc0 lor (n lsr 6); con n]
  | n when n < 0x10000 -> [0xe0 lor (n lsr 12); con (n lsr 6); con n]
  | n when n < 0x110000 ->
      [0xf0 lor (n lsr 18); con (n lsr 12); con (n lsr 6); con n]
  | _ -> raise Utf8

let rec encode ns =
  let open Lwt.Syntax in
  let+ ns = Lazy_vector.Int32Vector.to_list ns in
  Lib.String.implode (List.map Char.chr (encode' ns))

and encode' = function
  | [] -> []
  | n :: ns ->
      (* Returns at most a list of 4 chars, hence concat shouldn't be costly. *)
      encode_int n @ encode' ns

let rec encode_list ns = Lib.String.implode (List.map Char.chr (encode' ns))

and encode' = function
  | [] -> []
  | n :: ns ->
      (* Returns at most a list of 4 chars, hence concat shouldn't be costly. *)
      encode_int n @ encode' ns

let encode_unsafe ns =
  let ns = List.filter_map snd (Lazy_vector.Int32Vector.loaded_bindings ns) in
  encode_list ns

let con b = if b land 0xc0 = 0x80 then b land 0x3f else raise Utf8

let code min n =
  if n < min || (0xd800 <= n && n < 0xe000) || n >= 0x110000 then raise Utf8
  else n

let decode_step get s =
  let open Lwt.Syntax in
  let get s =
    (* In the testsuite, some tests are supposed to break during reading the
       UTF8. As such, the module is not fully implemented and `get` might come
       across the end of the module, and raise `EOS`. This doesn't happen in the
       original implementation since the UTF8 string is read fully and splitted,
       and the end of file state is actually caught by the list pattern
       matching. In that case, [decode] raises [Utf8]. *)
    try get s with Decode_error.Error _ -> raise Utf8
  in
  let* b1 = get s in
  if b1 < 0x80 then Lwt.return (code 0x0 b1, [b1])
  else if b1 < 0xc0 then raise Utf8
  else
    let* b2 = get s in
    if b1 < 0xe0 then
      let decoded = code 0x80 (((b1 land 0x1f) lsl 6) + con b2) in
      Lwt.return (decoded, [b1; b2])
    else
      let* b3 = get s in
      if b1 < 0xf0 then
        let decoded =
          code 0x800 (((b1 land 0x0f) lsl 12) + (con b2 lsl 6) + con b3)
        in
        Lwt.return (decoded, [b1; b2; b3])
      else
        let* b4 = get s in
        if b1 < 0xf8 then
          let decoded =
            code
              0x10000
              (((b1 land 0x07) lsl 18)
              + (con b2 lsl 12)
              + (con b3 lsl 6)
              + con b4)
          in
          Lwt.return (decoded, [b1; b2; b3; b4])
        else raise Utf8

let rec decode s =
  Lazy_vector.Int32Vector.of_list
    (decode' (List.map Char.code (Lib.String.explode s)))

and decode' = function
  | [] -> []
  | b1 :: bs when b1 < 0x80 -> code 0x0 b1 :: decode' bs
  | b1 :: _ when b1 < 0xc0 -> raise Utf8
  | b1 :: b2 :: bs when b1 < 0xe0 ->
      code 0x80 (((b1 land 0x1f) lsl 6) + con b2) :: decode' bs
  | b1 :: b2 :: b3 :: bs when b1 < 0xf0 ->
      code 0x800 (((b1 land 0x0f) lsl 12) + (con b2 lsl 6) + con b3)
      :: decode' bs
  | b1 :: b2 :: b3 :: b4 :: bs when b1 < 0xf8 ->
      code
        0x10000
        (((b1 land 0x07) lsl 18) + (con b2 lsl 12) + (con b3 lsl 6) + con b4)
      :: decode' bs
  | _ -> raise Utf8
