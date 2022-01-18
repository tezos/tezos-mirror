(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type jsonm_lexeme =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe ]

let string_of_float f =
  let fract, intr = modf f in
  if fract = 0.0 then Format.asprintf "%.0f" intr else Format.asprintf "%g" f

let string_needs_escaping_at index s =
  let exception At of int in
  try
    for i = index to String.length s - 1 do
      match s.[i] with
      | '\"' | '\n' | '\r' | '\b' | '\t' | '\\' | '\x00' .. '\x1F' ->
          raise (At i)
      | _ -> ()
    done ;
    -1
  with At i -> i

let do_escape_string s =
  let buff = Buffer.create (String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\"' -> Buffer.add_string buff "\\\""
    | '\n' -> Buffer.add_string buff "\\n"
    | '\r' -> Buffer.add_string buff "\\r"
    | '\b' -> Buffer.add_string buff "\\b"
    | '\t' -> Buffer.add_string buff "\\t"
    | '\\' -> Buffer.add_string buff "\\\\"
    | '\x00' .. '\x1F' as c ->
        Format.kasprintf (Buffer.add_string buff) "\\u%04x" (Char.code c)
    | c -> Buffer.add_char buff c
  done ;
  Buffer.contents buff

let escape_string s =
  if string_needs_escaping_at 0 s >= 0 then do_escape_string s else s

(** small_string_seq_of_jsonm_lexeme_seq: converts a seq of lexeme into a naive
    seq of small strings. This may or may not be appripriate depending on the
    way the resulting seq is consumed. *)
let small_string_seq_of_jsonm_lexeme_seq ~newline (s : jsonm_lexeme Seq.t) :
    string Seq.t =
  let rec sseq first depth seq () =
    match seq () with
    | Seq.Nil ->
        assert (depth = 0) ;
        if newline then Seq.Cons ("\n", Seq.empty) else Seq.Nil
    | Seq.Cons (`Null, seq) ->
        let tail = Seq.Cons ("null", sseq false depth seq) in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`Bool true, seq) ->
        let tail = Seq.Cons ("true", sseq false depth seq) in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`Bool false, seq) ->
        let tail = Seq.Cons ("false", sseq false depth seq) in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`As, seq) ->
        let tail = Seq.Cons ("[", sseq true (depth + 1) seq) in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`Ae, seq) -> Seq.Cons ("]", sseq false (depth - 1) seq)
    | Seq.Cons (`Os, seq) ->
        let tail = Seq.Cons ("{", sseq true (depth + 1) seq) in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`Oe, seq) -> Seq.Cons ("}", sseq false (depth - 1) seq)
    | Seq.Cons (`String s, seq) ->
        let tail =
          Seq.Cons
            ( "\"",
              fun () ->
                Seq.Cons
                  ( escape_string s,
                    fun () -> Seq.Cons ("\"", sseq false depth seq) ) )
        in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`Float f, seq) ->
        let f = string_of_float f in
        let tail = Seq.Cons (f, sseq false depth seq) in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
    | Seq.Cons (`Name n, seq) ->
        let tail =
          Seq.Cons
            ( "\"",
              fun () ->
                Seq.Cons
                  ( escape_string n,
                    fun () ->
                      Seq.Cons
                        ("\"", fun () -> Seq.Cons (":", sseq true depth seq)) )
            )
        in
        if (not first) && depth > 0 then Seq.Cons (",", fun () -> tail)
        else tail
  in
  sseq false 0 s

let dump_unescaped_string chunk_size_hint buff str index seq =
  let rec aux bytes_left_in_buff index =
    if bytes_left_in_buff > String.length str - index then (
      Buffer.add_substring buff str index (String.length str - index) ;
      seq ())
    else (
      Buffer.add_substring buff str index bytes_left_in_buff ;
      let s = Buffer.contents buff in
      Buffer.clear buff ;
      Seq.Cons (s, fun () -> aux chunk_size_hint (index + bytes_left_in_buff)))
  in
  aux (chunk_size_hint - Buffer.length buff) index

(* This is written with the assumption that there aren't many characters that
   need escaping: a few here and there, maybe just a couple of newlines in a
   block of text.

   Breaking this assumption does not cause errors. However, the performances
   might degrade somewhat. *)
let dump_escaped_string chunk_size_hint buff str seq =
  let rec aux_outer bytes_left_in_buff index =
    let next_escape = string_needs_escaping_at index str in
    if bytes_left_in_buff <= 6 then (
      let s = Buffer.contents buff in
      Buffer.clear buff ;
      Seq.Cons (s, fun () -> aux_outer chunk_size_hint index))
    else if next_escape < 0 then
      (* string does not need escaping: dump the rest of the string as is *)
      dump_unescaped_string chunk_size_hint buff str index seq
    else if next_escape = 0 then (
      (* index is at character that needs escaping *)
      (match str.[index] with
      | '\"' -> Buffer.add_string buff "\\\""
      | '\n' -> Buffer.add_string buff "\\n"
      | '\r' -> Buffer.add_string buff "\\r"
      | '\b' -> Buffer.add_string buff "\\b"
      | '\t' -> Buffer.add_string buff "\\t"
      | '\\' -> Buffer.add_string buff "\\\\"
      | '\x00' .. '\x1F' as c ->
          Format.kasprintf (Buffer.add_string buff) "\\u%04x" (Char.code c)
      | c -> Buffer.add_char buff c) ;
      aux_outer (chunk_size_hint - Buffer.length buff) (index + 1))
    else
      (* string needs escaping but later: write the non-empty non-escaped
            prefix and loop back *)
      let to_write_unescaped = next_escape - index in
      if bytes_left_in_buff > to_write_unescaped then (
        Buffer.add_substring buff str index to_write_unescaped ;
        aux_outer
          (bytes_left_in_buff - to_write_unescaped)
          (index + to_write_unescaped))
      else
        let rec aux_inner bytes_left_in_buff index to_write_unescaped continue =
          if bytes_left_in_buff < to_write_unescaped then (
            Buffer.add_substring buff str index bytes_left_in_buff ;
            let s = Buffer.contents buff in
            Seq.Cons
              ( s,
                fun () ->
                  aux_inner
                    chunk_size_hint
                    (index + bytes_left_in_buff)
                    (to_write_unescaped - bytes_left_in_buff)
                    continue ))
          else (
            Buffer.add_substring buff str index to_write_unescaped ;
            continue (index + to_write_unescaped))
        in
        aux_inner bytes_left_in_buff index to_write_unescaped (fun index ->
            aux_outer (chunk_size_hint - Buffer.length buff) index)
  in
  aux_outer (chunk_size_hint - Buffer.length buff) 0

let dump_string_literal chunk_size_hint buff literal seq =
  Buffer.add_char buff '"' ;
  dump_escaped_string chunk_size_hint buff literal (fun () ->
      Buffer.add_char buff '"' ;
      seq ())

let string_seq_of_jsonm_lexeme_seq ~newline ~chunk_size_hint
    (s : jsonm_lexeme Seq.t) : string Seq.t =
  (* we need chunk_size_hint to be reasonably high to accommodate all small
     literals *)
  let chunk_size_hint = min chunk_size_hint 16 in
  (* we occasionally print several characters before checking the length of the
     buffer (e.g., in the case of a key-value name with non-printable character
     at the end: 6 for one hex-encoded non-printable character in a string + 1
     for the string closing double-quote + 1 for the key-value colon separator
     = 8 in total) (e.g., [Float.pred 0.] is ["-4.94066e-324"] which is 12
     characters long).
     So we allocate just above chunk_size +8 to avoid the need to resize. *)
  let buff_size = chunk_size_hint + 16 in
  (* single buffer for the whole serialisation *)
  let buff = Buffer.create buff_size in
  let rec sseq first depth seq () =
    if Buffer.length buff >= buff_size then (
      (* emit buffer content if we have reached the chunk size *)
      let b = Buffer.contents buff in
      Buffer.clear buff ;
      Seq.Cons (b, fun () -> (sseq [@ocaml.tailcall]) first depth seq ()))
    else
      match seq () with
      (* termination *)
      | Seq.Nil ->
          assert (depth = 0) ;
          if newline then Buffer.add_char buff '\n' ;
          (* value terminator: newline *)
          if Buffer.length buff = 0 then
            (* corner case: we just flushed and haven't added a newline *)
            Seq.Nil
          else
            let b = Buffer.contents buff in
            Buffer.clear buff ;
            Seq.Cons (b, Seq.empty)
      (* fixed length, small lexemes *)
      | Seq.Cons (`Null, seq) ->
          (* if we are inside an object/array (i.e., depth is > 0) and we are
             not the first item (i.e., first is false) then we put a delimiter
             character. *)
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          (* then the value *)
          Buffer.add_string buff "null" ;
          (* and we continue with the rest. Note that depth is unchanged
             but first is false whatever it's original value (because whatever
             follows _follows_) *)
          (sseq [@ocaml.tailcall]) false depth seq ()
      | Seq.Cons (`Bool true, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          Buffer.add_string buff "true" ;
          (sseq [@ocaml.tailcall]) false depth seq ()
      | Seq.Cons (`Bool false, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          Buffer.add_string buff "false" ;
          (sseq [@ocaml.tailcall]) false depth seq ()
      | Seq.Cons (`As, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          Buffer.add_char buff '[' ;
          (* We increase the depth and mark the next value as being the first
             value of an array. *)
          (sseq [@ocaml.tailcall]) true (depth + 1) seq ()
      | Seq.Cons (`Ae, seq) ->
          assert (depth > 0) ;
          Buffer.add_char buff ']' ;
          (sseq [@ocaml.tailcall]) false (depth - 1) seq ()
      | Seq.Cons (`Os, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          Buffer.add_char buff '{' ;
          (sseq [@ocaml.tailcall]) true (depth + 1) seq ()
      | Seq.Cons (`Oe, seq) ->
          assert (depth > 0) ;
          Buffer.add_char buff '}' ;
          (sseq [@ocaml.tailcall]) false (depth - 1) seq ()
      | Seq.Cons (`String s, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          (* we delegate string literals to [dump_string_literal]. Note that we
             pass the rest of the sequence as a kind of continuation. This is
             because [dump_string_literal] may fill up the buffer and then some
             (depending on the size of the literal) and so it needs to be able
             to stick a few things in front. *)
          dump_string_literal chunk_size_hint buff s (fun () ->
              (sseq [@ocaml.tailcall]) false depth seq ())
      | Seq.Cons (`Float f, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          let f = string_of_float f in
          Buffer.add_string buff f ;
          (sseq [@ocaml.tailcall]) false depth seq ()
      | Seq.Cons (`Name n, seq) ->
          if (not first) && depth > 0 then Buffer.add_char buff ',' ;
          dump_string_literal chunk_size_hint buff n (fun () ->
              (* set first to true to avoid printing of separator *)
              Buffer.add_char buff ':' ;
              (sseq [@ocaml.tailcall]) true depth seq ())
  in
  sseq false 0 s

let biseq_escaped_string_content buffer offset s k =
  let can_be_written = Bytes.length buffer - offset in
  if String.length s + 1 > can_be_written + Bytes.length buffer then
    if
      (* large string and.. *)
      (* TODO: present the string as a sequence of of blit instructions with
         increasing offsets *)
      offset < Bytes.length buffer / 2
    then (
      (* ..and the current buffer is almost empty:
             dump as much as we can on the current buffer to avoid sending a
             small buffer in the seq, and then use the rest of the string as its
             own buffer *)
      Bytes.blit_string s 0 buffer offset can_be_written ;
      let offset = offset + can_be_written in
      assert (offset = Bytes.length buffer) ;
      Seq.Cons
        ( (buffer, 0, offset),
          fun () ->
            let s = Bytes.unsafe_of_string s in
            Seq.Cons
              ( (s, can_be_written, Bytes.length s - can_be_written),
                fun () -> k 0 ) ))
    else
      (* ..and the buffer is reasonably full:
             put the current buffer in the seq and then the string as a single
             chunk *)
      Seq.Cons
        ( (buffer, 0, offset),
          fun () ->
            let s = Bytes.unsafe_of_string s in
            Seq.Cons ((s, 0, Bytes.length s), fun () -> k 0) )
  else if String.length s + 1 <= can_be_written then (
    (* we [+ 1] to account for the closing quote that will be added by [k] *)
    (* small string: we dump it on the buffer and continue *)
    Bytes.blit_string s 0 buffer offset (String.length s) ;
    let offset = offset + String.length s in
    k offset)
  else (
    (* medium string: we blit two parts onto the buffer *)
    Bytes.blit_string s 0 buffer offset can_be_written ;
    let offset = offset + can_be_written in
    assert (offset = Bytes.length buffer) ;
    Seq.Cons
      ( (buffer, 0, offset),
        fun () ->
          let remain_to_be_written = String.length s - can_be_written in
          Bytes.blit_string s can_be_written buffer 0 remain_to_be_written ;
          let offset = remain_to_be_written in
          k offset ))

let biseq_string_literal buffer offset s k =
  Bytes.set buffer offset '"' ;
  let offset = offset + 1 in
  let first_escape = string_needs_escaping_at 0 s in
  if first_escape < 0 then
    biseq_escaped_string_content buffer offset s (fun offset ->
        Bytes.set buffer offset '"' ;
        k (offset + 1))
  else
    (* NOTE: offset can't be 0 because we just wrote '"', also the string cannot
             be empty because this is matched-for earlier *)
    (* TODO: optimise by escaping using the available buffer *)
    let s = do_escape_string s in
    biseq_escaped_string_content buffer offset s (fun offset ->
        Bytes.set buffer offset '"' ;
        k (offset + 1))

let blit_instructions_seq_of_jsonm_lexeme_seq ~newline ~buffer lexeme_seq =
  let buffer_size = Bytes.length buffer in
  if buffer_size < 32 then
    raise
      (Invalid_argument
         "Data_encoding.blit_instructions_seq_of_jsonm_lexeme_seq") ;
  let flush_at = buffer_size - 16 in
  let[@ocaml.inline] sep first depth offset =
    (* if we are inside an object/array (i.e., depth is > 0) and we are
       not the first item (i.e., first is false) then we put a delimiter
       character. *)
    if (not first) && depth > 0 then (
      Bytes.set buffer offset ',' ;
      offset + 1)
    else offset
  in
  let rec biseq first depth offset seq () =
    if offset >= flush_at then
      (* emit buffer content if we have reached the chunk size *)
      Seq.Cons
        ( (buffer, 0, offset),
          fun () -> (biseq [@ocaml.tailcall]) first depth 0 seq () )
    else
      match seq () with
      (* termination *)
      | Seq.Nil ->
          assert (depth = 0) ;
          let offset =
            if newline then (
              Bytes.set buffer offset '\n' ;
              offset + 1)
            else offset
          in
          if offset = 0 then
            (* corner case: we just flushed (and haven't added a newline) *)
            Seq.Nil
          else Seq.Cons ((buffer, 0, offset), fun () -> Seq.Nil)
      (* fixed length, small lexemes *)
      | Seq.Cons (`Null, seq) ->
          let offset = sep first depth offset in
          Bytes.blit_string "null" 0 buffer offset 4 ;
          let offset = offset + 4 in
          (biseq [@ocaml.tailcall]) false depth offset seq ()
      | Seq.Cons (`Bool true, seq) ->
          let offset = sep first depth offset in
          Bytes.blit_string "true" 0 buffer offset 4 ;
          let offset = offset + 4 in
          (biseq [@ocaml.tailcall]) false depth offset seq ()
      | Seq.Cons (`Bool false, seq) ->
          let offset = sep first depth offset in
          Bytes.blit_string "false" 0 buffer offset 5 ;
          let offset = offset + 5 in
          (biseq [@ocaml.tailcall]) false depth offset seq ()
      | Seq.Cons (`As, seq) ->
          let offset = sep first depth offset in
          Bytes.set buffer offset '[' ;
          let offset = offset + 1 in
          (biseq [@ocaml.tailcall]) true (depth + 1) offset seq ()
      | Seq.Cons (`Ae, seq) ->
          Bytes.set buffer offset ']' ;
          let offset = offset + 1 in
          (biseq [@ocaml.tailcall]) false (depth - 1) offset seq ()
      | Seq.Cons (`Os, seq) ->
          let offset = sep first depth offset in
          Bytes.set buffer offset '{' ;
          let offset = offset + 1 in
          (biseq [@ocaml.tailcall]) true (depth + 1) offset seq ()
      | Seq.Cons (`Oe, seq) ->
          Bytes.set buffer offset '}' ;
          let offset = offset + 1 in
          (biseq [@ocaml.tailcall]) false (depth - 1) offset seq ()
      | Seq.Cons (`String "", seq) ->
          let offset = sep first depth offset in
          Bytes.blit_string "\"\"" 0 buffer offset 2 ;
          let offset = offset + 2 in
          (biseq [@ocaml.tailcall]) false depth offset seq ()
      | Seq.Cons (`String s, seq) ->
          let offset = sep first depth offset in
          (* we delegate string literals to [dump_string_literal]. Note that we
             pass the rest of the sequence as a kind of continuation. This is
             because [dump_string_literal] may fill up the buffer and then some
             (depending on the size of the literal) and so it needs to be able
             to stick a few things in front. *)
          biseq_string_literal buffer offset s (fun offset ->
              (biseq [@ocaml.tailcall]) false depth offset seq ())
      | Seq.Cons (`Float f, seq) ->
          let offset = sep first depth offset in
          let f = string_of_float f in
          biseq_escaped_string_content buffer offset f (fun offset ->
              (biseq [@ocaml.tailcall]) false depth offset seq ())
      | Seq.Cons (`Name n, seq) ->
          let offset = sep first depth offset in
          biseq_string_literal buffer offset n (fun offset ->
              if offset = buffer_size then
                Seq.Cons
                  ( (buffer, 0, offset),
                    fun () ->
                      Bytes.set buffer 0 ':' ;
                      let offset = 1 in
                      (* set first to true to avoid printing of separator *)
                      (biseq [@ocaml.tailcall]) true depth offset seq () )
              else (
                (* set first to true to avoid printing of separator *)
                Bytes.set buffer offset ':' ;
                let offset = offset + 1 in
                (biseq [@ocaml.tailcall]) true depth offset seq ()))
  in
  biseq false 0 0 lexeme_seq
