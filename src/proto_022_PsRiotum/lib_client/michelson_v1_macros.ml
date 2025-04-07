(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol_client_context
open Tezos_micheline
open Micheline
module IntMap = Map.Make (Compare.Int)

type 'l node = ('l, string) Micheline.node

type error += Unexpected_macro_annotation of string

type error += Sequence_expected of string

type error += Invalid_arity of string * int * int

let rec check_letters str i j f =
  i > j || (f str.[i] && check_letters str (i + 1) j f)

let expand_caddadr original =
  let open Result_syntax in
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if
        len > 3
        && str.[0] = 'C'
        && str.[len - 1] = 'R'
        && check_letters str 1 (len - 2) (function
               | 'A' | 'D' -> true
               | _ -> false)
      then
        let* () =
          match args with
          | [] -> return_unit
          | _ :: _ -> tzfail (Invalid_arity (str, List.length args, 0))
        in
        let path_annot =
          List.filter (function "@%" | "@%%" -> true | _ -> false) annot
        in
        let rec parse i acc =
          if i = 0 then Seq (loc, acc)
          else
            let annot = if i = len - 2 then annot else path_annot in
            match str.[i] with
            | 'A' -> parse (i - 1) (Prim (loc, "CAR", [], annot) :: acc)
            | 'D' -> parse (i - 1) (Prim (loc, "CDR", [], annot) :: acc)
            | _ -> assert false
        in
        return_some (parse (len - 2) [])
      else return_none
  | _ -> return_none

let expand_carn original =
  let open Result_syntax in
  match original with
  | Prim (loc, "CAR", [Int (loc2, n)], annot) ->
      return_some
        (Seq
           ( loc,
             [
               Prim
                 (loc, "GET", [Int (loc2, Z.(of_int 1 + (n * of_int 2)))], annot);
             ] ))
  | _ -> return_none

let expand_cdrn original =
  let open Result_syntax in
  match original with
  | Prim (loc, "CDR", [Int (loc2, n)], annot) ->
      return_some
        (Seq (loc, [Prim (loc, "GET", [Int (loc2, Z.(n * of_int 2))], annot)]))
  | _ -> return_none

let extract_field_annots annot =
  List.partition
    (fun a ->
      match a.[0] with
      | '%' -> true
      | _ -> false
      | exception Invalid_argument _ -> false)
    annot

let expand_set_caddadr original =
  let open Result_syntax in
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if
        len >= 7
        && String.sub str 0 5 = "SET_C"
        && str.[len - 1] = 'R'
        && check_letters str 5 (len - 2) (function
               | 'A' | 'D' -> true
               | _ -> false)
      then
        let* () =
          match args with
          | [] -> return_unit
          | _ :: _ -> tzfail (Invalid_arity (str, List.length args, 0))
        in
        let* field_annot, annot =
          match extract_field_annots annot with
          | [], annot -> return (None, annot)
          | [f], annot -> return (Some f, annot)
          | _, _ -> tzfail (Unexpected_macro_annotation str)
        in
        let rec parse i acc =
          if i = 4 then acc
          else
            let annot = if i = 5 then annot else [] in
            match str.[i] with
            | 'A' ->
                let acc =
                  Seq
                    ( loc,
                      [
                        Prim (loc, "DUP", [], []);
                        Prim
                          ( loc,
                            "DIP",
                            [Seq (loc, [Prim (loc, "CAR", [], ["@%%"]); acc])],
                            [] );
                        Prim (loc, "CDR", [], ["@%%"]);
                        Prim (loc, "SWAP", [], []);
                        Prim (loc, "PAIR", [], "%@" :: "%@" :: annot);
                      ] )
                in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq
                    ( loc,
                      [
                        Prim (loc, "DUP", [], []);
                        Prim
                          ( loc,
                            "DIP",
                            [Seq (loc, [Prim (loc, "CDR", [], ["@%%"]); acc])],
                            [] );
                        Prim (loc, "CAR", [], ["@%%"]);
                        Prim (loc, "PAIR", [], "%@" :: "%@" :: annot);
                      ] )
                in
                parse (i - 1) acc
            | _ -> assert false
        in
        match str.[len - 2] with
        | 'A' ->
            let access_check =
              match field_annot with
              | None -> []
              | Some f ->
                  [
                    Prim (loc, "DUP", [], []);
                    Prim (loc, "CAR", [], [f]);
                    Prim (loc, "DROP", [], []);
                  ]
            in
            let encoding =
              [Prim (loc, "CDR", [], ["@%%"]); Prim (loc, "SWAP", [], [])]
            in
            let pair =
              [
                Prim
                  ( loc,
                    "PAIR",
                    [],
                    [Option.value field_annot ~default:"%"; "%@"] );
              ]
            in
            let init = Seq (loc, access_check @ encoding @ pair) in
            return_some (parse (len - 3) init)
        | 'D' ->
            let access_check =
              match field_annot with
              | None -> []
              | Some f ->
                  [
                    Prim (loc, "DUP", [], []);
                    Prim (loc, "CDR", [], [f]);
                    Prim (loc, "DROP", [], []);
                  ]
            in
            let encoding = [Prim (loc, "CAR", [], ["@%%"])] in
            let pair =
              [
                Prim
                  ( loc,
                    "PAIR",
                    [],
                    ["%@"; Option.value field_annot ~default:"%"] );
              ]
            in
            let init = Seq (loc, access_check @ encoding @ pair) in
            return_some (parse (len - 3) init)
        | _ -> assert false
      else return_none
  | _ -> return_none

let expand_map_caddadr original =
  let open Result_syntax in
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if
        len >= 7
        && String.sub str 0 5 = "MAP_C"
        && str.[len - 1] = 'R'
        && check_letters str 5 (len - 2) (function
               | 'A' | 'D' -> true
               | _ -> false)
      then
        let* code =
          match args with
          | [(Seq _ as code)] -> return code
          | [_] -> tzfail (Sequence_expected str)
          | [] | _ :: _ :: _ ->
              tzfail (Invalid_arity (str, List.length args, 1))
        in
        let* field_annot, annot =
          match extract_field_annots annot with
          | [], annot -> return (None, annot)
          | [f], annot -> return (Some f, annot)
          | _, _ -> tzfail (Unexpected_macro_annotation str)
        in
        let rec parse i acc =
          if i = 4 then acc
          else
            let annot = if i = 5 then annot else [] in
            match str.[i] with
            | 'A' ->
                let acc =
                  Seq
                    ( loc,
                      [
                        Prim (loc, "DUP", [], []);
                        Prim
                          ( loc,
                            "DIP",
                            [Seq (loc, [Prim (loc, "CAR", [], ["@%%"]); acc])],
                            [] );
                        Prim (loc, "CDR", [], ["@%%"]);
                        Prim (loc, "SWAP", [], []);
                        Prim (loc, "PAIR", [], "%@" :: "%@" :: annot);
                      ] )
                in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq
                    ( loc,
                      [
                        Prim (loc, "DUP", [], []);
                        Prim
                          ( loc,
                            "DIP",
                            [Seq (loc, [Prim (loc, "CDR", [], ["@%%"]); acc])],
                            [] );
                        Prim (loc, "CAR", [], ["@%%"]);
                        Prim (loc, "PAIR", [], "%@" :: "%@" :: annot);
                      ] )
                in
                parse (i - 1) acc
            | _ -> assert false
        in
        let cr_annot =
          match field_annot with
          | None -> []
          | Some f -> ["@" ^ String.sub f 1 (String.length f - 1)]
        in
        match str.[len - 2] with
        | 'A' ->
            let init =
              Seq
                ( loc,
                  [
                    Prim (loc, "DUP", [], []);
                    Prim (loc, "CDR", [], ["@%%"]);
                    Prim
                      ( loc,
                        "DIP",
                        [Seq (loc, [Prim (loc, "CAR", [], cr_annot); code])],
                        [] );
                    Prim (loc, "SWAP", [], []);
                    Prim
                      ( loc,
                        "PAIR",
                        [],
                        [Option.value field_annot ~default:"%"; "%@"] );
                  ] )
            in
            return_some (parse (len - 3) init)
        | 'D' ->
            let init =
              Seq
                ( loc,
                  [
                    Prim (loc, "DUP", [], []);
                    Prim (loc, "CDR", [], cr_annot);
                    code;
                    Prim (loc, "SWAP", [], []);
                    Prim (loc, "CAR", [], ["@%%"]);
                    Prim
                      ( loc,
                        "PAIR",
                        [],
                        ["%@"; Option.value field_annot ~default:"%"] );
                  ] )
            in
            return_some (parse (len - 3) init)
        | _ -> assert false
      else return_none
  | _ -> return_none

exception Not_a_roman

let decimal_of_roman roman =
  (* http://rosettacode.org/wiki/Roman_numerals/Decode#OCaml *)
  let arabic = ref 0 in
  let lastval = ref 0 in
  for i = String.length roman - 1 downto 0 do
    let n =
      match roman.[i] with
      | 'M' -> 1000
      | 'D' -> 500
      | 'C' -> 100
      | 'L' -> 50
      | 'X' -> 10
      | 'V' -> 5
      | 'I' -> 1
      | _ -> raise_notrace Not_a_roman
    in
    if Compare.Int.(n < !lastval) then arabic := !arabic - n
    else arabic := !arabic + n ;
    lastval := n
  done ;
  !arabic

let dip ~loc ?(annot = []) depth instr =
  assert (depth >= 0) ;
  if depth = 1 then Prim (loc, "DIP", [instr], annot)
  else Prim (loc, "DIP", [Int (loc, Z.of_int depth); instr], annot)

let expand_deprecated_dxiiivp original =
  let open Result_syntax in
  (* transparently expands deprecated macro [DI...IP] to instruction [DIP n] *)
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len > 3 && str.[0] = 'D' && str.[len - 1] = 'P' then
        try
          let depth = decimal_of_roman (String.sub str 1 (len - 2)) in
          match args with
          | [(Seq (_, _) as arg)] ->
              let expanded = dip ~loc ~annot depth arg in
              Format.eprintf
                "Warning: Use of deprecated macro %a. This macro will soon be \
                 removed, please use the %a instruction instead.@."
                Micheline_printer.print_expr_unwrapped
                (Micheline_printer.printable
                   Fun.id
                   (Micheline.strip_locations original))
                Micheline_printer.print_expr_unwrapped
                (Micheline_printer.printable
                   Fun.id
                   (Micheline.strip_locations expanded)) ;
              return_some expanded
          | [_] -> tzfail (Sequence_expected str)
          | [] | _ :: _ :: _ ->
              tzfail (Invalid_arity (str, List.length args, 1))
        with Not_a_roman -> return_none
      else return_none
  | _ -> return_none

exception Not_a_pair

type pair_item = A | I | P of int * pair_item * pair_item

let parse_pair_substr str ~len start =
  let rec parse ?left i =
    if i = len - 1 then raise_notrace Not_a_pair
    else if str.[i] = 'P' then
      let next_i, l = parse ~left:true (i + 1) in
      let next_i, r = parse ~left:false next_i in
      (next_i, P (i, l, r))
    else if str.[i] = 'A' && left = Some true then (i + 1, A)
    else if str.[i] = 'I' && left <> Some true then (i + 1, I)
    else raise_notrace Not_a_pair
  in
  let last, ast = parse start in
  if last <> len - 1 then raise_notrace Not_a_pair else ast

let unparse_pair_item ast =
  let rec unparse ast acc =
    match ast with
    | P (_, l, r) -> unparse r (unparse l ("P" :: acc))
    | A -> "A" :: acc
    | I -> "I" :: acc
  in
  List.rev ("R" :: unparse ast []) |> String.concat ""

let pappaiir_annots_pos ast annot =
  let rec find_annots_pos p_pos ast annots acc =
    match (ast, annots) with
    | _, [] -> (annots, acc)
    | P (i, left, right), _ ->
        let annots, acc = find_annots_pos i left annots acc in
        find_annots_pos i right annots acc
    | A, a :: annots ->
        let pos =
          match IntMap.find p_pos acc with
          | None -> ([a], [])
          | Some (_, cdr) -> ([a], cdr)
        in
        (annots, IntMap.add p_pos pos acc)
    | I, a :: annots ->
        let pos =
          match IntMap.find p_pos acc with
          | None -> ([], [a])
          | Some (car, _) -> (car, [a])
        in
        (annots, IntMap.add p_pos pos acc)
  in
  snd (find_annots_pos 0 ast annot IntMap.empty)

let expand_pappaiir original =
  let open Result_syntax in
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if
        len > 4
        && str.[0] = 'P'
        && str.[len - 1] = 'R'
        && check_letters str 1 (len - 2) (function
               | 'P' | 'A' | 'I' -> true
               | _ -> false)
      then
        try
          let field_annots, annot = extract_field_annots annot in
          let ast = parse_pair_substr str ~len 0 in
          let field_annots_pos = pappaiir_annots_pos ast field_annots in
          let rec parse p (depth, acc) =
            match p with
            | P (i, left, right) ->
                let annot =
                  match (i, IntMap.find i field_annots_pos) with
                  | 0, None -> annot
                  | _, None -> []
                  | 0, Some ([], cdr_annot) -> ("%" :: cdr_annot) @ annot
                  | _, Some ([], cdr_annot) -> "%" :: cdr_annot
                  | 0, Some (car_annot, cdr_annot) ->
                      car_annot @ cdr_annot @ annot
                  | _, Some (car_annot, cdr_annot) -> car_annot @ cdr_annot
                in
                let acc =
                  if depth = 0 then Prim (loc, "PAIR", [], annot) :: acc
                  else
                    dip ~loc depth (Seq (loc, [Prim (loc, "PAIR", [], annot)]))
                    :: acc
                in
                (depth, acc) |> parse left |> parse right
            | A | I -> (depth + 1, acc)
          in
          let _, expanded = parse ast (0, []) in
          let* () =
            match args with
            | [] -> return_unit
            | _ :: _ -> tzfail (Invalid_arity (str, List.length args, 0))
          in
          return_some (Seq (loc, expanded))
        with Not_a_pair -> return_none
      else return_none
  | _ -> return_none

let expand_unpappaiir original =
  let open Result_syntax in
  match original with
  | Prim (loc, str, args, _annot) ->
      let len = String.length str in
      if
        len > 6
        && String.sub str 0 3 = "UNP"
        && str.[len - 1] = 'R'
        && check_letters str 3 (len - 2) (function
               | 'P' | 'A' | 'I' -> true
               | _ -> false)
      then
        try
          let unpair = Prim (loc, "UNPAIR", [], []) in
          let ast = parse_pair_substr str ~len 2 in
          let rec parse p (depth, acc) =
            match p with
            | P (_i, left, right) ->
                let acc =
                  if depth = 0 then unpair :: acc
                  else dip ~loc depth (Seq (loc, [unpair])) :: acc
                in
                (depth, acc) |> parse left |> parse right
            | A | I -> (depth + 1, acc)
          in
          let _, rev_expanded = parse ast (0, []) in
          let expanded = Seq (loc, List.rev rev_expanded) in
          let* () =
            match args with
            | [] -> return_unit
            | _ :: _ -> tzfail (Invalid_arity (str, List.length args, 0))
          in
          return_some expanded
        with Not_a_pair -> return_none
      else return_none
  | _ -> return_none

exception Not_a_dup

let expand_deprecated_duuuuup original =
  (* transparently expands deprecated macro [DU...UP] to [{ DUP n }] *)
  let open Result_syntax in
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if
        len > 3
        && str.[0] = 'D'
        && str.[len - 1] = 'P'
        && check_letters str 1 (len - 2) (( = ) 'U')
      then
        let* () =
          match args with
          | [] -> return_unit
          | _ :: _ -> tzfail (Invalid_arity (str, List.length args, 0))
        in
        try
          let rec parse i =
            if i = 1 then
              Prim (loc, "DUP", [Int (loc, Z.of_int (len - 2))], annot)
            else if str.[i] = 'U' then parse (i - 1)
            else raise_notrace Not_a_dup
          in
          let expanded = parse (len - 2) in
          Format.eprintf
            "Warning: Use of deprecated macro %a. This macro will soon be \
             removed, please use the %a instruction instead.@."
            Micheline_printer.print_expr_unwrapped
            (Micheline_printer.printable
               Fun.id
               (Micheline.strip_locations original))
            Micheline_printer.print_expr_unwrapped
            (Micheline_printer.printable
               Fun.id
               (Micheline.strip_locations expanded)) ;
          return_some expanded
        with Not_a_dup -> return_none
      else return_none
  | _ -> return_none

let expand_compare original =
  let open Result_syntax in
  let cmp loc is annot =
    let is =
      match List.rev_map (fun i -> Prim (loc, i, [], [])) is with
      | Prim (loc, i, args, _) :: r -> List.rev (Prim (loc, i, args, annot) :: r)
      | is -> List.rev is
    in
    return_some (Seq (loc, is))
  in
  let ifcmp loc is l r annot =
    let is =
      List.map (fun i -> Prim (loc, i, [], [])) is
      @ [Prim (loc, "IF", [l; r], annot)]
    in
    return_some (Seq (loc, is))
  in
  match original with
  | Prim (loc, "CMPEQ", [], annot) -> cmp loc ["COMPARE"; "EQ"] annot
  | Prim (loc, "CMPNEQ", [], annot) -> cmp loc ["COMPARE"; "NEQ"] annot
  | Prim (loc, "CMPLT", [], annot) -> cmp loc ["COMPARE"; "LT"] annot
  | Prim (loc, "CMPGT", [], annot) -> cmp loc ["COMPARE"; "GT"] annot
  | Prim (loc, "CMPLE", [], annot) -> cmp loc ["COMPARE"; "LE"] annot
  | Prim (loc, "CMPGE", [], annot) -> cmp loc ["COMPARE"; "GE"] annot
  | Prim
      ( _,
        (("CMPEQ" | "CMPNEQ" | "CMPLT" | "CMPGT" | "CMPLE" | "CMPGE") as str),
        args,
        [] ) ->
      tzfail (Invalid_arity (str, List.length args, 0))
  | Prim (loc, "IFCMPEQ", [l; r], annot) ->
      ifcmp loc ["COMPARE"; "EQ"] l r annot
  | Prim (loc, "IFCMPNEQ", [l; r], annot) ->
      ifcmp loc ["COMPARE"; "NEQ"] l r annot
  | Prim (loc, "IFCMPLT", [l; r], annot) ->
      ifcmp loc ["COMPARE"; "LT"] l r annot
  | Prim (loc, "IFCMPGT", [l; r], annot) ->
      ifcmp loc ["COMPARE"; "GT"] l r annot
  | Prim (loc, "IFCMPLE", [l; r], annot) ->
      ifcmp loc ["COMPARE"; "LE"] l r annot
  | Prim (loc, "IFCMPGE", [l; r], annot) ->
      ifcmp loc ["COMPARE"; "GE"] l r annot
  | Prim (loc, "IFEQ", [l; r], annot) -> ifcmp loc ["EQ"] l r annot
  | Prim (loc, "IFNEQ", [l; r], annot) -> ifcmp loc ["NEQ"] l r annot
  | Prim (loc, "IFLT", [l; r], annot) -> ifcmp loc ["LT"] l r annot
  | Prim (loc, "IFGT", [l; r], annot) -> ifcmp loc ["GT"] l r annot
  | Prim (loc, "IFLE", [l; r], annot) -> ifcmp loc ["LE"] l r annot
  | Prim (loc, "IFGE", [l; r], annot) -> ifcmp loc ["GE"] l r annot
  | Prim
      ( _,
        (( "IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT" | "IFCMPGT" | "IFCMPLE"
         | "IFCMPGE" | "IFEQ" | "IFNEQ" | "IFLT" | "IFGT" | "IFLE" | "IFGE" ) as
         str),
        args,
        [] ) ->
      tzfail (Invalid_arity (str, List.length args, 2))
  | Prim
      ( _,
        (( "IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT" | "IFCMPGT" | "IFCMPLE"
         | "IFCMPGE" | "IFEQ" | "IFNEQ" | "IFLT" | "IFGT" | "IFLE" | "IFGE" ) as
         str),
        [],
        _ :: _ ) ->
      tzfail (Unexpected_macro_annotation str)
  | _ -> return_none

let expand_asserts original =
  let open Result_syntax in
  let may_rename loc = function
    | [] -> Seq (loc, [])
    | annot -> Seq (loc, [Prim (loc, "RENAME", [], annot)])
  in
  let fail_false ?(annot = []) loc =
    [may_rename loc annot; Seq (loc, [Prim (loc, "FAIL", [], [])])]
  in
  let fail_true ?(annot = []) loc =
    [Seq (loc, [Prim (loc, "FAIL", [], [])]); may_rename loc annot]
  in
  match original with
  | Prim (loc, "ASSERT", [], []) ->
      return_some (Seq (loc, [Prim (loc, "IF", fail_false loc, [])]))
  | Prim (loc, "ASSERT_NONE", [], []) ->
      return_some (Seq (loc, [Prim (loc, "IF_NONE", fail_false loc, [])]))
  | Prim (loc, "ASSERT_SOME", [], annot) ->
      return_some (Seq (loc, [Prim (loc, "IF_NONE", fail_true ~annot loc, [])]))
  | Prim (loc, "ASSERT_LEFT", [], annot) ->
      return_some
        (Seq (loc, [Prim (loc, "IF_LEFT", fail_false ~annot loc, [])]))
  | Prim (loc, "ASSERT_RIGHT", [], annot) ->
      return_some (Seq (loc, [Prim (loc, "IF_LEFT", fail_true ~annot loc, [])]))
  | Prim
      ( _,
        (( "ASSERT" | "ASSERT_NONE" | "ASSERT_SOME" | "ASSERT_LEFT"
         | "ASSERT_RIGHT" ) as str),
        args,
        [] ) ->
      tzfail (Invalid_arity (str, List.length args, 0))
  | Prim (_, (("ASSERT" | "ASSERT_NONE") as str), [], _ :: _) ->
      tzfail (Unexpected_macro_annotation str)
  | Prim (loc, s, args, annot)
    when String.(length s > 7 && equal (sub s 0 7) "ASSERT_") -> (
      let* () =
        match args with
        | [] -> return_unit
        | _ :: _ -> tzfail (Invalid_arity (s, List.length args, 0))
      in
      let* () =
        match annot with
        | _ :: _ -> tzfail (Unexpected_macro_annotation s)
        | [] -> return_unit
      in
      let remaining = String.(sub s 7 (length s - 7)) in
      let remaining_prim = Prim (loc, remaining, [], []) in
      match remaining with
      | "EQ" | "NEQ" | "LT" | "LE" | "GE" | "GT" ->
          return_some
            (Seq (loc, [remaining_prim; Prim (loc, "IF", fail_false loc, [])]))
      | _ -> (
          let* seq_opt = expand_compare remaining_prim in
          match seq_opt with
          | None -> return_none
          | Some seq ->
              return
              @@ Some (Seq (loc, [seq; Prim (loc, "IF", fail_false loc, [])]))))
  | _ -> return_none

let expand_if_some =
  let open Result_syntax in
  function
  | Prim (loc, "IF_SOME", [right; left], annot) ->
      return_some (Seq (loc, [Prim (loc, "IF_NONE", [left; right], annot)]))
  | Prim (_, "IF_SOME", args, _annot) ->
      tzfail (Invalid_arity ("IF_SOME", List.length args, 2))
  | _ -> return_none

let expand_if_right =
  let open Result_syntax in
  function
  | Prim (loc, "IF_RIGHT", [right; left], annot) ->
      return_some (Seq (loc, [Prim (loc, "IF_LEFT", [left; right], annot)]))
  | Prim (_, "IF_RIGHT", args, _annot) ->
      tzfail (Invalid_arity ("IF_RIGHT", List.length args, 2))
  | _ -> return_none

let expand_fail =
  let open Result_syntax in
  function
  | Prim (loc, "FAIL", [], []) ->
      return_some
        (Seq (loc, [Prim (loc, "UNIT", [], []); Prim (loc, "FAILWITH", [], [])]))
  | _ -> return_none

let expand original =
  let open Result_syntax in
  let rec try_expansions = function
    | [] -> return original
    | expander :: expanders -> (
        let* rewritten_opt = expander original in
        match rewritten_opt with
        | None -> try_expansions expanders
        | Some rewritten -> return rewritten)
  in
  try_expansions
    [
      expand_carn;
      expand_cdrn;
      expand_caddadr;
      expand_set_caddadr;
      expand_map_caddadr;
      expand_deprecated_dxiiivp;
      (* expand_paaiair ; *)
      expand_pappaiir;
      (* expand_unpaaiair ; *)
      expand_unpappaiir;
      expand_deprecated_duuuuup;
      expand_compare;
      expand_asserts;
      expand_if_some;
      expand_if_right;
      expand_fail;
    ]

let expand_rec expr =
  let rec error_map (expanded, errors) f = function
    | [] -> (List.rev expanded, List.rev errors)
    | hd :: tl ->
        let new_expanded, new_errors = f hd in
        error_map
          (new_expanded :: expanded, List.rev_append new_errors errors)
          f
          tl
  in
  let error_map = error_map ([], []) in
  let rec expand_rec expr =
    match expand expr with
    | Ok expanded -> (
        match expanded with
        | Seq (loc, items) ->
            let items, errors = error_map expand_rec items in
            (Seq (loc, items), errors)
        | Prim (loc, name, args, annot) ->
            let args, errors = error_map expand_rec args in
            (Prim (loc, name, args, annot), errors)
        | (Int _ | String _ | Bytes _) as atom -> (atom, []))
    | Error errors -> (expr, errors)
  in
  expand_rec expr

let unexpand_carn_and_cdrn expanded =
  match expanded with
  | Seq (loc, [Prim (_, "GET", [Int (locn, n)], annot)]) ->
      let half, parity = Z.ediv_rem n (Z.of_int 2) in
      if Z.(parity = zero) then
        Some (Prim (loc, "CDR", [Int (locn, half)], annot))
      else Some (Prim (loc, "CAR", [Int (locn, half)], annot))
  | _ -> None

let unexpand_caddadr expanded =
  let rec rsteps acc = function
    | [] -> Some acc
    | Prim (_, "CAR", [], []) :: rest -> rsteps ("A" :: acc) rest
    | Prim (_, "CDR", [], []) :: rest -> rsteps ("D" :: acc) rest
    | _ -> None
  in
  match expanded with
  | Seq (loc, (Prim (_, "CAR", [], []) :: _ as nodes))
  | Seq (loc, (Prim (_, "CDR", [], []) :: _ as nodes)) -> (
      match rsteps [] nodes with
      | Some steps ->
          let name = String.concat "" ("C" :: List.rev ("R" :: steps)) in
          Some (Prim (loc, name, [], []))
      | None -> None)
  | _ -> None

let unexpand_set_caddadr expanded =
  let rec steps acc annots = function
    | Seq
        ( loc,
          [
            Prim (_, "CDR", [], _);
            Prim (_, "SWAP", [], _);
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "A" :: acc, annots)
    | Seq
        ( loc,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "CAR", [], [field_annot]);
            Prim (_, "DROP", [], []);
            Prim (_, "CDR", [], _);
            Prim (_, "SWAP", [], []);
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "A" :: acc, field_annot :: annots)
    | Seq (loc, [Prim (_, "CAR", [], _); Prim (_, "PAIR", [], _)]) ->
        Some (loc, "D" :: acc, annots)
    | Seq
        ( loc,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "CDR", [], [field_annot]);
            Prim (_, "DROP", [], []);
            Prim (_, "CAR", [], _);
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "D" :: acc, field_annot :: annots)
    | Seq
        ( _,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "DIP", [Seq (_, [Prim (_, "CAR", [], _); sub])], []);
            Prim (_, "CDR", [], _);
            Prim (_, "SWAP", [], []);
            Prim (_, "PAIR", [], pair_annots);
          ] ) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("A" :: acc) (List.rev_append pair_annots annots) sub
    | Seq
        ( _,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "DIP", [Seq (_, [Prim (_, "CDR", [], _); sub])], []);
            Prim (_, "CAR", [], _);
            Prim (_, "PAIR", [], pair_annots);
          ] ) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("D" :: acc) (List.rev_append pair_annots annots) sub
    | _ -> None
  in
  match steps [] [] expanded with
  | Some (loc, steps, annots) ->
      let name = String.concat "" ("SET_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [], List.rev annots))
  | None -> None

let unexpand_map_caddadr expanded =
  let rec steps acc annots = function
    | Seq
        ( loc,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "CDR", [], _);
            Prim (_, "SWAP", [], []);
            Prim (_, "DIP", [Seq (_, [Prim (_, "CAR", [], []); code])], []);
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "A" :: acc, annots, code)
    | Seq
        ( loc,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "CDR", [], _);
            Prim (_, "SWAP", [], []);
            Prim
              ( _,
                "DIP",
                [Seq (_, [Prim (_, "CAR", [], [field_annot]); code])],
                [] );
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "A" :: acc, field_annot :: annots, code)
    | Seq
        ( loc,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "CDR", [], []);
            code;
            Prim (_, "SWAP", [], []);
            Prim (_, "CAR", [], _);
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "D" :: acc, annots, code)
    | Seq
        ( loc,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "CDR", [], [field_annot]);
            code;
            Prim (_, "SWAP", [], []);
            Prim (_, "CAR", [], _);
            Prim (_, "PAIR", [], _);
          ] ) ->
        Some (loc, "D" :: acc, field_annot :: annots, code)
    | Seq
        ( _,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "DIP", [Seq (_, [Prim (_, "CAR", [], _); sub])], []);
            Prim (_, "CDR", [], _);
            Prim (_, "SWAP", [], []);
            Prim (_, "PAIR", [], pair_annots);
          ] ) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("A" :: acc) (List.rev_append pair_annots annots) sub
    | Seq
        ( _,
          [
            Prim (_, "DUP", [], []);
            Prim (_, "DIP", [Seq (_, [Prim (_, "CDR", [], []); sub])], []);
            Prim (_, "CAR", [], []);
            Prim (_, "PAIR", [], pair_annots);
          ] ) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("D" :: acc) (List.rev_append pair_annots annots) sub
    | _ -> None
  in
  match steps [] [] expanded with
  | Some (loc, steps, annots, code) ->
      let name = String.concat "" ("MAP_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [code], List.rev annots))
  | None -> None

let unexpand_deprecated_dxiiivp expanded =
  (* transparently turn the old expansion of deprecated [DI...IP] to [DIP n] *)
  match expanded with
  | Seq
      ( loc,
        [Prim (_, "DIP", [(Seq (_, [Prim (_, "DIP", [_], [])]) as sub)], [])] )
    ->
      let rec count acc = function
        | Seq (_, [Prim (_, "DIP", [sub], [])]) -> count (acc + 1) sub
        | sub -> (acc, sub)
      in
      let depth, sub = count 1 sub in
      Some (Prim (loc, "DIP", [Int (loc, Z.of_int depth); sub], []))
  | _ -> None

let unexpand_dupn expanded =
  match expanded with
  | Seq
      ( loc,
        [
          Prim
            (_, "DIP", [Int (_, np); Seq (_, [Prim (_, "DUP", [], annot)])], []);
          Prim (_, "DIG", [Int (nloc, ng)], []);
        ] )
    when Z.equal np (Z.pred ng) ->
      Some (Prim (loc, "DUP", [Int (nloc, ng)], annot))
  | _ -> None

let unexpand_deprecated_duuuuup expanded =
  (* transparently turn the old expansion of deprecated [DU...UP] to [DUP n] *)
  let rec expand n = function
    | Seq (loc, [Prim (nloc, "DUP", [], annot)]) ->
        if n = 1 then None
        else Some (Prim (loc, "DUP", [Int (nloc, Z.of_int n)], annot))
    | Seq (_, [Prim (_, "DIP", [expanded'], []); Prim (_, "SWAP", [], [])]) ->
        expand (n + 1) expanded'
    | _ -> None
  in
  expand 1 expanded

let rec normalize_pair_item ?(right = false) = function
  | P (i, a, b) ->
      P (i, normalize_pair_item a, normalize_pair_item ~right:true b)
  | A when right -> I
  | A -> A
  | I -> I

let unexpand_pappaiir expanded =
  match expanded with
  | Seq (_, [Prim (_, "PAIR", [], [])]) -> Some expanded
  | Seq (loc, (_ :: _ as nodes)) -> (
      let rec exec stack nodes =
        match (nodes, stack) with
        | [], _ -> stack
        (* support new expansion using [DIP n] *)
        | ( Prim (ploc, "DIP", [Int (loc, n); Seq (sloc, sub)], []) :: rest,
            a :: rstack )
          when Z.to_int n > 1 ->
            exec
              (a
              :: exec
                   rstack
                   [
                     Prim
                       (ploc, "DIP", [Int (loc, Z.pred n); Seq (sloc, sub)], []);
                   ])
              rest
        | Prim (_, "DIP", [Int (_, n); Seq (_, sub)], []) :: rest, a :: rstack
          when Z.to_int n = 1 ->
            exec (a :: exec rstack sub) rest
        | Prim (ploc, "DIP", [Int (loc, n); Seq (sloc, sub)], []) :: rest, []
          when Z.to_int n > 1 ->
            exec
              (A
              :: exec
                   []
                   [
                     Prim
                       (ploc, "DIP", [Int (loc, Z.pred n); Seq (sloc, sub)], []);
                   ])
              rest
        | Prim (_, "DIP", [Int (_, n); Seq (_, sub)], []) :: rest, []
          when Z.to_int n = 1 ->
            exec (A :: exec [] sub) rest
        (* support old expansion using [DIP] *)
        | Prim (_, "DIP", [Seq (_, sub)], []) :: rest, a :: rstack ->
            exec (a :: exec rstack sub) rest
        | Prim (_, "DIP", [Seq (_, sub)], []) :: rest, [] ->
            exec (A :: exec [] sub) rest
        | Prim (_, "PAIR", [], []) :: rest, a :: b :: rstack ->
            exec (P (0, a, b) :: rstack) rest
        | Prim (_, "PAIR", [], []) :: rest, [a] -> exec [P (0, a, I)] rest
        | Prim (_, "PAIR", [], []) :: rest, [] -> exec [P (0, A, I)] rest
        | _ -> raise_notrace Not_a_pair
      in
      match exec [] nodes with
      | [] -> None
      | res :: _ ->
          let res = normalize_pair_item res in
          let name = unparse_pair_item res in
          Some (Prim (loc, name, [], []))
      | exception Not_a_pair -> None)
  | _ -> None

let unexpand_unpappaiir expanded =
  match expanded with
  | Seq (loc, (_ :: _ as nodes)) -> (
      let rec exec stack nodes =
        match (nodes, stack) with
        | [], _ -> stack
        (* support new expansion using [DIP n] *)
        | ( Prim (ploc, "DIP", [Int (loc, n); Seq (sloc, sub)], []) :: rest,
            a :: rstack )
          when Z.to_int n > 1 ->
            exec
              (a
              :: exec
                   rstack
                   [
                     Prim
                       (ploc, "DIP", [Int (loc, Z.pred n); Seq (sloc, sub)], []);
                   ])
              rest
        | Prim (_, "DIP", [Int (_, n); Seq (_, sub)], []) :: rest, a :: rstack
          when Z.to_int n = 1 ->
            exec (a :: exec rstack sub) rest
        | Prim (ploc, "DIP", [Int (loc, n); Seq (sloc, sub)], []) :: rest, []
          when Z.to_int n > 1 ->
            exec
              (A
              :: exec
                   []
                   [
                     Prim
                       (ploc, "DIP", [Int (loc, Z.pred n); Seq (sloc, sub)], []);
                   ])
              rest
        | Prim (_, "DIP", [Int (_, n); Seq (_, sub)], []) :: rest, []
          when Z.to_int n = 1 ->
            exec (A :: exec [] sub) rest
        (* support old expansion using [DIP] *)
        | Prim (_, "DIP", [Seq (_, sub)], []) :: rest, a :: rstack ->
            exec (a :: exec rstack sub) rest
        | Prim (_, "DIP", [Seq (_, sub)], []) :: rest, [] ->
            exec (A :: exec [] sub) rest
        | ( Seq
              ( _,
                [
                  Prim (_, "DUP", [], []);
                  Prim (_, "CAR", [], []);
                  Prim (_, "DIP", [Seq (_, [Prim (_, "CDR", [], [])])], []);
                ] )
            :: rest,
            a :: b :: rstack ) ->
            exec (P (0, a, b) :: rstack) rest
        | ( Seq
              ( _,
                [
                  Prim (_, "DUP", [], []);
                  Prim (_, "CAR", [], []);
                  Prim (_, "DIP", [Seq (_, [Prim (_, "CDR", [], [])])], []);
                ] )
            :: rest,
            [a] ) ->
            exec [P (0, a, I)] rest
        | ( Seq
              ( _,
                [
                  Prim (_, "DUP", [], []);
                  Prim (_, "CAR", [], []);
                  Prim (_, "DIP", [Seq (_, [Prim (_, "CDR", [], [])])], []);
                ] )
            :: rest,
            [] ) ->
            exec [P (0, A, I)] rest
        | _ -> raise_notrace Not_a_pair
      in
      match exec [] (List.rev nodes) with
      | [] -> None
      | res :: _ ->
          let res = normalize_pair_item res in
          let name = "UN" ^ unparse_pair_item res in
          Some (Prim (loc, name, [], []))
      | exception Not_a_pair -> None)
  | _ -> None

let unexpand_compare expanded =
  match expanded with
  | Seq (loc, [Prim (_, "COMPARE", [], _); Prim (_, "EQ", [], annot)]) ->
      Some (Prim (loc, "CMPEQ", [], annot))
  | Seq (loc, [Prim (_, "COMPARE", [], _); Prim (_, "NEQ", [], annot)]) ->
      Some (Prim (loc, "CMPNEQ", [], annot))
  | Seq (loc, [Prim (_, "COMPARE", [], _); Prim (_, "LT", [], annot)]) ->
      Some (Prim (loc, "CMPLT", [], annot))
  | Seq (loc, [Prim (_, "COMPARE", [], _); Prim (_, "GT", [], annot)]) ->
      Some (Prim (loc, "CMPGT", [], annot))
  | Seq (loc, [Prim (_, "COMPARE", [], _); Prim (_, "LE", [], annot)]) ->
      Some (Prim (loc, "CMPLE", [], annot))
  | Seq (loc, [Prim (_, "COMPARE", [], _); Prim (_, "GE", [], annot)]) ->
      Some (Prim (loc, "CMPGE", [], annot))
  | Seq
      ( loc,
        [
          Prim (_, "COMPARE", [], _);
          Prim (_, "EQ", [], _);
          Prim (_, "IF", args, annot);
        ] ) ->
      Some (Prim (loc, "IFCMPEQ", args, annot))
  | Seq
      ( loc,
        [
          Prim (_, "COMPARE", [], _);
          Prim (_, "NEQ", [], _);
          Prim (_, "IF", args, annot);
        ] ) ->
      Some (Prim (loc, "IFCMPNEQ", args, annot))
  | Seq
      ( loc,
        [
          Prim (_, "COMPARE", [], _);
          Prim (_, "LT", [], _);
          Prim (_, "IF", args, annot);
        ] ) ->
      Some (Prim (loc, "IFCMPLT", args, annot))
  | Seq
      ( loc,
        [
          Prim (_, "COMPARE", [], _);
          Prim (_, "GT", [], _);
          Prim (_, "IF", args, annot);
        ] ) ->
      Some (Prim (loc, "IFCMPGT", args, annot))
  | Seq
      ( loc,
        [
          Prim (_, "COMPARE", [], _);
          Prim (_, "LE", [], _);
          Prim (_, "IF", args, annot);
        ] ) ->
      Some (Prim (loc, "IFCMPLE", args, annot))
  | Seq
      ( loc,
        [
          Prim (_, "COMPARE", [], _);
          Prim (_, "GE", [], _);
          Prim (_, "IF", args, annot);
        ] ) ->
      Some (Prim (loc, "IFCMPGE", args, annot))
  | Seq (loc, [Prim (_, "EQ", [], _); Prim (_, "IF", args, annot)]) ->
      Some (Prim (loc, "IFEQ", args, annot))
  | Seq (loc, [Prim (_, "NEQ", [], _); Prim (_, "IF", args, annot)]) ->
      Some (Prim (loc, "IFNEQ", args, annot))
  | Seq (loc, [Prim (_, "LT", [], _); Prim (_, "IF", args, annot)]) ->
      Some (Prim (loc, "IFLT", args, annot))
  | Seq (loc, [Prim (_, "GT", [], _); Prim (_, "IF", args, annot)]) ->
      Some (Prim (loc, "IFGT", args, annot))
  | Seq (loc, [Prim (_, "LE", [], _); Prim (_, "IF", args, annot)]) ->
      Some (Prim (loc, "IFLE", args, annot))
  | Seq (loc, [Prim (_, "GE", [], _); Prim (_, "IF", args, annot)]) ->
      Some (Prim (loc, "IFGE", args, annot))
  | _ -> None

let unexpand_asserts expanded =
  match expanded with
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF",
              [
                Seq (_, []);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT", [], []))
  | Seq
      ( loc,
        [
          Seq (_, [Prim (_, "COMPARE", [], []); Prim (_, comparison, [], [])]);
          Prim
            ( _,
              "IF",
              [
                Seq (_, []);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_CMP" ^ comparison, [], []))
  | Seq
      ( loc,
        [
          Prim (_, comparison, [], []);
          Prim
            ( _,
              "IF",
              [
                Seq (_, []);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_" ^ comparison, [], []))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_NONE",
              [
                Seq (_, [Prim (_, "RENAME", [], annot)]);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_NONE", [], annot))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_NONE",
              [
                Seq (_, []);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_NONE", [], []))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_NONE",
              [
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
                Seq (_, []);
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_SOME", [], []))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_NONE",
              [
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
                Seq (_, [Prim (_, "RENAME", [], annot)]);
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_SOME", [], annot))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_LEFT",
              [
                Seq (_, []);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_LEFT", [], []))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_LEFT",
              [
                Seq (_, [Prim (_, "RENAME", [], annot)]);
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_LEFT", [], annot))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_LEFT",
              [
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
                Seq (_, []);
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_RIGHT", [], []))
  | Seq
      ( loc,
        [
          Prim
            ( _,
              "IF_LEFT",
              [
                Seq
                  ( _,
                    [
                      Seq
                        ( _,
                          [
                            Prim (_, "UNIT", [], []);
                            Prim (_, "FAILWITH", [], []);
                          ] );
                    ] );
                Seq (_, [Prim (_, "RENAME", [], annot)]);
              ],
              [] );
        ] ) ->
      Some (Prim (loc, "ASSERT_RIGHT", [], annot))
  | _ -> None

let unexpand_if_some = function
  | Seq (loc, [Prim (_, "IF_NONE", [left; right], annot)]) ->
      Some (Prim (loc, "IF_SOME", [right; left], annot))
  | _ -> None

let unexpand_if_right = function
  | Seq (loc, [Prim (_, "IF_LEFT", [left; right], annot)]) ->
      Some (Prim (loc, "IF_RIGHT", [right; left], annot))
  | _ -> None

let unexpand_fail = function
  | Seq (loc, [Prim (_, "UNIT", [], []); Prim (_, "FAILWITH", [], [])]) ->
      Some (Prim (loc, "FAIL", [], []))
  | _ -> None

let unexpand original =
  let try_unexpansions unexpanders =
    Option.value
      ~default:original
      (List.fold_left
         (fun acc f -> Option.either_f acc (fun () -> f original))
         None
         unexpanders)
  in
  try_unexpansions
    [
      unexpand_asserts;
      unexpand_carn_and_cdrn;
      unexpand_caddadr;
      unexpand_set_caddadr;
      unexpand_map_caddadr;
      unexpand_deprecated_dxiiivp;
      unexpand_pappaiir;
      unexpand_unpappaiir;
      unexpand_deprecated_duuuuup;
      unexpand_dupn;
      unexpand_compare;
      unexpand_if_some;
      unexpand_if_right;
      unexpand_fail;
    ]

(*
   If an argument of Prim is a sequence, we do not want to unexpand
   its root in case the source already contains an expanded macro. In
   which case unexpansion would remove surrounding braces and generate
   ill-formed code.

   For example, DIIP { DIP { DUP }; SWAP } is not unexpandable but
   DIIP {{ DIP { DUP }; SWAP }} (note the double braces) is unexpanded
   to DIIP { DUUP }.

   unexpand_rec_but_root is the same as unexpand_rec but does not try
   to unexpand at root *)

let rec unexpand_rec expr = unexpand_rec_but_root (unexpand expr)

and unexpand_rec_but_root = function
  | Seq (loc, items) -> Seq (loc, List.map unexpand_rec items)
  | Prim (loc, name, args, annot) ->
      Prim (loc, name, List.map unexpand_rec_but_root args, annot)
  | (Int _ | String _ | Bytes _) as atom -> atom

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"michelson.macros.unexpected_annotation"
    ~title:"Unexpected annotation"
    ~description:
      "A macro had an annotation, but no annotation was permitted on this \
       macro."
    ~pp:(fun ppf -> Format.fprintf ppf "Unexpected annotation on macro %s.")
    (obj1 (req "macro_name" string))
    (function Unexpected_macro_annotation str -> Some str | _ -> None)
    (fun s -> Unexpected_macro_annotation s) ;
  register_error_kind
    `Permanent
    ~id:"michelson.macros.sequence_expected"
    ~title:"Macro expects a sequence"
    ~description:"An macro expects a sequence, but a sequence was not provided"
    ~pp:(fun ppf name ->
      Format.fprintf
        ppf
        "Macro %s expects a sequence, but did not receive one."
        name)
    (obj1 (req "macro_name" string))
    (function Sequence_expected name -> Some name | _ -> None)
    (fun name -> Sequence_expected name) ;
  register_error_kind
    `Permanent
    ~id:"michelson.macros.bas_arity"
    ~title:"Wrong number of arguments to macro"
    ~description:"A wrong number of arguments was provided to a macro"
    ~pp:(fun ppf (name, got, exp) ->
      Format.fprintf
        ppf
        "Macro %s expects %d arguments, was given %d."
        name
        exp
        got)
    (obj3
       (req "macro_name" string)
       (req "given_number_of_arguments" uint16)
       (req "expected_number_of_arguments" uint16))
    (function
      | Invalid_arity (name, got, exp) -> Some (name, got, exp) | _ -> None)
    (fun (name, got, exp) -> Invalid_arity (name, got, exp))
