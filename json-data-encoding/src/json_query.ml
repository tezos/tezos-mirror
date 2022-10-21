(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright 2014 OCamlPro                                                   *)
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

type path = path_item list

and path_item = [`Field of string | `Index of int | `Star | `Next]

exception Illegal_pointer_notation of string * int * string

exception Unsupported_path_item of path_item * string

exception Cannot_merge of path

(*-- path operations -------------------------------------------------------*)

let print_path_as_json_path ?(wildcards = true) ppf = function
  | [] -> Format.fprintf ppf "/"
  | nonempty ->
      let rec print ppf = function
        | [] -> ()
        | `Field n :: rem -> Format.fprintf ppf "/%s%a" n print rem
        | `Index n :: rem -> Format.fprintf ppf "[%d]%a" n print rem
        | `Next :: rem when wildcards -> Format.fprintf ppf "-%a" print rem
        | `Star :: rem when wildcards -> Format.fprintf ppf "*%a" print rem
        | (`Next | `Star) :: _ ->
            raise (Unsupported_path_item (`Star, "JSON path w/o wildcards"))
      in
      print ppf nonempty

let print_path_as_json_pointer ?(wildcards = true) ppf = function
  | [] -> Format.fprintf ppf "/"
  | nonempty ->
      let rec print ppf = function
        | [] -> ()
        | `Field n :: rem -> Format.fprintf ppf "/%s%a" n print rem
        | `Index n :: rem -> Format.fprintf ppf "/%d%a" n print rem
        | `Next :: rem when wildcards -> Format.fprintf ppf "/-%a" print rem
        | `Next :: _ ->
            raise (Unsupported_path_item (`Star, "JSON pointer w/o wildcards"))
        | `Star :: _ -> raise (Unsupported_path_item (`Star, "JSON pointer"))
      in
      print ppf nonempty

let json_pointer_of_path ?wildcards path =
  Format.asprintf "%a" (print_path_as_json_pointer ?wildcards) path

let path_of_json_pointer ?(wildcards = true) str =
  let buf = Buffer.create 100 in
  let len = String.length str in
  let rec slashes acc i =
    if i >= len then List.rev acc
    else if str.[i] = '/' then slashes acc (i + 1)
    else item acc i
  and item acc i =
    if i >= len then List.rev (interp () :: acc)
    else
      match str.[i] with
      | '/' -> slashes (interp () :: acc) i
      | '~' ->
          if i + 1 >= len then
            raise
              (Illegal_pointer_notation (str, i, "Unterminated escape sequence")) ;
          (match str.[i] with
          | '0' -> Buffer.add_char buf '~'
          | '1' -> Buffer.add_char buf '/'
          | _illegal ->
              raise
                (Illegal_pointer_notation
                   (str, i + 1, "Illegal escape character"))) ;
          item acc (i + 1)
      | unescaped ->
          Buffer.add_char buf unescaped ;
          item acc (i + 1)
  and interp () =
    let field = Buffer.contents buf in
    Buffer.clear buf ;
    if field = "-" then
      if wildcards then `Next
      else raise (Unsupported_path_item (`Next, "JSON pointer w/o wildcards"))
    else try `Index (int_of_string field) with _ -> `Field field
  in
  if len = 0 then []
  else if str.[0] <> '/' then
    raise (Illegal_pointer_notation (str, 0, "Missing initial slash"))
  else slashes [] 1

(*-- queries ---------------------------------------------------------------*)

module Make (Repr : Json_repr.Repr) = struct
  let rec query path json =
    match (path, Repr.view json) with
    | [], _ -> json
    | `Field n :: rempath, `O ((n', v) :: rem) ->
        if n = n' then query rempath v else query path (Repr.repr (`O rem))
    | `Index i :: rempath, `A cells ->
        let i = if i < 0 then List.length cells - i else i in
        query rempath (List.nth cells i)
    | `Star :: rempath, `O ((_, v) :: rem) -> (
        try query rempath v with Not_found -> query path (Repr.repr (`O rem)))
    | `Star :: rempath, `A (v :: rem) -> (
        try query rempath v with Not_found -> query path (Repr.repr (`A rem)))
    | _, _ -> raise Not_found

  let query_all path json =
    let res = ref [] in
    let rec query path json =
      match (path, Repr.view json) with
      | [], _ -> res := json :: !res
      | `Field n :: rempath, `O ((n', v) :: rem) ->
          if n = n' then query rempath v else query path (Repr.repr (`O rem))
      | `Index i :: rempath, `A cells ->
          let i = if i < 0 then List.length cells - i else i in
          query rempath (List.nth cells i)
      | `Star :: rempath, `O fields ->
          List.iter (fun (_, v) -> query rempath v) fields
      | `Star :: rempath, `A cells -> List.iter (query rempath) cells
      | _, _ -> ()
    in
    query path json ;
    !res

  (*-- updates ---------------------------------------------------------------*)

  let sort_fields = List.sort (fun (l, _) (r, _) -> String.compare l r)

  let equals l r =
    let rec canon v =
      match Repr.view v with
      | `O l ->
          Repr.repr
            (`O (List.rev_map (fun (n, o) -> (n, canon o)) l |> sort_fields))
      | `A l -> Repr.repr (`A (List.rev_map canon l))
      | _ -> v
    in
    canon l = canon r

  let merge l r =
    let rec merge path l r =
      match (Repr.view l, Repr.view r) with
      | `O l, `O r ->
          Repr.repr
            (`O (merge_fields path [] (sort_fields (List.rev_append l r))))
      | `Null, v | v, `Null -> Repr.repr v
      | `A l, `A r -> Repr.repr (`A (merge_cells path 0 [] l r))
      | _ -> if equals l r then l else raise (Cannot_merge (List.rev path))
    and merge_cells path i acc l r =
      match (l, r) with
      | [], rem | rem, [] -> List.rev_append acc rem
      | l :: ls, r :: rs ->
          let item = merge (`Index i :: path) l r in
          merge_cells path (succ i) (item :: acc) ls rs
    and merge_fields path acc = function
      | (lf, lv) :: ((rf, rv) :: rem as rrem) ->
          if lf = rf then
            let item = merge (`Field lf :: path) lv rv in
            merge_fields path ((lf, item) :: acc) rem
          else merge_fields path ((lf, lv) :: acc) rrem
      | ([_] | []) as last -> last
    in
    merge [] l r

  let insert ?(merge = merge) path value root =
    let norevpath sub =
      let rec loop acc = function
        | l when l == sub -> acc
        | item :: items -> loop (item :: acc) items
        | [] -> (* absurd *) assert false
      in
      loop [] path
    in
    let revpath sub = List.rev (norevpath sub) in
    let merge path l r =
      try merge l r
      with Cannot_merge sub ->
        raise (Cannot_merge (List.rev_append (norevpath path) sub))
    in
    let rec nulls acc n last =
      if n <= 0 then List.rev (last :: acc)
      else nulls (Repr.repr `Null :: acc) (pred n) last
    in
    let rec insert ?root path =
      let root = Option.map Repr.view root in
      match (path, root) with
      (* create objects *)
      | `Field n :: rempath, None -> Repr.repr (`O [(n, insert rempath)])
      | (`Index 0 | `Star | `Next) :: rempath, None ->
          Repr.repr (`A [insert rempath])
      | `Index i :: rempath, None ->
          if i < 0 then raise (Cannot_merge (revpath path)) ;
          Repr.repr (`A (nulls [] (max 0 (pred i)) (insert rempath)))
      | [], None -> value
      (* insert in existing *)
      | [], Some value' -> merge path value (Repr.repr value')
      | `Field n :: rempath, Some (`O fields) ->
          Repr.repr (`O (insert_fields [] n rempath fields))
      | `Index i :: rempath, Some (`A cells) ->
          let i = if i < 0 then List.length cells - i else i in
          if i < 0 then raise (Cannot_merge (revpath path)) ;
          Repr.repr (`A (insert_cells [] i rempath cells))
      | `Next :: rempath, Some (`A cells) ->
          Repr.repr (`A (List.rev_append (List.rev cells) [insert rempath]))
      (* multiple insertions *)
      | `Star :: rempath, Some (`A cells) ->
          Repr.repr (`A (List.map (fun root -> insert ~root rempath) cells))
      | `Star :: rempath, Some (`O fields) ->
          Repr.repr
            (`O (List.map (fun (n, root) -> (n, insert ~root rempath)) fields))
      | [`Star], Some root -> merge path value (Repr.repr root)
      (* FIXME: make explicit unhandled cases *)
      | _, Some _ -> raise (Cannot_merge (revpath path))
    and insert_fields acc n rempath fields =
      match fields with
      | [] -> List.rev ((n, insert rempath) :: acc)
      | (n', root) :: rem when n = n' ->
          List.rev_append ((n, insert ~root rempath) :: acc) rem
      | other :: rem -> insert_fields (other :: acc) n rempath rem
    and insert_cells acc n rempath cells =
      match (cells, n) with
      | [], n -> nulls acc n (insert rempath)
      | root :: rem, 0 -> List.rev_append (insert ~root rempath :: acc) rem
      | other :: rem, n -> insert_cells (other :: acc) (n - 1) rempath rem
    in
    insert ~root path

  let replace path value root =
    insert ~merge:(fun value _prev -> value) path value root

  let insert path value root = insert path value root
end

let path_operator_name = function
  | `Field _ -> "field access"
  | `Index _ -> "array access"
  | `Star -> "wildcard"
  | `Next -> "array append"

let print_error ?print_unknown ppf err =
  match err with
  | Illegal_pointer_notation (notation, pos, msg) ->
      Format.fprintf
        ppf
        "@[<v 2>Illegal pointer notation@,At character %d of %S@,%s@]"
        pos
        notation
        msg
  | Unsupported_path_item (item, msg) ->
      Format.fprintf
        ppf
        "Path operator %s unsupported by %s"
        (path_operator_name item)
        msg
  | Cannot_merge [] -> Format.fprintf ppf "Unmergeable objects"
  | Cannot_merge path ->
      Format.fprintf
        ppf
        "Unmergeable objects, incompatibility at %a"
        (print_path_as_json_path ~wildcards:true)
        path
  | exn -> (
      match print_unknown with
      | Some print_unknown -> print_unknown ppf exn
      | None -> Format.fprintf ppf "Unhandled error %s" (Printexc.to_string exn)
      )

include Make (Json_repr.Ezjsonm)
