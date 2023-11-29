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

let default_prefix = "michelson_test_scripts"

let default_tzt_prefix = "tzt_reference_test_suite"

type version_range = {range_start : int; range_end : int option}

let unversioned_range = {range_start = 0; range_end = None}

let in_range {range_start; range_end} protocol =
  let n = Protocol.number protocol in
  let range_end = Option.value range_end ~default:Int.max_int in
  range_start <= n && n <= range_end

type t = {
  prefix : string;
  dirname : string list;
  basename : string;
  name : string;
  version_range : version_range;
  extension : string;
  depth : int;
}

let pretty_string
    {prefix; dirname; basename; name; version_range; extension; depth} =
  let option_to_string f opt =
    match opt |> Option.map f with None -> "None" | Some s -> "Some " ^ s
  in
  String.concat
    "\n"
    [
      "{";
      "  prefix = " ^ prefix;
      "  dirname = " ^ String.concat Filename.dir_sep dirname;
      "  basename = " ^ basename;
      "  name = " ^ name;
      "  version_range = ";
      "  {";
      "    range_start = " ^ string_of_int version_range.range_start;
      "    range_end = "
      ^ option_to_string string_of_int version_range.range_end;
      "  }";
      "  extension = " ^ extension;
      "  depth = " ^ string_of_int depth;
      "}";
    ]

let parse_basename : string -> (string * version_range * string) option =
  let re3 = rex "(.*)_([0-9]{3,})_([0-9]{3,})\\.(tz.*)" in
  let re2 = rex "(.*)_([0-9]{3,})\\.(tz.*)" in
  let re1 = rex "(.*).(tz.*)" in
  fun s ->
    match s =~**** re3 with
    | Some (name, range_start, range_end, extension) ->
        let version_range =
          {
            range_start = int_of_string range_start;
            range_end = Some (int_of_string range_end);
          }
        in
        Some (name, version_range, extension)
    | None -> (
        match s =~*** re2 with
        | Some (name, range_start, extension) ->
            let version_range =
              {range_start = int_of_string range_start; range_end = None}
            in
            Some (name, version_range, extension)
        | None -> (
            match s =~** re1 with
            | Some (name, extension) -> Some (name, unversioned_range, extension)
            | None -> None))

let memoize f =
  let cache = Hashtbl.create 16 in
  fun key ->
    match Hashtbl.find_opt cache key with
    | Some res -> res
    | None ->
        let res = f key in
        Hashtbl.add cache key res ;
        res

let walk (prefix, maxdepth) =
  let rec loop depth dirname =
    if depth >= maxdepth then []
    else
      List.fold_left ( // ) prefix dirname
      |> Sys.readdir |> Array.to_list
      |> List.concat_map @@ fun basename ->
         let dirname' = dirname @ [basename] in
         let dirname_s' = List.fold_left ( // ) prefix dirname' in
         if Sys.is_directory dirname_s' then loop (depth + 1) dirname'
         else
           match parse_basename basename with
           | None -> []
           | Some (name, version_range, extension) ->
               [
                 {
                   prefix;
                   dirname;
                   basename;
                   name;
                   version_range;
                   extension;
                   depth;
                 };
               ]
  in
  loop 1 [] |> List.sort (Stdlib.compare : t -> t -> int)

let walk =
  let f = memoize walk in
  fun ?(prefix = default_prefix) ?(maxdepth = Int.max_int) () ->
    f (prefix, maxdepth)

let path ?(no_prefix = false) t =
  if no_prefix then String.concat Filename.dir_sep t.dirname // t.basename
  else t.prefix // String.concat Filename.dir_sep t.dirname // t.basename

let split_name name =
  match List.rev name with
  | [] -> Error (__LOC__, "name must not be an empty list")
  | last :: dirname_rev -> Ok (List.rev dirname_rev, last)

let name t = t.dirname @ [t.name]

let name_s t = name t |> String.concat "/"

(** A private helper type for use in [choose_versioned]. Describes a script
    being either unversioned (such as foo.tz), or versioned (such as
    foo_NNN.tz or foo_NNN_MMM.tz) *)
type kind = Versioned | Unversioned

exception Test_fail of string * string

(** [choose_versioned t1 t2] decides which of [t1] or [t2] to keep. It assumes
    that both [t1] and [t2] have already been checked against a protocol, and
    that the protocol falls within their ranges.

    If [t1] and [t2] have a different [kind], then the Versioned one is always
    chosen. Consider:
    - foo.tz
    - foo_015.tz

    Assuming the protocol we care about is 015, and we look at each version range
    in isolation, then we have:
    - foo.tz (0 to infinity)
    - foo_015.tz (015 to infinity)

    So we can say foo_015.tz supercedes foo.tz and the version_ranges should
    actually be:
    - foo.tz (0 to 014)
    - foo_015.tz (015 to infinity)

    Next, if both [t1] and [t2] are versioned, then we do a few checks:
    - If the [range_start]s are the same, then we cannot disambiguate them.

      For example:
      - foo_014.tz (014 to infinity)
      - foo_014_015.tz (014 to 015)

      If we are looking for protocol 015, then we cannot decide between the two,
      so we return an error.
    - If the [range_start]s are different, then we determine which one has the higher
      range_start.

      For example, assume the following:
      - foo_014.tz (014 to infinity)
      - foo_015.tz (015 to infinity)

      If we are searching for protocol 015, both files are individually in
      range. If the end range of the smaller one is infinity, then we say that
      the larger range_start supercedes the smaller one, and we get:
      - foo_014.tz (014 to 014)
      - foo_015.tz (015 to infinity)
    - If the smaller range_start has a range_end which is > the larger scripts'
      range_start, then we cannot disambiguate.

      For example, assume the following:
      - foo_014_015.tz
      - foo_015.tz

      If we are looking for protocol 015, then we cannot say which one of the
      scripts is the correct one, so we return an error. *)
let choose_versioned_exn t1 t2 ~protocol =
  let kind v = if v = unversioned_range then Unversioned else Versioned in
  let res =
    match (kind t1.version_range, kind t2.version_range) with
    | Versioned, Unversioned -> Ok t1
    | Unversioned, Versioned -> Ok t2
    | Unversioned, Unversioned ->
        (* This case should never happen. It would indicate that we found two
           unversioned scripts with the same name, such as foo.tz and foo.tz. *)
        Error Unversioned
    | Versioned, Versioned -> (
        let s1 = t1.version_range.range_start in
        let s2 = t2.version_range.range_start in
        if s1 = s2 then Error Versioned
        else
          let tsmall, tlarge = if s1 < s2 then (t1, t2) else (t2, t1) in
          match tsmall.version_range.range_end with
          | None -> Ok tlarge
          | Some e1 ->
              if e1 < tlarge.version_range.range_start then Ok tlarge
              else Error Versioned)
  in
  match res with
  | Ok x -> x
  | Error kind ->
      let kind =
        match kind with
        | Versioned -> "versioned"
        | Unversioned -> "unversioned"
      in
      raise
      @@ Test_fail
           ( __LOC__,
             sf
               "found more than one %s match for (name: %s) and (protocol: %03d)\n\
               \ - %s\n\
               \ - %s"
               kind
               (name_s t1)
               (Protocol.number protocol)
               (path ~no_prefix:true t1)
               (path ~no_prefix:true t2) )

let find_all_res (prefix, maxdepth, protocol) =
  (* Group by name and reduce matches down to just one. *)
  try
    walk ~prefix ~maxdepth ()
    |> List.fold_left
         (fun acc t ->
           if not (protocol |> in_range t.version_range) then acc
           else
             String_map.update
               (name_s t)
               (function
                 | None -> Some t
                 | Some t' -> Some (choose_versioned_exn t t' ~protocol))
               acc)
         String_map.empty
    |> String_map.bindings |> List.map snd |> Result.ok
  with Test_fail (__LOC__, msg) -> Error (__LOC__, msg)

let find_all_res =
  let f = memoize find_all_res in
  fun ?(prefix = default_prefix) ?(maxdepth = Int.max_int) protocol ->
    f (prefix, maxdepth, protocol)

let ok_or_fail = function
  | Ok x -> x
  | Error (__LOC__, msg) -> Test.fail ~__LOC__ "%s" msg

let find_all ?prefix ?maxdepth protocol =
  find_all_res ?prefix ?maxdepth protocol |> ok_or_fail

let find_res (prefix, maxdepth, name, protocol) =
  let ( let* ) = Result.bind in
  let* dirname, name = split_name name in
  let* v = find_all_res ~prefix ~maxdepth protocol in
  match v |> List.find_opt @@ fun t -> t.dirname = dirname && t.name = name with
  | Some t -> Ok t
  | None ->
      let dirname_s = String.concat Filename.dir_sep dirname in
      let version = Protocol.number protocol in
      Error
        ( __LOC__,
          sf
            "could not find Michelson script %S for protocol %03d in %s: found \
             no file named %s_NNN.tz such that 000 <= NNN <= %03d; found no \
             file named %s_NNN_MMM.tz such that 000 <= %03d <= MMM; found no \
             unversioned file named %s.tz"
            (dirname_s // name)
            version
            prefix
            name
            version
            name
            version
            name )

let find_res =
  let f = memoize find_res in
  fun ?(prefix = default_prefix) ?(maxdepth = Int.max_int) name protocol ->
    f (prefix, maxdepth, name, protocol)

let find ?prefix ?maxdepth name protocol =
  find_res ?prefix ?maxdepth name protocol |> ok_or_fail

let blacklist dirs ts =
  ts
  |> List.filter @@ fun (t : t) ->
     not @@ List.exists (fun dir -> List.equal String.equal dir t.dirname) dirs

let whitelist dirs ts =
  ts
  |> List.filter @@ fun (t : t) ->
     List.exists (fun dir -> List.equal String.equal dir t.dirname) dirs

let find_all_in ?prefix ?maxdepth protocol dirs =
  find_all ?prefix ?maxdepth protocol |> whitelist [dirs]

let find_all_legacy ?prefix ?maxdepth protocol =
  find_all ?prefix ?maxdepth protocol |> whitelist [["legacy"]]

let find_all_well_typed ?prefix ?maxdepth protocol =
  find_all ?prefix ?maxdepth protocol |> blacklist [["ill_typed"]; ["legacy"]]

let find_all_ill_typed ?prefix ?maxdepth protocol =
  find_all ?prefix ?maxdepth protocol |> whitelist [["ill_typed"]]

let find_all_tzt_tests ?(prefix = default_tzt_prefix) ?maxdepth protocol =
  find_all ~prefix ?maxdepth protocol
