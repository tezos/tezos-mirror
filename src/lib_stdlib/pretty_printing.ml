(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Format
module Handled_tags = Set.Make (String)

let handled_tags =
  let colors =
    [
      "default";
      "black";
      "blue";
      "cyan";
      "green";
      "magenta";
      "red";
      "white";
      "yellow";
    ]
  in
  let grounds = ["fg"; "bg"] in
  let ansi_colors =
    List.fold_left
      (fun acc ground ->
        List.fold_left
          (fun acc color -> (ground ^ "_" ^ color) :: acc)
          acc
          colors)
      []
      grounds
  in
  let emphasis = ["bold"; "italic"; "underline"] in
  let negate tag = "/" ^ tag in
  let shorten tag = String.sub tag 0 1 in
  List.fold_left
    (fun acc tag ->
      tag :: shorten tag :: negate tag :: negate (shorten tag) :: acc)
    ansi_colors
    emphasis
  |> Handled_tags.of_list

let handles tag = Handled_tags.mem tag handled_tags

module type Attribute = sig
  type t

  val of_string : string -> t

  val to_ansi : t -> int
end

module type Disablable_Attribute = sig
  include Attribute

  val disable : t -> t
end

module Color : Disablable_Attribute = struct
  (** Type representing a color with no distinction between a foreground
      or background one. The default color is an ANSI code that returns to
      the default color set by the underlying printing engine (meaning that
      Default is not Black for foreground or White for background) *)

  type t =
    | Default
    | Black
    | Blue
    | Cyan
    | Green
    | Magenta
    | Red
    | White
    | Yellow

  let of_string = function
    | "default" -> Default
    | "black" -> Black
    | "blue" -> Blue
    | "cyan" -> Cyan
    | "green" -> Green
    | "magenta" -> Magenta
    | "red" -> Red
    | "white" -> White
    | "yellow" -> Yellow
    | s ->
        failwith
          (Printf.sprintf "'%s' is not a known color for pretty-printing" s)

  let to_ansi = function
    | Default -> 9
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7

  let disable _ = Default
end

module Ground : Attribute = struct
  type t = Foreground | Background

  let of_string = function
    | "fg" -> Foreground
    | "bg" -> Background
    | s ->
        failwith
          (Printf.sprintf
             "'%s' is not a known ground for pretty-printing. Known grounds \
              are 'fg' and 'bg'"
             s)

  let to_ansi = function Foreground -> 30 | Background -> 40
end

module Ansi_color : Disablable_Attribute = struct
  type t = {color : Color.t; ground : Ground.t}

  let of_string s =
    match String.split_on_char '_' s with
    | [ground; color] ->
        {color = Color.of_string color; ground = Ground.of_string ground}
    | _ ->
        failwith
          (Printf.sprintf
             "'%s' is not a known ansi color for pretty-printing. Ansi colors \
              are defined with 'fg|bg_color'"
             s)

  let to_ansi {color; ground} = Ground.to_ansi ground + Color.to_ansi color

  let disable t = {t with color = Color.disable t.color}
end

module Emphasis : Disablable_Attribute = struct
  type emphasis = Bold | Italic | Underline

  type t = emphasis * bool

  let of_string s =
    (* If the emphasis starts with / we want it disabled
       In either case we match the string where / has been removed *)
    let s, enabled =
      if String.starts_with ~prefix:"/" s then
        (String.sub s 1 (String.length s - 1), false)
      else (s, true)
    in
    let emphasis =
      match s with
      | "bold" | "b" -> Bold
      | "italic" | "i" -> Italic
      | "underline" | "u" -> Underline
      | s ->
          failwith
            (Printf.sprintf
               "'%s' is not a known emphasis for pretty-printing. Try 'bold', \
                'italic' or 'underline'"
               s)
    in
    (emphasis, enabled)

  let to_ansi_emphasis = function Bold -> 1 | Italic -> 3 | Underline -> 4

  let to_ansi (emphasis, enable) =
    match emphasis with
    | Bold ->
        (* Of course there was an exception. 21 is not "unbold", it's
           "double underline" and in some terminals its "unbold" *)
        if enable then to_ansi_emphasis Bold else 22
    | _ -> to_ansi_emphasis emphasis + if enable then 0 else 20

  (* This will also disable already disabled emphasis
     but that's not an issue *)
  let disable (emphasis, _) = (emphasis, false)
end

module Style : Disablable_Attribute = struct
  type t = Emphasis of Emphasis.t | Ansi_color of Ansi_color.t

  let of_string s =
    if String.contains s '_' then Ansi_color (Ansi_color.of_string s)
    else Emphasis (Emphasis.of_string s)

  let to_ansi = function
    | Emphasis e -> Emphasis.to_ansi e
    | Ansi_color c -> Ansi_color.to_ansi c

  let disable = function
    | Emphasis e -> Emphasis (Emphasis.disable e)
    | Ansi_color c -> Ansi_color (Ansi_color.disable c)
end

module Semantic_tag = struct
  let extract_string = function
    (* Styles are separated by ';', we split them immediately *)
    | String_tag s -> String.split_on_char ';' s |> List.map String.trim
    | _stag -> Stdlib.failwith "Pretty printing: invalid semantic tag"

  let ansi_tag l =
    Format.asprintf
      "\x1B[%am"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";")
         pp_print_int)
      l

  (* This function could be made simpler if [extract_string] returned
     immediately the style instead of a string list but this solution is
     more readable and allows to call [List.map] only once with the complete
     stream of actions in a single place *)
  let start_mark_ansi_stag t =
    extract_string t
    |> List.map (fun s -> Style.of_string s |> Style.to_ansi)
    |> ansi_tag

  let stop_mark_ansi_stag t =
    extract_string t
    |> List.map (fun s -> Style.of_string s |> Style.disable |> Style.to_ansi)
    |> ansi_tag

  let add_ansi_marking formatter =
    let open Format in
    let old_stag_functions = pp_get_formatter_stag_functions formatter () in
    let old_print_tags = pp_get_print_tags formatter () in
    let old_mark_tags = pp_get_mark_tags formatter () in
    pp_set_mark_tags formatter true ;
    pp_set_print_tags formatter false ;
    pp_set_formatter_stag_functions
      formatter
      {
        old_stag_functions with
        mark_open_stag = start_mark_ansi_stag;
        mark_close_stag = stop_mark_ansi_stag;
      } ;
    fun () ->
      pp_set_mark_tags formatter old_mark_tags ;
      pp_set_print_tags formatter old_print_tags ;
      pp_set_formatter_stag_functions formatter old_stag_functions
end

include Semantic_tag

let pp_with_padding left right char ppf s =
  let left_padding = String.make left char in
  let right_padding = String.make right char in
  Format.fprintf ppf "%s%s%s" left_padding s right_padding

let pp_centered ?char width ppf s =
  let start = (width / 2) + (String.length s / 2) in
  match char with
  | None -> Format.fprintf ppf "%*s%*s" start s (width - start) ""
  | Some char -> pp_with_padding start (width - start) char ppf s

let pp_right_aligned ?char width ppf s =
  let left_margin = width - String.length s in
  match char with
  | None -> Format.fprintf ppf "%*s%s" left_margin "" s
  | Some char -> pp_with_padding left_margin 0 char ppf s

let pp_left_aligned ?char width ppf s =
  let right_margin = width - String.length s in
  match char with
  | None -> Format.fprintf ppf "%s%*s" s right_margin ""
  | Some char -> pp_with_padding 0 right_margin char ppf s
