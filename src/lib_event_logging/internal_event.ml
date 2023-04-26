(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Error_monad

module List = struct
  include List
  include Tezos_stdlib.TzList
end

module String = struct
  include String
  include Tezos_stdlib.TzString
  module Set = Tezos_error_monad.TzLwtreslib.Set.Make (String)
end

let valid_char c =
  match c with
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '@' | '-' | '_' | '+' | '=' | '~' ->
      true
  | _ -> false

let check_name_exn : string -> (string -> char -> exn) -> unit =
 fun name make_exn ->
  String.iter
    (fun c -> if valid_char c then () else raise (make_exn name c))
    name ;
  ()

(* Levels are declared from the lowest to the highest so that
   polymorphic comparison can be used to check whether a message
   should be printed. *)
type level = Debug | Info | Notice | Warning | Error | Fatal

let default_error_fallback_logger =
  ref (fun s ->
      Format.eprintf "%s" s ;
      Lwt.return_unit)

module Level = struct
  type t = level

  let default = Info

  let to_string = function
    | Debug -> "debug"
    | Info -> "info"
    | Notice -> "notice"
    | Warning -> "warning"
    | Error -> "error"
    | Fatal -> "fatal"

  let of_string str =
    let str = String.lowercase_ascii str in
    match str with
    | "debug" -> Some Debug
    | "info" -> Some Info
    | "notice" -> Some Notice
    | "warning" -> Some Warning
    | "error" -> Some Error
    | "fatal" -> Some Fatal
    | _ -> None

  let encoding =
    let open Data_encoding in
    string_enum
      (List.map
         (fun l -> (to_string l, l))
         [Debug; Info; Notice; Warning; Error; Fatal])

  include Compare.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
  end)
end

module Section : sig
  type t

  include Compare.S with type t := t

  val empty : t

  val make : string list -> t

  val make_sanitized : string list -> t

  val name : t -> string

  val is_prefix : prefix:t -> t -> bool

  val encoding : t Data_encoding.t

  val to_string_list : t -> string list

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end = struct
  type t = {path : string list}

  include Compare.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
  end)

  let empty = {path = []}

  let name s = String.concat "." s.path

  let make sl =
    List.iter
      (fun s ->
        check_name_exn s (fun name char ->
            Printf.ksprintf
              (fun s -> Invalid_argument s)
              "Internal_event.Section: invalid name %S (contains %c)"
              name
              char))
      sl ;
    {path = sl}

  let make_sanitized sl =
    List.map (String.map (fun c -> if valid_char c then c else '_')) sl |> make

  let to_string_list s = s.path

  let is_prefix ~prefix main =
    try
      let _ =
        List.fold_left
          (fun prev elt ->
            match prev with
            | t :: q when String.equal t elt -> q
            | _ -> raise Not_found)
          main.path
          prefix.path
      in
      true
    with Not_found -> false

  let encoding =
    let open Data_encoding in
    conv (fun {path; _} -> path) (fun l -> make l) (list string)

  let pp fmt section =
    Format.fprintf
      fmt
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
         Format.pp_print_string)
      section.path
end

let registered_sections = ref String.Set.empty

let get_registered_sections () = String.Set.to_seq !registered_sections

let register_section section =
  registered_sections :=
    String.Set.add (Section.name section) !registered_sections

module type EVENT_DEFINITION = sig
  type t

  val section : Section.t option

  val name : string

  val doc : string

  val pp : all_fields:bool -> block:bool -> Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val level : level
end

module type EVENT = sig
  include EVENT_DEFINITION

  val emit : ?section:Section.t -> t -> unit tzresult Lwt.t
end

type 'a event_definition = (module EVENT_DEFINITION with type t = 'a)

module type SINK = sig
  type t

  val uri_scheme : string

  val configure : Uri.t -> t tzresult Lwt.t

  val should_handle : ?section:Section.t -> t -> _ event_definition -> bool

  val handle :
    t -> 'a event_definition -> ?section:Section.t -> 'a -> unit tzresult Lwt.t

  val close : t -> unit tzresult Lwt.t
end

type 'a sink_definition = (module SINK with type t = 'a)

module All_sinks = struct
  type registered =
    | Registered : {
        scheme : string;
        definition : 'a sink_definition;
      }
        -> registered

  type active =
    | Active : {
        scheme : string;
        configuration : Uri.t;
        sink : 'a;
        definition : 'a sink_definition;
      }
        -> active

  let registered : registered list ref = ref []

  let active : active list ref = ref []

  let find_registered scheme_to_find =
    List.find
      (function Registered {scheme; _} -> String.equal scheme scheme_to_find)
      !registered

  let register (type a) m =
    let module S = (val m : SINK with type t = a) in
    match find_registered S.uri_scheme with
    | None ->
        registered :=
          Registered {scheme = S.uri_scheme; definition = m} :: !registered
    | Some _ ->
        (* This should be considered a programming error: *)
        Printf.ksprintf
          Stdlib.invalid_arg
          "Internal_event: registering duplicate URI scheme: %S"
          S.uri_scheme

  type activation_error_reason =
    | Missing_uri_scheme of string
    | Uri_scheme_not_registered of string

  type error += Activation_error of activation_error_reason

  let () =
    let description =
      "Activation of an Internal Event SINK with an URI failed"
    in
    let title = "Internal Event Sink: Wrong Activation URI" in
    register_error_kind
      `Permanent
      ~id:"internal-event-activation-error"
      ~title
      ~description
      ~pp:
        (fun ppf -> function
          | Missing_uri_scheme uri ->
              Format.fprintf ppf "%s: Missing URI scheme %S" title uri
          | Uri_scheme_not_registered uri ->
              Format.fprintf ppf "%s: URI scheme not registered %S" title uri)
      Data_encoding.(
        union
          [
            case
              ~title:"missing-uri-scheme"
              (Tag 0)
              (obj1 (req "missing-uri-scheme" (obj1 (req "uri" string))))
              (function Missing_uri_scheme uri -> Some uri | _ -> None)
              (fun uri -> Missing_uri_scheme uri);
            case
              ~title:"non-registered-uri-scheme"
              (Tag 2)
              (obj1 (req "non-registered-uri-scheme" (obj1 (req "uri" string))))
              (function Uri_scheme_not_registered uri -> Some uri | _ -> None)
              (fun uri -> Uri_scheme_not_registered uri);
          ])
      (function Activation_error reason -> Some reason | _ -> None)
      (fun reason -> Activation_error reason)

  let activate uri =
    let open Lwt_result_syntax in
    match Uri.scheme uri with
    | None -> tzfail (Activation_error (Missing_uri_scheme (Uri.to_string uri)))
    | Some scheme_to_activate ->
        let* act =
          match find_registered scheme_to_activate with
          | None ->
              tzfail
                (Activation_error
                   (Uri_scheme_not_registered (Uri.to_string uri)))
          | Some (Registered {scheme; definition}) ->
              (* We need the intermediate function to introduce the type *)
              let activate (type a) scheme definition =
                let module S = (val definition : SINK with type t = a) in
                let* sink = S.configure uri in
                return (Active {scheme; configuration = uri; definition; sink})
              in
              activate scheme definition
        in
        active := act :: !active ;
        return_unit

  let close ?(except = fun _ -> false) () =
    let open Lwt_syntax in
    let close_one (type a) sink definition =
      let module S = (val definition : SINK with type t = a) in
      S.close sink
    in
    (* We want to filter the list in one Lwt-go (atomically), and only then
       call close on the ones that are being deleted. *)
    let next_active, to_close_list =
      List.partition
        (fun act ->
          match act with Active {configuration; _} -> except configuration)
        !active
    in
    active := next_active ;
    (* We don't want one failure to prevent the attempt at closing as many
       sinks as possible, so we record all errors and combine them: *)
    let+ close_results =
      List.map_s
        (fun (Active {sink; definition; _}) -> close_one sink definition)
        to_close_list
    in
    Result_syntax.tzjoin close_results

  let handle def section v =
    let handle (type a) sink definition =
      let open Lwt_result_syntax in
      let module S = (val definition : SINK with type t = a) in
      if S.should_handle ?section sink def then S.handle ?section sink def v
      else return_unit
    in
    List.iter_es
      (function Active {sink; definition; _} -> handle sink definition)
      !active

  let pp_state fmt () =
    let open Format in
    let pp_list_of_sinks name list pp =
      pp_open_box fmt 2 ;
      pp_print_if_newline fmt () ;
      pp_print_string fmt "* " ;
      fprintf fmt "%s: [" name ;
      pp_print_cut fmt () ;
      pp_print_list
        ~pp_sep:(fun fmt () ->
          pp_print_string fmt "," ;
          pp_print_space fmt ())
        pp
        fmt
        list ;
      pp_close_box fmt () ;
      pp_print_cut fmt () ;
      pp_print_string fmt "]"
    in
    pp_open_box fmt 0 ;
    pp_list_of_sinks
      "Registered sinks"
      !registered
      (fun fmt (Registered {scheme; _}) -> fprintf fmt "\"%s://..\"" scheme) ;
    pp_print_break fmt 2 0 ;
    pp_list_of_sinks
      "Active sinks"
      !active
      (fun fmt (Active {configuration; _}) ->
        fprintf fmt "\"%a\"" Uri.pp_hum configuration) ;
    pp_print_cut fmt () ;
    pp_close_box fmt () ;
    ()
end

module Generic = struct
  type definition =
    | Definition :
        (Section.t option * string * 'a event_definition)
        -> definition

  type event = Event : (string * 'a event_definition * 'a) -> event

  type with_name = < doc : string ; name : string >

  let json_schema (Definition (_, _, d)) :
      < schema : Json_schema.schema ; with_name > =
    let aux (type a) (ev : a event_definition) =
      let module E = (val ev : EVENT_DEFINITION with type t = a) in
      object
        method name = E.name

        method doc = E.doc

        method schema = Data_encoding.Json.schema E.encoding
      end
    in
    aux d

  let explode_event (Event (_, def, ev)) =
    let aux (type a) def ev =
      let module M = (val def : EVENT_DEFINITION with type t = a) in
      object
        method name = M.name

        method doc = M.doc

        method pp fmt () = M.pp ~all_fields:true ~block:true fmt ev

        method json = Data_encoding.Json.construct M.encoding ev
      end
    in
    aux def ev
end

module All_definitions = struct
  open Generic

  let all : definition list ref = ref []

  let registration_exn fmt =
    Format.kasprintf
      (fun s ->
        (* This should be considered a programming error: *)
        Invalid_argument ("Internal_event registration error: " ^ s))
      fmt

  let add (type a) ev =
    let module E = (val ev : EVENT_DEFINITION with type t = a) in
    match
      List.find
        (function Definition (s, n, _) -> E.section = s && E.name = n)
        !all
    with
    | Some _ ->
        raise
          (registration_exn
             "duplicate Event name: %a %S"
             (Format.pp_print_option Section.pp)
             E.section
             E.name)
    | None ->
        check_name_exn
          E.name
          (registration_exn "invalid event name: %S contains '%c'") ;
        all := Definition (E.section, E.name, ev) :: !all

  let get () = !all

  let find match_name =
    List.find (function Definition (_, n, _) -> match_name n) !all
end

module Make (E : EVENT_DEFINITION) : EVENT with type t = E.t = struct
  include E

  let emit ?section x = All_sinks.handle (module E) section x

  let () = All_definitions.add (module E)
end

module Simple = struct
  (* This type is mostly there to make usage less error-prone, by
     explicitly splitting the place where the partial application
     takes place. Indeed, it is important that events are declared
     only once. *)
  type 'a t = 'a -> unit tzresult Lwt.t

  let emit simple_event parameters =
    Lwt.try_bind
      (fun () -> simple_event parameters)
      (function
        | Ok () -> Lwt.return_unit
        | Error trace ->
            (* Having to handle errors when sending events would make the
               code very heavy. We are much more likely to just use [let*]
               to propagate the error, assuming that sending events cannot
               fail. But consider this example:
               - we log that we are going to do some cleanup, like remove
                 temporary directories...
               - and then because we failed to log, we don't actually
                 clean the temporary directories.
               Instead we just print the error on stderr. *)
            Format.eprintf
              "@[<hv 2>Failed to send event:@ %a@]@."
              Error_monad.pp_print_trace
              trace ;
            Lwt.return_unit)
      (fun exc ->
        (* For the same reason we also just print exceptions *)
        Format.eprintf
          "@[<hv 2>Failed to send event:@ %s@]@."
          (Printexc.to_string exc) ;
        Lwt.return_unit)

  let emit__dont_wait__use_with_care simple_event parameters =
    Lwt.dont_wait
      (fun () -> emit simple_event parameters)
      (fun exc -> raise exc)
  (* emit never lets exceptions escape *)

  let make_section names =
    match names with
    | None -> None
    | Some names ->
        let section = Section.make_sanitized names in
        register_section section ;
        Some section

  let pp_print_compact_float fmt value = Format.fprintf fmt "%g" value

  let max_shortened_string_length = 64

  let ellipsis = "[...]"

  let pp_print_shortened_string fmt value =
    let len = String.length value in
    if len = 0 then Format.pp_print_string fmt "\"\""
    else
      let escape len =
        let rec loop i =
          if i >= len then false
          else
            match value.[i] with
            | '\000' .. '\032' | '\127' .. '\255' ->
                (* invisible character (including space) or non-ASCII: needs to be escaped *)
                true
            | '\033' .. '\126' ->
                (* visible, non-space character *)
                loop (i + 1)
        in
        loop 0
      in
      if String.length value > max_shortened_string_length then
        let length_without_ellipsis =
          max_shortened_string_length - String.length ellipsis
        in
        let prefix = String.sub value 0 length_without_ellipsis in
        if escape length_without_ellipsis then
          Format.fprintf fmt "\"%s%s\"" prefix ellipsis
        else Format.fprintf fmt "%s%s" prefix ellipsis
      else if escape len then Format.fprintf fmt "%S" value
      else Format.pp_print_string fmt value

  (* Default pretty-printer for parameters.
     Simple types are printed in a compact way.
     Structured types are not printed.

     If [never_empty] is [false], do not print anything for:
     - structured values, like objects;
     - empty values, like null.
     This is useful to ignore non-inline parameters in log messages.

     If [never_empty] is [true], always print something.
     This is useful for inline parameters. *)
  let rec pp_human_readable :
            'a. never_empty:bool -> 'a Data_encoding.t -> _ -> 'a -> _ =
    fun (type a) ~never_empty (encoding : a Data_encoding.t) fmt (value : a) ->
     match encoding.encoding with
     | Null -> if never_empty then Format.pp_print_string fmt "N/A"
     | Empty -> if never_empty then Format.pp_print_string fmt "N/A"
     | Ignore -> if never_empty then Format.pp_print_string fmt "N/A"
     | Constant name -> pp_print_shortened_string fmt name
     | Bool -> Format.pp_print_bool fmt value
     | Int8 -> Format.pp_print_int fmt value
     | Uint8 -> Format.pp_print_int fmt value
     | Int16 -> Format.pp_print_int fmt value
     | Uint16 -> Format.pp_print_int fmt value
     | Int31 -> Format.pp_print_int fmt value
     | Int32 -> Format.fprintf fmt "%ld" value
     | Int64 -> Format.fprintf fmt "%Ld" value
     | N -> Format.pp_print_string fmt (Z.to_string value)
     | Z -> Format.pp_print_string fmt (Z.to_string value)
     | RangedInt _ -> Format.pp_print_int fmt value
     | RangedFloat _ -> pp_print_compact_float fmt value
     | Float -> pp_print_compact_float fmt value
     | Bytes _ -> pp_print_shortened_string fmt (Bytes.to_string value)
     | String _ -> pp_print_shortened_string fmt value
     | Padded (encoding, _) -> pp_human_readable ~never_empty encoding fmt value
     | String_enum (table, _) -> (
         match Stdlib.Hashtbl.find_opt table value with
         | None -> if never_empty then Format.pp_print_string fmt "N/A"
         | Some (name, _) -> pp_print_shortened_string fmt name)
     | Array _ -> if never_empty then Format.pp_print_string fmt "<array>"
     | List _ -> if never_empty then Format.pp_print_string fmt "<list>"
     | Obj (Req {encoding; _} | Dft {encoding; _}) ->
         pp_human_readable ~never_empty encoding fmt value
     | Obj (Opt {encoding; _}) ->
         Option.iter (pp_human_readable ~never_empty encoding fmt) value
     | Objs _ -> if never_empty then Format.pp_print_string fmt "<obj>"
     | Tup encoding -> pp_human_readable ~never_empty encoding fmt value
     | Tups _ -> if never_empty then Format.pp_print_string fmt "<tuple>"
     | Union
         {
           cases =
             [
               Case {encoding; proj; _};
               Case {encoding = {encoding = Null; _}; _};
             ];
           _;
         } -> (
         (* Probably an [option] type or similar.
            We only print the value if it is not null,
            unless [never_empty] is [true]. *)
         match proj value with
         | None -> if never_empty then Format.pp_print_string fmt "null"
         | Some value -> pp_human_readable ~never_empty encoding fmt value)
     | Union _ -> if never_empty then Format.pp_print_string fmt "<union>"
     | Mu _ -> if never_empty then Format.pp_print_string fmt "<recursive>"
     | Conv {proj; encoding; _} ->
         (* TODO: it may be worth it to take a look at [encoding]
            before calling [proj], to try and predict whether the value
            will actually be printed. *)
         pp_human_readable ~never_empty encoding fmt (proj value)
     | Describe {encoding; _} ->
         pp_human_readable ~never_empty encoding fmt value
     | Splitted {json_encoding; _} -> (
         (* Generally, [Splitted] nodes imply that the JSON encoding
            is more human-friendly, as JSON is a human-friendly
            format. A typical example is Blake2B hashes.
            So for log outputs we use the JSON encoding.
            Unfortunately, [Json_encoding.t] is abstract so we have
            to [construct] the JSON value and continue from here. *)
         (* TODO: it may be worth it to take a look at [encoding]
            before constructing the JSON value, to try and predict
            whether the value will actually be printed (same as [Conv]). *)
         match Json_encoding.construct json_encoding value with
         | `Null -> if never_empty then Format.pp_print_string fmt "N/A"
         | `Bool value -> Format.pp_print_bool fmt value
         | `Float value -> pp_print_compact_float fmt value
         | `String value -> pp_print_shortened_string fmt value
         | `A _ -> if never_empty then Format.pp_print_string fmt "<list>"
         | `O _ -> if never_empty then Format.pp_print_string fmt "<obj>")
     | Dynamic_size {encoding; _} ->
         pp_human_readable ~never_empty encoding fmt value
     | Check_size {encoding; _} ->
         pp_human_readable ~never_empty encoding fmt value
     | Delayed make_encoding ->
         pp_human_readable ~never_empty (make_encoding ()) fmt value

  type parameter =
    | Parameter :
        string
        * 'a Data_encoding.t
        * 'a
        * (Format.formatter -> 'a -> unit) option
        -> parameter

  type msg_atom = Text of string | Variable of int | Space

  let invalid_msg reason msg =
    invalid_arg
      (Printf.sprintf
         "Internal_event.Simple: invalid message string: %S: %s"
         msg
         reason)

  let parse_msg variable_names msg =
    let len = String.length msg in
    let rec find_variable_begin acc atom_start i =
      let add_text () =
        if i <= atom_start then acc
        else Text (String.sub msg atom_start (i - atom_start)) :: acc
      in
      if i >= len then add_text ()
      else if msg.[i] = '{' then
        let acc = add_text () in
        let i = i + 1 in
        find_variable_end acc i i
      else if msg.[i] = ' ' then
        let acc = Space :: add_text () in
        let i = i + 1 in
        find_variable_begin acc i i
      else find_variable_begin acc atom_start (i + 1)
    and find_variable_end acc atom_start i =
      if i >= len then invalid_msg "unmatched '{'" msg
      else if msg.[i] = '}' then
        let variable_name = String.sub msg atom_start (i - atom_start) in
        let rec loop index = function
          | [] ->
              invalid_msg
                (Printf.sprintf "unbound variable: %S" variable_name)
                msg
          | varname :: _ when String.equal varname variable_name ->
              let acc = Variable index :: acc in
              let i = i + 1 in
              find_variable_begin acc i i
          | _ :: variable_names -> loop (index + 1) variable_names
        in
        loop 0 variable_names
      else find_variable_end acc atom_start (i + 1)
    in
    find_variable_begin [] 0 0 |> List.rev

  let pp_log_message ~all_fields ~block (msg : msg_atom list) fmt fields =
    (* Add a boolean reference to each field telling whether the field was used. *)
    let fields = List.map (fun field -> (field, ref false)) fields in
    if block then Format.fprintf fmt "@[<hov 2>" ;
    (* First, print [msg], including interpolated variables. *)
    let pp_msg_atom = function
      | Text text -> Format.pp_print_string fmt text
      | Variable index -> (
          match List.nth_opt fields index with
          | None ->
              (* Not supposed to happen, by construction.
                 But it's just logging, no need to fail here. *)
              Format.pp_print_string fmt "???"
          | Some (Parameter (_name, enc, value, pp), used) -> (
              used := true ;
              match pp with
              | None -> pp_human_readable ~never_empty:true enc fmt value
              | Some pp -> pp fmt value))
      | Space ->
          if block then Format.pp_print_space fmt () else Format.fprintf fmt " "
    in
    List.iter pp_msg_atom msg ;
    (* Then, print variables that were not used by [msg]. *)
    let first_field = ref true in
    let print_field (Parameter (name, enc, value, pp), used) =
      if not !used then
        let value =
          let pp =
            match pp with
            | None -> pp_human_readable ~never_empty:false enc
            | Some pp -> pp
          in
          Format.asprintf "%a" pp value
        in
        if String.length value > 0 then
          if !first_field then (
            first_field := false ;
            Format.fprintf fmt "@ (%s = %s" name value)
          else Format.fprintf fmt ",@ %s = %s" name value
    in
    if all_fields then List.iter print_field fields ;
    if not !first_field then Format.fprintf fmt ")" ;
    if block then Format.fprintf fmt "@]"

  let with_version ~name encoding =
    Data_encoding.With_version.encoding
      ~name
      (Data_encoding.With_version.first_version encoding)

  let declare_0 ?section ~name ~msg ?(level = Info) () =
    let section = make_section section in
    let parsed_msg = parse_msg [] msg in
    let module Definition : EVENT_DEFINITION with type t = unit = struct
      type t = unit

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt () =
        pp_log_message ~all_fields ~block parsed_msg fmt []

      let encoding = with_version ~name Data_encoding.unit

      let level = level
    end in
    let module Event = Make (Definition) in
    fun () -> Event.emit ?section ()

  let declare_1 (type a) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg = parse_msg [f1_name] msg in
    let module Definition : EVENT_DEFINITION with type t = a = struct
      type t = a

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt f1 =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [Parameter (f1_name, f1_enc, f1, pp1)]

      let encoding = with_version ~name f1_enc

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameter -> Event.emit ?section parameter

  let declare_2 (type a b) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg = parse_msg [f1_name; f2_name] msg in
    let module Definition : EVENT_DEFINITION with type t = a * b = struct
      type t = a * b

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj2
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters

  let declare_3 (type a b c) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) ?pp3
      (f3_name, (f3_enc : c Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg = parse_msg [f1_name; f2_name; f3_name] msg in
    let module Definition : EVENT_DEFINITION with type t = a * b * c = struct
      type t = a * b * c

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2, f3) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
            Parameter (f3_name, f3_enc, f3, pp3);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj3
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)
             (Data_encoding.req f3_name f3_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters

  let declare_4 (type a b c d) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) ?pp3
      (f3_name, (f3_enc : c Data_encoding.t)) ?pp4
      (f4_name, (f4_enc : d Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg = parse_msg [f1_name; f2_name; f3_name; f4_name] msg in
    let module Definition : EVENT_DEFINITION with type t = a * b * c * d =
    struct
      type t = a * b * c * d

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2, f3, f4) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
            Parameter (f3_name, f3_enc, f3, pp3);
            Parameter (f4_name, f4_enc, f4, pp4);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj4
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)
             (Data_encoding.req f3_name f3_enc)
             (Data_encoding.req f4_name f4_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters

  let declare_5 (type a b c d e) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) ?pp3
      (f3_name, (f3_enc : c Data_encoding.t)) ?pp4
      (f4_name, (f4_enc : d Data_encoding.t)) ?pp5
      (f5_name, (f5_enc : e Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg =
      parse_msg [f1_name; f2_name; f3_name; f4_name; f5_name] msg
    in
    let module Definition : EVENT_DEFINITION with type t = a * b * c * d * e =
    struct
      type t = a * b * c * d * e

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2, f3, f4, f5) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
            Parameter (f3_name, f3_enc, f3, pp3);
            Parameter (f4_name, f4_enc, f4, pp4);
            Parameter (f5_name, f5_enc, f5, pp5);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj5
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)
             (Data_encoding.req f3_name f3_enc)
             (Data_encoding.req f4_name f4_enc)
             (Data_encoding.req f5_name f5_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters

  let declare_6 (type a b c d e f) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) ?pp3
      (f3_name, (f3_enc : c Data_encoding.t)) ?pp4
      (f4_name, (f4_enc : d Data_encoding.t)) ?pp5
      (f5_name, (f5_enc : e Data_encoding.t)) ?pp6
      (f6_name, (f6_enc : f Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg =
      parse_msg [f1_name; f2_name; f3_name; f4_name; f5_name; f6_name] msg
    in
    let module Definition :
      EVENT_DEFINITION with type t = a * b * c * d * e * f = struct
      type t = a * b * c * d * e * f

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2, f3, f4, f5, f6) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
            Parameter (f3_name, f3_enc, f3, pp3);
            Parameter (f4_name, f4_enc, f4, pp4);
            Parameter (f5_name, f5_enc, f5, pp5);
            Parameter (f6_name, f6_enc, f6, pp6);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj6
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)
             (Data_encoding.req f3_name f3_enc)
             (Data_encoding.req f4_name f4_enc)
             (Data_encoding.req f5_name f5_enc)
             (Data_encoding.req f6_name f6_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters

  let declare_7 (type a b c d e f g) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) ?pp3
      (f3_name, (f3_enc : c Data_encoding.t)) ?pp4
      (f4_name, (f4_enc : d Data_encoding.t)) ?pp5
      (f5_name, (f5_enc : e Data_encoding.t)) ?pp6
      (f6_name, (f6_enc : f Data_encoding.t)) ?pp7
      (f7_name, (f7_enc : g Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg =
      parse_msg
        [f1_name; f2_name; f3_name; f4_name; f5_name; f6_name; f7_name]
        msg
    in
    let module Definition :
      EVENT_DEFINITION with type t = a * b * c * d * e * f * g = struct
      type t = a * b * c * d * e * f * g

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2, f3, f4, f5, f6, f7) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
            Parameter (f3_name, f3_enc, f3, pp3);
            Parameter (f4_name, f4_enc, f4, pp4);
            Parameter (f5_name, f5_enc, f5, pp5);
            Parameter (f6_name, f6_enc, f6, pp6);
            Parameter (f7_name, f7_enc, f7, pp7);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj7
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)
             (Data_encoding.req f3_name f3_enc)
             (Data_encoding.req f4_name f4_enc)
             (Data_encoding.req f5_name f5_enc)
             (Data_encoding.req f6_name f6_enc)
             (Data_encoding.req f7_name f7_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters

  let declare_8 (type a b c d e f g h) ?section ~name ~msg ?(level = Info) ?pp1
      (f1_name, (f1_enc : a Data_encoding.t)) ?pp2
      (f2_name, (f2_enc : b Data_encoding.t)) ?pp3
      (f3_name, (f3_enc : c Data_encoding.t)) ?pp4
      (f4_name, (f4_enc : d Data_encoding.t)) ?pp5
      (f5_name, (f5_enc : e Data_encoding.t)) ?pp6
      (f6_name, (f6_enc : f Data_encoding.t)) ?pp7
      (f7_name, (f7_enc : g Data_encoding.t)) ?pp8
      (f8_name, (f8_enc : h Data_encoding.t)) =
    let section = make_section section in
    let parsed_msg =
      parse_msg
        [f1_name; f2_name; f3_name; f4_name; f5_name; f6_name; f7_name; f8_name]
        msg
    in
    let module Definition :
      EVENT_DEFINITION with type t = a * b * c * d * e * f * g * h = struct
      type t = a * b * c * d * e * f * g * h

      let doc = msg

      let section = section

      let name = name

      let pp ~all_fields ~block fmt (f1, f2, f3, f4, f5, f6, f7, f8) =
        pp_log_message
          ~all_fields
          ~block
          parsed_msg
          fmt
          [
            Parameter (f1_name, f1_enc, f1, pp1);
            Parameter (f2_name, f2_enc, f2, pp2);
            Parameter (f3_name, f3_enc, f3, pp3);
            Parameter (f4_name, f4_enc, f4, pp4);
            Parameter (f5_name, f5_enc, f5, pp5);
            Parameter (f6_name, f6_enc, f6, pp6);
            Parameter (f7_name, f7_enc, f7, pp7);
            Parameter (f8_name, f8_enc, f8, pp8);
          ]

      let encoding =
        with_version ~name
        @@ Data_encoding.obj8
             (Data_encoding.req f1_name f1_enc)
             (Data_encoding.req f2_name f2_enc)
             (Data_encoding.req f3_name f3_enc)
             (Data_encoding.req f4_name f4_enc)
             (Data_encoding.req f5_name f5_enc)
             (Data_encoding.req f6_name f6_enc)
             (Data_encoding.req f7_name f7_enc)
             (Data_encoding.req f8_name f8_enc)

      let level = level
    end in
    let module Event = Make (Definition) in
    fun parameters -> Event.emit ?section parameters
end

module Legacy_logging = struct
  module Make (P : sig
    val name : string
  end) =
  struct
    let name_split = String.split_on_char '.' P.name

    let section = Section.make name_split

    let name = "legacy_logging_event-" ^ String.concat "-" name_split

    module Make_definition (P : sig
      val level : level
    end) =
    struct
      type t = string

      let name = Printf.sprintf "%s-%s" name (Level.to_string P.level)

      let v0_encoding =
        let open Data_encoding in
        obj1 (req "message" string)

      let encoding =
        Data_encoding.With_version.(encoding ~name (first_version v0_encoding))

      let pp ~all_fields:_ ~block:_ ppf message =
        Format.fprintf ppf "%s" message

      let doc = "Generic event legacy / string-based information logging."

      let level = P.level

      let section = Some section
    end

    let () = registered_sections := String.Set.add P.name !registered_sections

    module Debug_event = Make (Make_definition (struct
      let level = Debug
    end))

    module Info_event = Make (Make_definition (struct
      let level = Info
    end))

    module Notice_event = Make (Make_definition (struct
      let level = Notice
    end))

    module Warning_event = Make (Make_definition (struct
      let level = Warning
    end))

    module Error_event = Make (Make_definition (struct
      let level = Error
    end))

    module Fatal_event = Make (Make_definition (struct
      let level = Fatal
    end))

    let emit_async (emit : ?section:Section.t -> string -> unit tzresult Lwt.t)
        fmt =
      Format.kasprintf
        (fun message -> Lwt.ignore_result (emit ~section message))
        fmt

    let emit_lwt (emit : ?section:Section.t -> string -> unit tzresult Lwt.t)
        fmt =
      let open Lwt_syntax in
      Format.kasprintf
        (fun message ->
          let* r = emit ~section message in
          match r with
          | Ok () -> Lwt.return_unit
          | Error el ->
              Format.kasprintf
                !default_error_fallback_logger
                "%a@\n"
                pp_print_trace
                el)
        fmt

    let debug f = emit_async Debug_event.emit f

    let log_info f = emit_async Info_event.emit f

    let log_notice f = emit_async Notice_event.emit f

    let warn f = emit_async Warning_event.emit f

    let log_error f = emit_async Error_event.emit f

    let fatal_error f = emit_async Fatal_event.emit f

    let lwt_debug f = emit_lwt Debug_event.emit f

    let lwt_log_info f = emit_lwt Info_event.emit f

    let lwt_log_notice f = emit_lwt Notice_event.emit f

    let lwt_warn f = emit_lwt Warning_event.emit f

    let lwt_log_error f = emit_lwt Error_event.emit f

    let lwt_fatal_error f = emit_lwt Fatal_event.emit f
  end
end

module Debug_event = struct
  type t = {message : string; attachment : Data_encoding.Json.t}

  let make ?(attach = `Null) message = {message; attachment = attach}

  let v0_encoding =
    let open Data_encoding in
    conv
      (fun {message; attachment} -> (message, attachment))
      (fun (message, attachment) -> {message; attachment})
      (obj2 (req "message" string) (req "attachment" json))

  module Definition = struct
    let section = None

    let name = "debug-event"

    type nonrec t = t

    let encoding =
      Data_encoding.With_version.(encoding ~name (first_version v0_encoding))

    let pp ~all_fields:_ ~block:_ ppf {message; attachment} =
      let open Format in
      fprintf ppf "%s:@ %s@ %a" name message Data_encoding.Json.pp attachment

    let doc = "Generic event for semi-structured debug information."

    let level = Debug
  end

  include (Make (Definition) : EVENT with type t := t)
end

module Lwt_worker_logger = struct
  module Started_event = Make (struct
    type t = unit

    let section = None

    let name = "lwt-worker_started"

    let encoding = Data_encoding.constant "started"

    let pp ~all_fields:_ ~block:_ ppf () = Format.fprintf ppf "started"

    let doc = "Worker started event"

    let level = Debug
  end)

  module Ended_event = Make (struct
    type t = unit

    let section = None

    let name = "lwt-worker_ended"

    let encoding = Data_encoding.constant "ended"

    let pp ~all_fields:_ ~block:_ ppf () = Format.fprintf ppf "ended"

    let doc = "Worker ended event"

    let level = Debug
  end)

  module Failed_event = Make (struct
    type t = string

    let section = None

    let name = "lwt-worker_failed"

    let encoding = Data_encoding.(obj1 (req "error" string))

    let pp ~all_fields:_ ~block:_ ppf error =
      Format.fprintf ppf "failed with %s" error

    let doc = "Worker failed event"

    let level = Error
  end)

  let on_event name event =
    let open Lwt_syntax in
    let section = Section.make_sanitized ["lwt-worker"; name] in
    let* r =
      match event with
      | `Started -> Started_event.emit ~section ()
      | `Ended -> Ended_event.emit ~section ()
      | `Failed msg -> Failed_event.emit ~section msg
    in
    match r with
    | Ok () -> Lwt.return_unit
    | Error errs ->
        Format.kasprintf
          !default_error_fallback_logger
          "failed to log worker event:@ %a@\n"
          Error_monad.pp_print_trace
          errs
end
