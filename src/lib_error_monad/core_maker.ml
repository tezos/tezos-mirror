(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

let json_pp id description encoding ppf data =
  Format.pp_print_string ppf @@ Data_encoding.Json.to_string
  @@
  let pp_encoding =
    Data_encoding.(
      obj3 (req "id" string) (req "description" string) (req "data" encoding))
  in
  Data_encoding.Json.construct pp_encoding (id, description, data)

let set_error_encoding_cache_dirty = ref (fun () -> ())

module Make (Prefix : Sig.PREFIX) : sig
  include Sig.CORE

  include Sig.EXT with type error := error

  include Sig.WITH_WRAPPED with type error := error
end = struct
  type error = ..

  module type Wrapped_error_monad = sig
    type unwrapped = ..

    include Sig.CORE with type error := unwrapped

    include Sig.EXT with type error := unwrapped

    val unwrap : error -> unwrapped option

    val wrap : unwrapped -> error
  end

  type full_error_category =
    | Main of Sig.error_category
    | Wrapped of (module Wrapped_error_monad)

  (* the toplevel store for error kinds *)
  type error_kind =
    | Error_kind : {
        id : string;
        title : string;
        description : string;
        from_error : error -> 'err option;
        category : full_error_category;
        encoding_case : error Data_encoding.case;
        pp : Format.formatter -> 'err -> unit;
      }
        -> error_kind

  type error_info = {
    category : Sig.error_category;
    id : string;
    title : string;
    description : string;
    schema : Data_encoding.json_schema;
  }

  let error_kinds : error_kind list ref = ref []

  let get_registered_errors () : error_info list =
    List.flatten
      (List.map
         (function
           | Error_kind {id = ""; _} ->
               []
           | Error_kind
               { id;
                 title;
                 description;
                 category = Main category;
                 encoding_case;
                 _ } ->
               [ {
                   id;
                   title;
                   description;
                   category;
                   schema =
                     Data_encoding.Json.schema
                       (Data_encoding.union [encoding_case]);
                 } ]
           | Error_kind {category = Wrapped (module WEM); _} ->
               List.map
                 (fun {WEM.id; title; description; category; schema} ->
                   {id; title; description; category; schema})
                 (WEM.get_registered_errors ()))
         !error_kinds)

  let error_encoding_cache = ref None

  let () =
    let cont = !set_error_encoding_cache_dirty in
    set_error_encoding_cache_dirty :=
      fun () ->
        cont () ;
        error_encoding_cache := None

  let string_of_category = function
    | `Permanent ->
        "permanent"
    | `Temporary ->
        "temporary"
    | `Branch ->
        "branch"

  let pp_info ppf {category; id; title; description; schema} =
    Format.fprintf
      ppf
      "@[<v 2>category : %s\n\
       id : %s\n\
       title : %s\n\
       description : %s\n\
       schema : %a@]"
      (string_of_category category)
      id
      title
      description
      (Json_repr.pp (module Json_repr.Ezjsonm))
      (Json_schema.to_json schema)

  (* Catch all error when 'serializing' an error. *)
  type error += Unclassified of string

  let () =
    let id = "" in
    let category = Main `Temporary in
    let to_error msg = Unclassified msg in
    let from_error = function
      | Unclassified msg ->
          Some msg
      | error ->
          let msg = Obj.Extension_constructor.(name @@ of_val error) in
          Some ("Unclassified error: " ^ msg ^ ". Was the error registered?")
    in
    let title = "Generic error" in
    let description = "An unclassified error" in
    let encoding_case =
      let open Data_encoding in
      case
        Json_only
        ~title:"Generic error"
        ( def "generic_error" ~title ~description
        @@ conv (fun x -> ((), x)) (fun ((), x) -> x)
        @@ obj2 (req "kind" (constant "generic")) (req "error" string) )
        from_error
        to_error
    in
    let pp ppf s = Format.fprintf ppf "@[<h 0>%a@]" Format.pp_print_text s in
    error_kinds :=
      Error_kind
        {id; title; description; from_error; category; encoding_case; pp}
      :: !error_kinds

  (* Catch all error when 'deserializing' an error. *)
  type error += Unregistred_error of Data_encoding.json

  let () =
    let id = "" in
    let category = Main `Temporary in
    let to_error msg = Unregistred_error msg in
    let from_error = function
      | Unregistred_error json ->
          Some json
      | _ ->
          None
    in
    let encoding_case =
      let open Data_encoding in
      case Json_only ~title:"Unregistred error" json from_error to_error
    in
    let pp ppf json =
      Format.fprintf
        ppf
        "@[<v 2>Unregistred error:@ %a@]"
        Data_encoding.Json.pp
        json
    in
    error_kinds :=
      Error_kind
        {
          id;
          title = "";
          description = "";
          from_error;
          category;
          encoding_case;
          pp;
        }
      :: !error_kinds

  let raw_register_error_kind category ~id:name ~title ~description ?pp
      encoding from_error to_error =
    let name = Prefix.id ^ name in
    if List.exists (fun (Error_kind {id; _}) -> name = id) !error_kinds then
      invalid_arg
        (Printf.sprintf "register_error_kind: duplicate error name: %s" name) ;
    let encoding_case =
      let open Data_encoding in
      match category with
      | Wrapped (module WEM) ->
          let unwrap err =
            match WEM.unwrap err with
            | Some (WEM.Unclassified _) ->
                None
            | Some (WEM.Unregistred_error _) ->
                Format.eprintf "What %s@." name ;
                None
            | res ->
                res
          in
          let wrap err =
            match err with
            | WEM.Unclassified _ ->
                failwith "ignore wrapped error when serializing"
            | WEM.Unregistred_error _ ->
                failwith "ignore wrapped error when deserializing"
            | res ->
                WEM.wrap res
          in
          case Json_only ~title:name WEM.error_encoding unwrap wrap
      | Main category ->
          let with_id_and_kind_encoding =
            merge_objs
              (obj2
                 (req "kind" (constant (string_of_category category)))
                 (req "id" (constant name)))
              encoding
          in
          case
            Json_only
            ~title
            ~description
            (conv
               (fun x -> (((), ()), x))
               (fun (((), ()), x) -> x)
               with_id_and_kind_encoding)
            from_error
            to_error
    in
    !set_error_encoding_cache_dirty () ;
    error_kinds :=
      Error_kind
        {
          id = name;
          category;
          title;
          description;
          from_error;
          encoding_case;
          pp = Option.unopt ~default:(json_pp name description encoding) pp;
        }
      :: !error_kinds

  let register_wrapped_error_kind (module WEM : Wrapped_error_monad) ~id ~title
      ~description =
    raw_register_error_kind
      (Wrapped (module WEM))
      ~id
      ~title
      ~description
      ~pp:WEM.pp
      WEM.error_encoding
      WEM.unwrap
      WEM.wrap

  let register_error_kind category ~id ~title ~description ?pp encoding
      from_error to_error =
    if not (Data_encoding.is_obj encoding) then
      invalid_arg
        (Printf.sprintf
           "Specified encoding for \"%s%s\" is not an object, but error \
            encodings must be objects."
           Prefix.id
           id) ;
    raw_register_error_kind
      (Main category)
      ~id
      ~title
      ~description
      ?pp
      encoding
      from_error
      to_error

  let error_encoding () =
    match !error_encoding_cache with
    | None ->
        let cases =
          List.map
            (fun (Error_kind {encoding_case; _}) -> encoding_case)
            !error_kinds
        in
        let union_encoding = Data_encoding.union cases in
        let encoding =
          let open Data_encoding in
          dynamic_size
          @@ splitted
               ~json:union_encoding
               ~binary:
                 (conv
                    (Json.construct union_encoding)
                    (Json.destruct union_encoding)
                    json)
        in
        error_encoding_cache := Some encoding ;
        encoding
    | Some encoding ->
        encoding

  let error_encoding = Data_encoding.delayed error_encoding

  let json_of_error error = Data_encoding.Json.construct error_encoding error

  let error_of_json json = Data_encoding.Json.destruct error_encoding json

  let classify_error error =
    let rec find e = function
      | [] ->
          `Temporary
      (* assert false (\* See "Generic error" *\) *)
      | Error_kind {from_error; category; _} :: rest -> (
        match from_error e with
        | Some _ -> (
          match category with
          | Main error_category ->
              error_category
          | Wrapped (module WEM) -> (
            match WEM.unwrap e with
            | Some e ->
                WEM.classify_error e
            | None ->
                find e rest ) )
        | None ->
            find e rest )
    in
    find error !error_kinds

  let pp ppf error =
    let rec find = function
      | [] ->
          assert false (* See "Generic error" *)
      | Error_kind {from_error; pp; _} :: errors -> (
        match from_error error with None -> find errors | Some x -> pp ppf x )
    in
    find !error_kinds
end
