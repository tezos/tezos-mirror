(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Kaitai.Types
module MuSet = Set.Make (String)
module StringSet = Set.Make (String)

(* We need to access the definition of data-encoding's [descr] type. For this
   reason we alias the private/internal module [Data_encoding__Encoding] (rather
   than the public module [Data_encoding.Encoding]. *)
module DataEncoding = Data_encoding__Encoding

module AnyEncoding = struct
  module T = struct
    type t = AnyEncoding : _ DataEncoding.t -> t [@@unboxed]

    let hash = Hashtbl.hash

    (* We rely on [AnyEncoding] to be unboxed so that we can preserve
       physical equality. We can't use structural equality because
       encoding contain closures. *)
    let equal a b = a == b

    let pack x = AnyEncoding x
  end

  include T
  module Tbl = Hashtbl.Make (T)
end

module Types : sig
  type t

  val create : unit -> t

  val to_list : t -> (string * ClassSpec.t) list

  val add : t -> string * ClassSpec.t -> unit

  val add_uniq : t -> string -> AttrSpec.t list -> string
end = struct
  (* This module is used to track kaitai types emitted during the translation.

     Because type name must be unique, we have to keep enough information to allocate meaningful fresh names.
     - A table of all types [all_types] indexed by their name.
     - A table of all [names] given to a given [ClassSpec].

     A [ClassSpec] can have multiple names, because the name is derived from the context and the same encoding
     can be used in different context.
  *)

  type t = {
    all_types : (string, ClassSpec.t) Hashtbl.t;
    names : (ClassSpec.t, string) Hashtbl.t;
        (* We rely on the [Hashtbl.add] semantics of not replacing
           previous bindings. See [Hashtbl.find_all] below. *)
  }

  let create () = {all_types = Hashtbl.create 20; names = Hashtbl.create 20}

  let to_list t =
    Hashtbl.to_seq t.all_types
    |> Seq.map (fun (name, (typ : ClassSpec.t)) -> (name, typ))
    |> List.of_seq

  let find_name t ~prefix =
    if not (Hashtbl.mem t.all_types prefix) then prefix
    else
      let rec loop i =
        let candidate = Printf.sprintf "%s_%d" prefix i in
        if Hashtbl.mem t.all_types candidate then loop (succ i) else candidate
      in
      loop 0

  let name_prefix s =
    match String.rindex_opt s '_' with
    | None -> s
    | Some i ->
        if
          String.for_all
            (function '0' .. '9' -> true | _ -> false)
            (String.sub s (i + 1) (String.length s - i - 1))
        then String.sub s 0 i
        else s

  let same_id a b = String.equal (name_prefix a) (name_prefix b)

  let add t (name, typ) =
    match Hashtbl.find_opt t.all_types name with
    | None ->
        Hashtbl.add t.all_types name typ ;
        Hashtbl.add t.names typ name
    | Some cl ->
        if typ = cl then ()
        else invalid_arg (Printf.sprintf "[Types.add] Duplicated types %s" name)

  let add_uniq t id attrs =
    let name, typ = (id, Helpers.class_spec_of_attrs attrs) in
    let name =
      match Hashtbl.find_all t.names typ with
      | [] -> find_name t ~prefix:name
      | l -> (
          match List.find_opt (same_id id) l with
          | Some id -> id
          | None -> find_name t ~prefix:name)
    in
    add t (name, typ) ;
    name
end

type state = {
  enums : Ground.Enum.assoc;
  types : Types.t;
  mus : MuSet.t;
  imports : StringSet.t;
  extern : string AnyEncoding.Tbl.t;
}

(* Identifiers have a strict pattern to follow, this function removes the
   irregularities.

   Without escaping, the kaitai-struct-compiler throws the following error
   message: [error: invalid meta ID, expected /^[a-z][a-z0-9_]*$/] *)
let escape_id id =
  if String.length id < 1 then raise (Invalid_argument "empty id") ;
  let b = Buffer.create (String.length id) in
  if match id.[0] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true then
    Buffer.add_string b "id_" ;
  String.iter
    (function
      | ('a' .. 'z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | 'A' .. 'Z' as c -> Buffer.add_char b (Char.lowercase_ascii c)
      | '.' | '-' | ' ' -> Buffer.add_string b "__"
      | c ->
          (* we print [%S] to force escaping of the character *)
          raise
            (Failure
               (Format.asprintf
                  "Unsupported: special character (%C) in id (%S)"
                  c
                  id)))
    id ;
  Buffer.contents b

let add_enum state enum =
  {state with enums = Helpers.add_uniq_assoc state.enums enum}

let summary ~title ~description =
  match (title, description) with
  | None, None -> None
  | None, (Some _ as s) | (Some _ as s), None -> s
  | Some t, Some d -> Some (t ^ ": " ^ d)

(* when an encoding has id [x],
   then the attr for its size has id [len_id_of_id x].
   Kaitai recomment to use the [len_] prefix in this case. *)
let len_id_of_id id = "len_" ^ id

(* in kaitai-struct, some fields can be added to single attributes but not to a
   group of them. When we want to attach a field to a group of attributes, we
   need to create an indirection to a named type. [redirect] is a function for
   adding a field to an indirection. *)
let redirect state attrs fattr id_orig =
  let id = Types.add_uniq state.types id_orig attrs in
  let attr =
    fattr
      {
        (Helpers.default_attr_spec ~id:id_orig) with
        dataType = DataType.(ComplexDataType (UserType id));
      }
  in
  (state, attr)

(* [redirect_if_any] is like [redirect] but
   - it only does the redirection when there are multiple attributes
   - it adds the attribute directly if there is only one
   - it returns None in case of empty attributes.

   [redirect_if_any] is useful inside objs where there can be constant
   field used for prettier json encoding. Translating constant
   field would return empty attributes as they are not reflected in
   the binary encoding *)
let redirect_if_any :
    state ->
    AttrSpec.t list ->
    (AttrSpec.t -> AttrSpec.t) ->
    string ->
    state * AttrSpec.t option =
 fun state attrs fattr id ->
  match attrs with
  | [] -> (state, None)
  | [attr] -> (state, Some {(fattr attr) with id})
  | _ :: _ :: _ as attrs ->
      let state, attr = redirect state attrs fattr id in
      (state, Some attr)

(* [redirect_if_many] is like [redirect] but it only does the redirection when
   there are multiple attributes, otherwise it adds the field directly. *)
let redirect_if_many :
    state ->
    AttrSpec.t list ->
    (AttrSpec.t -> AttrSpec.t) ->
    string ->
    state * AttrSpec.t =
 fun state attrs fattr id ->
  match attrs with
  | [] -> failwith "redirect_if_many: empty list not supported"
  | [attr] -> (state, {(fattr attr) with id})
  | _ :: _ :: _ as attrs -> redirect state attrs fattr id

let rec seq_field_of_data_encoding0 :
    type a. state -> a DataEncoding.t -> string -> state * AttrSpec.t list =
 fun state ({encoding; _} as whole_encoding) id ->
  let id = escape_id id in
  match encoding with
  | Null -> (state, [])
  | Empty -> (state, [])
  | Ignore -> (state, [])
  | Constant _ -> (state, [])
  | Bool ->
      let state = add_enum state Ground.Enum.bool in
      (state, [Ground.Attr.bool ~id])
  | Uint8 -> (state, [Ground.Attr.uint8 ~id])
  | Int8 -> (state, [Ground.Attr.int8 ~id])
  | Uint16 -> (state, [Ground.Attr.uint16 ~id])
  | Int16 -> (state, [Ground.Attr.int16 ~id])
  | Int32 -> (state, [Ground.Attr.int32 ~id])
  | Int64 -> (state, [Ground.Attr.int64 ~id])
  | Int31 -> (state, [Ground.Attr.int31 ~id])
  | RangedInt {minimum; maximum} ->
      let size = Data_encoding__Binary_size.range_to_size ~minimum ~maximum in
      if minimum <= 0 then
        let valid =
          Some
            (ValidationSpec.ValidationRange
               {min = Ast.IntNum minimum; max = Ast.IntNum maximum})
        in
        let uvalid =
          if minimum = 0 then
            Some (ValidationSpec.ValidationMax (Ast.IntNum maximum))
          else valid
        in
        match size with
        | `Uint8 -> (state, [{(Ground.Attr.uint8 ~id) with valid = uvalid}])
        | `Uint16 -> (state, [{(Ground.Attr.uint16 ~id) with valid = uvalid}])
        | `Uint30 -> (state, [{(Ground.Attr.uint30 ~id) with valid = uvalid}])
        | `Int8 -> (state, [{(Ground.Attr.int8 ~id) with valid}])
        | `Int16 -> (state, [{(Ground.Attr.int16 ~id) with valid}])
        | `Int31 -> (state, [{(Ground.Attr.int31 ~id) with valid}])
      else
        (* when [minimum > 0] (as is the case in this branch), data-encoding
           shifts the value of the binary representation so that the minimum is at
           [0]. E.g., the interval [200]–[300] is represented on the wire as the
           interval [0]-[100] and the de/serialisation function is responsible for
           shifting to/from the actual range. *)
        let shift = minimum in
        let shifted_id = id ^ "_shifted_to_zero" in
        let shifted_encoding : a DataEncoding.t =
          {
            encoding =
              RangedInt {minimum = minimum - shift; maximum = maximum - shift};
            json_encoding = None;
          }
        in
        let state, represented_interval_attrs =
          seq_field_of_data_encoding state shifted_encoding shifted_id
        in
        let represented_interval_class =
          Helpers.class_spec_of_attrs
            ~instances:
              [
                ( "value",
                  {
                    doc =
                      {
                        summary =
                          Some
                            "The interval is represented shifted towards 0 for \
                             compactness, this instance corrects the shift.";
                        refs = [];
                      };
                    descr =
                      ValueInstanceSpec
                        {
                          id = "value";
                          value =
                            BinOp
                              {
                                left = Name shifted_id;
                                op = Add;
                                right = IntNum shift;
                              };
                          ifExpr = None;
                        };
                  } );
              ]
            represented_interval_attrs
        in
        Types.add state.types (shifted_id, represented_interval_class) ;
        ( state,
          [
            {
              (Helpers.default_attr_spec ~id) with
              dataType = ComplexDataType (UserType shifted_id);
            };
          ] )
  | Float -> (state, [Ground.Attr.float ~id])
  | RangedFloat {minimum; maximum} ->
      let valid =
        Some
          (ValidationSpec.ValidationRange
             {min = Ast.FloatNum minimum; max = Ast.FloatNum maximum})
      in
      (state, [{(Ground.Attr.float ~id) with valid}])
  | Bytes (`Fixed n, _) -> (state, [Ground.Attr.bytes ~id (Fixed n)])
  | Bytes (`Variable, _) -> (state, [Ground.Attr.bytes ~id Variable])
  | Dynamic_size {kind; encoding = {encoding = Bytes (`Variable, _); _}} ->
      let len_id = len_id_of_id id in
      let len_attr = Ground.Attr.binary_length_kind ~id:len_id kind in
      (state, [len_attr; Ground.Attr.bytes ~id (Dynamic len_id)])
  | String (`Fixed n, _) -> (state, [Ground.Attr.string ~id (Fixed n)])
  | String (`Variable, _) -> (state, [Ground.Attr.string ~id Variable])
  | Dynamic_size {kind; encoding = {encoding = String (`Variable, _); _}} ->
      let len_id = len_id_of_id id in
      let size_attr = Ground.Attr.binary_length_kind ~id:len_id kind in
      (state, [size_attr; Ground.Attr.string ~id (Dynamic len_id)])
  | Dynamic_size {kind; encoding = {encoding = Check_size {limit; encoding}; _}}
    ->
      let len_id = len_id_of_id id in
      let len_attr =
        Helpers.merge_valid
          (Ground.Attr.binary_length_kind ~id:len_id kind)
          (ValidationMax (Ast.IntNum limit))
      in
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let state, attr =
        redirect
          state
          attrs
          (fun attr -> {attr with size = Some (Ast.Name len_id)})
          id
      in
      (state, [len_attr; attr])
  | Padded (encoding, pad) ->
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let pad_attr =
        let id = id ^ "_padding" in
        let doc =
          {
            Helpers.default_doc_spec with
            summary = Some "This field is for padding, ignore";
          }
        in
        {(Ground.Attr.bytes ~id (Fixed pad)) with doc}
      in
      (state, attrs @ [pad_attr])
  | N ->
      Types.add state.types Ground.Type.n_chunk ;
      Types.add state.types Ground.Type.n ;
      (state, [Ground.Attr.n ~id])
  | Z ->
      Types.add state.types Ground.Type.n_chunk ;
      Types.add state.types Ground.Type.z ;
      (state, [Ground.Attr.z ~id])
  | Conv {encoding; _} -> seq_field_of_data_encoding state encoding id
  | Tup _ ->
      (* single-field tup *)
      let tid_gen =
        let already_called = ref false in
        fun () ->
          if !already_called then
            raise (Invalid_argument "multiple fields inside a single-field tup") ;
          already_called := true ;
          id
      in
      seq_field_of_tups state tid_gen encoding
  | Tups _ ->
      (* multi-field tup *)
      let tid_gen = Helpers.mk_tid_gen id in
      seq_field_of_tups state tid_gen encoding
  | List {length_limit; length_encoding; elts} ->
      seq_field_of_collection state length_limit length_encoding elts id
  | Array {length_limit; length_encoding; elts} ->
      seq_field_of_collection state length_limit length_encoding elts id
  | Obj f -> seq_field_of_field state f
  | Objs {kind = _; left; right} ->
      let state, left = seq_field_of_data_encoding state left id in
      let state, right = seq_field_of_data_encoding state right id in
      let seq = left @ right in
      (state, seq)
  | Union {kind = _; tag_size; tagged_cases = _; match_case = _; cases} ->
      seq_field_of_union state tag_size cases id
  | Dynamic_size {kind; encoding} ->
      let len_id = len_id_of_id id in
      let len_attr =
        match kind with
        | `N -> failwith "Dynamic_size N not implemented"
        | `Uint30 -> Ground.Attr.uint30 ~id:len_id
        | `Uint16 -> Ground.Attr.uint16 ~id:len_id
        | `Uint8 -> Ground.Attr.uint8 ~id:len_id
      in
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let state, attr =
        redirect
          state
          attrs
          (fun attr -> {attr with size = Some (Ast.Name len_id)})
          id
      in
      (state, [len_attr; attr])
  | Splitted {encoding; json_encoding = _; is_obj = _; is_tup = _} ->
      seq_field_of_data_encoding state encoding id
  | Describe {encoding; id; description; title} ->
      let id = escape_id id in
      let summary = summary ~title ~description in
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let state, attr =
        redirect
          state
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (state, [attr])
  | Check_size {limit = _; encoding} ->
      (* TODO: Add a guard for check size.*)
      seq_field_of_data_encoding state encoding id
  | Delayed mk ->
      (* TODO: once data-encoding is monorepoed: remove delayed and have "cached" *)
      let e = mk () in
      seq_field_of_data_encoding state e id
  | Mu {name; title; description; fix; kind = _} ->
      let summary = summary ~title ~description in
      let name = escape_id name in
      if MuSet.mem name state.mus then
        (* This node was already visited, we just put a pointer. *)
        ( state,
          [
            {
              (Helpers.default_attr_spec ~id) with
              dataType = ComplexDataType (UserType name);
            };
          ] )
      else
        let state = {state with mus = MuSet.add name state.mus} in
        let fixed = fix whole_encoding in
        let state, attrs = seq_field_of_data_encoding state fixed name in
        let state, attr =
          redirect
            state
            attrs
            (fun attr -> Helpers.merge_summaries attr summary)
            name
        in
        (state, [attr])
  | String_enum (h, a) ->
      (* In kaitai, [id]s inside [EnumSpec] must be valid identifier and
         be unique for a given enum.  Here we:
          - use [escape_id] so that we have valid identifier.
         - try to preserve the original name when 2 escaped id collide.
         - find unique identifier by suffixing a number. *)
      let allocated_names =
        let t = Hashtbl.create 17 in
        Hashtbl.iter
          (fun _ (original_name, i) ->
            let name = escape_id original_name in
            if String.equal name original_name && not (Hashtbl.mem t name) then
              (* This name has is already a valid id. Let's reserve it. *)
              Hashtbl.add t name i)
          h ;
        t
      in
      let rec find_available_name ~prefix:name n =
        let name' = Printf.sprintf "%s_%d" name n in
        if Hashtbl.mem allocated_names name' then
          find_available_name ~prefix:name (succ n)
        else name'
      in
      let map =
        Hashtbl.to_seq_values h
        |> Seq.map (fun (original_name, i) ->
               (* [escape_id original_name] could fail because
                  [original_name] can be an arbitrary string. Let's only
                  revisit if we ever encounter the usecase. *)
               let name = escape_id original_name in
               let name =
                 match Hashtbl.find_opt allocated_names name with
                 | None ->
                     Hashtbl.add allocated_names name i ;
                     name
                 | Some j when i = j -> (* This name was reserved above *) name
                 | Some _ ->
                     let name' = find_available_name ~prefix:name 0 in
                     Hashtbl.add allocated_names name' i ;
                     name'
               in
               let doc =
                 DocSpec.
                   {
                     refs = [];
                     summary =
                       (if String.equal original_name name then None
                       else Some original_name);
                   }
               in
               (i, EnumValueSpec.{name; doc}))
        |> List.of_seq
        |> List.sort (fun (t1, _) (t2, _) -> compare t1 t2)
      in
      let enumspec = EnumSpec.{map} in
      let state = add_enum state (id, enumspec) in
      let dataType =
        DataType.NumericType
          (Int_type
             (match Data_encoding__Binary_size.enum_size a with
             | `Uint8 -> Int1Type {signed = false}
             | `Uint16 ->
                 IntMultiType {signed = false; width = W2; endian = None}
             | `Uint30 ->
                 IntMultiType {signed = false; width = W4; endian = None}))
      in
      let attr =
        {(Helpers.default_attr_spec ~id) with dataType; enum = Some id}
      in
      (state, [attr])

and seq_field_of_data_encoding :
    type a. state -> a DataEncoding.t -> string -> state * AttrSpec.t list =
 fun state whole_encoding id ->
  match
    AnyEncoding.Tbl.find_opt state.extern (AnyEncoding.pack whole_encoding)
  with
  | Some name ->
      let name = escape_id name in
      let state = {state with imports = StringSet.add name state.imports} in
      ( state,
        [
          {
            (Helpers.default_attr_spec ~id:(escape_id id)) with
            dataType = DataType.(ComplexDataType (UserType name));
          };
        ] )
  | None -> seq_field_of_data_encoding0 state whole_encoding id

and seq_field_of_tups :
    type a.
    state -> Helpers.tid_gen -> a DataEncoding.desc -> state * AttrSpec.t list =
 fun state tid_gen d ->
  match d with
  | Tup {encoding = Tup _ as e; json_encoding = _} ->
      seq_field_of_tups state tid_gen e
  | Tup {encoding = Tups _ as e; json_encoding = _} ->
      seq_field_of_tups state tid_gen e
  | Tup e ->
      let id = tid_gen () in
      let state, attrs = seq_field_of_data_encoding state e id in
      let state, attrs =
        match attrs with
        | [] -> (state, [])
        | [attr] ->
            if attr.id = id then (state, attrs)
            else
              (* [id] was over-ridden by a [Describe] or other construct, we
                 restore it here to guarantee names are unique. *)
              (state, [Helpers.merge_summaries {attr with id} (Some attr.id)])
        | _ :: _ ->
            let state, attr = redirect state attrs Fun.id id in
            (state, [attr])
      in
      (state, attrs)
  | Tups {kind = _; left; right} ->
      let state, left = seq_field_of_tups state tid_gen left.encoding in
      let state, right = seq_field_of_tups state tid_gen right.encoding in
      let seq = left @ right in
      (state, seq)
  | _ -> failwith "Non-tup(s) inside a tups"

and seq_field_of_field :
    type a. state -> a DataEncoding.field -> state * AttrSpec.t list =
 fun state f ->
  match f with
  | Req {name; encoding; title; description} ->
      let id = escape_id name in
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let summary = summary ~title ~description in
      let state, attr_o =
        redirect_if_any
          state
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (state, Option.to_list attr_o)
  | Opt {name; kind = _; encoding; title; description} ->
      let cond_id = escape_id (name ^ "_tag") in
      let state = add_enum state Ground.Enum.bool in
      let cond_attr = Ground.Attr.bool ~id:cond_id in
      let cond =
        {
          Helpers.cond_no_cond with
          ifExpr =
            Some
              Ast.(
                Compare
                  {
                    left = Name cond_id;
                    ops = Eq;
                    right =
                      EnumByLabel
                        {
                          enumName = fst Ground.Enum.bool;
                          label = Ground.Enum.bool_true_name;
                          inType = Ast.empty_typeId;
                        };
                  });
        }
      in
      let id = escape_id name in
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let summary = summary ~title ~description in
      let state, attr_o =
        redirect_if_any
          state
          attrs
          (fun attr -> {(Helpers.merge_summaries attr summary) with cond})
          id
      in
      (state, cond_attr :: Option.to_list attr_o)
  | Dft {name; encoding; default = _; title; description} ->
      (* NOTE: in binary format Dft is the same as Req *)
      let id = escape_id name in
      let state, attrs = seq_field_of_data_encoding state encoding id in
      let summary = summary ~title ~description in
      let state, attr_o =
        redirect_if_any
          state
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (state, Option.to_list attr_o)

and seq_field_of_union :
    type a.
    state ->
    Data_encoding__Binary_size.tag_size ->
    a DataEncoding.case list ->
    string ->
    state * AttrSpec.t list =
 fun state tag_size cases id ->
  let tagged_cases : (int * string * string option * AnyEncoding.t) list =
    (* Some cases are JSON-only, we filter those out and we also get rid of
       parts we don't care about like injection and projection functions. *)
    List.filter_map
      (fun (DataEncoding.Case
             {title; tag; description; encoding; proj = _; inj = _}) ->
        Data_encoding__Uint_option.fold
          ~none:None
          ~some:(fun tag ->
            Some (tag, escape_id title, description, AnyEncoding.pack encoding))
          tag)
      cases
  in
  let id = escape_id id in
  let tag_type : Kaitai.Types.DataType.int_type =
    match tag_size with
    | `Uint8 -> Int1Type {signed = false}
    | `Uint16 -> IntMultiType {signed = false; width = W2; endian = None}
  in
  let tag_id = id ^ "_tag" in
  let tag_enum_map =
    List.map
      (fun (tag, id, description, _) ->
        ( tag,
          EnumValueSpec.
            {name = id; doc = DocSpec.{refs = []; summary = description}} ))
      tagged_cases
    |> List.sort (fun (t1, _) (t2, _) -> compare t1 t2)
  in
  let tag_enum = EnumSpec.{map = tag_enum_map} in
  let state = add_enum state (tag_id, tag_enum) in
  let tag_attr =
    {
      (Helpers.default_attr_spec ~id:tag_id) with
      dataType = DataType.(NumericType (Int_type tag_type));
      enum = Some tag_id;
    }
  in
  let state, payload_attrs =
    List.fold_left
      (fun (state, payload_attrs)
           (_, case_id, _, AnyEncoding.AnyEncoding encoding) ->
        let state, attrs = seq_field_of_data_encoding state encoding case_id in
        match attrs with
        | [] -> (state, payload_attrs)
        | _ :: _ as attrs ->
            let state, attr =
              redirect_if_many
                state
                attrs
                (fun attr ->
                  {
                    attr with
                    cond =
                      {
                        Helpers.cond_no_cond with
                        ifExpr =
                          Some
                            (Compare
                               {
                                 left = Name tag_id;
                                 ops = Eq;
                                 right =
                                   EnumByLabel
                                     {
                                       enumName = tag_id;
                                       label = case_id;
                                       inType = Ast.empty_typeId;
                                     };
                               });
                      };
                  })
                case_id
            in
            (state, attr :: payload_attrs))
      (state, [])
      tagged_cases
  in
  let payload_attrs = List.rev payload_attrs in
  (state, tag_attr :: payload_attrs)

and seq_field_of_collection :
    type a.
    state ->
    DataEncoding.limit ->
    int DataEncoding.encoding option ->
    a DataEncoding.encoding ->
    string ->
    state * AttrSpec.t list =
 fun state length_limit length_encoding elts id ->
  match (length_limit, length_encoding, elts) with
  | At_most max_length, Some le, elts ->
      (* Kaitai recommend to use the [num_] prefix *)
      let length_id = "num_" ^ id in
      let state, length_attrs = seq_field_of_data_encoding state le length_id in
      let length_attr =
        match length_attrs with
        | [] -> assert false
        | [attr] ->
            Helpers.merge_valid
              attr
              (ValidationSpec.ValidationMax (Ast.IntNum max_length))
        | _ :: _ :: _ ->
            (* TODO: Big number length size not yet supported. We expect
                     [`Uint30/16/8] to produce only one attribute. *)
            failwith "Not supported (N-like header)"
      in
      let elt_id = id ^ "_elt" in
      let state, attrs = seq_field_of_data_encoding state elts elt_id in
      let state, attr =
        (* We do uncoditional redirect because there can be issues where the
           [size] of elements and the [size] of the list get mixed up.
           TODO: redirect on (a) multiple attrs and (b) single attr with [size] *)
        redirect
          state
          attrs
          (fun attr ->
            {
              attr with
              cond =
                {
                  Helpers.cond_no_cond with
                  repeat = RepeatExpr (Ast.Name length_id);
                };
            })
          (id ^ "_entries")
      in
      (state, [length_attr; attr])
  | (Exactly _ | No_limit), Some _, _ ->
      (* The same [assert false] exists in the de/serialisation functions of
         [data_encoding]. This specific case is rejected by data-encoding
         because the length of the list is both known statically and determined
         dynamically by a header which is a waste of space. *)
      assert false
  | length_limit, None, elts ->
      let elt_id = id ^ "_elt" in
      let state, attrs = seq_field_of_data_encoding state elts elt_id in
      let state, attr =
        redirect
          state
          attrs
          (fun attr ->
            match length_limit with
            | No_limit ->
                {
                  attr with
                  cond = {Helpers.cond_no_cond with repeat = RepeatEos};
                }
            | At_most _max_length ->
                {
                  attr with
                  (* TODO: Add guard of length *)
                  cond = {Helpers.cond_no_cond with repeat = RepeatEos};
                }
            | Exactly exact_length ->
                {
                  attr with
                  cond =
                    {
                      Helpers.cond_no_cond with
                      repeat = RepeatExpr (Ast.IntNum exact_length);
                    };
                })
          (id ^ "_entries")
      in
      (state, [attr])

let add_original_id_to_description ?description id =
  match description with
  | None -> "Encoding id: " ^ id
  | Some description -> "Encoding id: " ^ id ^ "\nDescription: " ^ description

let from_data_encoding :
    type a.
    id:string ->
    ?extern:string AnyEncoding.Tbl.t ->
    a DataEncoding.t ->
    ClassSpec.t =
 fun ~id ?(extern = AnyEncoding.Tbl.create 0) encoding ->
  let encoding_name = escape_id id in
  let extern = AnyEncoding.Tbl.copy extern in
  AnyEncoding.Tbl.remove extern (AnyEncoding.pack encoding) ;
  let state =
    {
      enums = [];
      types = Types.create ();
      mus = MuSet.empty;
      imports = StringSet.empty;
      extern;
    }
  in
  let sort l = List.sort (fun (t1, _) (t2, _) -> compare t1 t2) l in
  let set_name (spec : ClassSpec.t) =
    {spec with meta = {spec.meta with id = Some encoding_name}}
  in
  match encoding.encoding with
  | Describe {encoding; description; id = descrid; _} ->
      let description = add_original_id_to_description ?description id in
      let state, attrs = seq_field_of_data_encoding state encoding descrid in
      Helpers.class_spec_of_attrs
        ~description
        ~enums:(sort state.enums)
        ~types:(Types.to_list state.types |> sort)
        ~imports:(StringSet.elements state.imports)
        ~instances:[]
        attrs
      |> set_name
  | _ ->
      let description = add_original_id_to_description id in
      let state, attrs =
        seq_field_of_data_encoding state encoding encoding_name
      in
      Helpers.class_spec_of_attrs
        ~description
        ~enums:(sort state.enums)
        ~types:(Types.to_list state.types |> sort)
        ~imports:(StringSet.elements state.imports)
        ~instances:[]
        attrs
      |> set_name
