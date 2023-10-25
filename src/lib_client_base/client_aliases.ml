(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* Tezos Command line interface - Local Storage for Configuration *)

let rec try_alternatives input = function
  | [] -> failwith "Could not parse input."
  | (_, f) :: alts -> either_f (f input) (fun () -> try_alternatives input alts)

let parse_alternatives alts input =
  match String.split ~limit:1 ':' input with
  | [_] -> try_alternatives input alts
  | [format; value] -> (
      match List.assoc_opt ~equal:String.equal format alts with
      | Some f -> f value
      | None -> try_alternatives input alts)
  | _ -> assert false
(* cannot happen due to String.split's implementation. *)

module type Entity = sig
  type t

  val encoding : t Data_encoding.t

  val of_source : string -> t tzresult Lwt.t

  val to_source : t -> string tzresult Lwt.t

  val name : string

  include Compare.S with type t := t
end

module type Alias = sig
  type t

  type fresh_param

  val encoding : t Data_encoding.t

  val load : #Client_context.wallet -> (string * t) list tzresult Lwt.t

  val set : #Client_context.wallet -> (string * t) list -> unit tzresult Lwt.t

  val find : #Client_context.wallet -> string -> t tzresult Lwt.t

  val find_opt : #Client_context.wallet -> string -> t option tzresult Lwt.t

  val rev_find : #Client_context.wallet -> t -> string option tzresult Lwt.t

  val rev_find_all : #Client_context.wallet -> t -> string list tzresult Lwt.t

  val name : #Client_context.wallet -> t -> string tzresult Lwt.t

  val mem : #Client_context.wallet -> string -> bool tzresult Lwt.t

  val add :
    force:bool -> #Client_context.wallet -> string -> t -> unit tzresult Lwt.t

  val add_many :
    #Client_context.wallet -> (string * t) list -> unit tzresult Lwt.t

  val del : #Client_context.wallet -> string -> unit tzresult Lwt.t

  val update : #Client_context.wallet -> string -> t -> unit tzresult Lwt.t

  val of_source : string -> t tzresult Lwt.t

  val to_source : t -> string tzresult Lwt.t

  val alias_parameter :
    unit -> (string * t, #Client_context.wallet) Tezos_clic.parameter

  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'b)) Tezos_clic.params ->
    (string * t -> 'a, 'b) Tezos_clic.params

  val aliases_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'b)) Tezos_clic.params ->
    ((string * t) list -> 'a, 'b) Tezos_clic.params

  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (< .. > as 'obj)) Tezos_clic.params ->
    (fresh_param -> 'a, 'obj) Tezos_clic.params

  val force_switch : unit -> (bool, _) Tezos_clic.arg

  val of_fresh :
    #Client_context.wallet -> bool -> fresh_param -> string tzresult Lwt.t

  val parse_source_string : #Client_context.wallet -> string -> t tzresult Lwt.t

  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'obj)) Tezos_clic.params ->
    (t -> 'a, 'obj) Tezos_clic.params

  val source_arg :
    ?long:string ->
    ?placeholder:string ->
    ?doc:string ->
    unit ->
    (t option, (#Client_context.wallet as 'obj)) Tezos_clic.arg

  val autocomplete : #Client_context.wallet -> string list tzresult Lwt.t
end

module Alias (Entity : Entity) = struct
  open Client_context
  module Map = Map.Make (String)

  let wallet_encoding : (string * Entity.t) list Data_encoding.encoding =
    let open Data_encoding in
    list (obj2 (req "name" string) (req "value" Entity.encoding))

  type cache = {
    mutable mtime : float option;
        (** [None] if the associated file does not exist; otherwise is the last
            modification time of the associated file. *)
    mutable list_assoc : (string * Entity.t) list;
    mutable map : Entity.t Map.t;
  }

  (** Bindings of wallet to cache. The base directory of wallets are used as
      keys. *)
  type caches = (string * cache) list ref

  let caches : caches = ref []

  (** [peek_cache wallet] returns {Some v} if the binding of [wallet] in the
      cache is {v}, or {None} if no binding for [wallet] exists. *)
  let peek_cache (wallet : #wallet) =
    List.assoc_opt ~equal:String.equal wallet#get_base_dir !caches

  (** [update_assoc key value list] returns a list containing the same bindings
      as [list], except for the bindings of [key]. If [value] is {None}, the
      bindings are removed if it exists; otherwise, if [value] is {Some v} then
      the bindings of [key] are replaced by one binding of [key] to {v} in the
      resulting list. *)
  let update_assoc key value list =
    let remove key = List.filter (fun (n, _) -> not (String.equal n key)) in
    match value with
    | Some value -> (key, value) :: remove key list
    | None -> remove key list

  (** [replace_cache wallet ?mtime list_assoc] replaces the cache bind to
      [wallet] by a new cache {cache}. If [mtime] is {Some mt}, then
      {cache.mtime = mt}; otherwise, {cache.mtime} is generated by
      [wallet#last_modification_time]. *)
  let replace_cache (wallet : #wallet) ?mtime list_assoc =
    let open Lwt_result_syntax in
    let* mtime =
      match mtime with
      | None -> wallet#last_modification_time Entity.name
      | Some mtime -> return mtime
    in
    let map = Map.of_seq (List.to_seq list_assoc) in

    let cache = {mtime; list_assoc; map} in
    caches := update_assoc wallet#get_base_dir (Some cache) !caches ;
    return cache

  (** [get_cache wallet] reloads the cache bind to [wallet] if the associated
      file does not exist or if its last modification time changed; then
      returns it. *)
  let get_cache (wallet : #wallet) =
    let open Lwt_result_syntax in
    let* mtime = wallet#last_modification_time Entity.name in
    let cache = peek_cache wallet in
    match (mtime, cache) with
    | Some fresh_mtime, Some {mtime = Some cache_mtime; _}
      when fresh_mtime = cache_mtime ->
        return (WithExceptions.Option.get ~loc:__LOC__ cache)
    | _ ->
        let* list_assoc =
          wallet#load
            Entity.name
            ~default:([] : (string * Entity.t) list)
            wallet_encoding
        in
        replace_cache wallet ~mtime list_assoc

  (** [update_cache wallet cache key value] updates the cache bind to
      [wallet] and the associated file with a cache containing the same
      bindings as [cache], except for the bindings of [key]. If [value] is
      {None}, the bindings are removed if it exists; otherwise, if [value] is
      {Some v}, then the bindings of [key] are replaced by one binding of [key]
      to {v} in the resulting cache. *)
  let update_cache (wallet : #wallet) cache key value =
    let open Lwt_result_syntax in
    (match value with
    | Some value ->
        cache.list_assoc <- update_assoc key (Some value) cache.list_assoc ;
        cache.map <- Map.add key value cache.map
    | None ->
        cache.list_assoc <- update_assoc key None cache.list_assoc ;
        cache.map <- Map.remove key cache.map) ;
    let* () = wallet#write Entity.name cache.list_assoc wallet_encoding in
    let* mtime = wallet#last_modification_time Entity.name in
    cache.mtime <- mtime ;
    return_unit

  let load (wallet : #wallet) =
    let open Lwt_result_syntax in
    let* cache = get_cache wallet in
    return cache.list_assoc

  let load_map (wallet : #wallet) =
    let open Lwt_result_syntax in
    let* cache = get_cache wallet in
    return cache.map

  let set (wallet : #wallet) entries =
    let open Lwt_result_syntax in
    let* () = wallet#write Entity.name entries wallet_encoding in
    let* _cache = replace_cache wallet entries in
    return_unit

  let autocomplete wallet =
    let open Lwt_syntax in
    let* r = load wallet in
    match r with
    | Error _ -> return_ok_nil
    | Ok list -> return_ok (List.map fst list)

  let find_opt (wallet : #wallet) name =
    let open Lwt_result_syntax in
    let+ map = load_map wallet in
    Map.find name map

  let find (wallet : #wallet) name =
    let open Lwt_result_syntax in
    let* map = load_map wallet in
    match Map.find name map with
    | Some v -> return v
    | None -> failwith "no %s alias named %s" Entity.name name

  let rev_find (wallet : #wallet) v =
    let open Lwt_result_syntax in
    let+ list = load wallet in
    Option.map fst @@ List.find (fun (_, v') -> Entity.(v = v')) list

  let rev_find_all (wallet : #wallet) v =
    let open Lwt_result_syntax in
    let* list = load wallet in
    return
      (List.filter_map
         (fun (n, v') -> if Entity.(v = v') then Some n else None)
         list)

  let mem (wallet : #wallet) name =
    let open Lwt_result_syntax in
    let+ map = load_map wallet in
    Map.mem name map

  let add ~force (wallet : #wallet) name value =
    let open Lwt_result_syntax in
    let keep = ref false in
    let* cache = get_cache wallet in
    let* () =
      if force then return_unit
      else
        List.iter_es
          (fun (n, v) ->
            if Compare.String.(n = name) && Entity.(v = value) then (
              keep := true ;
              return_unit)
            else if Compare.String.(n = name) && Entity.(v <> value) then
              failwith
                "another %s is already aliased as %s, use --force to update"
                Entity.name
                n
            else if Compare.String.(n <> name) && Entity.(v = value) then
              failwith
                "this %s is already aliased as %s, use --force to insert \
                 duplicate"
                Entity.name
                n
            else return_unit)
          cache.list_assoc
    in
    if !keep then return_unit else update_cache wallet cache name (Some value)

  let add_many (wallet : #wallet) xs =
    let open Lwt_result_syntax in
    let* cache = get_cache wallet in
    let map_to_add = Map.of_seq (List.to_seq xs) in
    cache.map <- Map.union (fun _key x _existing -> Some x) map_to_add cache.map ;
    cache.list_assoc <- List.of_seq (Map.to_seq cache.map) ;
    let* () = wallet#write Entity.name cache.list_assoc wallet_encoding in
    let* mtime = wallet#last_modification_time Entity.name in
    cache.mtime <- mtime ;
    return_unit

  let del (wallet : #wallet) name =
    let open Lwt_result_syntax in
    let* cache = get_cache wallet in
    update_cache wallet cache name None

  let update (wallet : #wallet) name value =
    let open Lwt_result_syntax in
    let* cache = get_cache wallet in
    update_cache wallet cache name (Some value)

  include Entity

  let alias_parameter () =
    let open Lwt_result_syntax in
    Tezos_clic.parameter ~autocomplete (fun cctxt s ->
        let* v = find cctxt s in
        return (s, v))

  let alias_param ?(name = "name")
      ?(desc = "existing " ^ Entity.name ^ " alias") next =
    Tezos_clic.param ~name ~desc (alias_parameter ()) next

  let aliases_parameter () =
    let open Lwt_result_syntax in
    Tezos_clic.parameter ~autocomplete (fun cctxt s ->
        String.split_no_empty ',' s
        |> List.map_es (fun s ->
               let* pkh = find cctxt s in
               return (s, pkh)))

  let aliases_param ?(name = "name")
      ?(desc = "existing " ^ Entity.name ^ " aliases") next =
    Tezos_clic.param ~name ~desc (aliases_parameter ()) next

  type fresh_param = Fresh of string

  let of_fresh (wallet : #wallet) force (Fresh s) =
    let open Lwt_result_syntax in
    let* list = load wallet in
    let* () =
      if force then return_unit
      else
        List.iter_es
          (fun (n, v) ->
            if String.equal n s then
              let* value = Entity.to_source v in
              failwith
                "@[<v 2>The %s alias %s already exists.@,\
                 The current value is %s.@,\
                 Use --force to update@]"
                Entity.name
                n
                value
            else return_unit)
          list
    in
    return s

  let fresh_alias_param ?(name = "new")
      ?(desc = "new " ^ Entity.name ^ " alias") next =
    Tezos_clic.param
      ~name
      ~desc
      (Tezos_clic.parameter (fun (_ : < .. >) s -> Lwt.return_ok (Fresh s)))
      next

  let parse_source_string cctxt s =
    let open Lwt_result_syntax in
    parse_alternatives
      [
        ("alias", fun alias -> find cctxt alias);
        ( "file",
          fun path ->
            let* input = cctxt#read_file path in
            of_source input );
        ("text", of_source);
      ]
      s

  let source_param ?(name = "src") ?(desc = "source " ^ Entity.name) next =
    let desc =
      Format.asprintf
        "%s\n\
         Can be a %s name, a file or a raw %s literal. If the parameter is not \
         the name of an existing %s, the client will look for a file \
         containing a %s, and if it does not exist, the argument will be read \
         as a raw %s.\n\
         Use 'alias:name', 'file:path' or 'text:literal' to disable autodetect."
        desc
        Entity.name
        Entity.name
        Entity.name
        Entity.name
        Entity.name
    in
    Tezos_clic.param ~name ~desc (Tezos_clic.parameter parse_source_string) next

  let source_arg ?(long = "source " ^ Entity.name) ?(placeholder = "src")
      ?(doc = "") () =
    let doc =
      Format.asprintf
        "%s\n\
         Can be a %s name, a file or a raw %s literal. If the parameter is not \
         the name of an existing %s, the client will look for a file \
         containing a %s, and if it does not exist, the argument will be read \
         as a raw %s.\n\
         Use 'alias:name', 'file:path' or 'text:literal' to disable autodetect."
        doc
        Entity.name
        Entity.name
        Entity.name
        Entity.name
        Entity.name
    in
    Tezos_clic.arg
      ~long
      ~placeholder
      ~doc
      (Tezos_clic.parameter parse_source_string)

  let force_switch () =
    Tezos_clic.switch
      ~long:"force"
      ~short:'f'
      ~doc:("overwrite existing " ^ Entity.name)
      ()

  let name (wallet : #wallet) d =
    let open Lwt_result_syntax in
    let* o = rev_find wallet d in
    match o with None -> Entity.to_source d | Some name -> return name
end
