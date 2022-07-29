(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(* The use of polymorphic variant in these two (opaque) phantom types
   is motivated by the need to circumvent warning 37
   ([unused-constructor]). *)

type 'a value = [`Value of 'a]

type ('a, 'b) dict = [`Dict of 'a * 'b]

module Make (T : Tree.S) = struct
  type tree = T.tree

  module Schema = struct
    type 'a decoder = tree -> 'a option Lwt.t

    type 'a encoder = tree -> string list -> 'a -> tree Lwt.t

    type !'a t = {folders : string list; descr : 'a schema}

    and !'a schema =
      | Value_s : 'a encoder * 'a decoder -> 'a value schema
      | Tup2_s : 'a field * 'b field -> ('a * 'b) schema
      | Tup3_s : 'a field * 'b field * 'c field -> ('a * 'b * 'c) schema
      | Tup4_s :
          'a field * 'b field * 'c field * 'd field
          -> ('a * 'b * 'c * 'd) schema
      | Tup5_s :
          'a field * 'b field * 'c field * 'd field * 'e field
          -> ('a * 'b * 'c * 'd * 'e) schema
      | Dict_s : ('a -> string) * 'b t -> ('a, 'b) dict schema

    and !'a field = {directory : string; schema : 'a t}

    let encoding : 'a Data_encoding.t -> 'a value t =
     fun encoding ->
      let encoder tree key value =
        T.add tree key (Data_encoding.Binary.to_bytes_exn encoding value)
      in
      let decoder tree =
        let open Lwt_syntax in
        let* bytes = T.find tree [] in
        match bytes with
        | Some bytes ->
            Lwt.return
            @@ Some (Data_encoding.Binary.of_bytes_exn encoding bytes)
        | None -> Lwt.return None
      in
      {folders = []; descr = Value_s (encoder, decoder)}

    let custom ~encoder ~decoder =
      {folders = []; descr = Value_s (encoder, decoder)}

    let req directory schema = {directory; schema}

    let folders str {folders; descr} = {folders = folders @ List.rev str; descr}

    let obj2 b1 b2 = {folders = []; descr = Tup2_s (b1, b2)}

    let obj3 b1 b2 b3 = {folders = []; descr = Tup3_s (b1, b2, b3)}

    let obj4 b1 b2 b3 b4 = {folders = []; descr = Tup4_s (b1, b2, b3, b4)}

    let obj5 b1 b2 b3 b4 b5 =
      {folders = []; descr = Tup5_s (b1, b2, b3, b4, b5)}

    let dict encoder schema = {folders = []; descr = Dict_s (encoder, schema)}
  end

  type 'a schema = 'a Schema.t

  type !'a shallow =
    | Shallow : tree option -> 'a shallow
        (** [Shalow (Some tree)] encodes a piece of data that has not
            been fetched from [tree]. [Shallow None] encodes a piece
            of data that is absent from the original tree, or that has
            been cut from the tree. *)
    | Tup2 : 'a t * 'b t -> ('a * 'b) shallow
    | Tup3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) shallow
    | Tup4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) shallow
    | Tup5 :
        'a t * 'b t * 'c t * 'd t * 'e t
        -> ('a * 'b * 'c * 'd * 'e) shallow
    | Value : 'a -> 'a value shallow
    | Dict : tree option * ('a * 'b t) list -> ('a, 'b) dict shallow

  and !'a t = {value : 'a shallow ref; schema : 'a schema}

  type !'a thunk = 'a t

  let shallow_t tree schema = {value = ref (Shallow tree); schema}

  let decode : 'a schema -> tree -> 'a thunk =
   fun schema tree -> shallow_t (Some tree) schema

  let encode : type a. tree -> a thunk -> tree Lwt.t =
   fun tree ->
    let rec encode : type a. string list -> tree -> a thunk -> tree Lwt.t =
     fun prefix tree thunk ->
      let open Lwt_syntax in
      let tree_prefix = prefix in
      let value_prefix = thunk.schema.folders @ tree_prefix in
      match (!(thunk.value), thunk.schema.descr) with
      | Shallow (Some tree'), _ ->
          let* tree = T.add_tree tree (List.rev tree_prefix) tree' in
          return tree
      | Shallow None, _ ->
          let* tree = T.remove tree (List.rev tree_prefix) in
          return tree
      | Value x, Value_s (encoder, _) ->
          let* tree = encoder tree (List.rev value_prefix) x in
          return tree
      | Dict (Some tree', assoc), Dict_s (key_encoder, _schema) ->
          let* tree' =
            Lwt_list.fold_left_s
              (fun tree' (k, v) ->
                encode (key_encoder k :: thunk.schema.folders) tree' v)
              tree'
              assoc
          in
          let* tree = T.add_tree tree (List.rev tree_prefix) tree' in
          return tree
      | Dict (None, assoc), Dict_s (key_encoder, _schema) ->
          let* tree = T.remove tree (List.rev tree_prefix) in
          let* tree =
            Lwt_list.fold_left_s
              (fun tree (k, v) -> encode (key_encoder k :: value_prefix) tree v)
              tree
              assoc
          in
          return tree
      | Tup2 (x, y), Tup2_s (fst, snd) ->
          let* tree = encode (fst.directory :: value_prefix) tree x in
          let* tree = encode (snd.directory :: value_prefix) tree y in
          return tree
      | Tup3 (x, y, z), Tup3_s (fst, snd, thd) ->
          let* tree = encode (fst.directory :: value_prefix) tree x in
          let* tree = encode (snd.directory :: value_prefix) tree y in
          let* tree = encode (thd.directory :: value_prefix) tree z in
          return tree
      | Tup4 (a, b, c, d), Tup4_s (a_s, b_s, c_s, d_s) ->
          let* tree = encode (a_s.directory :: value_prefix) tree a in
          let* tree = encode (b_s.directory :: value_prefix) tree b in
          let* tree = encode (c_s.directory :: value_prefix) tree c in
          let* tree = encode (d_s.directory :: value_prefix) tree d in
          return tree
      | Tup5 (a, b, c, d, e), Tup5_s (a_s, b_s, c_s, d_s, e_s) ->
          let* tree = encode (a_s.directory :: value_prefix) tree a in
          let* tree = encode (b_s.directory :: value_prefix) tree b in
          let* tree = encode (c_s.directory :: value_prefix) tree c in
          let* tree = encode (d_s.directory :: value_prefix) tree d in
          let* tree = encode (e_s.directory :: value_prefix) tree e in
          return tree
    in
    encode [] tree

  let find : type a. a value t -> a option Lwt.t =
   fun thunk ->
    let open Lwt_syntax in
    match (!(thunk.value), thunk.schema.descr) with
    | Value x, Value_s (_, _) -> return (Some x)
    | Shallow (Some tree), Value_s (_, decoder) -> (
        let* tree = T.find_tree tree (List.rev thunk.schema.folders) in
        match tree with
        | Some tree ->
            let* x = decoder tree in
            (thunk.value :=
               match x with Some x -> Value x | None -> Shallow None) ;
            return x
        | None -> return None)
    | Shallow None, Value_s (_, _decoder) -> return None

  let get thunk =
    let open Lwt_syntax in
    let* x = find thunk in
    match x with
    | Some x -> return x
    | None -> raise (Invalid_argument "get: missing value")

  let set : type a. a value t -> a -> unit =
   fun thunk x ->
    match !(thunk.value) with Value _ | Shallow _ -> thunk.value := Value x

  let cut : type a. a t -> unit = fun thunk -> thunk.value := Shallow None

  type ('a, 'b) lens = 'a thunk -> 'b thunk Lwt.t

  let ( ^. ) : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens =
   fun l1 l2 thunk ->
    let open Lwt_syntax in
    let* thunk = l1 thunk in
    l2 thunk

  let tup2_0 : type a b. (a * b, a) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup2 (x, _y), _ -> return x
    | Shallow (Some tree), Tup2_s (x, y) ->
        let* tree_x = T.find_tree tree (prefix x.directory) in
        let* tree_y = T.find_tree tree (prefix y.directory) in
        let x = shallow_t tree_x x.schema in
        let y = shallow_t tree_y y.schema in
        thunk.value := Tup2 (x, y) ;
        return @@ x
    | Shallow None, Tup2_s (x, y) ->
        let x = shallow_t None x.schema in
        let y = shallow_t None y.schema in
        thunk.value := Tup2 (x, y) ;
        return @@ x

  let tup2_1 : type a b. (a * b, b) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup2 (_x, y), _ -> return y
    | Shallow (Some tree), Tup2_s (x, y) ->
        let* tree_x = T.find_tree tree (prefix x.directory) in
        let* tree_y = T.find_tree tree (prefix y.directory) in
        let x = shallow_t tree_x x.schema in
        let y = shallow_t tree_y y.schema in
        thunk.value := Tup2 (x, y) ;
        return y
    | Shallow None, Tup2_s (x, y) ->
        let x = shallow_t None x.schema in
        let y = shallow_t None y.schema in
        thunk.value := Tup2 (x, y) ;
        return y

  let tup3_0 : type a b c. (a * b * c, a) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup3 (x, _y, _z), _ -> return x
    | Shallow (Some tree), Tup3_s (x, y, z) ->
        let* tree_x = T.find_tree tree (prefix x.directory) in
        let* tree_y = T.find_tree tree (prefix y.directory) in
        let* tree_z = T.find_tree tree (prefix z.directory) in
        let x = shallow_t tree_x x.schema in
        let y = shallow_t tree_y y.schema in
        let z = shallow_t tree_z z.schema in
        thunk.value := Tup3 (x, y, z) ;
        return x
    | Shallow None, Tup3_s (x, y, z) ->
        let x = shallow_t None x.schema in
        let y = shallow_t None y.schema in
        let z = shallow_t None z.schema in
        thunk.value := Tup3 (x, y, z) ;
        return x

  let tup3_1 : type a b c. (a * b * c, b) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup3 (_x, y, _z), _ -> return y
    | Shallow (Some tree), Tup3_s (x, y, z) ->
        let* tree_x = T.find_tree tree (prefix x.directory) in
        let* tree_y = T.find_tree tree (prefix y.directory) in
        let* tree_z = T.find_tree tree (prefix z.directory) in
        let x = shallow_t tree_x x.schema in
        let y = shallow_t tree_y y.schema in
        let z = shallow_t tree_z z.schema in
        thunk.value := Tup3 (x, y, z) ;
        return y
    | Shallow None, Tup3_s (x, y, z) ->
        let x = shallow_t None x.schema in
        let y = shallow_t None y.schema in
        let z = shallow_t None z.schema in
        thunk.value := Tup3 (x, y, z) ;
        return y

  let tup3_2 : type a b c. (a * b * c, c) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup3 (_x, _y, z), _ -> return z
    | Shallow (Some tree), Tup3_s (x, y, z) ->
        let* tree_x = T.find_tree tree (prefix x.directory) in
        let* tree_y = T.find_tree tree (prefix y.directory) in
        let* tree_z = T.find_tree tree (prefix z.directory) in
        let x = shallow_t tree_x x.schema in
        let y = shallow_t tree_y y.schema in
        let z = shallow_t tree_z z.schema in
        thunk.value := Tup3 (x, y, z) ;
        return z
    | Shallow None, Tup3_s (x, y, z) ->
        let x = shallow_t None x.schema in
        let y = shallow_t None y.schema in
        let z = shallow_t None z.schema in
        thunk.value := Tup3 (x, y, z) ;
        return z

  let tup4_0 : type a b c d. (a * b * c * d, a) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup4 (a, _b, _c, _d), _ -> return a
    | Shallow (Some tree), Tup4_s (a_f, b_f, c_f, d_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return a
    | Shallow None, Tup4_s (a_f, b_f, c_f, d_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return a

  let tup4_1 : type a b c d. (a * b * c * d, b) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup4 (_a, b, _c, _d), _ -> return b
    | Shallow (Some tree), Tup4_s (a_f, b_f, c_f, d_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return b
    | Shallow None, Tup4_s (a_f, b_f, c_f, d_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return b

  let tup4_2 : type a b c d. (a * b * c * d, c) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup4 (_a, _b, c, _d), _ -> return c
    | Shallow (Some tree), Tup4_s (a_f, b_f, c_f, d_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return c
    | Shallow None, Tup4_s (a_f, b_f, c_f, d_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return c

  let tup4_3 : type a b c d. (a * b * c * d, d) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup4 (_a, _b, _c, d), _ -> return d
    | Shallow (Some tree), Tup4_s (a_f, b_f, c_f, d_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return d
    | Shallow None, Tup4_s (a_f, b_f, c_f, d_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        thunk.value := Tup4 (a, b, c, d) ;
        return d

  let tup5_0 : type a b c d e. (a * b * c * d * e, a) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup5 (a, _b, _c, _d, _e), _ -> return a
    | Shallow (Some tree), Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let* tree_e = T.find_tree tree (prefix e_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        let e = shallow_t tree_e e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return a
    | Shallow None, Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        let e = shallow_t None e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return a

  let tup5_1 : type a b c d e. (a * b * c * d * e, b) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup5 (_a, b, _c, _d, _e), _ -> return b
    | Shallow (Some tree), Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let* tree_e = T.find_tree tree (prefix e_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        let e = shallow_t tree_e e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return b
    | Shallow None, Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        let e = shallow_t None e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return b

  let tup5_2 : type a b c d e. (a * b * c * d * e, c) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup5 (_a, _b, c, _d, _e), _ -> return c
    | Shallow (Some tree), Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let* tree_e = T.find_tree tree (prefix e_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        let e = shallow_t tree_e e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return c
    | Shallow None, Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        let e = shallow_t None e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return c

  let tup5_3 : type a b c d e. (a * b * c * d * e, d) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup5 (_a, _b, _c, d, _e), _ -> return d
    | Shallow (Some tree), Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let* tree_e = T.find_tree tree (prefix e_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        let e = shallow_t tree_e e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return d
    | Shallow None, Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        let e = shallow_t None e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return d

  let tup5_4 : type a b c d e. (a * b * c * d * e, e) lens =
   fun thunk ->
    let open Lwt_syntax in
    let prefix dir = List.rev @@ (dir :: thunk.schema.folders) in
    match (!(thunk.value), thunk.schema.descr) with
    | Tup5 (_a, _b, _c, _d, e), _ -> return e
    | Shallow (Some tree), Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let* tree_a = T.find_tree tree (prefix a_f.directory) in
        let* tree_b = T.find_tree tree (prefix b_f.directory) in
        let* tree_c = T.find_tree tree (prefix c_f.directory) in
        let* tree_d = T.find_tree tree (prefix d_f.directory) in
        let* tree_e = T.find_tree tree (prefix e_f.directory) in
        let a = shallow_t tree_a a_f.schema in
        let b = shallow_t tree_b b_f.schema in
        let c = shallow_t tree_c c_f.schema in
        let d = shallow_t tree_d d_f.schema in
        let e = shallow_t tree_e e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return e
    | Shallow None, Tup5_s (a_f, b_f, c_f, d_f, e_f) ->
        let a = shallow_t None a_f.schema in
        let b = shallow_t None b_f.schema in
        let c = shallow_t None c_f.schema in
        let d = shallow_t None d_f.schema in
        let e = shallow_t None e_f.schema in
        thunk.value := Tup5 (a, b, c, d, e) ;
        return e

  let entry : type a b. a -> ((a, b) dict, b) lens =
   fun k thunk ->
    let open Lwt_syntax in
    match (!(thunk.value), thunk.schema.descr) with
    | Dict (Some tree, assoc), Dict_s (encoder, schema) -> (
        match List.assq_opt k assoc with
        | Some v -> return v
        | None ->
            let entry = encoder k in
            let* tree' = T.find_tree tree (entry :: thunk.schema.folders) in
            let v = shallow_t tree' schema in
            thunk.value := Dict (Some tree, (k, v) :: assoc) ;
            return v)
    | Shallow (Some tree), Dict_s (encoder, schema) ->
        let entry = encoder k in
        let* tree' = T.find_tree tree (entry :: thunk.schema.folders) in
        let v = shallow_t tree' schema in
        thunk.value := Dict (Some tree, [(k, v)]) ;
        return v
    | (Dict (None, []) | Shallow None), Dict_s (_encoder, schema) ->
        let v = shallow_t None schema in
        thunk.value := Dict (None, [(k, v)]) ;
        return v
    | Dict (None, assoc), Dict_s (_encoder, schema) -> (
        match List.assq_opt k assoc with
        | Some v -> return v
        | None ->
            let v = shallow_t None schema in
            thunk.value := Dict (None, (k, v) :: assoc) ;
            return v)

  module Lazy_list = struct
    type 'a t = int32 value * (int32, 'a) dict

    let schema : 'a schema -> 'a t schema =
     fun schema ->
      let open Schema in
      obj2
        (req "len" @@ encoding Data_encoding.int32)
        (req "contents" (dict Int32.to_string schema))

    let length : 'a t thunk -> int32 Lwt.t =
     fun thunk ->
      let open Lwt_syntax in
      let* len = tup2_0 thunk in
      let* len = find len in
      return @@ Option.value ~default:0l len

    let nth ~check idx : ('a t, 'a) lens =
     fun thunk ->
      let open Lwt_syntax in
      let* c = length thunk in
      if (not check) || idx < c then
        (tup2_1 ^. entry Int32.(pred @@ sub c idx)) thunk
      else
        raise
          (Invalid_argument
             ("nth: index " ^ Int32.to_string idx ^ " out of bound"))

    let alloc_cons : 'a t thunk -> (int32 * 'a thunk) Lwt.t =
     fun thunk ->
      let open Lwt_syntax in
      let* c = length thunk in
      let* len = tup2_0 thunk in
      let () = set len (Int32.succ c) in
      let* cons = (tup2_1 ^. entry c) thunk in
      return (c, cons)

    let cons : 'a value t thunk -> 'a -> int32 Lwt.t =
     fun thunk x ->
      let open Lwt_syntax in
      let* idx, cell = alloc_cons thunk in
      let () = set cell x in
      return idx
  end

  module Syntax = struct
    let ( ^-> ) x f = f x

    let ( let*^? ) x k =
      let open Lwt_syntax in
      let* x = x in
      let* x = find x in
      k x

    let ( let*^ ) x k =
      let open Lwt_syntax in
      let* x = x in
      let* x = get x in
      k x

    let ( ^:= ) x v =
      let open Lwt_syntax in
      let* x = x in
      set x v ;
      return ()
  end
end
