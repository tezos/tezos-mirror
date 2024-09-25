(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This module is based on Map.Make (String), with some functions removed or
   added depending on the needs of Plonk modules. This module is used as a data
   structure for polynomials (under all possible forms), commitments or scalars
*)

type +!'a t

include Map.S with type 'a t := 'a t and type key := string

val t : 'a Repr.ty -> 'a t Repr.ty

val of_list : (string * 'a) list -> 'a t

val keys : 'a t -> string list

val values : 'a t -> 'a list

val to_pair : ('a * 'b) t -> 'a t * 'b t

(* End of the functions taken from [Map.Make (String)]. The following functions
   suit our specific needs in Plonk *)

(** [add_unique k v map] adds [k -> v] to [map] & throw an error if [k] is
    already in [map] *)
val add_unique : string -> 'a -> 'a t -> 'a t

(** Return the union of two maps. The keys of the maps have to be disjoint unless
    specifically stated in common_keys. In this case both key's values
    are asserted to be equal, with a given equality function.
    If no equal function is given, the polymorphic equality is used. *)
val union_disjoint :
  ?common_keys_equal_elt:string list * ('a -> 'a -> bool) ->
  'a t ->
  'a t ->
  'a t

(** Applies [union_disjoint] on a list of maps *)
val union_disjoint_list :
  ?common_keys_equal_elt:string list * ('a -> 'a -> bool) -> 'a t list -> 'a t

(** given a list of maps, outputs a single map with the union of all keys and
     containing lists which consist of the concatenation of the data elements
     under the same key (order is preserved) *)
val map_list_to_list_map : 'a t list -> 'a list t

(** [sub_map m1 m2] returns m2 without the keys that do not appear in m1.
    Raises failure if some key of m1 is not in m2 *)
val sub_map : 'a t -> 'b t -> 'b t

(** USE WITH CAUTION : make sure your update function won’t create duplicates
    (only one will be kept) *)
val update_keys : (string -> string) -> 'a t -> 'a t

(** This module is used to handle proof aggregation in Plonk
    The format of keys for aggregation is <circuit~proof~key> *)
module Aggregation : sig
  (** The separator that distinguish the different parts of the key, currently
  ['~'] *)
  val sep : string

  (** applies the input function on the last part of the input string (parts
      are delimited by sep).
      [update_key_name f ("hello" ^ sep ^ "world" ^ sep ^ "!!")]
      returns ["hello" ^ sep ^ "world" ^ sep ^ (f "!!")] *)
  val update_key_name : (string -> string) -> string -> string

  (** [add_prefix ~n ~i ~shift prefix str] return idx^prefix^sep^str
      idx = [i] + [shift] as a string, eventually padded with '0' before to
      allow a numbering until [n] with the same number of caracters
      for instance, [prefix ~n:11 ~i:5 ~shift:1 "hello" "world"] will return
      "06~hello~world"
      [n] is zero by default, this means if no [n] is specified, no idx will be
      added
      [no_sep] is false by default ; if set to true, the separator before the
       string to prefix will be ommitted :
      [prefix ~no_sep:true ~n:11 ~i:5 ~shift:1 "hello" "world"] will return
      "06~helloworld" *)
  val add_prefix :
    ?no_sep:bool -> ?n:int -> ?i:int -> ?shift:int -> string -> string -> string

  (** [build_all_names prefix n k] build the list of all prefixed [k] with
       n proofs : [build_all_names "hello" 11 "world"] will return
       ["hello~00~world" ; "hello~01~world" ; … ; "hello~10~world"] *)
  val build_all_names : string -> int -> string -> string list

  (** adds prefix to each key of str_map ; [i] will be added as a string
      before the prefix
      For instance [prefix_map ~n:3000 ~i:5 ~shift:1 "hello" map] will prefix
      all the keys of [map] with "0006~hello~" *)
  val prefix_map : ?n:int -> ?i:int -> ?shift:int -> string -> 'a t -> 'a t

  (** [Aggregation.of_list ~n ~shift s name l] is the same as
      [of_list @@ List.mapi (fun i x -> Aggregation.add_prefix ~n ~i ~shift s name, x) l] *)
  val of_list : ?n:int -> ?shift:int -> string -> string -> 'a list -> 'a t

  (** ["c1" -> {"a" ; "b"} ; "c2" -> {"a" ; "c"}] becomes
      [{"c1~a" ; "c1~b" ; "c2~a" ; "c2~c"}] with the same values *)
  val smap_of_smap_smap : 'a t t -> 'a t

  (** Converts a map of list of map in a map, by merging each list of map in
      one map, prefixing all keys with their proof index, and then merging all
      the new maps into one prefixing the keys with the outside map’s keys.
      shifts_maps map outside key to pairs of integers.
      'key1' -> (7, 20) means that 20 proofs will be produced for key1 in
      total and we should start from the 8th one, assuming 7 of them were done
      independently. (Note that we may not even finish the whole 20, this
      depends on the map_list length).
      For example, on input:
      {[ 'circuit_foo' -> [ {'a' -> fa0; 'b' -> fb0; 'c' -> fc0};
                           {'a' -> fa1; 'b' -> fb1; 'c' -> fc1} ];
         'circuit_bar' -> [ {'a' -> ga0; 'b' -> gb0; 'c' -> gc0} ]; ]}
      outputs
      {[ 'circuit_foo~0~a' -> fa0
         'circuit_foo~0~b' -> fb0
         'circuit_foo~0~c' -> fc0
         'circuit_foo~1~a' -> fa1
         'circuit_foo~1~b' -> fb1
         'circuit_foo~1~c' -> fc1
         'circuit_bar~0~a' -> ga0
         'circuit_bar~0~b' -> gb0
         'circuit_bar~0~c' -> gc0
      ]}
    *)
  val gather_maps : ?shifts_map:(int * int) t -> 'a t list t -> 'a t

  (** Filter a map keeping the elements whose key corresponds to the given
      circuit name *)
  val filter_by_circuit_name : string -> 'a t -> 'a t

  (** [select_answers_by_circuit circuit_name s_map_map] takes a [circuit_name]
      and a map with the structure:
      {[ 'x' -> { 'circuit_foo~0~a' -> [scalar] ;
                  'circuit_foo~0~b' -> [scalar] ;
                  ...
               }
      ]}
      and filters the keys of the inner map, keeping the elements whose key
      corresponds to the given circuit name. *)
  val select_answers_by_circuit : string -> 'a t t -> 'a t t

  (** [add_map_list_map m1 m2] will merge [m1] & [m2] ; the resulting map will contain the same keys as [m1] ; [m1] & [m2] can be disjoint, if a key is not found in [m2], the resulting map contains the same binding as [m1] for this key  *)
  val add_map_list_map : 'a t list t -> 'a t list t -> 'a t list t
end
