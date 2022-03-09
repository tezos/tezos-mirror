(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type 'error trace = 'error Support.Lib.Monad.trace

module type BASE = sig
  val name : string

  type 'a elt

  type 'a_elt t

  val of_list : int list -> int t

  val to_list : int t -> int list

  val pp : Format.formatter -> int t -> unit
end

module type ITER_VANILLA = sig
  type 'a elt

  type 'a t

  val iter : ('a elt -> unit) -> 'a t -> unit
end

module type ITER_SEQUENTIAL = sig
  include ITER_VANILLA

  val iter_e :
    ('a elt -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

  val iter_s : ('a elt -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_es :
    ('a elt -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace) result Lwt.t
end

module type ITER_PARALLEL = sig
  include ITER_SEQUENTIAL

  val iter_p : ('a elt -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_ep :
    ('a elt -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t
end

module type ITERI_VANILLA = sig
  type 'a elt

  type 'a t

  val iteri : (int -> 'a elt -> unit) -> 'a t -> unit
end

module type ITERI_SEQUENTIAL = sig
  include ITERI_VANILLA

  val iteri_e :
    (int -> 'a elt -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

  val iteri_s : (int -> 'a elt -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iteri_es :
    (int -> 'a elt -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace) result Lwt.t
end

module type MAP_VANILLA = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type MAP_SEQUENTIAL = sig
  include MAP_VANILLA

  val map_e : ('a -> ('b, 'trace) result) -> 'a t -> ('b t, 'trace) result

  val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

  val map_es :
    ('a -> ('b, 'trace) result Lwt.t) -> 'a t -> ('b t, 'trace) result Lwt.t
end

module type MAP_PARALLEL = sig
  include MAP_SEQUENTIAL

  val map_p : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

  val map_ep :
    ('a -> ('b, 'error trace) result Lwt.t) ->
    'a t ->
    ('b t, 'error trace) result Lwt.t
end

module type REV_VANILLA = sig
  type 'a t

  val rev : 'a t -> 'a t
end

module type REVMAP_VANILLA = sig
  type 'a t

  val rev_map : ('a -> 'b) -> 'a t -> 'b t
end

module type REVMAP_SEQUENTIAL = sig
  include REVMAP_VANILLA

  val rev_map_e : ('a -> ('b, 'trace) result) -> 'a t -> ('b t, 'trace) result

  val rev_map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

  val rev_map_es :
    ('a -> ('b, 'trace) result Lwt.t) -> 'a t -> ('b t, 'trace) result Lwt.t
end

module type REVMAP_PARALLEL = sig
  include REVMAP_SEQUENTIAL

  val rev_map_p : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

  val rev_map_ep :
    ('a -> ('b, 'error trace) result Lwt.t) ->
    'a t ->
    ('b t, 'error trace) result Lwt.t
end

module type FOLDLEFT_VANILLA = sig
  type 'a elt

  type 'a t

  val fold_left : ('a -> 'b elt -> 'a) -> 'a -> 'b t -> 'a
end

module type FOLDLEFT_SEQUENTIAL = sig
  include FOLDLEFT_VANILLA

  val fold_left_e :
    ('a -> 'b elt -> ('a, 'trace) result) -> 'a -> 'b t -> ('a, 'trace) result

  val fold_left_s : ('a -> 'b elt -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t

  val fold_left_es :
    ('a -> 'b elt -> ('a, 'trace) result Lwt.t) ->
    'a ->
    'b t ->
    ('a, 'trace) result Lwt.t
end

module type FOLDLEFTMAP_VANILLA = sig
  type 'a elt

  type 'a t

  val fold_left_map : ('a -> 'b elt -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c list
end

module type FOLDLEFTMAP_SEQUENTIAL = sig
  include FOLDLEFTMAP_VANILLA

  val fold_left_map_e :
    ('a -> 'b elt -> ('a * 'c, 'trace) result) ->
    'a ->
    'b t ->
    ('a * 'c list, 'trace) result

  val fold_left_map_s :
    ('a -> 'b elt -> ('a * 'c) Lwt.t) -> 'a -> 'b t -> ('a * 'c list) Lwt.t

  val fold_left_map_es :
    ('a -> 'b elt -> ('a * 'c, 'trace) result Lwt.t) ->
    'a ->
    'b t ->
    ('a * 'c list, 'trace) result Lwt.t
end

module type FOLDRIGHT_VANILLA = sig
  type 'a t

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type FOLDRIGHT_SEQUENTIAL = sig
  include FOLDRIGHT_VANILLA

  val fold_right_e :
    ('a -> 'b -> ('b, 'trace) result) -> 'a t -> 'b -> ('b, 'trace) result

  val fold_right_s : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  val fold_right_es :
    ('a -> 'b -> ('b, 'trace) result Lwt.t) ->
    'a t ->
    'b ->
    ('b, 'trace) result Lwt.t
end

(* Fold Out-Of-Order (more precisely, in unspecified order) *)
module type FOLDOOO_VANILLA = sig
  type 'a elt

  type 'a t

  val fold : ('b elt -> 'a -> 'a) -> 'b t -> 'a -> 'a
end

module type FOLDOOO_SEQUENTIAL = sig
  include FOLDOOO_VANILLA

  val fold_e :
    ('b elt -> 'a -> ('a, 'trace) result) -> 'b t -> 'a -> ('a, 'trace) result

  val fold_s : ('b elt -> 'a -> 'a Lwt.t) -> 'b t -> 'a -> 'a Lwt.t

  val fold_es :
    ('b elt -> 'a -> ('a, 'trace) result Lwt.t) ->
    'b t ->
    'a ->
    ('a, 'trace) result Lwt.t
end

module type EXISTFORALL_VANILLA = sig
  type 'a elt

  type 'a t

  val exists : ('a elt -> bool) -> 'a t -> bool

  val for_all : ('a elt -> bool) -> 'a t -> bool
end

module type EXISTFORALL_SEQUENTIAL = sig
  include EXISTFORALL_VANILLA

  val exists_e :
    ('a elt -> (bool, 'trace) result) -> 'a t -> (bool, 'trace) result

  val exists_s : ('a elt -> bool Lwt.t) -> 'a t -> bool Lwt.t

  val exists_es :
    ('a elt -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    (bool, 'trace) result Lwt.t

  val for_all_e :
    ('a elt -> (bool, 'trace) result) -> 'a t -> (bool, 'trace) result

  val for_all_s : ('a elt -> bool Lwt.t) -> 'a t -> bool Lwt.t

  val for_all_es :
    ('a elt -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    (bool, 'trace) result Lwt.t
end

module type EXISTFORALL_PARALLEL = sig
  include EXISTFORALL_SEQUENTIAL

  val exists_p : ('a elt -> bool Lwt.t) -> 'a t -> bool Lwt.t

  val exists_ep :
    ('a elt -> (bool, 'error trace) result Lwt.t) ->
    'a t ->
    (bool, 'error trace) result Lwt.t

  val for_all_p : ('a elt -> bool Lwt.t) -> 'a t -> bool Lwt.t

  val for_all_ep :
    ('a elt -> (bool, 'error trace) result Lwt.t) ->
    'a t ->
    (bool, 'error trace) result Lwt.t
end

module type FILTER_VANILLA = sig
  type 'a elt

  type 'a t

  val filter : ('a -> bool) -> 'a t -> 'a t
end

module type FILTER_EXTRAS = sig
  include FILTER_VANILLA

  val filter_left : ('a, 'b) Either.t t -> 'a t

  val filter_right : ('b, 'a) Either.t t -> 'a t

  val filter_ok : ('a, 'b) result t -> 'a t

  val filter_error : ('b, 'a) result t -> 'a t
end

module type FILTER_SEQUENTIAL = sig
  include FILTER_VANILLA

  val filter_e : ('a -> (bool, 'trace) result) -> 'a t -> ('a t, 'trace) result

  val filter_s : ('a -> bool Lwt.t) -> 'a t -> 'a t Lwt.t

  val filter_es :
    ('a -> (bool, 'trace) result Lwt.t) -> 'a t -> ('a t, 'trace) result Lwt.t
end

module type FILTER_PARALLEL = sig
  type 'a t

  val filter_p : ('a -> bool Lwt.t) -> 'a t -> 'a t Lwt.t

  val filter_ep :
    ('a -> (bool, 'error trace) result Lwt.t) ->
    'a t ->
    ('a t, 'error trace) result Lwt.t
end

module type FILTERI_VANILLA = sig
  type 'a elt

  type 'a t

  val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
end

module type FILTERI_SEQUENTIAL = sig
  include FILTERI_VANILLA

  val filteri_e :
    (int -> 'a -> (bool, 'trace) result) -> 'a t -> ('a t, 'trace) result

  val filteri_s : (int -> 'a -> bool Lwt.t) -> 'a t -> 'a t Lwt.t

  val filteri_es :
    (int -> 'a -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    ('a t, 'trace) result Lwt.t
end

module type FILTERI_PARALLEL = sig
  type 'a t

  val filteri_p : (int -> 'a -> bool Lwt.t) -> 'a t -> 'a t Lwt.t

  val filteri_ep :
    (int -> 'a -> (bool, 'error trace) result Lwt.t) ->
    'a t ->
    ('a t, 'error trace) result Lwt.t
end

module type FILTERMAP_VANILLA = sig
  type 'a t

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
end

module type FILTERMAP_SEQUENTIAL = sig
  include FILTERMAP_VANILLA

  val filter_map_e :
    ('a -> ('b option, 'trace) result) -> 'a t -> ('b t, 'trace) result

  val filter_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b t Lwt.t

  val filter_map_es :
    ('a -> ('b option, 'trace) result Lwt.t) ->
    'a t ->
    ('b t, 'trace) result Lwt.t
end

module type FILTERMAP_PARALLEL = sig
  type 'a t

  val filter_map_p : ('a -> 'b option Lwt.t) -> 'a t -> 'b t Lwt.t

  val filter_map_ep :
    ('a -> ('b option, 'error trace) result Lwt.t) ->
    'a t ->
    ('b t, 'error trace) result Lwt.t
end

module type CONCAT_VANILLA = sig
  type 'a t

  val concat : 'a t t -> 'a t
end

module type CONCATMAP_VANILLA = sig
  type 'a t

  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
end

module type CONCATMAP_SEQUENTIAL = sig
  include CONCATMAP_VANILLA

  val concat_map_s : ('a -> 'b t Lwt.t) -> 'a t -> 'b t Lwt.t

  val concat_map_e :
    ('a -> ('b t, 'error) result) -> 'a t -> ('b t, 'error) result

  val concat_map_es :
    ('a -> ('b t, 'error) result Lwt.t) -> 'a t -> ('b t, 'error) result Lwt.t
end

module type CONCATMAP_PARALLEL = sig
  type 'a t

  val concat_map_p : ('a -> 'b t Lwt.t) -> 'a t -> 'b t Lwt.t

  val concat_map_ep :
    ('a -> ('b t, 'error trace) result Lwt.t) ->
    'a t ->
    ('b t, 'error trace) result Lwt.t
end

module type REV_CONCATMAP_VANILLA = sig
  type 'a t

  val rev_concat_map : ('a -> 'b t) -> 'a t -> 'b t
end

module type REV_CONCATMAP_SEQUENTIAL = sig
  include REV_CONCATMAP_VANILLA

  val rev_concat_map_s : ('a -> 'b t Lwt.t) -> 'a t -> 'b t Lwt.t

  val rev_concat_map_e :
    ('a -> ('b t, 'error) result) -> 'a t -> ('b t, 'error) result

  val rev_concat_map_es :
    ('a -> ('b t, 'error) result Lwt.t) -> 'a t -> ('b t, 'error) result Lwt.t
end

module type FIND_VANILLA = sig
  type 'a t

  val find : ('a -> bool) -> 'a t -> 'a option
end

module type FIND_SEQUENTIAL = sig
  include FIND_VANILLA

  val find_e :
    ('a -> (bool, 'trace) result) -> 'a t -> ('a option, 'trace) result

  val find_s : ('a -> bool Lwt.t) -> 'a t -> 'a option Lwt.t

  val find_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    ('a option, 'trace) result Lwt.t
end

module type FINDMAP_VANILLA = sig
  type 'a t

  val find_map : ('a -> 'b option) -> 'a t -> 'b option
end

module type FINDMAP_SEQUENTIAL = sig
  include FINDMAP_VANILLA

  val find_map_e :
    ('a -> ('b option, 'trace) result) -> 'a t -> ('b option, 'trace) result

  val find_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b option Lwt.t

  val find_map_es :
    ('a -> ('b option, 'trace) result Lwt.t) ->
    'a t ->
    ('b option, 'trace) result Lwt.t
end

module type PARTITION_VANILLA = sig
  type 'a t

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
end

module type PARTITION_EXTRAS = sig
  include PARTITION_VANILLA

  val partition_either : ('a, 'b) Either.t t -> 'a t * 'b t

  val partition_result : ('a, 'b) result t -> 'a t * 'b t
end

module type PARTITION_SEQUENTIAL = sig
  include PARTITION_VANILLA

  val partition_e :
    ('a -> (bool, 'trace) result) -> 'a t -> ('a t * 'a t, 'trace) result

  val partition_s : ('a -> bool Lwt.t) -> 'a t -> ('a t * 'a t) Lwt.t

  val partition_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    ('a t * 'a t, 'trace) result Lwt.t
end

module type PARTITION_PARALLEL = sig
  include PARTITION_SEQUENTIAL

  val partition_p : ('a -> bool Lwt.t) -> 'a t -> ('a t * 'a t) Lwt.t

  val partition_ep :
    ('a -> (bool, 'error trace) result Lwt.t) ->
    'a t ->
    ('a t * 'a t, 'error trace) result Lwt.t
end

module type PARTITIONMAP_VANILLA = sig
  type 'a t

  val partition_map : ('a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
end

module type PARTITIONMAP_SEQUENTIAL = sig
  include PARTITIONMAP_VANILLA

  val partition_map_e :
    ('a -> (('b, 'c) Either.t, 'trace) result) ->
    'a t ->
    ('b t * 'c t, 'trace) result

  val partition_map_s :
    ('a -> ('b, 'c) Either.t Lwt.t) -> 'a t -> ('b t * 'c t) Lwt.t

  val partition_map_es :
    ('a -> (('b, 'c) Either.t, 'trace) result Lwt.t) ->
    'a t ->
    ('b t * 'c t, 'trace) result Lwt.t
end

module type PARTITIONMAP_PARALLEL = sig
  include PARTITIONMAP_SEQUENTIAL

  val partition_map_p :
    ('a -> ('b, 'c) Either.t Lwt.t) -> 'a t -> ('b t * 'c t) Lwt.t

  val partition_map_ep :
    ('a -> (('b, 'c) Either.t, 'error trace) result Lwt.t) ->
    'a t ->
    ('b t * 'c t, 'error trace) result Lwt.t
end

module type COMBINE_VANILLA = sig
  type 'a t

  val combine :
    when_different_lengths:'trace ->
    'a t ->
    'b t ->
    (('a * 'b) t, 'trace) result

  val combine_with_leftovers :
    'a t -> 'b t -> ('a * 'b) t * ('a list, 'b list) Either.t option
end

module type ALLDOUBLE_VANILLA = sig
  type 'a t

  val iter2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> unit) ->
    'a t ->
    'b t ->
    (unit, 'trace) result

  val map2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result

  val rev_map2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result

  val fold_left2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'a) ->
    'a ->
    'b t ->
    'c t ->
    ('a, 'trace) result

  val fold_right2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'c) ->
    'a t ->
    'b t ->
    'c ->
    ('c, 'trace) result

  val for_all2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool) ->
    'a t ->
    'b t ->
    (bool, 'trace) result

  val exists2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool) ->
    'a t ->
    'b t ->
    (bool, 'trace) result
end

module type ALLDOUBLE_SEQENTIAL = sig
  include ALLDOUBLE_VANILLA

  val iter2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> (unit, 'trace) result) ->
    'a t ->
    'b t ->
    (unit, 'trace) result

  val iter2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> unit Lwt.t) ->
    'a t ->
    'b t ->
    (unit, 'trace) result Lwt.t

  val iter2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    (unit, 'trace) result Lwt.t

  val map2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result

  val map2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c Lwt.t) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result Lwt.t

  val map2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result Lwt.t

  val rev_map2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result

  val rev_map2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c Lwt.t) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result Lwt.t

  val rev_map2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    ('c t, 'trace) result Lwt.t

  val fold_left2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('a, 'trace) result) ->
    'a ->
    'b t ->
    'c t ->
    ('a, 'trace) result

  val fold_left2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'a Lwt.t) ->
    'a ->
    'b t ->
    'c t ->
    ('a, 'trace) result Lwt.t

  val fold_left2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('a, 'trace) result Lwt.t) ->
    'a ->
    'b t ->
    'c t ->
    ('a, 'trace) result Lwt.t

  val fold_right2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('c, 'trace) result) ->
    'a t ->
    'b t ->
    'c ->
    ('c, 'trace) result

  val fold_right2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'c Lwt.t) ->
    'a t ->
    'b t ->
    'c ->
    ('c, 'trace) result Lwt.t

  val fold_right2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('c, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    'c ->
    ('c, 'trace) result Lwt.t

  val for_all2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result) ->
    'a t ->
    'b t ->
    (bool, 'trace) result

  val for_all2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool Lwt.t) ->
    'a t ->
    'b t ->
    (bool, 'trace) result Lwt.t

  val for_all2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    (bool, 'trace) result Lwt.t

  val exists2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result) ->
    'a t ->
    'b t ->
    (bool, 'trace) result

  val exists2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool Lwt.t) ->
    'a t ->
    'b t ->
    (bool, 'trace) result Lwt.t

  val exists2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    (bool, 'trace) result Lwt.t
end
