(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

type annot = string list

type ('l, 'p) node =
  | Int of 'l * Z.t
  | String of 'l * string
  | Bytes of 'l * Bytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list

type canonical_location = int

let dummy_location = -1

type 'p canonical = Canonical of (canonical_location, 'p) node [@@unboxed]

let location = function
  | Int (loc, _) -> loc
  | String (loc, _) -> loc
  | Bytes (loc, _) -> loc
  | Seq (loc, _) -> loc
  | Prim (loc, _, _, _) -> loc

let annotations = function
  | Int (_, _) -> []
  | String (_, _) -> []
  | Bytes (_, _) -> []
  | Seq (_, _) -> []
  | Prim (_, _, _, annots) -> annots

let root (Canonical expr) = expr

(* We use a defunctionalized CPS implementation. The type below corresponds to that of
   continuations. *)
type ('l, 'p, 'la, 'pa) cont =
  | Seq_cont of 'la * ('l, 'p, 'la, 'pa) list_cont
  | Prim_cont of 'la * 'pa * annot * ('l, 'p, 'la, 'pa) list_cont

and ('l, 'p, 'la, 'pa) list_cont =
  | List_cont of
      ('l, 'p) node list * ('la, 'pa) node list * ('l, 'p, 'la, 'pa) cont
  | Return

let strip_locations (type a b) (root : (a, b) node) : b canonical =
  let id =
    let id = ref (-1) in
    fun () ->
      incr id ;
      !id
  in
  let rec strip_locations l k =
    let id = id () in
    match l with
    | Int (_, v) -> (apply [@tailcall]) k (Int (id, v))
    | String (_, v) -> (apply [@tailcall]) k (String (id, v))
    | Bytes (_, v) -> (apply [@tailcall]) k (Bytes (id, v))
    | Seq (_, seq) ->
        (strip_locations_list [@tailcall]) seq [] (Seq_cont (id, k))
    | Prim (_, name, seq, annots) ->
        (strip_locations_list [@tailcall])
          seq
          []
          (Prim_cont (id, name, annots, k))
  and strip_locations_list ls acc k =
    match ls with
    | [] -> (apply_list [@tailcall]) k (List.rev acc)
    | x :: tl -> (strip_locations [@tailcall]) x (List_cont (tl, acc, k))
  and apply k node =
    match k with
    | List_cont (tl, acc, k) ->
        (strip_locations_list [@tailcall]) tl (node :: acc) k
    | Return -> node
  and apply_list k node_list =
    match k with
    | Seq_cont (id, k) -> (apply [@tailcall]) k (Seq (id, node_list))
    | Prim_cont (id, name, annots, k) ->
        (apply [@tailcall]) k (Prim (id, name, node_list, annots))
  in
  Canonical (strip_locations root Return)

let extract_locations : type l p.
    (l, p) node -> p canonical * (canonical_location * l) list =
 fun root ->
  let id =
    let id = ref (-1) in
    fun () ->
      incr id ;
      !id
  in
  let loc_table = ref [] in
  let rec strip_locations l k =
    let id = id () in
    match l with
    | Int (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        (apply [@tailcall]) k (Int (id, v))
    | String (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        (apply [@tailcall]) k (String (id, v))
    | Bytes (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        (apply [@tailcall]) k (Bytes (id, v))
    | Seq (loc, seq) ->
        loc_table := (id, loc) :: !loc_table ;
        (strip_locations_list [@tailcall]) seq [] (Seq_cont (id, k))
    | Prim (loc, name, seq, annots) ->
        loc_table := (id, loc) :: !loc_table ;
        (strip_locations_list [@tailcall])
          seq
          []
          (Prim_cont (id, name, annots, k))
  and strip_locations_list ls acc k =
    match ls with
    | [] -> (apply_list [@tailcall]) k (List.rev acc)
    | x :: tl -> (strip_locations [@tailcall]) x (List_cont (tl, acc, k))
  and apply k node =
    match k with
    | List_cont (tl, acc, k) ->
        (strip_locations_list [@tailcall]) tl (node :: acc) k
    | Return -> node
  and apply_list k node_list =
    match k with
    | Seq_cont (id, k) -> (apply [@tailcall]) k (Seq (id, node_list))
    | Prim_cont (id, name, annots, k) ->
        (apply [@tailcall]) k (Prim (id, name, node_list, annots))
  in
  let stripped = strip_locations root Return in
  (Canonical stripped, List.rev !loc_table)

let inject_locations : type l p.
    (canonical_location -> l) -> p canonical -> (l, p) node =
 fun lookup (Canonical root) ->
  let rec inject_locations l k =
    match l with
    | Int (loc, v) -> (apply [@tailcall]) k (Int (lookup loc, v))
    | String (loc, v) -> (apply [@tailcall]) k (String (lookup loc, v))
    | Bytes (loc, v) -> (apply [@tailcall]) k (Bytes (lookup loc, v))
    | Seq (loc, seq) ->
        (inject_locations_list [@tailcall]) seq [] (Seq_cont (lookup loc, k))
    | Prim (loc, name, seq, annots) ->
        (inject_locations_list [@tailcall])
          seq
          []
          (Prim_cont (lookup loc, name, annots, k))
  and inject_locations_list ls acc k =
    match ls with
    | [] -> (apply_list [@tailcall]) k (List.rev acc)
    | x :: tl -> (inject_locations [@tailcall]) x (List_cont (tl, acc, k))
  and apply k node =
    match k with
    | List_cont (tl, acc, k) ->
        (inject_locations_list [@tailcall]) tl (node :: acc) k
    | Return -> node
  and apply_list k node_list =
    match k with
    | Seq_cont (id, k) -> (apply [@tailcall]) k (Seq (id, node_list))
    | Prim_cont (id, name, annots, k) ->
        (apply [@tailcall]) k (Prim (id, name, node_list, annots))
  in
  inject_locations root Return

let map : type a b. (a -> b) -> a canonical -> b canonical =
 fun f (Canonical expr) ->
  let rec map_node l k =
    match l with
    | (Int _ | String _ | Bytes _) as node -> (apply [@tailcall]) k node
    | Seq (loc, seq) -> (map_list [@tailcall]) seq [] (Seq_cont (loc, k))
    | Prim (loc, name, seq, annots) ->
        (map_list [@tailcall]) seq [] (Prim_cont (loc, f name, annots, k))
  and map_list ls acc k =
    match ls with
    | [] -> (apply_list [@tailcall]) k (List.rev acc)
    | x :: tl -> (map_node [@tailcall]) x (List_cont (tl, acc, k))
  and apply k node =
    match k with
    | List_cont (tl, acc, k) -> (map_list [@tailcall]) tl (node :: acc) k
    | Return -> node
  and apply_list k node_list =
    match k with
    | Seq_cont (id, k) -> (apply [@tailcall]) k (Seq (id, node_list))
    | Prim_cont (id, name, annots, k) ->
        (apply [@tailcall]) k (Prim (id, name, node_list, annots))
  in
  Canonical (map_node expr Return)

let map_node : type la lb pa pb.
    (la -> lb) -> (pa -> pb) -> (la, pa) node -> (lb, pb) node =
 fun fl fp node ->
  let rec map_node fl fp node k =
    match node with
    | Int (loc, v) -> (apply [@tailcall]) fl fp k (Int (fl loc, v))
    | String (loc, v) -> (apply [@tailcall]) fl fp k (String (fl loc, v))
    | Bytes (loc, v) -> (apply [@tailcall]) fl fp k (Bytes (fl loc, v))
    | Seq (loc, seq) ->
        (map_node_list [@tailcall]) fl fp seq [] (Seq_cont (fl loc, k))
    | Prim (loc, name, seq, annots) ->
        (map_node_list [@tailcall])
          fl
          fp
          seq
          []
          (Prim_cont (fl loc, fp name, annots, k))
  and map_node_list fl fp ls acc k =
    match ls with
    | [] -> (apply_list [@tailcall]) fl fp k (List.rev acc)
    | x :: tl -> (map_node [@tailcall]) fl fp x (List_cont (tl, acc, k))
  and apply fl fp k node =
    match k with
    | List_cont (tl, acc, k) ->
        (map_node_list [@tailcall]) fl fp tl (node :: acc) k
    | Return -> node
  and apply_list fl fp k node_list =
    match k with
    | Seq_cont (id, k) -> (apply [@tailcall]) fl fp k (Seq (id, node_list))
    | Prim_cont (id, name, annots, k) ->
        (apply [@tailcall]) fl fp k (Prim (id, name, node_list, annots))
  in
  (map_node [@tailcall]) fl fp node Return

(** Testing
    -------
    Component:    Micheline
    Invocation:   dune build @src/lib_micheline/runtest
    Subject:      Test preservation of semantics wrt original implementation
*)

let%test_module "semantics_preservation" =
  (module struct
    module Original = struct
      let strip_locations root =
        let id =
          let id = ref (-1) in
          fun () ->
            incr id ;
            !id
        in
        let rec strip_locations l =
          let id = id () in
          match l with
          | Int (_, v) -> Int (id, v)
          | String (_, v) -> String (id, v)
          | Bytes (_, v) -> Bytes (id, v)
          | Seq (_, seq) -> Seq (id, List.map strip_locations seq)
          | Prim (_, name, seq, annots) ->
              Prim (id, name, List.map strip_locations seq, annots)
        in
        Canonical (strip_locations root)

      let extract_locations root =
        let id =
          let id = ref (-1) in
          fun () ->
            incr id ;
            !id
        in
        let loc_table = ref [] in
        let rec strip_locations l =
          let id = id () in
          match l with
          | Int (loc, v) ->
              loc_table := (id, loc) :: !loc_table ;
              Int (id, v)
          | String (loc, v) ->
              loc_table := (id, loc) :: !loc_table ;
              String (id, v)
          | Bytes (loc, v) ->
              loc_table := (id, loc) :: !loc_table ;
              Bytes (id, v)
          | Seq (loc, seq) ->
              loc_table := (id, loc) :: !loc_table ;
              Seq (id, List.map strip_locations seq)
          | Prim (loc, name, seq, annots) ->
              loc_table := (id, loc) :: !loc_table ;
              Prim (id, name, List.map strip_locations seq, annots)
        in
        let stripped = strip_locations root in
        (Canonical stripped, List.rev !loc_table)

      let inject_locations lookup (Canonical root) =
        let rec inject_locations l =
          match l with
          | Int (loc, v) -> Int (lookup loc, v)
          | String (loc, v) -> String (lookup loc, v)
          | Bytes (loc, v) -> Bytes (lookup loc, v)
          | Seq (loc, seq) -> Seq (lookup loc, List.map inject_locations seq)
          | Prim (loc, name, seq, annots) ->
              Prim (lookup loc, name, List.map inject_locations seq, annots)
        in
        inject_locations root

      let map f (Canonical expr) =
        let rec map_node f = function
          | (Int _ | String _ | Bytes _) as node -> node
          | Seq (loc, seq) -> Seq (loc, List.map (map_node f) seq)
          | Prim (loc, name, seq, annots) ->
              Prim (loc, f name, List.map (map_node f) seq, annots)
        in
        Canonical (map_node f expr)

      let rec map_node fl fp = function
        | Int (loc, v) -> Int (fl loc, v)
        | String (loc, v) -> String (fl loc, v)
        | Bytes (loc, v) -> Bytes (fl loc, v)
        | Seq (loc, seq) -> Seq (fl loc, List.map (map_node fl fp) seq)
        | Prim (loc, name, seq, annots) ->
            Prim (fl loc, fp name, List.map (map_node fl fp) seq, annots)
    end

    module Sampler = struct
      (* Sampler copied from [micheline_benchmarks.ml] - lib-micheline cannot depend
         on lib-shell-benchmarks. *)

      type 'a sampler = Random.State.t -> 'a

      type width_function = depth:int -> int sampler

      type node_kind =
        | Int_node
        | String_node
        | Bytes_node
        | Seq_node
        | Prim_node

      (* We skew the distribution towards non-leaf nodes by repeating the
           relevant kinds ;) *)
      let all_kinds = [|Int_node; String_node; Bytes_node; Seq_node; Prim_node|]

      let sample_kind : node_kind sampler =
       fun rng_state ->
        let i = Random.State.int rng_state (Array.length all_kinds) in
        all_kinds.(i)

      let sample_string _ = ""

      let sample_bytes _ = Bytes.empty

      let sample_z _ = Z.zero

      let sample (w : width_function) rng_state =
        let rec sample depth rng_state k =
          match sample_kind rng_state with
          | Int_node -> k (Int (0, sample_z rng_state))
          | String_node -> k (String (0, sample_string rng_state))
          | Bytes_node -> k (Bytes (0, sample_bytes rng_state))
          | Seq_node ->
              let width = w ~depth rng_state in
              sample_list
                depth
                width
                []
                (fun terms -> k (Seq (0, terms)))
                rng_state
          | Prim_node ->
              let width = w ~depth rng_state in
              sample_list
                depth
                width
                []
                (fun terms -> k (Prim (0, (), terms, [])))
                rng_state
        and sample_list depth width acc k rng_state =
          if width < 0 then invalid_arg "sample_list: negative width"
          else if width = 0 then k (List.rev acc)
          else
            sample (depth + 1) rng_state (fun x ->
                sample_list depth (width - 1) (x :: acc) k rng_state)
        in
        sample 0 rng_state (fun x -> x)

      let sample_in_interval min max state =
        if max - min >= 0 then min + Random.State.int state (max - min + 1)
        else invalid_arg "sample_in_interval"

      let reasonable_width_function ~depth rng_state =
        (* Entirely ad-hoc *)
        sample_in_interval 0 (20 / (Bits.numbits depth + 1)) rng_state

      let sample = sample reasonable_width_function
    end

    let rng_state = Random.State.make [|0x1337; 0x533D|]

    let rec sample_and_check_n_times' ~map_term n f g =
      if n <= 0 then ()
      else
        let term = Sampler.sample rng_state in
        let term = map_term term in
        (* Is this a legit use of polymorphic equality? *)
        (* It would be better to print the two terms instead of the
           assert as it would give more information in case of failure *)
        assert (f term = g term) ;
        sample_and_check_n_times' ~map_term (n - 1) f g

    let sample_and_check_n_times_canon n =
      sample_and_check_n_times' ~map_term:strip_locations n

    let sample_and_check_n_times n =
      sample_and_check_n_times' ~map_term:(fun x -> x) n

    let%expect_test "strip_locations" =
      sample_and_check_n_times 1_000 Original.strip_locations strip_locations ;
      [%expect {||}]

    let%expect_test "extract_locations" =
      sample_and_check_n_times
        1_000
        Original.extract_locations
        extract_locations ;
      [%expect {||}]

    let%expect_test "inject_locations" =
      sample_and_check_n_times_canon
        1_000
        (Original.inject_locations (fun i -> i))
        (inject_locations (fun i -> i)) ;
      [%expect {||}]

    let%expect_test "map" =
      sample_and_check_n_times_canon
        1_000
        (Original.map (fun _i -> ()))
        (map (fun _i -> ())) ;
      [%expect {||}]

    let%expect_test "map_node" =
      sample_and_check_n_times
        1_000
        (Original.map_node (fun _i -> ()) (fun _i -> ()))
        (map_node (fun _i -> ()) (fun _i -> ())) ;
      [%expect {||}]
  end)
