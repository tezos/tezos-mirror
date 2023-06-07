(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module type S = Traced_sigs.Trace.S

module type EXTENDED = sig
  include S

  val pp :
    (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err trace -> unit

  val pp_top :
    (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err trace -> unit

  val fold : ('a -> 'error -> 'a) -> 'a -> 'error trace -> 'a

  val salvage :
    ('error -> 'a option) -> 'error trace -> ('a, 'error trace) result

  val salvage_s :
    ('error -> 'a Lwt.t option) ->
    'error trace ->
    ('a, 'error trace) result Lwt.t

  val salvage_e :
    ('error -> ('a, 'error trace) result option) ->
    'error trace ->
    ('a, 'error trace) result

  val salvage_es :
    ('error -> ('a, 'error trace) result Lwt.t option) ->
    'error trace ->
    ('a, 'error trace) result Lwt.t

  val recover :
    ('error -> 'a option) -> ('error trace -> 'a) -> 'error trace -> 'a

  val recover_s :
    ('error -> 'a Lwt.t option) ->
    ('error trace -> 'a Lwt.t) ->
    'error trace ->
    'a Lwt.t

  val recover_e :
    ('error -> ('a, 'error trace) result option) ->
    ('error trace -> ('a, 'error trace) result) ->
    'error trace ->
    ('a, 'error trace) result

  val recover_es :
    ('error -> ('a, 'error trace) result Lwt.t option) ->
    ('error trace -> ('a, 'error trace) result Lwt.t) ->
    'error trace ->
    ('a, 'error trace) result Lwt.t

  val wrap : ('a -> 'b) -> 'a trace -> 'b trace
end

module SingletonR : EXTENDED = struct
  type 'error trace = 'error

  let make e = e

  let cons e _ = e

  let cons_list e _ = e

  let conp e _ = e

  let conp_list e _ = e

  let pp pp_error fmt e = pp_error fmt e

  let pp_top pp_error fmt e = pp_error fmt e

  let fold f acc e = f acc e

  open Bare_structs.Monad

  let salvage f e = match f e with None -> Error e | Some x -> Ok x

  let salvage_s f e =
    let open Lwt_syntax in
    match f e with
    | None -> return_error e
    | Some x ->
        let* o = x in
        return_ok o

  let salvage_e f e = match f e with None -> Error e | Some x -> x

  let salvage_es f e =
    match f e with None -> Lwt.return (Error e) | Some x -> x

  let recover f g e = match f e with None -> g e | Some x -> x

  let recover_s f g e = recover f g e

  let recover_e f g e = recover f g e

  let recover_es f g e = recover f g e

  let wrap f e = f e
end

module SingletonL : EXTENDED = struct
  type 'error trace = 'error

  let make e = e

  let cons e _ = e

  let cons_list e _ = e

  let conp _ e = e

  let rec conp_list e = function [] -> e | e :: es -> conp_list e es

  let pp pp_error fmt e = pp_error fmt e

  let pp_top pp_error fmt e = pp_error fmt e

  let fold f acc e = f acc e

  open Bare_structs.Monad

  let salvage f e = match f e with None -> Error e | Some x -> Ok x

  let salvage_s f e =
    let open Lwt_syntax in
    match f e with
    | None -> return_error e
    | Some x ->
        let* o = x in
        return_ok o

  let salvage_e f e = match f e with None -> Error e | Some x -> x

  let salvage_es f e =
    match f e with None -> Lwt.return (Error e) | Some x -> x

  let recover f g e = match f e with None -> g e | Some x -> x

  let recover_s f g e = recover f g e

  let recover_e f g e = recover f g e

  let recover_es f g e = recover f g e

  let wrap f e = f e
end

module SingletonND : EXTENDED = struct
  let prng = Random.State.make_self_init ()

  let either a b = if Random.State.bool prng then a else b

  let rec any e = function
    | [] -> e
    | x :: xs -> if Random.State.bool prng then e else any x xs

  type 'error trace = 'error

  let make e = e

  let cons = either

  let cons_list = any

  let conp = either

  let conp_list = any

  let pp pp_error fmt e = pp_error fmt e

  let pp_top pp_error fmt e = pp_error fmt e

  let fold f acc e = f acc e

  open Bare_structs.Monad

  let salvage f e = match f e with None -> Error e | Some x -> Ok x

  let salvage_s f e =
    let open Lwt_syntax in
    match f e with
    | None -> return_error e
    | Some x ->
        let* o = x in
        return_ok o

  let salvage_e f e = match f e with None -> Error e | Some x -> x

  let salvage_es f e =
    match f e with None -> Lwt.return (Error e) | Some x -> x

  let recover f g e = match f e with None -> g e | Some x -> x

  let recover_s f g e = recover f g e

  let recover_e f g e = recover f g e

  let recover_es f g e = recover f g e

  let wrap f e = f e
end

module Flat : EXTENDED = struct
  type 'error trace = 'error list

  let make e = [e]

  let cons e t = e :: t

  let cons_list e es = e :: es

  let conp el er = el @ er

  let conp_list e es = Stdlib.List.flatten (e :: es)

  let pp pp_error fmt t =
    Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_error fmt t

  let pp_top pp_error fmt t =
    let e = Stdlib.List.hd t in
    pp_error fmt e

  let fold f acc e = Stdlib.List.fold_left f acc e

  open Bare_structs.Monad

  let salvage f t =
    let e = Stdlib.List.hd t in
    match f e with None -> Error t | Some x -> Ok x

  let salvage_s f t =
    let open Lwt_syntax in
    let e = Stdlib.List.hd t in
    match f e with
    | Some x ->
        let* o = x in
        return_ok o
    | None -> return_error t

  let salvage_e f t =
    let e = Stdlib.List.hd t in
    match f e with None -> Error t | Some x -> x

  let salvage_es f t =
    let e = Stdlib.List.hd t in
    match f e with None -> Lwt.return (Error t) | Some x -> x

  let recover f g t =
    let e = Stdlib.List.hd t in
    match f e with None -> g t | Some x -> x

  let recover_s f g t = recover f g t

  let recover_e f g t = recover f g t

  let recover_es f g t = recover f g t

  let wrap f t = Stdlib.List.map f t
end

module Full : EXTENDED = struct
  type 'a tree =
    | Par of 'a tree list (* invariant: never empty *)
    | Seq of 'a * 'a tree
    | Singl of 'a

  type 'error trace = 'error tree

  let make e = Singl e

  let cons e t = Seq (e, t)

  let cons_list e es =
    match List.rev es with
    | [] -> Singl e
    | [ee] -> Seq (e, Singl ee)
    | last :: rev_es ->
        Seq (e, List.fold_left (fun acc e -> Seq (e, acc)) (Singl last) rev_es)

  let conp el = function Par er -> Par (el :: er) | _ as er -> Par [el; er]

  let conp_list e es = Par (e :: es)

  (* TODO: use the printbox package instead *)
  let rec pp pp_error fmt = function
    | Par ts ->
        Format.pp_open_vbox fmt 2 ;
        List.iter (pp pp_error fmt) ts ;
        Format.pp_close_box fmt ()
    | Seq (e, t) ->
        pp_error fmt e ;
        Format.pp_force_newline fmt () ;
        pp pp_error fmt t
    | Singl e -> pp_error fmt e

  let rec pp_top pp_error fmt = function
    | Par ts ->
        Format.pp_open_vbox fmt 2 ;
        List.iter (pp_top pp_error fmt) ts ;
        Format.pp_close_box fmt ()
    | Seq (e, _) | Singl e -> pp_error fmt e

  let rec fold f acc = function
    | Par ts -> List.fold_left (fold f) acc ts
    | Seq (e, t) -> fold f (f acc e) t
    | Singl e -> f acc e

  open Bare_structs.Monad

  let pre_salvage f t =
    let rec aux_t = function
      | Par ts -> aux_par ts
      | Seq (e, _) | Singl e -> f e
    and aux_par = function
      | [] -> None
      | t :: ts -> (
          match aux_t t with
          | Some _ as salvaged -> salvaged
          | None -> aux_par ts)
    in
    aux_t t

  let salvage f t =
    match pre_salvage f t with Some x -> Ok x | None -> Error t

  let salvage_s f t =
    let open Lwt_syntax in
    match pre_salvage f t with
    | None -> return_error t
    | Some x ->
        let* o = x in
        return_ok o

  let salvage_e f t = match pre_salvage f t with Some x -> x | None -> Error t

  let salvage_es f t =
    let open Lwt_syntax in
    match pre_salvage f t with None -> return_error t | Some x -> x

  let recover f g t = match pre_salvage f t with Some x -> x | None -> g t

  let recover_s f g t = recover f g t

  let recover_e f g t = recover f g t

  let recover_es f g t = recover f g t

  let rec wrap f = function
    | Par ts -> Par (List.map (wrap f) ts)
    | Seq (e, t) -> Seq (f e, wrap f t)
    | Singl e -> Singl (f e)
end

module Unit : S with type 'error trace = unit = struct
  type 'error trace = unit

  let make _ = ()

  let cons _ () = ()

  let cons_list _ _ = ()

  let conp _ _ = ()

  let conp_list _ _ = ()
end
