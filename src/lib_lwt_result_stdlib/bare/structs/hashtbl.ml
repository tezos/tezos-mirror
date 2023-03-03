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

let hash = Stdlib.Hashtbl.hash

let seeded_hash = Stdlib.Hashtbl.seeded_hash

let hash_param ~meaningful ~total v =
  Stdlib.Hashtbl.hash_param meaningful total v

let seeded_hash_param ~meaningful ~total seed v =
  Stdlib.Hashtbl.seeded_hash_param meaningful total seed v

module type S = Bare_functor_outputs.Hashtbl.S

module Make (H : Stdlib.Hashtbl.HashedType) : S with type key = H.t = struct
  open Seq
  include Stdlib.Hashtbl.Make (H)

  let iter_e f t = E.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_s f t = S.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_es f t = ES.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_p f t = iter_p (fun (k, v) -> f k v) (to_seq t)

  let iter_ep f t = iter_ep (fun (k, v) -> f k v) (to_seq t)

  let fold_e f t init =
    E.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let fold_s f t init =
    S.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let fold_es f t init =
    ES.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let find = find_opt

  let try_map_inplace f t =
    filter_map_inplace
      (fun k v -> match f k v with Error _ -> None | Ok r -> Some r)
      t
end

module type SeededS = Bare_functor_outputs.Hashtbl.SeededS

module MakeSeeded (H : Stdlib.Hashtbl.SeededHashedType) :
  SeededS with type key = H.t = struct
  open Seq
  include Stdlib.Hashtbl.MakeSeeded (H)

  let iter_e f t = E.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_s f t = S.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_es f t = ES.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_ep f t = iter_ep (fun (k, v) -> f k v) (to_seq t)

  let iter_p f t = iter_p (fun (k, v) -> f k v) (to_seq t)

  let fold_e f t init =
    E.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let fold_s f t init =
    S.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let fold_es f t init =
    ES.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let find = find_opt

  let try_map_inplace f t =
    filter_map_inplace
      (fun k v -> match f k v with Error _ -> None | Ok r -> Some r)
      t
end

module type S_ES = Bare_functor_outputs.Hashtbl.S_ES

module Make_es (H : Stdlib.Hashtbl.HashedType) : S_ES with type key = H.t =
struct
  (* This [_es] overlay on top of Hashtables prevents programmers from shooting
     themselves in the feet with some common errors. Specifically, it prevents
     race-conditions whereby the same key is bound again before the promise it
     is already bound to resolves.

     More details in the interface: {!Bare_functor_outputs.Hashtbl.S_ES}

     To achieve this, the library maintains the following invariant:
     - at any point in time, keys are associated to at most one promise *)

  open Seq
  open Monad
  module T = Stdlib.Hashtbl.Make (H)

  type key = H.t

  type ('a, 'trace) t = ('a, 'trace) result Lwt.t T.t

  let create n = T.create n

  let clear t =
    T.iter (fun _ a -> Lwt.cancel a) t ;
    T.clear t

  let reset t =
    T.iter (fun _ a -> Lwt.cancel a) t ;
    T.reset t

  let find_or_make t k make =
    match T.find_opt t k with
    | Some a -> a
    | None ->
        let p = Lwt.apply make () in
        (match Lwt.state p with
        | Return (Ok _) -> T.add t k p
        | Return (Error _) -> ()
        | Fail _ -> ()
        | Sleep ->
            T.add t k p ;
            Lwt.on_any
              p
              (function Ok _ -> () | Error _ -> T.remove t k)
              (fun _ -> T.remove t k)) ;
        p

  let find t k = T.find_opt t k

  let remove t k =
    (match T.find_opt t k with None -> () | Some a -> Lwt.cancel a) ;
    (* NOTE: we still need to call [T.remove] in case the promise is not
         cancelable (in which case it is not rejected and thus not removed) *)
    T.remove t k

  let mem t k = T.mem t k

  let iter_with_waiting_es f t =
    ES.iter
      (fun (k, p) ->
        let open Lwt_result_syntax in
        Lwt.try_bind
          (fun () -> p)
          (function Error _ -> return_unit | Ok v -> f k v)
          (fun _ -> return_unit))
      (T.to_seq t)

  let fold_with_waiting_es f t init =
    ES.fold_left
      (fun acc (k, p) ->
        let open Lwt_result_syntax in
        Lwt.try_bind
          (fun () -> p)
          (function Error _ -> return acc | Ok v -> f k v acc)
          (fun _ -> return acc))
      init
      (T.to_seq t)

  let fold_keys f t init = T.fold (fun k _ acc -> f k acc) t init

  let fold_promises f t init = T.fold f t init

  let fold_resolved f t init =
    T.fold
      (fun k p acc ->
        match Lwt.state p with
        | Lwt.Return (Ok v) -> f k v acc
        | Lwt.Return (Error _) | Lwt.Fail _ | Lwt.Sleep -> acc)
      t
      init

  let iter_with_waiting_ep f t =
    let open Lwt_result_syntax in
    join
    @@ fold_promises
         (fun k p acc ->
           let promise =
             Lwt.try_bind
               (fun () -> p)
               (function Error _ -> return_unit | Ok v -> f k v)
               (fun _ -> return_unit)
           in
           promise :: acc)
         t
         []

  let length t = T.length t

  let stats t = T.stats t
end
