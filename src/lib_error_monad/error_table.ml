(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module type S = sig
  type key

  type 'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val reset : 'a t -> unit

  val find_or_make :
    'a t -> key -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

  val remove : 'a t -> key -> unit

  val find_opt : 'a t -> key -> 'a tzresult Lwt.t option

  val mem : 'a t -> key -> bool

  val iter_s : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_p : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val fold : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  val fold_promises :
    (key -> 'a tzresult Lwt.t -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_resolved : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_keys : (key -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length : 'a t -> int
end

module Make (T : Hashtbl.S) : S with type key = T.key = struct
  type key = T.key

  type 'a t = {table : 'a tzresult Lwt.t T.t; cleaners : unit Lwt.t T.t}

  let create n = {table = T.create n; cleaners = T.create n}

  let clear t =
    T.iter (fun _ cleaner -> Lwt.cancel cleaner) t.cleaners ;
    T.iter (fun _ a -> Lwt.cancel a) t.table ;
    T.clear t.cleaners ;
    T.clear t.table

  let reset t =
    T.iter (fun _ cleaner -> Lwt.cancel cleaner) t.cleaners ;
    T.iter (fun _ a -> Lwt.cancel a) t.table ;
    T.reset t.cleaners ;
    T.reset t.table

  let find_or_make t k i =
    match T.find_opt t.table k with
    | Some a ->
        a
    | None ->
        let p = i () in
        T.add t.table k p ;
        T.add
          t.cleaners
          k
          ( p
          >>= function
          | Ok _ ->
              T.remove t.cleaners k ; Lwt.return_unit
          | Error _ ->
              T.remove t.table k ; T.remove t.cleaners k ; Lwt.return_unit ) ;
        p

  let remove t k =
    (match T.find_opt t.cleaners k with None -> () | Some a -> Lwt.cancel a) ;
    T.remove t.cleaners k ;
    (match T.find_opt t.table k with None -> () | Some a -> Lwt.cancel a) ;
    T.remove t.table k

  let find_opt t k = T.find_opt t.table k

  let mem t k = T.mem t.table k

  let iter_s f t =
    T.fold (fun k a acc -> (k, a) :: acc) t.table []
    |> Lwt_list.iter_s (fun (k, a) ->
           a >>= function Error _ -> Lwt.return_unit | Ok a -> f k a)

  let iter_p f t =
    T.fold (fun k a acc -> (k, a) :: acc) t.table []
    |> Lwt_list.iter_p (fun (k, a) ->
           a >>= function Error _ -> Lwt.return_unit | Ok a -> f k a)

  let fold f t acc =
    T.fold (fun k a acc -> (k, a) :: acc) t.table []
    |> Lwt_list.fold_left_s
         (fun acc (k, a) ->
           a >>= function Error _ -> Lwt.return acc | Ok a -> f k a acc)
         acc

  let fold_promises f t acc = T.fold f t.table acc

  let fold_resolved f t acc =
    T.fold
      (fun k a acc ->
        match Lwt.state a with
        | Lwt.Sleep | Lwt.Fail _ | Lwt.Return (Error _) ->
            acc
        | Lwt.Return (Ok a) ->
            f k a acc)
      t.table
      acc

  let fold_keys f t acc = T.fold (fun k _ acc -> f k acc) t.table acc

  let length t = T.length t.table
end
