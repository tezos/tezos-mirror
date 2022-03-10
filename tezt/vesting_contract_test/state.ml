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

type ('a, 's) t = 's -> 'a * 's

let get : ('s, 's) t = fun s -> (s, s)

let getf : ('s -> 'a) -> ('a, 's) t = fun f s -> (f s, s)

let put : 's -> (unit, 's) t = fun s _ -> ((), s)

let update : ('s -> 's) -> (unit, 's) t = fun f s -> ((), f s)

let map : ('a -> 'b) -> ('a, 's) t -> ('b, 's) t =
 fun f m s ->
  let (a, s') = m s in
  (f a, s')

let bind : ('a -> ('b, 's) t) -> ('a, 's) t -> ('b, 's) t =
 fun f m s ->
  let (a, s') = m s in
  f a s'

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Monad (State : sig
  type t
end)
(M : MONAD) =
struct
  type nonrec 'a t = State.t -> ('a * State.t) M.t

  let return a s = M.return (a, s)

  let map f m s = M.map (fun (a, s') -> (f a, s')) @@ m s

  let bind m f s = M.bind (m s) @@ fun (a, s') -> f a s'

  let ( >|= ) m f = map f m

  let ( let+ ) m f = map f m

  let ( >>= ) m f = bind m f

  let ( let* ) m f = bind m f

  let lift m s = M.map (fun a -> (a, s)) m

  let get s = M.return (s, s)

  let getf f s = M.return (f s, s)

  let put s _ = M.return ((), s)

  let update f s = M.return ((), f s)

  let run s m = m s

  let eval s m = M.map fst (m s)

  let exec s m = M.map snd (m s)

  let rec list_map : ('a -> 'b t) -> 'a list -> 'b list t =
   fun f l ->
    match l with
    | [] -> return []
    | x :: xs ->
        let* y = f x in
        let* ys = list_map f xs in
        return (y :: ys)

  let iter_int : (int -> unit t) -> int -> unit t =
   fun f i ->
    let rec exec i =
      if i < 0 then return ()
      else
        let* () = f i in
        exec (i - 1)
    in
    exec (i - 1)

  let opt_map : ('a -> 'b t) -> 'a option -> 'b option t =
   fun f -> function None -> return None | Some a -> map Option.some (f a)
end
