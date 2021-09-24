(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

module Non_empty = struct
  type 'a t = {head : 'a; tail : 'a list}

  exception Empty_list

  let of_pair (x, y) = {head = x; tail = [y]}

  let of_list_exn x =
    match x with [] -> raise Empty_list | x :: xs -> {head = x; tail = xs}

  let length : type a. a t -> int = fun {tail; _} -> List.length tail + 1

  let nth_exn {head; tail} m =
    match m with
    | 0 -> head
    | n -> (
        match List.nth_opt tail (n - 1) with
        | Some x -> x
        | None -> raise Empty_list)
end

module Functor = struct
  module type S = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end
end

module Applicative = struct
  module type S = sig
    include Functor.S

    val return : 'a -> 'a t

    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    val product : 'a t -> 'b t -> ('a * 'b) t
  end
end

module Monad = struct
  module type S = sig
    include Applicative.S

    val join : 'a t t -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Syntax (M : S) = struct
    let ( let* ) = M.bind

    let ( let+ ) x f = M.map f x

    let ( and+ ) = M.product

    let ( and* ) = M.product
  end
end

module Identity = struct
  type 'a t = 'a

  let return x = x

  let map f x = f x

  let map2 f x y = f x y

  let join x = x

  let bind x f = f x

  let product x y = (x, y)

  let run x = x
end

module Stateful_gen = struct
  module type S = sig
    type 'a m

    include Monad.S

    val bool : bool t

    val nat_less_than : int -> int t

    val small_int : int t

    val replicate : int -> 'a t -> 'a list t

    val replicate_for_each : 'a list -> 'b t -> ('a * 'b) list t

    val traverse : ('a -> 'b t) -> 'a list -> 'b list t

    val oneof : 'a t Non_empty.t -> 'a t

    val small_list : 'a t -> 'a list t

    val opt : 'a t -> 'a option t

    val char_readable : char t

    val string_readable : string t

    val lift : 'a m -> 'a t

    val to_qcheck_gen : 'a t -> 'a m QCheck.Gen.t
  end

  module Make (F : Monad.S) = struct
    type 'a m = 'a F.t

    type 'a t = Random_pure.t -> 'a F.t

    let lift f _ = f

    let return x _ = F.return x

    let bind m f g =
      let (g1, g2) = Random_pure.split g in
      F.bind (m g1) (fun a -> f a g2)

    let ( let* ) = bind

    let map f x =
      let* a = x in
      return (f a)

    let map2 f x y g =
      let (g1, g2) = Random_pure.split g in
      F.map2 f (x g1) (y g2)

    let join x =
      let* y = x in
      y

    let product x y = map2 (fun x y -> (x, y)) x y

    let ( and+ ) = product

    let ( let+ ) x f = map f x

    let bool g = F.return @@ Random_pure.bool g

    let nat_less_than m g = F.return @@ Random_pure.int g m

    let small_int g = F.return @@ Random_pure.int g 32

    let replicate m f =
      let rec loop n =
        match n with
        | 0 -> return []
        | _ ->
            let+ x = f and+ xs = loop (n - 1) in
            x :: xs
      in
      loop m

    let rec traverse f xs =
      match xs with
      | [] -> return []
      | x :: xs -> map2 (fun x xs -> x :: xs) (f x) (traverse f xs)

    let replicate_for_each xs f =
      traverse
        (fun a ->
          let+ b = f in
          (a, b))
        xs

    let oneof xs =
      let* i = nat_less_than (Non_empty.length xs) in
      Non_empty.nth_exn xs i

    let small_list g =
      let* n = small_int in
      replicate n g

    let opt g =
      oneof (Non_empty.of_list_exn [return None; map (fun x -> Some x) g])

    let char_readable =
      let+ n = nat_less_than 26 in
      char_of_int @@ (65 + n)

    let string_readable =
      let+ l = small_list char_readable in
      String.of_seq (List.to_seq l)

    let to_qcheck_gen g std_random_state =
      g
        (Random_pure.of_seed
           (Stdlib.Random.State.int64 std_random_state Stdlib.Int64.max_int))
  end

  module Default = Make (Identity)
end
