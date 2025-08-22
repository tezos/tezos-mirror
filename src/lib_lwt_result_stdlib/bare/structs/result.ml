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

type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e

(* constructors as functions, including _s variants *)
let ok x = Ok x

let ok_s x = Lwt.return (Ok x)

let error x = Error x

let error_s x = Lwt.return (Error x)

let value r ~default = match r with Ok v -> v | Error _ -> default

let value_f r ~default = match r with Ok v -> v | Error _ -> default ()

let bind = Stdlib.Result.bind

let bind_s r f =
  match r with Ok v -> f v | Error _ as error -> Lwt.return error

let bind_error r f = match r with Ok _ as ok -> ok | Error e -> f e

let bind_error_s r f =
  match r with Ok _ as ok -> Lwt.return ok | Error e -> f e

let join = function
  | (Error _ as error) | Ok (Error _ as error) -> error
  | Ok (Ok _ as ok) -> ok

let map f = function Ok v -> Ok (f v) | Error _ as error -> error

let map_e f r = bind r f

let map_s f = function
  | Ok v -> Lwt.bind (f v) Lwt.return_ok
  | Error _ as error -> Lwt.return error

let map_es f r = bind_s r f

let map_error f = function Ok _ as ok -> ok | Error e -> Error (f e)

let map_error_e f r = bind_error r f

let map_error_s f = function
  | Ok v -> Lwt.return_ok v
  | Error e -> Lwt.bind (f e) Lwt.return_error

let map_error_es f r = bind_error_s r f

let fold ~ok ~error = function Ok v -> ok v | Error e -> error e

let iter f = function Ok v -> f v | Error _ -> ()

let iter_s f = function Ok v -> f v | Error _ -> Lwt.return_unit

let iter_error f = function Ok _ -> () | Error e -> f e

let iter_error_s f = function Ok _ -> Lwt.return_unit | Error e -> f e

let is_ok = function Ok _ -> true | Error _ -> false

let is_error = function Ok _ -> false | Error _ -> true

let equal ~ok ~error x y =
  match (x, y) with
  | Ok x, Ok y -> ok x y
  | Error x, Error y -> error x y
  | Ok _, Error _ | Error _, Ok _ -> false

let compare ~ok ~error x y =
  match (x, y) with
  | Ok x, Ok y -> ok x y
  | Error x, Error y -> error x y
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1

let to_option = function Ok v -> Some v | Error _ -> None

let of_option ~error = function Some v -> Ok v | None -> Error error

let to_list = function Ok v -> [v] | Error _ -> []

let to_seq = function
  | Ok v -> Stdlib.Seq.return v
  | Error _ -> Stdlib.Seq.empty

let catch ?(catch_only = fun _ -> true) f =
  match f () with
  | v -> Ok v
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then Error e else Lwt.reraise e

let catch_f ?(catch_only = fun _ -> true) f h =
  match f () with
  | v -> Ok v
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then Error (h e) else Lwt.reraise e

let catch_ef ?(catch_only = fun _ -> true) f h =
  match f () with
  | v -> v
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then Error (h e) else Lwt.reraise e

let catch_s ?(catch_only = fun _ -> true) f =
  Lwt.try_bind f Lwt.return_ok (function
    | (Stack_overflow | Out_of_memory) as e -> Lwt.reraise e
    | e -> if catch_only e then Lwt.return_error e else Lwt.reraise e)

let return x = Ok x

let return_unit = Ok ()

let return_none = Ok None

let return_some x = Ok (Some x)

let return_nil = Ok []

let return_true = Ok true

let return_false = Ok false
