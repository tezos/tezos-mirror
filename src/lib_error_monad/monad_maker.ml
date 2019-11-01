(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make (Error : Sig.CORE) : Sig.MONAD with type error := Error.error =
struct
  (* INVARIANT: traces are never empty, they must contain at least one error *)

  type trace = Error.error list

  let trace_encoding = Data_encoding.list Error.error_encoding

  let pp_print_error ppf = function
    | [] ->
        assert false
    | [error] ->
        Format.fprintf ppf "@[<v 2>Error:@ %a@]@." Error.pp error
    | error :: _ as errors ->
        Format.fprintf
          ppf
          "@[<v 2>Error:@ %a,@ trace:@ %a@]@."
          Error.pp
          error
          (Format.pp_print_list Error.pp)
          (List.rev errors)

  let classify_errors trace =
    List.fold_left
      (fun r e ->
        match (r, Error.classify_error e) with
        | (`Permanent, _) | (_, `Permanent) ->
            `Permanent
        | (`Branch, _) | (_, `Branch) ->
            `Branch
        | (`Temporary, `Temporary) ->
            `Temporary)
      `Temporary
      trace

  type 'a tzresult = ('a, trace) result

  let result_encoding a_encoding =
    let open Data_encoding in
    let errors_encoding = obj1 (req "error" trace_encoding) in
    let a_encoding = obj1 (req "result" a_encoding) in
    union
      ~tag_size:`Uint8
      [ case
          (Tag 0)
          a_encoding
          ~title:"Ok"
          (function Ok x -> Some x | _ -> None)
          (function res -> Ok res);
        case
          (Tag 1)
          errors_encoding
          ~title:"Error"
          (function Error x -> Some x | _ -> None)
          (function [] -> assert false | _ :: _ as errs -> Error errs) ]

  let ( >>= ) = Lwt.( >>= )

  let return v = Lwt.return_ok v

  let return_unit = Lwt.return (Ok ())

  let return_none = Lwt.return (Ok None)

  let return_some x = Lwt.return (Ok (Some x))

  let return_nil = Lwt.return (Ok [])

  let return_true = Lwt.return (Ok true)

  let return_false = Lwt.return (Ok false)

  let error s = Error [s]

  let ok v = Ok v

  let fail s = Lwt.return_error [s]

  let ( >>? ) v f = match v with Error _ as err -> err | Ok v -> f v

  let ( >>=? ) v f =
    v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

  let ( >>|? ) v f = v >>=? fun v -> Lwt.return_ok (f v)

  let ( >|= ) = Lwt.( >|= )

  let ( >|? ) v f = v >>? fun v -> Ok (f v)

  let rec map_s f l =
    match l with
    | [] ->
        return_nil
    | h :: t ->
        f h >>=? fun rh -> map_s f t >>=? fun rt -> return (rh :: rt)

  let mapi_s f l =
    let rec mapi_s f i l =
      match l with
      | [] ->
          return_nil
      | h :: t ->
          f i h
          >>=? fun rh -> mapi_s f (i + 1) t >>=? fun rt -> return (rh :: rt)
    in
    mapi_s f 0 l

  let rec rev_map_append_s acc f = function
    | [] ->
        return acc
    | hd :: tl ->
        f hd >>=? fun v -> rev_map_append_s (v :: acc) f tl

  let rev_map_s f l = rev_map_append_s [] f l

  let rec map_p f l =
    match l with
    | [] ->
        return_nil
    | x :: l -> (
        let tx = f x and tl = map_p f l in
        tx
        >>= fun x ->
        tl
        >>= fun l ->
        match (x, l) with
        | (Ok x, Ok l) ->
            Lwt.return_ok (x :: l)
        | (Error exn1, Error exn2) ->
            Lwt.return_error (exn1 @ exn2)
        | (Ok _, Error exn) | (Error exn, Ok _) ->
            Lwt.return_error exn )

  let mapi_p f l =
    let rec mapi_p f i l =
      match l with
      | [] ->
          return_nil
      | x :: l -> (
          let tx = f i x and tl = mapi_p f (i + 1) l in
          tx
          >>= fun x ->
          tl
          >>= fun l ->
          match (x, l) with
          | (Ok x, Ok l) ->
              Lwt.return_ok (x :: l)
          | (Error exn1, Error exn2) ->
              Lwt.return_error (exn1 @ exn2)
          | (Ok _, Error exn) | (Error exn, Ok _) ->
              Lwt.return_error exn )
    in
    mapi_p f 0 l

  let rec map2_s f l1 l2 =
    match (l1, l2) with
    | ([], []) ->
        return_nil
    | (_ :: _, []) | ([], _ :: _) ->
        invalid_arg "Error_monad.map2_s"
    | (h1 :: t1, h2 :: t2) ->
        f h1 h2 >>=? fun rh -> map2_s f t1 t2 >>=? fun rt -> return (rh :: rt)

  let mapi2_s f l1 l2 =
    let rec mapi2_s i f l1 l2 =
      match (l1, l2) with
      | ([], []) ->
          return_nil
      | (_ :: _, []) | ([], _ :: _) ->
          invalid_arg "Error_monad.mapi2_s"
      | (h1 :: t1, h2 :: t2) ->
          f i h1 h2
          >>=? fun rh ->
          mapi2_s (i + 1) f t1 t2 >>=? fun rt -> return (rh :: rt)
    in
    mapi2_s 0 f l1 l2

  let rec map2 f l1 l2 =
    match (l1, l2) with
    | ([], []) ->
        Ok []
    | (_ :: _, []) | ([], _ :: _) ->
        invalid_arg "Error_monad.map2"
    | (h1 :: t1, h2 :: t2) ->
        f h1 h2 >>? fun rh -> map2 f t1 t2 >>? fun rt -> Ok (rh :: rt)

  let rec filter_map_s f l =
    match l with
    | [] ->
        return_nil
    | h :: t -> (
        f h
        >>=? function
        | None ->
            filter_map_s f t
        | Some rh ->
            filter_map_s f t >>=? fun rt -> return (rh :: rt) )

  let rec filter_map_p f l =
    match l with
    | [] ->
        return_nil
    | h :: t -> (
        let th = f h and tt = filter_map_p f t in
        th
        >>=? function
        | None -> tt | Some rh -> tt >>=? fun rt -> return (rh :: rt) )

  let rec filter_s f l =
    match l with
    | [] ->
        return_nil
    | h :: t -> (
        f h
        >>=? function
        | false ->
            filter_s f t
        | true ->
            filter_s f t >>=? fun t -> return (h :: t) )

  let rec filter_p f l =
    match l with
    | [] ->
        return_nil
    | h :: t -> (
        let jh = f h and t = filter_p f t in
        jh >>=? function false -> t | true -> t >>=? fun t -> return (h :: t) )

  let rec iter_s f l =
    match l with [] -> return_unit | h :: t -> f h >>=? fun () -> iter_s f t

  let rec iter_p f l =
    match l with
    | [] ->
        return_unit
    | x :: l -> (
        let tx = f x and tl = iter_p f l in
        tx
        >>= fun tx_res ->
        tl
        >>= fun tl_res ->
        match (tx_res, tl_res) with
        | (Ok (), Ok ()) ->
            Lwt.return_ok ()
        | (Error exn1, Error exn2) ->
            Lwt.return_error (exn1 @ exn2)
        | (Ok (), Error exn) | (Error exn, Ok ()) ->
            Lwt.return_error exn )

  let iteri_p f l =
    let rec iteri_p i f l =
      match l with
      | [] ->
          return_unit
      | x :: l -> (
          let tx = f i x and tl = iteri_p (i + 1) f l in
          tx
          >>= fun tx_res ->
          tl
          >>= fun tl_res ->
          match (tx_res, tl_res) with
          | (Ok (), Ok ()) ->
              Lwt.return (Ok ())
          | (Error exn1, Error exn2) ->
              Lwt.return (Error (exn1 @ exn2))
          | (Ok (), Error exn) | (Error exn, Ok ()) ->
              Lwt.return (Error exn) )
    in
    iteri_p 0 f l

  let rec iter2_p f l1 l2 =
    match (l1, l2) with
    | ([], []) ->
        return_unit
    | ([], _) | (_, []) ->
        invalid_arg "Error_monad.iter2_p"
    | (x1 :: l1, x2 :: l2) -> (
        let tx = f x1 x2 and tl = iter2_p f l1 l2 in
        tx
        >>= fun tx_res ->
        tl
        >>= fun tl_res ->
        match (tx_res, tl_res) with
        | (Ok (), Ok ()) ->
            Lwt.return_ok ()
        | (Error exn1, Error exn2) ->
            Lwt.return_error (exn1 @ exn2)
        | (Ok (), Error exn) | (Error exn, Ok ()) ->
            Lwt.return_error exn )

  let iteri2_p f l1 l2 =
    let rec iteri2_p i f l1 l2 =
      match (l1, l2) with
      | ([], []) ->
          return_unit
      | ([], _) | (_, []) ->
          invalid_arg "Error_monad.iteri2_p"
      | (x1 :: l1, x2 :: l2) -> (
          let tx = f i x1 x2 and tl = iteri2_p (i + 1) f l1 l2 in
          tx
          >>= fun tx_res ->
          tl
          >>= fun tl_res ->
          match (tx_res, tl_res) with
          | (Ok (), Ok ()) ->
              Lwt.return_ok ()
          | (Error exn1, Error exn2) ->
              Lwt.return_error (exn1 @ exn2)
          | (Ok (), Error exn) | (Error exn, Ok ()) ->
              Lwt.return_error exn )
    in
    iteri2_p 0 f l1 l2

  let rec fold_left_s f init l =
    match l with
    | [] ->
        return init
    | h :: t ->
        f init h >>=? fun acc -> fold_left_s f acc t

  let rec fold_right_s f l init =
    match l with
    | [] ->
        return init
    | h :: t ->
        fold_right_s f t init >>=? fun acc -> f h acc

  let rec join = function
    | [] ->
        return_unit
    | t :: ts -> (
        t
        >>= function
        | Error _ as err ->
            join ts >>=? fun () -> Lwt.return err
        | Ok () ->
            join ts )

  let record_trace err result =
    match result with Ok _ as res -> res | Error errs -> Error (err :: errs)

  let trace err f =
    f
    >>= function
    | Error errs -> Lwt.return_error (err :: errs) | ok -> Lwt.return ok

  let record_trace_eval mk_err result =
    match result with
    | Ok _ as res ->
        res
    | Error errs ->
        mk_err () >>? fun err -> Error (err :: errs)

  let trace_eval mk_err f =
    f
    >>= function
    | Error errs ->
        mk_err () >>=? fun err -> Lwt.return_error (err :: errs)
    | ok ->
        Lwt.return ok

  let fail_unless cond exn = if cond then return_unit else fail exn

  let fail_when cond exn = if cond then fail exn else return_unit

  let unless cond f = if cond then return_unit else f ()

  let _when cond f = if cond then f () else return_unit

  type Error.error += Assert_error of string * string

  let () =
    Error.register_error_kind
      `Permanent
      ~id:"assertion"
      ~title:"Assertion failure"
      ~description:"A fatal assertion failed"
      ~pp:(fun ppf (loc, msg) ->
        Format.fprintf
          ppf
          "Assert failure (%s)%s"
          loc
          (if msg = "" then "." else ": " ^ msg))
      Data_encoding.(obj2 (req "loc" string) (req "msg" string))
      (function Assert_error (loc, msg) -> Some (loc, msg) | _ -> None)
      (fun (loc, msg) -> Assert_error (loc, msg))

  let _assert b loc fmt =
    if b then Format.ikfprintf (fun _ -> return_unit) Format.str_formatter fmt
    else Format.kasprintf (fun msg -> fail (Assert_error (loc, msg))) fmt
end
