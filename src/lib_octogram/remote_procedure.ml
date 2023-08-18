(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Jingoo.Jg_types

type ('a, 'uri) t = ..

type ('a, 'uri) remote_procedure = ('a, 'uri) t

type 'uri packed = Packed : ('a, 'uri) t -> 'uri packed

type ('a, 'b) eq = Eq : ('a, 'a) eq | Neq : ('a, 'b) eq

module type S = sig
  val name : string

  type 'a t

  type r

  val of_remote_procedure : ('a, 'uri) remote_procedure -> 'uri t option

  val to_remote_procedure : 'uri t -> (r, 'uri) remote_procedure

  val unify : ('a, 'uri) remote_procedure -> ('a, r) eq

  val encoding : 'uri Data_encoding.t -> 'uri t Data_encoding.t

  val r_encoding : r Data_encoding.t

  val tvalue_of_r : r -> tvalue

  val expand :
    self:Agent_name.t -> run:(string -> string) -> string t -> Uri.global_uri t

  val resolve :
    self:Agent_name.t -> Uri_resolver.t -> Uri.global_uri t -> Uri.agent_uri t

  val run : Agent_state.t -> Uri.agent_uri t -> r Lwt.t

  val on_completion :
    on_new_service:
      (string ->
      Services_cache.node_kind ->
      Services_cache.service_kind ->
      int ->
      unit) ->
    on_new_metrics_source:(string -> Services_cache.node_kind -> int -> unit) ->
    r ->
    unit
end

module type REGISTERED_PROCEDURE = sig
  include S

  val with_response :
    ('a, 'uri) remote_procedure -> 'a -> (r -> 'b) -> 'b option

  val unscope : ('a, 'uri) remote_procedure -> (('a -> r) * (r -> 'a)) option
end

module SMap = Map.Make (String)

let registered_remote_procedures = ref SMap.empty

let register (module P : S) =
  if SMap.mem P.name !registered_remote_procedures then
    failwith (sf "There is already a job named %s" P.name) ;

  let ((module R : REGISTERED_PROCEDURE) as proc) =
    (module struct
      include P

      let with_response (type a) (proc : (a, 'uri) remote_procedure) (res : a)
          (k : r -> 'b) =
        match unify proc with Eq -> Some (k res) | _ -> None

      let unscope (type a) (proc : (a, 'uri) remote_procedure) :
          ((a -> r) * (r -> a)) option =
        match unify proc with Eq -> Some (Fun.id, Fun.id) | Neq -> None
    end)
  in

  registered_remote_procedures :=
    SMap.add P.name proc !registered_remote_procedures

let packed_encoding uri_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    delayed @@ fun () ->
    union
      (SMap.to_seq !registered_remote_procedures
      |> Seq.map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
             c.mk_case
               P.name
               (obj1 (req P.name (P.encoding uri_encoding)))
               (function Packed proc -> P.of_remote_procedure proc)
               (fun proc -> Packed (P.to_remote_procedure proc)))
      |> List.of_seq))

let response_encoding proc =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           match P.unscope proc with
           | Some (f, g) -> Some (Data_encoding.conv f g P.r_encoding)
           | None -> None)
  in
  match candidate with
  | Some enc -> enc
  | None -> failwith "Unregistered procedure"

let encode_response proc response =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           P.with_response proc response @@ fun r ->
           Helpers.to_json_string P.r_encoding r)
  in
  match candidate with
  | Some str -> str
  | None -> failwith "Unregistered procedure"

let merged_encoding header_encoding uri_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    delayed @@ fun () ->
    union
      (SMap.to_seq !registered_remote_procedures
      |> Seq.map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
             c.mk_case
               P.name
               (merge_objs
                  header_encoding
                  (obj1 (req P.name (P.encoding uri_encoding))))
               (fun (header, Packed proc) ->
                 Option.map
                   (fun proc -> (header, proc))
                   (P.of_remote_procedure proc))
               (fun (header, proc) ->
                 (header, Packed (P.to_remote_procedure proc))))
      |> List.of_seq))

let tvalue_of_response (type a) (proc : (a, 'uri) remote_procedure)
    (response : a) =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           P.with_response proc response P.tvalue_of_r)
  in
  match candidate with
  | Some tval -> tval
  | None -> failwith "Unregistered procedure"

let on_completion ~on_new_service ~on_new_metrics_source proc response =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           P.with_response
             proc
             response
             (P.on_completion ~on_new_service ~on_new_metrics_source))
  in
  match candidate with
  | Some () -> ()
  | None -> failwith "Unregistered procedure"

let expand ~self ~vars ~agent ~re ~item (Packed proc) =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           match P.of_remote_procedure proc with
           | Some p ->
               Some
                 (Packed
                    (P.to_remote_procedure
                       (P.expand
                          ~self
                          ~run:(Template.run ~vars ~agent ~re ~item ~res:Tnull)
                          p)))
           | _ -> None)
  in
  match candidate with
  | Some packed -> packed
  | None -> failwith "Unregistered procedure"

let resolve_global_uris ~self resolver (Packed proc) =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           match P.of_remote_procedure proc with
           | Some p ->
               Some
                 (Packed (P.to_remote_procedure (P.resolve ~self resolver p)))
           | None -> None)
  in
  match candidate with
  | Some proc -> proc
  | None -> failwith "Unregistered procedure"

let run state proc =
  let candidate =
    SMap.to_seq !registered_remote_procedures
    |> Seq.find_map (fun (_, (module P : REGISTERED_PROCEDURE)) ->
           match (P.of_remote_procedure proc, P.unscope proc) with
           | Some proc, Some (_f, g) ->
               Some
                 (let* res = P.run state proc in
                  return (g res))
           | _ -> None)
  in
  match candidate with
  | Some enc -> enc
  | None -> failwith "Unregistered procedure"

let file_agent_uri ~self ~(resolver : Uri_resolver.t) =
  Uri.agent_uri_of_global_uri ~self ~services:(fun agent_name file ->
      resolver ~suffix:file Http_server Http agent_name "http")

let global_uri_of_string ~self ~run str =
  run str |> Uri.global_uri_of_string ~self
