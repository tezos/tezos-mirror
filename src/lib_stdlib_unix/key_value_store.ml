(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This LRU aims to contains ['value Store.t] which handles the
   persistent storage for this value. [Store.t] allows to keep in
   memory the content of the file. However, because there could be
   numerous values to store, we use an LRU to avoid memory leaks. *)
module LRU =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer
       (Aches.Rache.LRU)
       (struct
         type t = string

         let equal = String.equal

         let hash = Hashtbl.hash
       end))

type ('key, 'value) t =
  | E : {
      file_of : 'key -> 'value Stored_data.file;
          (* Map a [key] to the file containing the corresponding [value]. *)
      read_lru : ('value Stored_data.t, tztrace) LRU.t;
      (* LRU which keeps in memory the last [values] stored for reads. *)
      write_lru : ('value Stored_data.t, tztrace) LRU.t;
      (* LRU which keeps in memory the last [values] stored for writes. *)
      pool : unit Lwt_pool.t option;
          (* An optional pool to ensure an upper bound of the number
             of file descriptors opened by this store. *)
    }
      -> ('key, 'value) t

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4643

   The reason why there are two LRUs and not one, is that in the case
   of concurrent reads and writes, the LRU cannot prevent the absence
   of race. To prevent that we use two LRUs to be able to discriminate
   between the various concurrent accesses. In particular, while
   reading a value, we want to wait if there is a write in
   progress. Vice versa, if a read fails, we don't want to make the
   next write to fail.

   In practice, there should not be a duplication in memory of the
   values read since values are shared. *)

let with_pool pool f =
  match pool with None -> f () | Some pool -> Lwt_pool.use pool f

let init ?pool ~lru_size file_of =
  let write_lru = LRU.create lru_size in
  let read_lru = LRU.create lru_size in
  E {file_of; write_lru; read_lru; pool}

let read_value (E {write_lru; read_lru; file_of; pool}) key =
  let open Lwt_result_syntax in
  let file = file_of key in
  match LRU.take write_lru file.path with
  | None ->
      LRU.bind_or_put
        read_lru
        file.path
        (fun _path -> with_pool pool (fun () -> Stored_data.load file))
        (function
          | Error err -> fail err
          | Ok store ->
              let*! value = Stored_data.get store in
              Lwt.return_ok value)
  | Some store_promise ->
      LRU.put write_lru file.path store_promise ;
      let* store = store_promise in
      let*! value = Stored_data.get store in
      LRU.put read_lru file.path (return store) ;
      Lwt.return_ok value

let write_value (type value) ?(override = false)
    (E {write_lru; read_lru; file_of; pool}) key (value : value) =
  let open Lwt_result_syntax in
  let file = file_of key in
  let write () =
    LRU.bind_or_put
      write_lru
      file.path
      (fun _path ->
        with_pool pool (fun () ->
            let open Lwt_utils_unix in
            let*! () = create_dir (Filename.dirname file.path) in
            Stored_data.init file ~initial_data:value))
      (function
        | Error err -> fail err
        | Ok store ->
            LRU.put read_lru file.path (return store) ;
            let* x =
              Stored_data.update_with store (fun (_ : value) ->
                  Lwt.return value)
            in
            return x)
  in
  match LRU.take write_lru file.path with
  | None -> write ()
  | Some store_promise ->
      if override then (
        let new_store_promise =
          (* The code below is quite tricky. Assume the follow
             scenario:
             [W(key=0,override=false,value=0);W(key=0,override=true,value=1);
             R(0)]. All the three operations can be run in parallel,
             but the order is the one given by the list before.

             If we keep the current store, it may be possible that
             R(0) observes the value 0. This is because the LRU does
             not give the guarantee that the `update_with` store will
             be run after the one of the first write. *)
          let* _store = store_promise in
          let* store = Stored_data.load file in
          let* () = Stored_data.update_with store (fun _ -> Lwt.return value) in
          return store
        in
        LRU.put write_lru file.path new_store_promise ;
        let* _store = new_store_promise in
        return_unit)
      else (
        LRU.put write_lru file.path store_promise ;
        let* _store = store_promise in
        return_unit)

let write_values ?override t seq =
  Seq.ES.iter (fun (key, value) -> write_value ?override t key value) seq

let read_values t seq =
  let open Lwt_syntax in
  Seq_s.of_seq seq
  |> Seq_s.S.map (fun key ->
         let* maybe_value = read_value t key in
         return (key, maybe_value))
