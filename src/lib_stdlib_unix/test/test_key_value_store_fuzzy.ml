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

(* Testing
   -------
   Component:    Key-value store
   Invocation:   dune exec src/lib_stdlib_unix/test/main.exe \
                  -- --file test_key_value_store_fuzzy.ml
   Subject:      Test the key-value store
*)

open Error_monad

(* This test file checks the correctness of the key-value store
   (module [L]) with respect to the interface [S] using a reference
   implementation (see module [R]) which is obviously correct.

   The main property tested is that the implementation agrees with the
   reference implementation in a sequential and concurent
   setting. Because the reference implementation does not do I/Os, the
   property means that the key-value store is consistent with the
   order of the operations (ex: If for a given key, we write the value
   1, and then the value 2, any subsequent read will return the value
   2 for this key). Note that this property is not trivial if the
   writes are processed concurrently and actually is false if the
   function [write_values] is used (this is a reason why we do not
   expose it in the interface of [S]).

   This property is tested on scenarios. In this case, a scenario is
   roughly a list of actions and two consecutive actions can be either
   bound sequentially or in parallel.

   We check that both implementation return similar results on those
   scenarios. *)

module type S = sig
  type ('file, 'key, 'value) t

  val init :
    lru_size:int ->
    ('file -> ('key, 'value) Key_value_store.layout) ->
    ('file, 'key, 'value) t

  val close : ('file, 'key, 'value) t -> unit Lwt.t

  val write_value :
    ?override:bool ->
    ('file, 'key, 'value) t ->
    'file ->
    'key ->
    'value ->
    unit tzresult Lwt.t

  val read_value :
    ('file, 'key, 'value) t -> 'file -> 'key -> 'value tzresult Lwt.t

  val read_values :
    ('file, 'key, 'value) t ->
    ('file * 'key) Seq.t ->
    ('file * 'key * 'value tzresult) Seq_s.t

  val remove_file : ('file, 'key, 'value) t -> 'file -> unit tzresult Lwt.t
end

let value_size = 1

module L : S = Key_value_store

module R : S = struct
  type ('file, 'key, 'value) t = ('file * 'key, 'value) Stdlib.Hashtbl.t

  let init ~lru_size:_ _file = Stdlib.Hashtbl.create 100

  let close _ = Lwt.return_unit

  let write_value ?(override = false) t file key value =
    let open Lwt_result_syntax in
    if override || not (Stdlib.Hashtbl.mem t (file, key)) then (
      Stdlib.Hashtbl.replace t (file, key) value ;
      return_unit)
    else return_unit

  let read_value t file key =
    let key = (file, key) in
    let open Lwt_result_syntax in
    match Stdlib.Hashtbl.find_opt t key with
    | None -> failwith "key not found"
    | Some key -> return key

  let read_values t seq =
    let open Lwt_syntax in
    seq |> Seq_s.of_seq
    |> Seq_s.S.map (fun (file, key) ->
           let* value = read_value t file key in
           Lwt.return (file, key, value))

  let remove_file t file =
    Stdlib.Hashtbl.filter_map_inplace
      (fun (file', _) value -> if file = file' then None else Some value)
      t ;
    Lwt.return (Ok ())
end

module Helpers = struct
  type key = string * int

  let make_file d = Printf.sprintf "file_%d" d

  let key_gen ~number_of_files ~number_of_keys_per_file =
    let open QCheck2.Gen in
    let file_gen = map make_file (int_range 0 (number_of_files - 1)) in
    let key_gen = int_range 0 (number_of_keys_per_file - 1) in
    tup2 file_gen key_gen

  type value = Bytes.t

  type write_payload = {key : key; override : bool; default : bool}

  let write_payload_gen ~number_of_files ~number_of_keys_per_file =
    let open QCheck2.Gen in
    let key_gen = key_gen ~number_of_files ~number_of_keys_per_file in
    let gen = tup3 key_gen bool bool in
    map (fun (key, override, default) -> {key; override; default}) gen

  let pp_write_payload fmt {key = file, key; override; default} =
    Format.fprintf
      fmt
      "[key=%s/%d, override=%b, default=%b]"
      file
      key
      override
      default

  type action =
    | Write_value of write_payload
    | Read_value of key
    | Read_values of key Seq.t
    | Remove_file of string

  let seq_gen ~size_seq value_gen =
    let open QCheck2.Gen in
    let size_gen = pure size_seq in
    map (fun list -> List.to_seq list) (list_size size_gen value_gen)

  let key_seq_gen ~size_seq ~number_of_files ~number_of_keys_per_file =
    let key_gen = key_gen ~number_of_files ~number_of_keys_per_file in
    seq_gen ~size_seq key_gen

  let action_gen ~read_values_seq_size ~number_of_files ~number_of_keys_per_file
      =
    let open QCheck2.Gen in
    let write_value =
      write_payload_gen ~number_of_files ~number_of_keys_per_file
      |> map (fun x -> Write_value x)
    in
    let read_value =
      key_gen ~number_of_files ~number_of_keys_per_file
      |> map (fun x -> Read_value x)
    in
    let remove_file =
      key_gen ~number_of_files ~number_of_keys_per_file
      |> map (fun (file, _) -> Remove_file file)
    in
    let read_values =
      key_seq_gen
        ~size_seq:read_values_seq_size
        ~number_of_files
        ~number_of_keys_per_file
      |> map (fun x -> Read_values x)
    in
    oneof [write_value; read_value; read_values; remove_file]

  let pp_action fmt = function
    | Write_value payload -> Format.fprintf fmt "W%a" pp_write_payload payload
    | Read_value (file, key) -> Format.fprintf fmt "R[key=%s/%d]" file key
    | Read_values keys ->
        let str_keys =
          String.concat
            "; "
            (List.of_seq keys
            |> List.map (fun (file, key) -> Printf.sprintf "key=%s/%d" file key)
            )
        in
        Format.fprintf fmt "R[%s]" str_keys
    | Remove_file file -> Format.fprintf fmt "REMOVE[file=%s]" file

  type bind = Sequential | Parallel

  let bind_gen = QCheck2.Gen.oneofa [|Sequential; Parallel|]

  type parameters = {
    mutable uid : int;
    number_of_files : int;
    number_of_keys_per_file : int;
    read_values_seq_size : int;
    lru_size : int;
    value_size : int; (* in bytes *)
    values : (key, value) Stdlib.Hashtbl.t;
    overwritten : (key, value) Stdlib.Hashtbl.t;
  }

  let keys files_max keys_max =
    Stdlib.List.init files_max (fun file ->
        Stdlib.List.init keys_max (fun key -> (make_file file, key)))
    |> List.flatten |> Array.of_list

  (* Because a scenario creates files onto the disk, we need a way to
     generate unique names. For debugging purpose, and because of the
     shrinking of QCheck2, it is easier to track tries with a simple
     counter. *)
  let cpt = ref 0

  let parameters_gen =
    let open QCheck2.Gen in
    (* A small set of different values is enough to get interesting
       scenarios. *)
    let files_max = 3 in
    let keys_max = 4 in
    let number_of_files = pure files_max in
    let number_of_keys_per_file = pure files_max in
    let key_max = files_max * keys_max in
    let read_values_seq_size = int_range 1 key_max in
    let lru_size =
      let+ number_of_files in
      max 0 @@ (number_of_files - 2)
    in
    let char =
      int_range (Char.code 'a') (Char.code 'z') |> map (fun x -> Char.chr x)
    in
    let uid = pure 0 in
    let keys = keys files_max keys_max in
    let values =
      array_repeat key_max (bytes_size ~gen:char (return value_size))
      |> map (fun array ->
             Array.combine keys array |> Array.to_seq |> Stdlib.Hashtbl.of_seq)
    in
    (* same generator *)
    let overwritten = values in
    let tup_gen =
      tup8
        uid
        number_of_files
        number_of_keys_per_file
        read_values_seq_size
        lru_size
        (return value_size)
        values
        overwritten
    in
    map
      (fun ( uid,
             number_of_files,
             number_of_keys_per_file,
             read_values_seq_size,
             lru_size,
             value_size,
             values,
             overwritten ) ->
        {
          uid;
          number_of_files;
          number_of_keys_per_file;
          read_values_seq_size;
          lru_size;
          value_size;
          values;
          overwritten;
        })
      tup_gen

  let pp_parameters fmt
      {
        uid;
        number_of_files;
        number_of_keys_per_file;
        read_values_seq_size;
        lru_size;
        value_size;
        values;
        overwritten;
      } =
    let string_of_values values =
      values |> Stdlib.Hashtbl.to_seq |> List.of_seq
      |> List.map (fun ((file, key), value) ->
             Format.asprintf
               "[key=%s/%d,value=%s]"
               file
               key
               (Bytes.to_string value))
      |> String.concat " "
    in
    Format.fprintf fmt "UID = %d@." uid ;
    Format.fprintf fmt "number of files = %d@." number_of_files ;
    Format.fprintf fmt "number of keys per file = %d@." number_of_keys_per_file ;
    Format.fprintf fmt "sequence length for reads  = %d@." read_values_seq_size ;
    Format.fprintf fmt "lru size = %d@." lru_size ;
    Format.fprintf fmt "value size = %d (in bytes)@." value_size ;
    Format.fprintf fmt "default values = %s@." (string_of_values values) ;
    Format.fprintf fmt "override values = %s@." (string_of_values overwritten)

  (* A scenario is a list of actions. The bind elements tells whether
     the next bind waits for the previous promises running or is done
     in parallel. This datatype does not allow to bind sequentially a
     group of parallel actions though. *)
  type scenario = action * (bind * action) list

  (* [No_concurrency] means we never run two concurrent actions. *)
  type test_profile = No_concurrency | Concurrency

  let scenario_gen profile
      {read_values_seq_size; number_of_files; number_of_keys_per_file; _} :
      scenario QCheck2.Gen.t =
    let open QCheck2.Gen in
    let action_gen =
      action_gen ~read_values_seq_size ~number_of_files ~number_of_keys_per_file
    in
    let first_action = action_gen in
    let bind_gen =
      match profile with
      | No_concurrency -> pure Sequential
      | Concurrency -> bind_gen
    in
    let action_bind = tup2 bind_gen action_gen in
    tup2 first_action (list_repeat 2 action_bind)

  let pp_scenario fmt (action, next_actions) =
    let rec pp shift action fmt next_actions =
      let shift_str = "|| " in
      if shift then Format.fprintf fmt "%s " shift_str ;
      match next_actions with
      | [] ->
          Format.fprintf fmt "%a@." pp_action action ;
          if shift then Format.fprintf fmt "Wait"
      | (Parallel, next_action) :: actions ->
          if shift then
            Format.fprintf
              fmt
              "%a@.%a"
              pp_action
              action
              (pp true next_action)
              actions
          else
            Format.fprintf
              fmt
              "%a@.Wait@.%a"
              pp_action
              action
              (pp true next_action)
              actions
      | (Sequential, next_action) :: actions ->
          if shift then
            Format.fprintf
              fmt
              "Wait@.%a@.%a@."
              pp_action
              action
              (pp false next_action)
              actions
          else
            Format.fprintf
              fmt
              "%a@.%a@."
              pp_action
              action
              (pp false next_action)
              actions
    in
    Format.fprintf fmt "%a" (pp false action) next_actions
end

include Helpers

let run_scenario
    ({
       lru_size;
       values;
       overwritten;
       number_of_files;
       number_of_keys_per_file;
       _;
     } as t) scenario =
  let open Lwt_result_syntax in
  incr cpt ;
  if t.uid = 0 then t.uid <- !cpt ;
  let pid = Unix.getpid () in
  let tmp_dir = Filename.get_temp_dir_name () in
  (* To avoid any conflict with previous runs of this test. *)
  let dir_path =
    Format.asprintf "key-value-store-test-key-%d-%d" pid t.uid
    |> Filename.concat "tezos-pbt-tests"
    |> Filename.concat tmp_dir
  in
  let*! () = Lwt_utils_unix.create_dir dir_path in
  let layout_of dir =
    let filepath = Filename.concat dir_path dir in
    Key_value_store.layout
      ~encoding:(Data_encoding.Fixed.bytes value_size)
      ~filepath
      ~eq:( = )
      ~index_of:Fun.id
      ()
  in
  let left = L.init ~lru_size layout_of in
  let right = R.init ~lru_size layout_of in
  let action, next_actions = scenario in
  let n = ref 0 in
  let compare_result ~finalization (file, key) left_result right_result =
    let finalization = if finalization then "(finalization) " else "" in
    match (left_result, right_result) with
    | Ok left_value, Ok right_value ->
        if left_value = right_value then return_unit
        else
          failwith
            "%s Unexpected different value while reading key %s/%d.@.For run \
             %d at %s:@.Expected: %s@.Got: %s@."
            finalization
            file
            key
            !n
            dir_path
            (Bytes.to_string right_value)
            (Bytes.to_string left_value)
    | Error _, Error _ -> return_unit
    | Ok value, Error err ->
        failwith
          "%s Unexpected different result while reading key %s/%d.@. For run \
           %d at %s:@.Expected: %a@.Got: %s"
          finalization
          file
          key
          !n
          dir_path
          Error_monad.pp_print_trace
          err
          (Bytes.to_string value)
    | Error err, Ok value ->
        failwith
          "%s Unexpected different result while reading key %s/%d.@. For run \
           %d at %s:@.Expected: %s@.Got: %a"
          finalization
          file
          key
          !n
          dir_path
          (Bytes.to_string value)
          Error_monad.pp_print_trace
          err
  in
  let rec run_actions action next_actions promises_running_seq =
    incr n ;
    let value_of_key ~default file key =
      let key = (file, key) in
      let table = if default then values else overwritten in
      Stdlib.Hashtbl.find table key
    in
    let promise =
      match action with
      | Write_value {override; default; key = file, key} ->
          let value = value_of_key ~default file key in
          let left_promise =
            let* r = L.write_value ~override left file key value in
            return r
          in
          let right_promise = R.write_value ~override right file key value in
          tzjoin [left_promise; right_promise]
      | Read_value (file, key) ->
          let left_promise = L.read_value left file key in
          let right_promise = R.read_value right file key in
          let*! left_result = left_promise in
          let*! right_result = right_promise in
          compare_result
            ~finalization:false
            (file, key)
            left_result
            right_result
      | Read_values seq ->
          let left_promise =
            let seq_s = L.read_values left seq in
            Seq_s.E.iter (fun _ -> Ok ()) seq_s
          in
          let right_promise =
            let seq_s = R.read_values right seq in
            Seq_s.E.iter (fun _ -> Ok ()) seq_s
          in
          tzjoin [left_promise; right_promise]
      | Remove_file file ->
          let left_promise = L.remove_file left file in

          let right_promise = R.remove_file right file in

          tzjoin [left_promise; right_promise]
    in
    let finalize () =
      let left = L.init ~lru_size:number_of_files layout_of in
      let* () =
        Seq.ES.iter
          (fun (file, key) ->
            let*! left_result = L.read_value left file key in
            let*! right_result = R.read_value right file key in
            compare_result
              ~finalization:true
              (file, key)
              left_result
              right_result)
          (keys number_of_files number_of_keys_per_file |> Array.to_seq)
      in
      L.close left |> Lwt.map Result.ok
    in
    match next_actions with
    | [] ->
        let* () = promise in
        let* () =
          Seq_s.ES.iter
            (function Ok () -> return_unit | Error err -> fail err)
            promises_running_seq
        in
        let* () = finalize () in
        return (left, right)
    | (Sequential, action) :: next_actions ->
        let* () = promise in
        let* () =
          Seq_s.ES.iter
            (function Ok () -> return_unit | Error err -> fail err)
            promises_running_seq
        in
        run_actions action next_actions Seq_s.empty
    | (Parallel, action) :: next_actions ->
        (* We do not wait for promises to end and append them to the
           list of promises on-going. *)
        let promises_running_seq = Seq_s.cons_s promise promises_running_seq in
        run_actions action next_actions promises_running_seq
  in
  let* result = run_actions action next_actions Seq_s.empty in
  let*! () = L.close left in
  return result

let print (parameters, scenario) =
  Format.asprintf
    "@.Parameters:@.%a@.@.Scenario:@.%a@.@."
    pp_parameters
    parameters
    pp_scenario
    scenario

let sequential_test =
  let open Lwt_result_syntax in
  let open QCheck2 in
  let test_gen =
    Gen.bind parameters_gen (fun parameters ->
        Gen.map
          (fun scenario -> (parameters, scenario))
          (scenario_gen No_concurrency parameters))
  in
  Test.make
    ~print
    ~name:"key-value store sequential writes/reads"
    ~count:10_000
    ~max_fail:1 (*to stop shrinking after [max_fail] failures. *)
    ~retries:1
    test_gen
    (fun (parameters, scenario) ->
      let promise =
        let* _ = run_scenario parameters scenario in
        return_true
      in
      match Lwt_main.run promise with
      | Ok _ -> true
      | Error err ->
          QCheck2.Test.fail_reportf "%a@." Error_monad.pp_print_trace err)

let parallel_test =
  let open Lwt_result_syntax in
  let open QCheck2 in
  let test_gen =
    Gen.bind parameters_gen (fun parameters ->
        Gen.map
          (fun scenario -> (parameters, scenario))
          (scenario_gen Concurrency parameters))
  in
  Test.make
    ~print
    ~name:"key-value store concurrent writes/reads"
    ~count:10_000
    ~max_fail:1 (*to stop shrinking after [max_fail] failures. *)
    ~retries:1
    test_gen
    (fun (parameters, scenario) ->
      let promise =
        let* _ = run_scenario parameters scenario in
        return_true
      in
      match Lwt_main.run promise with
      | Ok _ -> true
      | Error err ->
          QCheck2.Test.fail_reportf "%a@." Error_monad.pp_print_trace err)

let () =
  Alcotest.run
    ~__FILE__
    "test-key-value-store-fuzzy"
    [
      ("sequential", [QCheck_alcotest.to_alcotest sequential_test]);
      ("parallel", [QCheck_alcotest.to_alcotest parallel_test]);
    ]
