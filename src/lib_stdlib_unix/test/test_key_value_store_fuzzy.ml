(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
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

(* MUST READ IF YOU PLAN TO MODIFY/DEBUG THIS TEST

   Modifying this test is in general a very frustrating experience.

   The property check against a generated value is by essence flaky
   since it depends on the Lwt scheduler and consequently on the
   underlying system scheduler. This is done on purpose so that we can
   detect an issue without relying on a specific sequence of actions
   from the scheduler perspective. This is not flaky if the scenario
   generated contains only sequential actions. This is why the default
   shrinker of the test always tries to replace any parallel action by
   a sequential one.

   However, it seems that in practice, while running the property
   multiple times on the same generated value, we get the same result
   most of the time.

   When we do not, [tezt-bam] may detect this and you'll get an error
   message saying that some non-determinism was detected. tezt-bam
   does not allow you at the moment to rerun the test specifically on
   this value, but this may be a good feature to add in [tezt-bam]
   (for example running the same example multiple times to observe the
   failure).

   It may also happen that there is a deadlock, in that case it is
   tempting to add a timeout so that the test does not run forever.
   Actually, it does not work either because using [Lwt.pick] will
   also block. It can be an issue for the CI, but in practice, a good
   old Ctrl+C will work. For the CI, there are other timeouts that
   should be sufficient in practice and since deadlocks are not
   supposed to happen, it should not be an issue either.

   If you observe what seems to be a deadlock, then it is a bug in the
   KVS that must be fixed. It is likely related to the strategy used
   by the KVS to ensure the number of opened files is below some limit
   (check the [add_lru] function)

   The default strategy for the generator seems adequate, but probably
   could be optimised. Feel free to use [--aggressive <n>] to get
   better counter-examples. Some comments are left in the file below
   when the shrinking strategy actually matters.

   [tezt-bam] is not smart! So you can assume when it applies that the
   distribution is uniform. If you think some corner-cases are checked
   too often or not enough, this must be handled by hand.

   The PPX associated with [tezt-bam] (the [@deriving gen] attribute)
   does not do smart things either. It will produce the naive
   generator you would write for the given type (it uses the
   underlying "monad", and reads records/ADT from left to right or top
   to bottom). The default behaviour can be tuned with attributes.

   If you observe any error while running this test, you may likely
   need to rely on "hello-debugging" to understand what is going on.

   Please be sure you have run the test in a loop before pushing any
   modification to make sure it is not flaky. No failure after a total
   of 100_000 runs sounds reasonable.
*)

(* This test file checks the correctness of the key-value store
   (module [L]) with respect to the interface [S] using a reference
   implementation (see module [R]) which is obviously correct.

   The main property tested is that the implementation agrees with the
   reference implementation in a sequential and concurrent
   setting. Because the reference implementation does not do I/Os, the
   property means that the key-value store is consistent with the
   order of the operations (ex: If for a given key, we write the value
   1, and then the value 2, any subsequent read will return the value
   2 for this key). Note that this property is not trivial if the
   writes are processed concurrently and actually is false if the
   function [write_values] is used (this is a reason why we do not
   expose it in the interface of [S]).

   This property is tested on random scenarios, where a scenario is roughly a
   list of actions and two consecutive actions can be bound either sequentially
   or in parallel.

   We check that both implementations return the same results on the generated
   scenarios. *)

module type S = sig
  type ('file, 'key, 'value) t

  val init :
    lru_size:int -> root_dir:string -> ('file, 'key, 'value) t tzresult Lwt.t

  val close : ('file, 'key, 'value) t -> unit tzresult Lwt.t

  val write_value :
    ?override:bool ->
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) Key_value_store.file_layout ->
    'file ->
    'key ->
    'value ->
    unit tzresult Lwt.t

  val read_value :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) Key_value_store.file_layout ->
    'file ->
    'key ->
    'value tzresult Lwt.t

  val read_values :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) Key_value_store.file_layout ->
    ('file * 'key) Seq.t ->
    ('file * 'key * 'value tzresult) Seq_s.t

  val remove_file :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) Key_value_store.file_layout ->
    'file ->
    unit tzresult Lwt.t

  val count_values :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) Key_value_store.file_layout ->
    'file ->
    int tzresult Lwt.t

  module View : sig
    val opened_files : ('file, 'key, 'value) t -> int

    val ongoing_actions : ('file, 'key, 'value) t -> int
  end
end

module L : S = Key_value_store

module R : S = struct
  type ('file, 'key, 'value) t = ('file * 'key, 'value) Stdlib.Hashtbl.t

  let init ~lru_size:_ ~root_dir:_ = Lwt.return_ok @@ Stdlib.Hashtbl.create 100

  let close _ = Lwt_syntax.return_ok_unit

  let write_value ?(override = false) t _file_layout file key value =
    let open Lwt_result_syntax in
    if override || not (Stdlib.Hashtbl.mem t (file, key)) then (
      Stdlib.Hashtbl.replace t (file, key) value ;
      return_unit)
    else return_unit

  let read_value t _file_layout file key =
    let key = (file, key) in
    let open Lwt_result_syntax in
    match Stdlib.Hashtbl.find_opt t key with
    | None -> failwith "key not found"
    | Some key -> return key

  let read_values t file_layout seq =
    let open Lwt_syntax in
    seq |> Seq_s.of_seq
    |> Seq_s.S.map (fun (file, key) ->
           let* value = read_value t file_layout file key in
           Lwt.return (file, key, value))

  let remove_file t _file_layout file =
    Stdlib.Hashtbl.filter_map_inplace
      (fun (file', _) value -> if file = file' then None else Some value)
      t ;
    Lwt.return (Ok ())

  let count_values t _file_layout file =
    Lwt_result_syntax.return
    @@ Stdlib.Hashtbl.fold
         (fun (file', _) _ count -> if file = file' then count + 1 else count)
         t
         0

  module View = struct
    let opened_files table =
      table |> Stdlib.Hashtbl.to_seq_keys |> Seq.map fst |> List.of_seq
      |> List.sort_uniq compare |> List.length

    let ongoing_actions table =
      table |> Stdlib.Hashtbl.to_seq_keys |> Seq.map fst |> List.of_seq
      |> List.sort_uniq compare |> List.length
  end
end

module Helpers = struct
  open Ppx_hash_lib.Std.Hash.Builtin
  open Bam.Std
  open Bam.Std.Syntax

  (* Ideally, we would like to generate those values for a given
     range. However in practice, taking those constants is more than
     enough to stumble across issues that would be raised with higher
     values.

     Anybody with an adventurer spirit can try to change this value.

     Do note that the higher those values are, the less is the
     coverage since the space to explore is higher. *)
  let number_of_files_max = 3

  let key_max = 4

  let value_size = 1

  let make_file id = Printf.sprintf "file_%d" id

  type filename = string [@@deriving hash]

  (* With [tezt-bam], the generator names are codified. They should
     start with [gen_] followed by the name of the type, except if the
     type is called [t] in that case the name is just [gen]. *)
  let gen_filename =
    let* n = int ~min:0 ~max:(number_of_files_max - 1) () in
    return (make_file n)

  type key = filename * int [@@deriving hash]

  let gen_key =
    let* filename = gen_filename in
    let* key = int ~min:0 ~max:(key_max - 1) () in
    return (filename, key)

  type value = String.t

  (* A value of size one is well enough to test reads/writes. One
     could update this test to test the case where the size of a value
     is more than 4KiB. *)
  let gen_value = string ~size:(return 1) ()

  type write_payload = {key : key; override : bool; default : bool}
  [@@deriving gen, hash]

  let pp_write_payload fmt {key = file, key; override; default} =
    Format.fprintf
      fmt
      "[key=%s/%d, override=%b, default=%b]"
      file
      key
      override
      default

  (* The generator associated with this type will have a default
     shrinking where the first constructor value is considered as
     smaller than the second one, ...

     As a consequence, the shrinking strategy will try to replace any
     action by [Count_values] first, then [Read_value], ... *)
  type action =
    | Count_values of filename
    | Read_value of key
    | Write_value of write_payload
    | Remove_file of filename
    | Read_values of key list [@max 5]
  [@@deriving gen, hash]

  let pp_action fmt = function
    | Write_value payload -> Format.fprintf fmt "W%a" pp_write_payload payload
    | Read_value (file, key) -> Format.fprintf fmt "R[key=%s/%d]" file key
    | Read_values keys ->
        let str_keys =
          String.concat
            "; "
            (keys
            |> List.map (fun (file, key) -> Printf.sprintf "key=%s/%d" file key)
            )
        in
        Format.fprintf fmt "R[%s]" str_keys
    | Remove_file file -> Format.fprintf fmt "REMOVE[file=%s]" file
    | Count_values file -> Format.fprintf fmt "COUNT[file=%s]" file

  type bind = Sequential | Parallel [@@deriving gen, hash]

  type parameters = {
    number_of_files : int;
    number_of_keys_per_file : int;
    read_values_seq_size : int;
    lru_size : int;
    value_size : int; (* in bytes *)
    values : (key, value) Stdlib.Hashtbl.t; [@hash.ignore]
    overwritten : (key, value) Stdlib.Hashtbl.t; [@hash.ignore]
  }
  [@@deriving hash]

  let gen_parameters =
    let* number_of_files =
      int ~min:number_of_files_max ~max:number_of_files_max ()
    in
    let* number_of_keys_per_file = int ~min:key_max ~max:key_max () in
    let* read_values_seq_size = int ~min:1 ~max:key_max () in
    let lru_size = max 0 (number_of_files - 2) in
    let all_keys =
      Stdlib.List.init number_of_files (fun file ->
          Stdlib.List.init number_of_keys_per_file (fun key ->
              (make_file file, key)))
      |> List.flatten
    in
    (* The trick here is to declare an internal generator that we will
       use twice just below. *)
    let values_gen =
      let* values = list ~size:(return (List.length all_keys)) gen_value in
      let bindings = Stdlib.List.combine all_keys values in
      return (bindings |> List.to_seq |> Stdlib.Hashtbl.of_seq)
    in
    let* values = values_gen in
    let* overwritten = values_gen in
    return
      {
        number_of_files;
        number_of_keys_per_file;
        read_values_seq_size;
        lru_size;
        value_size;
        values;
        overwritten;
      }

  let keys files_max keys_max =
    Stdlib.List.init (files_max - 1) (fun file ->
        Stdlib.List.init (keys_max - 1) (fun key -> (make_file file, key)))
    |> List.flatten |> Array.of_list

  let pp_parameters fmt
      {
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
             Format.asprintf "[key=%s/%d,value=%s]" file key value)
      |> String.concat " "
    in
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
  type scenario = action * (bind * action) list [@@deriving hash]

  let gen_scenario =
    let open Bam.Std in
    let bind_list =
      (* The default shrinking strategy for list is [Prefix], this
         means the shrinking will always tries to shrink to a prefix
         of the list and reducing values of the list according to the
         shrinking strategy of [value_gen].

         It may not be always a good strategy, but it seems to work
         pretty-well in practice. If you have to debug this test and
         are not satisfied with counter-examples found by the
         tezt-bam, you are invided to play with the various shrinking
         strategies that are implemented with [tezt-bam]. *)
      list
        ~shrinker:Shrinker.Prefix
          (* You can play with those values. I recommend not to
             make it too low, since from past experience, the generator
             may not detect issues that are detected with higher
             values. *)
        ~size:(int ~min:1 ~max:50 ())
        (pair gen_bind gen_action)
    in
    pair gen_action bind_list

  (* This printer is not ideal because it does not reflect well what
     is executed in parallel, what is not. Instead, we just print the
     AST and rely on the reader to know how it will be interpreted. *)
  let pp_scenario fmt (action, next_actions) =
    let rec pp fmt next_actions =
      match next_actions with
      | [] -> ()
      | (Parallel, next_action) :: actions ->
          Format.fprintf fmt "P %a@.%a@." pp_action next_action pp actions
      | (Sequential, next_action) :: actions ->
          Format.fprintf fmt "S %a@.%a@." pp_action next_action pp actions
    in
    Format.fprintf fmt "%a@." pp_action action ;
    Format.fprintf fmt "%a" pp next_actions
end

include Helpers

let run_scenario
    {lru_size; values; overwritten; number_of_files; number_of_keys_per_file; _}
    scenario =
  let open Lwt_result_syntax in
  let pid = Unix.getpid () in
  let tmp_dir = Filename.get_temp_dir_name () in
  let root_dir =
    Format.asprintf "key-value-store-test-key-%d" pid
    |> Filename.concat "tezos-pbt-tests"
    |> Filename.concat tmp_dir
  in
  (* To avoid any conflict with previous runs of this test. *)
  Unix.system @@ Format.asprintf "rm -rf %s" root_dir |> ignore ;
  let file_layout ~root_dir file =
    let filepath = Filename.concat root_dir file in
    Key_value_store.layout
      ~encoding:(Data_encoding.Fixed.bytes value_size)
      ~filepath
      ~eq:( = )
      ~index_of:Fun.id
      ~number_of_keys_per_file:4096
      ()
  in
  let* left = L.init ~lru_size ~root_dir in
  let* right = R.init ~lru_size ~root_dir in
  let action, next_actions = scenario in
  let compare_tzresult finalization pp_while pp_val left_result right_result =
    let pp_result fmt = function
      | Ok v -> pp_val fmt v
      | Error err -> Error_monad.pp_print_trace fmt err
    in
    let fail () =
      failwith
        "%s Unexpected different value while %a.@. At %s:@.Expected: %a@.Got: \
         %a@."
        finalization
        pp_while
        ()
        root_dir
        pp_result
        right_result
        pp_result
        left_result
    in
    match (left_result, right_result) with
    | Ok left_value, Ok right_value ->
        if left_value = right_value then return_unit else fail ()
    | Error _, Error _ -> return_unit
    | Ok _, Error _ | Error _, Ok _ -> fail ()
  in
  let compare_result ~finalization (file, key) left_result right_result =
    let finalization = if finalization then "(finalization) " else "" in
    compare_tzresult
      finalization
      (fun fmt () -> Format.fprintf fmt "reading key %s/%d" file key)
      (fun fmt b -> Format.fprintf fmt "%s" (Bytes.to_string b))
      left_result
      right_result
  in
  let rec run_actions action next_actions promises_running_seq =
    let value_of_key ~default file key =
      let key = (file, key) in
      let table = if default then values else overwritten in
      Stdlib.Hashtbl.find table key
    in
    let promise =
      match action with
      | Write_value {override; default; key = file, key} ->
          let value = value_of_key ~default file key |> Bytes.of_string in
          let left_promise =
            let* r = L.write_value ~override left file_layout file key value in
            return r
          in
          let right_promise =
            R.write_value ~override right file_layout file key value
          in
          tzjoin [left_promise; right_promise]
      | Read_value (file, key) ->
          let left_promise = L.read_value left file_layout file key in
          let right_promise = R.read_value right file_layout file key in
          let*! left_result = left_promise in
          let*! right_result = right_promise in
          compare_result
            ~finalization:false
            (file, key)
            left_result
            right_result
      | Read_values seq ->
          let left_promise =
            let seq_s = L.read_values left file_layout (List.to_seq seq) in
            Seq_s.E.iter (fun _ -> Ok ()) seq_s
          in
          let left_promise =
            let* promise = left_promise in
            return promise
          in
          let right_promise =
            let seq_s = R.read_values right file_layout (List.to_seq seq) in
            Seq_s.E.iter (fun _ -> Ok ()) seq_s
          in
          tzjoin [left_promise; right_promise]
      | Remove_file file ->
          let left_promise = L.remove_file left file_layout file in
          let left_promise =
            let* promise = left_promise in
            return promise
          in
          let right_promise = R.remove_file right file_layout file in

          tzjoin [left_promise; right_promise]
      | Count_values file ->
          let left_promise = L.count_values left file_layout file in
          let left_promise =
            let* promise = left_promise in
            return promise
          in
          let right_promise = R.count_values right file_layout file in
          let*! left_result = left_promise in
          let*! right_result = right_promise in
          compare_tzresult
            ""
            (fun fmt () -> Format.fprintf fmt "counting values in file %s" file)
            (fun fmt -> Format.fprintf fmt "%d")
            left_result
            right_result
    in
    let finalize () =
      let* left = L.init ~lru_size:number_of_files ~root_dir in
      let* () =
        Seq.ES.iter
          (fun (file, key) ->
            let*! left_result = L.read_value left file_layout file key in
            let*! right_result = R.read_value right file_layout file key in
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
        let* _ = finalize () in
        return (left, right)
    | (Sequential, action) :: next_actions ->
        let* () = promise in
        let* () =
          Seq_s.ES.iter
            (function Ok () -> return_unit | Error err -> fail err)
            promises_running_seq
        in
        (* After waiting for all the promises to be executed, the
           number of opened files in theory should not exceed the
           number of files in the LRU. *)
        if L.View.opened_files left <= lru_size then
          if L.View.ongoing_actions left <= lru_size then
            run_actions action next_actions Seq_s.empty
          else
            failwith
              "Expected size of actions table to be at most %d. Got %d \
               (remaining actions: %d)."
              lru_size
              (L.View.ongoing_actions left)
              (List.length next_actions + 1)
        else
          failwith
            "Expected size of files table to be at most %d. Got %d (remaining \
             actions: %d)."
            lru_size
            (L.View.opened_files left)
            (List.length next_actions + 1)
    | (Parallel, action) :: next_actions ->
        (* We do not wait for promises to end and append them to the
           list of promises on-going. *)
        let promises_running_seq = Seq_s.cons_s promise promises_running_seq in
        run_actions action next_actions promises_running_seq
  in
  let* result = run_actions action next_actions Seq_s.empty in
  let*! _ = L.close left in
  return result

let pp fmt (scenario, parameters) =
  Format.fprintf
    fmt
    "@.Parameters:@.%a@.@.Scenario:@.%a@.@."
    pp_parameters
    parameters
    pp_scenario
    scenario

module SS = Set.Make (struct
  type t = scenario * parameters

  let compare = compare
end)

let set = ref SS.empty

module SI = Set.Make (Int)

let count = ref SI.empty

let printed = ref false

let sequential_test =
  let open Tezt_bam in
  let property (scenario, parameters) =
    let promise = run_scenario parameters scenario in
    match Lwt_main.run promise with
    | Ok _ -> Ok ()
    | Error err ->
        Error (`Fail (Format.asprintf "%a" Error_monad.pp_print_trace err))
  in
  Pbt.register
    ~pp
    ~hash:[%hash: scenario * parameters]
    ~stop_after:(`Timeout 20.)
    ~title:"key-value store sequential writes/reads"
    ~__FILE__
    ~tags:["kvs"]
    ~gen:(Bam.Std.pair gen_scenario gen_parameters)
    ~property
    ()
