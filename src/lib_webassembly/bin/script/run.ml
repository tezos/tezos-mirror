open Lwt.Syntax
module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare
open Script
open Source

(* Errors & Tracing *)

module Script = Error.Make ()

module Abort = Error.Make ()

module Assert = Error.Make ()

module IO = Error.Make ()

exception Abort = Abort.Error

exception Assert = Assert.Error

exception IO = IO.Error

let trace name = if !Flags.trace then print_endline ("-- " ^ name)

let trace_lwt name =
  if !Flags.trace then Lwt_io.printf "-- %s\n" name else Lwt.return_unit

(* File types *)

let binary_ext = "wasm"

let sexpr_ext = "wat"

let script_binary_ext = "bin.wast"

let script_ext = "wast"

let js_ext = "js"

let dispatch_file_ext on_binary on_sexpr on_script_binary on_script on_js file =
  if Filename.check_suffix file binary_ext then on_binary file
  else if Filename.check_suffix file sexpr_ext then on_sexpr file
  else if Filename.check_suffix file script_binary_ext then
    on_script_binary file
  else if Filename.check_suffix file script_ext then on_script file
  else if Filename.check_suffix file js_ext then on_js file
  else raise (Sys_error (file ^ ": unrecognized file type"))

(* Output *)

let create_binary_file file _ get_module =
  let* () = trace_lwt ("Encoding (" ^ file ^ ")...") in
  let* s = Encode.encode (get_module ()) in
  Lwt_io.(
    with_file ~mode:Output file (fun oc ->
        let* () = trace_lwt "Writing..." in
        write oc s))

let create_sexpr_file file _ get_module =
  let* () = trace_lwt ("Writing (" ^ file ^ ")...") in
  Lwt_io.(
    with_file ~mode:Output file (fun oc ->
        Print.module_ oc !Flags.width (get_module ())))

let create_script_file mode file get_script _ =
  let* () = trace_lwt ("Writing (" ^ file ^ ")...") in
  Lwt_io.with_file ~mode:Lwt_io.Output file (fun oc ->
      Print.script oc !Flags.width mode (get_script ()))

let create_js_file file get_script _ =
  let* () = trace_lwt ("Converting (" ^ file ^ ")...") in
  let* js = Js.of_script (get_script ()) in
  Lwt_io.(
    with_file ~mode:Output file (fun oc ->
        let* () = trace_lwt "Writing..." in
        write oc js))

let output_file =
  dispatch_file_ext
    create_binary_file
    create_sexpr_file
    (create_script_file `Binary)
    (create_script_file `Textual)
    create_js_file

let output_stdout get_module =
  let* () = trace_lwt "Printing..." in
  Print.module_ Lwt_io.stdout !Flags.width (get_module ())

(* Input *)

let error at category msg =
  let* () = trace_lwt "Error: " in
  let+ () =
    Lwt_io.eprintf "%s: %s: %s\n%!" (Source.string_of_region at) category msg
  in
  false

let input_from get_script run =
  Lwt.catch
    (fun () ->
      let script = get_script () in
      let* () = trace_lwt "Running..." in
      let+ () = run script in
      true)
    (function
      | Decode.Code (at, msg) -> error at "decoding error" msg
      | Parse.Syntax (at, msg) -> error at "syntax error" msg
      | Valid.Invalid (at, msg) -> error at "invalid module" msg
      | Import.Unknown (at, msg) -> error at "link failure" msg
      | Eval.Link (at, msg) -> error at "link failure" msg
      | Eval.Trap (at, msg) -> error at "runtime trap" msg
      | Eval.Exhaustion (at, msg) -> error at "resource exhaustion" msg
      | Eval.Crash (at, msg) -> error at "runtime crash" msg
      | Encode.Code (at, msg) -> error at "encoding error" msg
      | Script.Error (at, msg) -> error at "script error" msg
      | IO (at, msg) -> error at "i/o error" msg
      | Assert (at, msg) -> error at "assertion failure" msg
      | Abort _ -> Lwt.return_false
      | Lazy_map.UnexpectedAccess ->
          error no_region "unexpected access" "Unexpected access in lazy map"
      | exn -> raise exn)

let input_script start name lexbuf run =
  input_from (fun _ -> Parse.parse name lexbuf start) run

let input_sexpr name lexbuf run =
  input_from
    (fun _ ->
      let var_opt, def = Parse.parse name lexbuf Parse.Module in
      [Module (var_opt, def) @@ no_region])
    run

let input_binary name buf run =
  let open Source in
  input_from
    (fun _ -> [Module (None, Encoded (name, buf) @@ no_region) @@ no_region])
    run

let input_sexpr_file input file run =
  let* () = trace_lwt ("Loading (" ^ file ^ ")...") in
  Lwt_io.with_file ~mode:Lwt_io.Input file (fun ic ->
      let* s = Lwt_io.read ic in
      let lexbuf = Lexing.from_string s in
      let* () = trace_lwt "Parsing..." in
      let success = input file lexbuf run in
      success)

let input_binary_file file run =
  let* () = trace_lwt ("Loading (" ^ file ^ ")...") in
  Lwt_io.(
    with_file ~mode:Input file (fun ic ->
        let* len = length ic in
        let len = Int64.to_int len in
        let buf = Bytes.make len '\x00' in
        let* () = read_into_exactly ic buf 0 len in
        let* () = trace_lwt "Decoding..." in
        input_binary file (Bytes.to_string buf) run))

let input_js_file file run =
  raise (Sys_error (file ^ ": unrecognized input file type"))

let input_file file run =
  dispatch_file_ext
    input_binary_file
    (input_sexpr_file input_sexpr)
    (input_sexpr_file (input_script Parse.Script))
    (input_sexpr_file (input_script Parse.Script))
    input_js_file
    file
    run

let input_string string run =
  let* () = trace_lwt ("Running (\"" ^ String.escaped string ^ "\")...") in
  let lexbuf = Lexing.from_string string in
  let* () = trace_lwt "Parsing..." in
  input_script Parse.Script "string" lexbuf run

(* Interactive *)

let continuing = ref false

let lexbuf_stdin buf len =
  let prompt = if !continuing then "  " else "> " in
  print_string prompt ;
  flush_all () ;
  continuing := true ;
  let rec loop i =
    if i = len then i
    else
      let ch = input_char stdin in
      Bytes.set buf i ch ;
      if ch = '\n' then i + 1 else loop (i + 1)
  in
  let n = loop 0 in
  if n = 1 then continuing := false else trace "Parsing..." ;
  n

let input_stdin run =
  (* TODO (#3334): Remove the unused REPL. *)
  let lexbuf = Lexing.from_function lexbuf_stdin in
  let rec loop () =
    let* success = input_script Parse.Script1 "stdin" lexbuf run in
    if not success then Lexing.flush_input lexbuf ;
    if Lexing.(lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len - 1) then
      continuing := false ;
    loop ()
  in
  Lwt.catch loop (function
      | End_of_file ->
          let* () = Lwt_io.printf "\n%!" in
          trace_lwt "Bye."
      | exn -> raise exn)

(* Printing *)

let map_to_list m = List.map snd (Lazy_vector.LwtInt32Vector.loaded_bindings m)

let print_import m im =
  let open Types in
  let open Lwt.Syntax in
  let+ category, annotation =
    let+ t = Ast.import_type m im in
    match t with
    | ExternFuncType t -> ("func", string_of_func_type t)
    | ExternTableType t -> ("table", string_of_table_type t)
    | ExternMemoryType t -> ("memory", string_of_memory_type t)
    | ExternGlobalType t -> ("global", string_of_global_type t)
  in
  Printf.printf
    "  import %s \"%s\" \"%s\" : %s\n"
    category
    (Ast.string_of_name im.it.Ast.module_name)
    (Ast.string_of_name im.it.Ast.item_name)
    annotation

let print_export m ex =
  let open Types in
  let open Lwt.Syntax in
  let+ category, annotation =
    let+ t = Ast.export_type m ex in
    match t with
    | ExternFuncType t -> ("func", string_of_func_type t)
    | ExternTableType t -> ("table", string_of_table_type t)
    | ExternMemoryType t -> ("memory", string_of_memory_type t)
    | ExternGlobalType t -> ("global", string_of_global_type t)
  in
  Printf.printf
    "  export %s \"%s\" : %s\n"
    category
    (Ast.string_of_name ex.it.Ast.name)
    annotation

let print_module x_opt m =
  Printf.printf
    "module%s :\n"
    (match x_opt with None -> "" | Some x -> " " ^ x.it) ;
  let* () =
    TzStdLib.List.iter_s (print_import m) (map_to_list m.it.Ast.imports)
  in
  let+ () =
    TzStdLib.List.iter_s (print_export m) (map_to_list m.it.Ast.exports)
  in
  flush_all ()

let print_values vs =
  let ts = List.map Values.type_of_value vs in
  Printf.printf
    "%s : %s\n"
    (Values.string_of_values vs)
    (Types.string_of_value_types ts) ;
  flush_all ()

let string_of_nan = function
  | CanonicalNan -> "nan:canonical"
  | ArithmeticNan -> "nan:arithmetic"

let type_of_result r =
  match r with
  | NumResult (NumPat n) -> Types.NumType (Values.type_of_num n.it)
  | NumResult (NanPat n) -> Types.NumType (Values.type_of_num n.it)
  | VecResult (VecPat _) -> Types.VecType Types.V128Type
  | RefResult (RefPat r) -> Types.RefType (Values.type_of_ref r.it)
  | RefResult (RefTypePat t) -> Types.RefType t

let string_of_num_pat (p : num_pat) =
  match p with
  | NumPat n -> Values.string_of_num n.it
  | NanPat nanop -> (
      match nanop.it with
      | Values.I32 _ | Values.I64 _ -> assert false
      | Values.F32 n | Values.F64 n -> string_of_nan n)

let string_of_vec_pat (p : vec_pat) =
  match p with
  | VecPat (Values.V128 (shape, ns)) ->
      String.concat " " (List.map string_of_num_pat ns)

let string_of_ref_pat (p : ref_pat) =
  match p with
  | RefPat r -> Values.string_of_ref r.it
  | RefTypePat t -> Types.string_of_refed_type t

let string_of_result r =
  match r with
  | NumResult np -> string_of_num_pat np
  | VecResult vp -> string_of_vec_pat vp
  | RefResult rp -> string_of_ref_pat rp

let string_of_results = function
  | [r] -> string_of_result r
  | rs -> "[" ^ String.concat " " (List.map string_of_result rs) ^ "]"

let print_results rs =
  let ts = List.map type_of_result rs in
  Printf.printf
    "%s : %s\n"
    (string_of_results rs)
    (Types.string_of_value_types ts) ;
  flush_all ()

(* Configuration *)

module Map = Map.Make (String)

let quote : script ref = ref []

let scripts : script Map.t ref = ref Map.empty

let modules : Ast.module_ Map.t ref = ref Map.empty

let instances : Instance.module_inst Map.t ref = ref Map.empty

let registry : Instance.module_inst Map.t ref = ref Map.empty

let bind map x_opt y =
  let map' = match x_opt with None -> !map | Some x -> Map.add x.it y !map in
  map := Map.add "" y map'

let lookup category map x_opt at =
  let key = match x_opt with None -> "" | Some x -> x.it in
  try Map.find key !map
  with Not_found ->
    IO.error
      at
      (if key = "" then "no " ^ category ^ " defined"
      else "unknown " ^ category ^ " " ^ key)

let lookup_script = lookup "script" scripts

let lookup_module = lookup "module" modules

let lookup_instance = lookup "module" instances

let lookup_registry module_name item_name _t =
  let* item_name = Lazy_vector.LwtInt32Vector.to_list item_name in
  let+ value = Instance.export (Map.find module_name !registry) item_name in
  match value with Some ext -> ext | None -> raise Not_found

(* Running *)

let rec run_definition def : Ast.module_ Lwt.t =
  match def.it with
  | Textual m -> Lwt.return m
  | Encoded (name, bytes) ->
      let* () = trace_lwt "Decoding..." in
      Decode.decode ~name ~bytes
  | Quoted (_, s) ->
      let* () = trace_lwt "Parsing quote..." in
      let def' = Parse.string_to_module s in
      run_definition def'

let run_action act : Values.value list Lwt.t =
  match act.it with
  | Invoke (x_opt, name, vs) -> (
      let* () =
        trace_lwt ("Invoking function \"" ^ Ast.string_of_name name ^ "\"...")
      in
      let inst = lookup_instance x_opt act.at in
      let* name = Lazy_vector.LwtInt32Vector.to_list name in
      let* export = Instance.export inst name in
      match export with
      | Some (Instance.ExternFunc f) ->
          let (Types.FuncType (ins, out)) = Func.type_of f in
          let* ins_l = Lazy_vector.LwtInt32Vector.to_list ins in
          if List.length vs <> List.length ins_l then
            Script.error act.at "wrong number of arguments" ;
          List.iter2
            (fun v t ->
              if Values.type_of_value v.it <> t then
                Script.error v.at "wrong type of argument")
            vs
            ins_l ;
          let+ _, result = Eval.invoke f (List.map (fun v -> v.it) vs) in
          result
      | Some _ -> Assert.error act.at "export is not a function"
      | None -> Assert.error act.at "undefined export")
  | Get (x_opt, name) -> (
      let* () =
        trace_lwt ("Getting global \"" ^ Ast.string_of_name name ^ "\"...")
      in
      let inst = lookup_instance x_opt act.at in
      let* name = Lazy_vector.LwtInt32Vector.to_list name in
      let+ export = Instance.export inst name in
      match export with
      | Some (Instance.ExternGlobal gl) -> [Global.load gl]
      | Some _ -> Assert.error act.at "export is not a global"
      | None -> Assert.error act.at "undefined export")

let assert_nan_pat n nan =
  let open Values in
  match (n, nan.it) with
  | F32 z, F32 CanonicalNan -> z = F32.pos_nan || z = F32.neg_nan
  | F64 z, F64 CanonicalNan -> z = F64.pos_nan || z = F64.neg_nan
  | F32 z, F32 ArithmeticNan ->
      let pos_nan = F32.to_bits F32.pos_nan in
      Int32.logand (F32.to_bits z) pos_nan = pos_nan
  | F64 z, F64 ArithmeticNan ->
      let pos_nan = F64.to_bits F64.pos_nan in
      Int64.logand (F64.to_bits z) pos_nan = pos_nan
  | _, _ -> false

let assert_num_pat n np =
  match np with
  | NumPat n' -> n = n'.it
  | NanPat nanop -> assert_nan_pat n nanop

let assert_vec_pat v p =
  let open Values in
  match (v, p) with
  | V128 v, VecPat (V128 (shape, ps)) ->
      let extract =
        match shape with
        | V128.I8x16 () -> fun v i -> I32 (V128.I8x16.extract_lane_s i v)
        | V128.I16x8 () -> fun v i -> I32 (V128.I16x8.extract_lane_s i v)
        | V128.I32x4 () -> fun v i -> I32 (V128.I32x4.extract_lane_s i v)
        | V128.I64x2 () -> fun v i -> I64 (V128.I64x2.extract_lane_s i v)
        | V128.F32x4 () -> fun v i -> F32 (V128.F32x4.extract_lane i v)
        | V128.F64x2 () -> fun v i -> F64 (V128.F64x2.extract_lane i v)
      in
      List.for_all2
        assert_num_pat
        (List.init (V128.num_lanes shape) (extract v))
        ps

let assert_ref_pat r p =
  match (r, p) with
  | r, RefPat r' -> r = r'.it
  | Instance.FuncRef _, RefTypePat Types.FuncRefType
  | ExternRef _, RefTypePat Types.ExternRefType ->
      true
  | _ -> false

let assert_pat v r =
  let open Values in
  match (v, r) with
  | Num n, NumResult np -> assert_num_pat n np
  | Vec v, VecResult vp -> assert_vec_pat v vp
  | Ref r, RefResult rp -> assert_ref_pat r rp
  | _, _ -> false

let assert_result at got expect =
  if
    List.length got <> List.length expect
    || List.exists2 (fun v r -> not (assert_pat v r)) got expect
  then (
    print_string "Result: " ;
    print_values got ;
    print_string "Expect: " ;
    print_results expect ;
    Assert.error at "wrong return values")

let assert_message at name msg re =
  if
    String.length msg < String.length re
    || String.sub msg 0 (String.length re) <> re
  then (
    print_endline ("Result: \"" ^ msg ^ "\"") ;
    print_endline ("Expect: \"" ^ re ^ "\"") ;
    Assert.error at ("wrong " ^ name ^ " error")) ;
  Lwt.return_unit

let run_assertion ass : unit Lwt.t =
  match ass.it with
  | AssertMalformed (def, re) ->
      let* () = trace_lwt "Asserting malformed..." in
      Lwt.try_bind
        (fun () -> run_definition def)
        (fun _ -> Assert.error ass.at "expected decoding/parsing error")
        (function
          | Decode.Code (_, msg) -> assert_message ass.at "decoding" msg re
          | Parse.Syntax (_, msg) -> assert_message ass.at "parsing" msg re
          | exn -> raise exn)
  | AssertInvalid (def, re) ->
      let* () = trace_lwt "Asserting invalid..." in
      Lwt.try_bind
        (fun () ->
          let* m = run_definition def in
          Valid.check_module m)
        (fun _ -> Assert.error ass.at "expected validation error")
        (function
          | Valid.Invalid (_, msg) -> assert_message ass.at "validation" msg re
          | exn -> raise exn)
  | AssertUnlinkable (def, re) ->
      let* () = trace_lwt "Asserting unlinkable..." in
      let* m = run_definition def in
      let* () =
        if not !Flags.unchecked then Valid.check_module m else Lwt.return_unit
      in
      Lwt.try_bind
        (fun () ->
          let* imports = Import.link m in
          Eval.init m imports)
        (fun _ -> Assert.error ass.at "expected linking error")
        (function
          | Import.Unknown (_, msg) | Eval.Link (_, msg) ->
              assert_message ass.at "linking" msg re
          | exn -> raise exn)
  | AssertUninstantiable (def, re) ->
      let* () = trace_lwt "Asserting trap..." in
      let* m = run_definition def in
      let* () =
        if not !Flags.unchecked then Valid.check_module m else Lwt.return_unit
      in
      Lwt.try_bind
        (fun () ->
          let* imports = Import.link m in
          Eval.init m imports)
        (fun _ -> Assert.error ass.at "expected instantiation error")
        (function
          | Eval.Trap (_, msg) -> assert_message ass.at "instantiation" msg re
          | exn -> raise exn)
  | AssertReturn (act, rs) ->
      let* () = trace_lwt "Asserting return..." in
      let+ got_vs = run_action act in
      let expect_rs = List.map (fun r -> r.it) rs in
      assert_result ass.at got_vs expect_rs
  | AssertTrap (act, re) ->
      let* () = trace_lwt "Asserting trap..." in
      Lwt.try_bind
        (fun () -> run_action act)
        (fun _ -> Assert.error ass.at "expected runtime error")
        (function
          | Eval.Trap (_, msg) -> assert_message ass.at "runtime" msg re
          | exn -> raise exn)
  | AssertExhaustion (act, re) ->
      let* () = trace_lwt "Asserting exhaustion..." in
      Lwt.try_bind
        (fun () -> run_action act)
        (fun _ -> Assert.error ass.at "expected exhaustion error")
        (function
          | Eval.Exhaustion (_, msg) ->
              assert_message ass.at "exhaustion" msg re
          | exn -> raise exn)

let rec run_command cmd : unit Lwt.t =
  match cmd.it with
  | Module (x_opt, def) ->
      quote := cmd :: !quote ;
      let* m = run_definition def in
      let* () =
        if not !Flags.unchecked then
          let* () = trace_lwt "Checking..." in
          let* () = Valid.check_module m in
          if !Flags.print_sig then
            let* () = trace_lwt "Signature:" in
            print_module x_opt m
          else Lwt.return_unit
        else Lwt.return_unit
      in
      bind scripts x_opt [cmd] ;
      bind modules x_opt m ;
      if not !Flags.dry then
        let* () = trace_lwt "Initializing..." in
        let* imports = Import.link m in
        let+ inst = Eval.init m imports in
        bind instances x_opt inst
      else Lwt.return_unit
  | Register (name, x_opt) ->
      quote := cmd :: !quote ;
      if not !Flags.dry then (
        trace ("Registering module \"" ^ Ast.string_of_name name ^ "\"...") ;
        let inst = lookup_instance x_opt cmd.at in
        let* utf8_name = Utf8.encode name in
        registry := Map.add utf8_name inst !registry ;
        Import.register name (lookup_registry utf8_name))
      else Lwt.return_unit
  | Action act ->
      quote := cmd :: !quote ;
      if not !Flags.dry then (
        let+ vs = run_action act in
        if vs <> [] then print_values vs)
      else Lwt.return_unit
  | Assertion ass ->
      quote := cmd :: !quote ;
      if not !Flags.dry then run_assertion ass else Lwt.return_unit
  | Meta cmd -> run_meta cmd

and run_meta cmd =
  match cmd.it with
  | Script (x_opt, script) ->
      let+ () = run_quote_script script in
      bind scripts x_opt (lookup_script None cmd.at)
  | Input (x_opt, file) ->
      let+ () =
        Lwt.catch
          (fun () ->
            let+ res = input_file file run_quote_script in
            if not res then Abort.error cmd.at "aborting")
          (function Sys_error msg -> IO.error cmd.at msg | exn -> raise exn)
      in
      bind scripts x_opt (lookup_script None cmd.at) ;
      if x_opt <> None then (
        bind modules x_opt (lookup_module None cmd.at) ;
        if not !Flags.dry then
          bind instances x_opt (lookup_instance None cmd.at))
  | Output (x_opt, Some file) ->
      Lwt.catch
        (fun () ->
          output_file
            file
            (fun () -> lookup_script x_opt cmd.at)
            (fun () -> lookup_module x_opt cmd.at))
        (function Sys_error msg -> IO.error cmd.at msg | exn -> raise exn)
  | Output (x_opt, None) ->
      Lwt.catch
        (fun () -> output_stdout (fun () -> lookup_module x_opt cmd.at))
        (function Sys_error msg -> IO.error cmd.at msg | exn -> raise exn)

and run_script script = TzStdLib.List.iter_s run_command script

and run_quote_script script =
  let save_quote = !quote in
  quote := [] ;
  let+ () =
    Lwt.catch
      (fun () -> run_script script)
      (fun exn ->
        quote := save_quote ;
        raise exn)
  in
  bind scripts None (List.rev !quote) ;
  quote := !quote @ save_quote

let run_file file = input_file file run_script

let run_string string = input_string string run_script

let run_stdin () = input_stdin run_script
