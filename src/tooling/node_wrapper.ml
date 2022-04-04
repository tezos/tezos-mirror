module Lib = struct
  type version = string

  type t = Hacl | Secp256k1 | Bls12_381

  let to_string = function
    | Hacl -> "hacl"
    | Secp256k1 -> "secp256k1"
    | Bls12_381 -> "bls12_381"

  let to_js_lib = function
    | Hacl -> "hacl-wasm"
    | Secp256k1 -> "@nomadic-labs/secp256k1wasm"
    | Bls12_381 -> "@dannywillems/ocaml-bls12-381"

  let to_load_ident x = Printf.sprintf "load_%s" (to_string x)

  let to_js x =
    let load_ident = to_load_ident x in
    let js_lib = to_js_lib x in
    match x with
    | Hacl ->
        Printf.sprintf
          {|
function %s() {
  /* We have to cheat to avoid the noise from hacl-wasm */
  var old_log = console.log;
  console.log = function () {};
  var loader = require('%s');
  console.log = old_log;
  return loader.getInitializedHaclModule().then(function(loaded){
    console.log('hacl loaded');
    global._HACL = loaded})
}
|}
          load_ident
          js_lib
    | Secp256k1 ->
        Printf.sprintf
          {|
function %s() {
  var loader = require('%s');
  return loader().then(function(loaded) {
    console.log('secp256k1 loaded');
    global._SECP256K1 = loaded})
}
|}
          load_ident
          js_lib
    | Bls12_381 ->
        Printf.sprintf
          {|
function %s() {
  var loader = require('%s');
  return loader().then(function(loaded) {
    console.log('bls12-381 loaded');
    global._BLS12381 = loaded})
}
           |}
          load_ident
          js_lib
end

let files = ref []

let libs = ref []

let args = ref []

let () =
  Arg.parse
    [
      ("--hacl", Unit (fun () -> libs := Lib.Hacl :: !libs), "Load hacl-wasm");
      ( "--secp256k1",
        Unit (fun () -> libs := Lib.Secp256k1 :: !libs),
        "Load @nomadic-labs/secp256k1wasm" );
      ( "--bls12-381",
        Unit (fun () -> libs := Lib.Bls12_381 :: !libs),
        "Load @dannywillems/ocaml-bls12-381" );
      ( "--",
        Rest_all (fun l -> args := List.rev_append l !args),
        "args to pass to the scripts" );
    ]
    (fun a ->
      match Filename.extension a with
      | ".js" -> files := a :: !files
      | _ -> args := a :: !args)
    (Printf.sprintf
       "%s [FLAGS] FILE.js -- args"
       (Filename.basename Sys.executable_name))

let run i file args =
  let argv = "node" :: file :: args in
  let file =
    if Filename.is_implicit file then Filename.concat "." file else file
  in
  let argv = String.concat ", " (List.map (Printf.sprintf "%S") argv) in
  Printf.sprintf
    {|
function run_%d () {
  console.log('Ready to run %s with argv = [ %s ]');
  var old_argv = process.argv;
  process.argv = [%s];
  require('%s');
  process.argv = old_argv;
}
|}
    i
    file
    argv
    argv
    file

let () =
  let libs = List.rev !libs in
  let files = List.rev !files in
  let args = List.rev !args in
  let b = Buffer.create 1024 in
  Buffer.add_string
    b
    {|
process.on('uncaughtException', function (error) {
   console.error(error.stack);
});

var major = process.version.match(/^v?([0-9]+)\./)[1];
var minimum_major = 14
if(parseInt(major) < minimum_major){
    console.error("Error: nodejs v" + minimum_major + " or greater is needed. Current version is " + process.version);
    process.exit(1);
}
|} ;
  List.iter (fun lib -> Buffer.add_string b (Lib.to_js lib)) libs ;
  List.iteri (fun i file -> Buffer.add_string b (run i file args)) files ;
  let promises = List.map Lib.to_load_ident libs in
  Buffer.add_string b "Promise.resolve('Loading')" ;
  List.iter
    (fun p -> Buffer.add_string b (Printf.sprintf ".then(%s)" p))
    promises ;
  List.iteri
    (fun i _file -> Buffer.add_string b (Printf.sprintf ".then(run_%d)" i))
    files ;
  Buffer.add_string
    b
    ".catch(function (e) { console.log(e); process.exit(1) })\n" ;
  print_newline () ;
  let oc = Unix.open_process_out "node" in
  Buffer.output_buffer oc b ;
  flush_all () ;
  match Unix.close_process_out oc with
  | WEXITED x -> exit x
  | WSIGNALED _ -> exit 1
  | WSTOPPED _ -> exit 1
