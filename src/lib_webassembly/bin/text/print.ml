module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare

let instr oc lookup_block width e =
  Sexpr.output oc width (Arrange.instr lookup_block e)

let func oc lookup_block width f =
  Sexpr.output oc width (Arrange.func lookup_block f)

let module_ oc width m =
  let open Lwt.Syntax in
  let* m = Arrange.module_ m in
  Sexpr.output oc width m

let script oc width mode s =
  let open Lwt.Syntax in
  let* script = Arrange.script mode s in
  TzStdLib.List.iter_s (Sexpr.output oc width) script
