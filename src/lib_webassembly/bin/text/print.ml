module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare

let instr oc width e = Sexpr.output oc width (Arrange.instr e)

let func oc width f = Sexpr.output oc width (Arrange.func f)

let module_ oc width m = Sexpr.output oc width (Arrange.module_ m)

let script oc width mode s =
  let script = Arrange.script mode s in
  TzStdLib.List.iter_s (Sexpr.output oc width) script
