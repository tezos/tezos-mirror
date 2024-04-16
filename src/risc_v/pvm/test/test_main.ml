open Octez_risc_v_pvm

let add_test () =
  let res = octez_risc_v_add 5l 6l in
  Alcotest.(check int32 "add" res 11l)

let tests = [("Main", [("add", `Quick, add_test)])]

let () = Alcotest.run ~__FILE__ "RISC-V interpreter" tests
