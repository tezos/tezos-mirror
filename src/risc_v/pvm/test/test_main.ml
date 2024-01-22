open Octez_risc_v_pvm

let add_test () =
  let res = Main.add 5 6 in
  Alcotest.(check int "add" res 11)

let tests = [("Main", [("add", `Quick, add_test)])]

let () = Alcotest.run ~__FILE__ "RISC-V interpreter" tests
