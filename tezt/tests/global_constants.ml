(** Tests originating a large contract
    with no constants. This serves for
    comparison with the case where we originate a
    much larger contract using constants. *)
let test_large_flat_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Originate a large, flat contract"
    ~tags:["global_constant"]
  @@ fun protocol ->
  let* (_, client) = Client.init_activate_bake ~protocol `Client () in
  let* _ =
    Client.originate_contract
      ~alias:"large_flat_contract"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/large_flat_contract.tz"
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 9999999)
      client
  in
  let* () = Client.bake_for client in
  return ()

(* To ensure a billion-laughs style attack is not possible,
   we construct a giant contract and try to originate it,
   expecting the size limit to reject it.
   
   We achieve this by registering a large initial value,
   then a constant with 10 of these values, then a constant
   with 10 of the first constant. We could quickly get to
   extremely large values, but the current size limit is set
   very low and only needs two layers. *)
let test_billion_laughs_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Global constants billion laughs attack"
    ~tags:["billion_laughs"; "global_constant"]
  @@ fun protocol ->
  let* (_, client) = Client.init_activate_bake ~protocol `Client () in
  let repeat_n_times n str start finish =
    start ^ (List.init n (fun _ -> str) |> String.concat " ") ^ finish
  in
  let value = repeat_n_times 250 "UNIT; DROP; " "{" "}" in
  let* hash =
    Client.register_global_constant
      ~src:"bootstrap1"
      ~value
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* () = Client.bake_for client in
  let value =
    repeat_n_times 10 (Format.sprintf "constant \"%s\";" hash) "{" "}"
  in
  let* hash =
    Client.register_global_constant
      ~src:"bootstrap1"
      ~value
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* () = Client.bake_for client in
  let value =
    repeat_n_times 10 (Format.sprintf "constant \"%s\";" hash) "{" "}"
  in
  let proc =
    Client.spawn_register_global_constant
      ~src:"bootstrap1"
      ~value
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* _ =
    Process.check_error ~msg:(rex "larger than the expression size limit") proc
  in
  let* _ = Client.bake_for client in
  (* Same test but for a contract

     Note the contract is ill-typed, having a extra CDR command.
     We expect the size error to occur before and prevent the type error. *)
  let prg =
    Format.sprintf
      "parameter unit; storage unit; code {%s; CDR; CDR; NIL operation; PAIR}"
      value
  in
  let proc =
    Client.spawn_originate_contract
      ~alias:"too_big"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 9999999)
      client
  in
  let* _ =
    Process.check_error ~msg:(rex "larger than the expression size limit") proc
  in
  Client.bake_for client

let register ~protocols =
  test_large_flat_contract ~protocols ;
  test_billion_laughs_contract ~protocols
