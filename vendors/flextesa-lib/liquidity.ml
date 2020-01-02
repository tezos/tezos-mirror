open Internal_pervasives
module MFmt = Experiments.Markup_fmt

let failf ?attach fmt =
  ksprintf (fun s -> fail ?attach (`Scenario_error s)) fmt

module Data = struct
  type t = Fmt of (Caml.Format.formatter -> unit)

  let to_string = function
    | Fmt f -> Caml.Format.asprintf "%a" (fun ppf () -> f ppf) ()

  let pp ppf (Fmt f) =
    let open Fmt in
    box (fun ppf () -> f ppf) ppf ()

  let rawf fmt =
    Caml.Format.kasprintf (fun s -> Fmt (fun ppf -> Fmt.string ppf s)) fmt

  let fmt f = Fmt f
  let address s = rawf "%s" s

  let list_like ~sep ~delimiter l =
    Fmt
      Fmt.(
        fun ppf ->
          delimiter
            (fun ppf () -> list ~sep (fun ppf (Fmt f) -> f ppf) ppf l)
            ppf ())

  let tuple l = list_like ~sep:Fmt.comma ~delimiter:Fmt.parens l
  let int i = Fmt (fun ppf -> Fmt.int ppf i)
  let string = rawf "%S"
  let nat d = rawf "%dp" d

  let tez (`Mutez d) =
    let million = 1_000_000. in
    rawf "%ftz" (Float.of_int d /. million)

  let semi_colon = Fmt.(fun ppf () -> string ppf ";" ; sp ppf ())
  let list l = list_like ~sep:semi_colon ~delimiter:Fmt.brackets l

  let set l =
    list_like ~sep:semi_colon l
      ~delimiter:Fmt.(fun x ppf -> pf ppf "@[<6>(Set [%a]@])" x)

  let empty_set = set []
  let key_hash s = rawf "%s" s
  let key s = rawf "%s" s
  let account_key a = Tezos_protocol.Account.pubkey a |> key
  let account_key_hash a = Tezos_protocol.Account.pubkey_hash a |> key_hash
  let signature s = rawf "%s" s
  let bytes s = rawf "0x%s" s
  let some s = fmt (fun ppf -> Fmt.pf ppf "@[<5>(Some %a)@]" pp s)
  let none = rawf "None"
  let typed_none t = rawf "(None : %s)" t

  let record l =
    list_like ~sep:semi_colon ~delimiter:Fmt.braces
      (List.map l ~f:(fun (k, v) ->
           fmt Fmt.(fun ppf -> pf ppf "@[<2>%s =@ %a@]" k pp v)))
end

module Contract = struct
  type t = {name: string; paths: string list; main_name: string option}

  let make ?(library = []) ?main_name name ~path =
    {name; paths= library @ [path]; main_name}

  let build_dir state t =
    Paths.root state // sprintf "liquidity-build-%s" t.name

  let ensure_build_dir state t =
    let dir = build_dir state t in
    Running_processes.run_successful_cmdf state "mkdir -p %s"
      (Caml.Filename.quote dir)
    >>= fun _ -> return dir

  let base_liquidity_command _state t =
    sprintf "liquidity %s %s"
      (List.map t.paths ~f:Caml.Filename.quote |> String.concat ~sep:" ")
      (Option.value_map t.main_name ~default:"" ~f:(sprintf "--main %s"))

  let michelson state t =
    let f = build_dir state t // sprintf "%s.tz" t.name in
    ensure_build_dir state t
    >>= fun _ ->
    Running_processes.run_successful_cmdf state "%s --no-annot -o %s"
      (base_liquidity_command state t)
      (Caml.Filename.quote f)
    >>= fun _ -> return f

  let storage_initialization state t ~tezos_node ~storage =
    ensure_build_dir state t
    >>= fun dir ->
    let out = dir // sprintf "%s-initial-storage.tz" t.name in
    Running_processes.run_successful_cmdf state
      "%s -o %s --tezos-node %s --init-storage %s"
      (base_liquidity_command state t)
      (Caml.Filename.quote out)
      (Caml.Filename.quote tezos_node)
      ( List.map storage ~f:(fun item ->
            Caml.Filename.quote (Data.to_string item))
      |> String.concat ~sep:" " )
    >>= fun _ -> System.read_file state out >>= fun content -> return content

  let arguments state t ~entry_point ~data =
    Running_processes.run_successful_cmdf state "%s --data %s %s"
      (base_liquidity_command state t)
      (Caml.Filename.quote entry_point)
      (Caml.Filename.quote (Data.to_string data))
    >>= fun res -> return (String.concat ~sep:" " res#out)

  let cmdliner_term ~prefix ~name () =
    let contract_name = name in
    let open Cmdliner in
    let open Term in
    let flag_name s = sprintf "%s-%s" prefix s in
    Arg.(
      pure (fun path main_name library ->
          make ~library ?main_name ~path contract_name)
      $ required
          (opt (some non_dir_file) None
             (info [flag_name "path"]
                ~doc:
                  (sprintf "Path to the liquidity %s contract." contract_name)))
      $ value
          (opt (some string) None
             (info [flag_name "main"]
                ~doc:
                  (sprintf "Name of “main” contract for %s." contract_name)))
      $ value
          (opt
             (list ~sep:',' non_dir_file)
             []
             (info [flag_name "library"]
                ~doc:
                  (sprintf
                     "Paths to extra liquidity %s contract-library files."
                     contract_name))))
end

module On_chain = struct
  let tezos_client_keyed_originate_contract ?(force = false)
      ?(transferring = 0) ?(burn_cap = 0.5) state keyed ~name ~source ~storage
      =
    let client = keyed.Tezos_client.Keyed.client in
    Tezos_client.successful_client_cmd state ~client
      ( [ "--wait"; "none"; "originate"; "contract"; name; "for"; keyed.key_name
        ; "transferring"; Int.to_string transferring; "from"; keyed.key_name
        ; "running"; source; "--init"; storage; "--burn-cap"
        ; Float.to_string burn_cap ]
      @ if force then ["--force"] else [] )

  let build_and_deploy ?(burn_cap = 10.1) state contract ~keyed_client ~storage
      ~balance =
    let name = contract.Contract.name in
    let tezos_node =
      sprintf "http://localhost:%d" keyed_client.Tezos_client.Keyed.client.port
    in
    Contract.michelson state contract
    >>= fun michetz ->
    Contract.storage_initialization state contract ~tezos_node
      ~storage:(List.map storage ~f:snd)
    >>= fun init ->
    tezos_client_keyed_originate_contract state keyed_client ~name
      ~transferring:balance ~source:michetz ~storage:init ~burn_cap ~force:true
    >>= fun _ ->
    Tezos_client.Keyed.bake state keyed_client (sprintf "%s origination" name)
    >>= fun () ->
    Tezos_client.successful_client_cmd state
      ~client:keyed_client.Tezos_client.Keyed.client
      ["show"; "known"; "contract"; name]
    >>= fun res ->
    let address = String.strip (String.concat ~sep:"" res#out) in
    Console.sayf state
      MFmt.(
        par (t "Deployed " @ hlf "“%s”" name)
        @ itemize [tf "Script: `%s`" michetz; tf "Address: `%s`" address]
        @ par (tf "Storage:")
        @ itemize
            (List.map storage ~f:(fun (name, data) ->
                 tf "%s:@ %a" name Data.pp data))
        |> to_fmt)
    >>= fun () -> return address

  (* This should go to flextesa soon... *)
  let silent_client_cmd state ~client args =
    Running_processes.run_cmdf state "sh -c %s"
      ( Tezos_client.client_command state client args
      |> Genspio.Compile.to_one_liner |> Caml.Filename.quote )
    >>= fun res ->
    let success = Poly.equal res#status (Lwt_unix.WEXITED 0) in
    return (success, res)

  let call ?msg ?(should = `Be_ok) ?(transferring = 0) ?(burn_cap = 0.3) state
      contract ~keyed_client ~entry_point ~data =
    Contract.arguments state contract ~entry_point ~data
    >>= fun low_level_arg ->
    silent_client_cmd state ~client:keyed_client.Tezos_client.Keyed.client
      [ "--wait"; "none"; "transfer"; Int.to_string transferring; "from"
      ; keyed_client.key_name; "to"; contract.name; "--burn-cap"
      ; Float.to_string burn_cap; "--arg"; low_level_arg ]
    >>= fun (succeeds, res) ->
    ( match succeeds with
    | false -> (
      match should with
      | `Fail -> return (`Expected `Failure)
      | `Script_failwith_re re ->
          let intersting_part =
            List.drop_while res#err ~f:(fun line ->
                String.is_prefix line ~prefix:"script reached FAILWITH")
            |> String.concat ~sep:" " in
          if Re.execp re intersting_part then return (`Expected `Failure)
          else return (`Failed `With_error_does_not_match)
      | `Command_stderr_re re ->
          if Re.execp re (String.concat ~sep:"\n" res#err) then
            return (`Expected `Failure)
          else return (`Failed `With_error_does_not_match)
      | `Be_ok -> return (`Failed `Not_ok) )
    | true when Poly.equal should `Be_ok ->
        silent_client_cmd state ~client:keyed_client.client
          [ "bake"; "for"; keyed_client.key_name; "--force"
          ; "--minimal-timestamp" ]
        >>= fun (_bake, _) -> return (`Expected `Ok)
    | true (* should is no ok *) -> return (`Failed `Unexpected_ok) )
    >>= fun test_status ->
    let test_full_name =
      sprintf "%s#%s%s" contract.name entry_point
        (Option.value_map msg ~default:"" ~f:(sprintf " (%s)")) in
    Console.sayf state
      MFmt.(
        let details =
          match test_status with
          | `Expected _ -> []
          | `Failed _ ->
              par (tf "Data:")
              @ verbatim [Data.to_string data]
              @ par (tf "Std-out:")
              @ verbatim res#out
              @ par (tf "Std-err:")
              @ verbatim res#err in
        par (tf "Test-call %s" test_full_name)
        @ itemize
            [ ( match test_status with
              | `Expected exp ->
                  hlf "Success: %s"
                    ( match exp with
                    | `Ok -> "op-baked"
                    | `Failure -> "expected-failure" )
              | `Failed reason ->
                  hlf "FAILURE: %s"
                    ( match reason with
                    | `Not_ok -> "Not-OK"
                    | `Unexpected_ok -> "Should-have-failed"
                    | `With_error_does_not_match ->
                        "Error-message-does-not-match" ) ) ]
        @ details
        |> to_fmt)
    >>= fun () ->
    match test_status with
    | `Expected _ -> return res
    | `Failed _ -> failf "Test failed: %s" test_full_name

  let key_with_type_json key =
    let open Ezjsonm in
    match key with
    | `Nat n ->
        ( dict [("int", `String (Int.to_string n))]
        , dict [("prim", `String "nat")] )

  let big_map_get state ~client ~address ~key =
    let post_json =
      let open Ezjsonm in
      let k, t = key_with_type_json key in
      dict [("key", k); ("type", t)] |> to_string ~minify:false in
    Tezos_client.rpc state ~client (`Post post_json)
      ~path:
        (sprintf "/chains/main/blocks/head/context/contracts/%s/big_map_get"
           address)
    >>= fun json ->
    return
      (object
         method post = post_json

         method result = json
      end)

  let show_contract_command state ~client ~name ~address ~pp_error =
    Console.Prompt.unit_and_loop
      ~description:(Fmt.str "Show status of the contract %s." address)
      [sprintf "show-%s" name] (fun _sexps ->
        Asynchronous_result.transform_error
          ~f:(fun e ->
            Caml.Format.kasprintf
              (fun s -> `Command_line s)
              "show-contract: %a" pp_error e)
          ( List.fold ["storage"; "balance"] ~init:(return [])
              ~f:(fun pm endpoint ->
                pm
                >>= fun l ->
                Tezos_client.rpc state ~client `Get
                  ~path:
                    (sprintf "/chains/main/blocks/head/context/contracts/%s/%s"
                       address endpoint)
                >>= fun json ->
                return
                  EF.(
                    desc (wf "/%s" endpoint)
                      (markdown_verbatim
                         (Ezjsonm.value_to_string ~minify:false json))
                    :: l))
          >>= fun l ->
          Console.say state
            EF.(
              desc
                (haf "Contract %s@%s" name address)
                (list ~sep:"" ~delimiters:("", "") l)) ))

  let big_map_get_command state ~names ~thing ~client ~name ~address
      ~key_of_string ~pp_error =
    Console.Prompt.unit_and_loop
      ~description:
        (Fmt.str "Get %s from the big-map of the contract %s@%s." thing name
           address) names (fun sexps ->
        Asynchronous_result.transform_error
          ~f:(fun e ->
            Caml.Format.kasprintf
              (fun s -> `Command_line s)
              "%s: %a" (List.hd_exn names) pp_error e)
          ( match sexps with
          | [Sexplib.Sexp.Atom s] ->
              key_of_string s
              >>= fun key ->
              big_map_get state ~client ~address ~key
              >>= fun getthing ->
              Console.sayf state
                MFmt.(
                  par (tf "Posted:")
                  @ verbatim_raw getthing#post
                  @ par (tf "Got:")
                  @ verbatim_ezjson getthing#result
                  |> to_fmt)
          | _ -> failf "Wrong s-exp command line" ))
end
