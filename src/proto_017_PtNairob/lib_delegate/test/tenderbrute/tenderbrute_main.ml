(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol

let bootstrap_accounts = ref None

let constants_overrides = ref None

let selection = ref []

let max = ref None

let random_seed = ref None

let report_err res =
  match res with
  | Error e ->
      Format.eprintf "\n%a@." Error_monad.pp_print_trace e ;
      exit 2
  | Ok res -> res

let delegate_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Public key hash"
        (Tag 0)
        Signature.Public_key_hash.encoding
        (function `Pkh p -> Some p | _ -> None)
        (fun p -> `Pkh p);
      case
        ~title:"Alias"
        (Tag 1)
        string
        (function `Alias a -> Some a | _ -> None)
        (fun a -> `Alias a);
    ]

let selection_encoding =
  let open Data_encoding in
  list
    (tup2
       Raw_level_repr.encoding
       (list (tup2 Round_repr.encoding delegate_encoding)))

let mk_bootstrap_aliases bootstrap_accounts_json =
  let parameters = Mockup.Protocol_parameters.default_value in
  let open Alpha_context.Parameters in
  match bootstrap_accounts_json with
  | None ->
      List.mapi
        (fun i a -> (Format.sprintf "bootstrap%d" i, a.public_key_hash))
        parameters.bootstrap_accounts
      |> return
  | Some j ->
      let open Data_encoding in
      let accounts = Json.destruct (list Mockup.Parsed_account.encoding) j in
      List.map_ep
        (fun a ->
          Mockup.Parsed_account.to_bootstrap_account a >|=? fun acc ->
          (a.name, acc.public_key_hash))
        accounts

let selection_to_pkhs bootstrap_accounts_json selection =
  mk_bootstrap_aliases bootstrap_accounts_json >|=? fun bootstrap_aliases ->
  List.map
    (fun (level, l) ->
      ( level,
        List.map
          (fun (round, d) ->
            ( round,
              match d with
              | `Pkh d -> d
              | `Alias a -> (
                  match List.assoc ~equal:String.equal a bootstrap_aliases with
                  | None -> Stdlib.failwith @@ "Unknown alias " ^ a
                  | Some d -> d) ))
          l ))
    selection

let parse_json_or_file s =
  Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file s >|= function
  | Ok json -> Ok json
  | Error errs -> (
      match Data_encoding.Json.from_string s with
      | Ok json -> Ok json
      | Error e -> Error (Exn (Failure e) :: errs))

let set_selection s =
  Lwt_main.run
    ( parse_json_or_file s >|= report_err >|= fun json ->
      selection := Data_encoding.Json.destruct selection_encoding json )

let main () =
  let thread =
    (match !bootstrap_accounts with
    | None -> return_none
    | Some read -> read >|=? Option.some)
    >>=? fun bootstrap_accounts_json ->
    (match !constants_overrides with
    | None -> return_none
    | Some read -> read >|=? Option.some)
    >>=? fun constants_overrides_json ->
    selection_to_pkhs bootstrap_accounts_json !selection >>=? fun selection ->
    Tenderbrute.bruteforce
      ~show_progress:true
      ?max:!max
      ?random_seed:!random_seed
      ?bootstrap_accounts_json
      ?constants_overrides_json
      selection
  in
  Lwt_main.run
    ( thread >|= report_err >|= fun seed ->
      let seed_str =
        match seed with None -> "None" | Some s -> State_hash.to_b58check s
      in
      Format.printf "%s@." seed_str )

let specs =
  [
    ( "--max",
      Arg.Int (fun m -> max := Some m),
      "<m> set maximum number of tries to <m>" );
    ( "--random-seed",
      Arg.Int (fun i -> random_seed := Some i),
      "<i> initialize the random generator with <i> (useful to spawn mutliple \
       instances)" );
    ( "--constants-overrides",
      Arg.String (fun s -> constants_overrides := Some (parse_json_or_file s)),
      "<json|file> set overrides for constants in protocol parameters (e.g. \
       blocks_per_cycle)" );
    ( "--bootstrap-accounts",
      Arg.String (fun s -> bootstrap_accounts := Some (parse_json_or_file s)),
      "<json|file> set bootstrap accounts of protocol" );
  ]

let usage =
  Format.sprintf
    {|usage: %s '[[1, [[0, "tz1..."], [1, "tz1..."]]], [2, [[0, "tz1..."]]]]'|}
    Sys.argv.(0)

let () =
  Arg.parse (Arg.align specs) set_selection usage ;
  main ()
