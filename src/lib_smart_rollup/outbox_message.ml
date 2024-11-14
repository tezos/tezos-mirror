(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type transaction_summary = {
  destination : string;
  entrypoint : string;
  parameters : string Tezos_micheline.Micheline.canonical;
  parameters_ty : string Tezos_micheline.Micheline.canonical option;
}

type summary =
  | Whitelist_update of Signature.Public_key_hash.t list option
  | Transaction_batch of transaction_summary list

let transaction_summary_encoding =
  let open Data_encoding in
  conv
    (fun {destination; entrypoint; parameters; parameters_ty} ->
      (destination, entrypoint, parameters, parameters_ty))
    (fun (destination, entrypoint, parameters, parameters_ty) ->
      {destination; entrypoint; parameters; parameters_ty})
  @@ obj4
       (req "destination" string)
       (dft "entrypoint" string "default")
       (dft
          "parameters"
          (Tezos_micheline.Micheline_encoding.canonical_encoding
             ~variant:"outbox_transaction_parameters"
             string)
          Tezos_micheline.Micheline.(
            strip_locations (Prim ((), "Unit", [], []))))
       (opt
          "parameters_ty"
          (Tezos_micheline.Micheline_encoding.canonical_encoding
             ~variant:"outbox_transaction_parameters_type"
             string))

let summary_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"whitelist_update"
        (Tag 0)
        (obj1
           (req
              "whitelist_update"
              (option (list Signature.Public_key_hash.encoding))))
        (function Whitelist_update pkhs -> Some pkhs | _ -> None)
        (fun pkhs -> Whitelist_update pkhs);
      case
        ~title:"transactions"
        (Tag 1)
        (obj1 (req "transactions" (list transaction_summary_encoding)))
        (function Transaction_batch trs -> Some trs | _ -> None)
        (fun trs -> Transaction_batch trs);
    ]

let pp_summary fmt = function
  | Whitelist_update None -> Format.pp_print_string fmt "Remove whitelist"
  | Whitelist_update (Some pkhs) ->
      Format.fprintf
        fmt
        "Set whitelist to @[<hov 2>[@,%a]@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           Signature.Public_key_hash.pp)
        pkhs
  | Transaction_batch txs ->
      Format.fprintf fmt "@[<v 2>Transactions:@," ;
      List.iter
        (fun {destination; entrypoint; parameters; parameters_ty = _} ->
          Format.fprintf
            fmt
            "@[<hov>%s:@ %s(%a)@]"
            destination
            entrypoint
            Tezos_micheline.Micheline_printer.print_expr_unwrapped
            (Tezos_micheline.Micheline_printer.printable Fun.id parameters))
        txs ;
      Format.fprintf fmt "@]"
