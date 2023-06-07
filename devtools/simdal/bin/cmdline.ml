(** [interval] is the type of inclusive intervals. *)
type interval = Interval.t

open Interval

type options = {
  prod_deg : interval option;
  att_deg : interval option;
  cons_deg : interval option;
  ngraphs : int;  (** Number of graphs to sample *)
  nroutings : int;  (** Number of routings per graph to sample *)
  autocorrelation_drop : int;
  with_consumers : bool;
  min_relayed_shards : int;
  redundancy : float;  (** Redundancy factor induced by erasure code *)
  verbose : bool;
  message_kb : float;
  data_root : string;
  flat_stake : bool;
  analysis : string list;
}

let default_interval = {lo = 200; hi = 500}

let default =
  {
    prod_deg = None;
    att_deg = None;
    cons_deg = None;
    ngraphs = 1000;
    nroutings = 1;
    autocorrelation_drop = 10_000;
    with_consumers = true;
    min_relayed_shards = 0;
    verbose = false;
    redundancy = 16.;
    message_kb = 8.0;
    data_root = ".";
    flat_stake = false;
    analysis = [];
  }

let pp_opt pp =
  Format.pp_print_option ~none:(fun fmtr () -> Format.fprintf fmtr "None") pp

let usage () =
  let open Format in
  printf "\n" ;
  printf
    "--prod-deg <nonnegative> <nonnegative>\n\
    \  degree bounds for producers (default %a)\n"
    (pp_opt Interval.pp)
    default.prod_deg ;
  printf
    "--att-deg <nonnegative> <nonnegative>\n\
    \  degree bounds for attesters (default %a)\n"
    (pp_opt Interval.pp)
    default.att_deg ;
  printf
    "--cons-deg <nonnegative> <nonnegative>\n\
    \  degree bounds for consumers (default %a)\n"
    (pp_opt Interval.pp)
    default.cons_deg ;
  printf
    "--ngraphs <nonnegative>\n  number of graphs to sample (default %d)\n"
    default.ngraphs ;
  printf
    "--nroutings <nonnegative>\n\
    \  number of routings per graph to sample (default %d)\n"
    default.nroutings ;
  printf
    "--autocorr-drop <nonnegative>\n\
    \  number of samples to drop between each graph (default %d)\n"
    default.autocorrelation_drop ;
  printf
    "--no-consumers (%s by default)\n"
    (if default.with_consumers then "unset" else "set") ;
  printf
    "--min-relayed <nonnegative int> (default %d)\n"
    default.min_relayed_shards ;
  printf
    "--verbose (%s by default)\n"
    (if default.verbose then "set" else "unset") ;
  printf "--message-kb <positive float> (default %.2f)\n" default.message_kb ;
  printf "--data-root <directory> (default none)\n" ;
  printf "--analyse <analysis-name> (default none)\n" ;
  printf
    "--flat-stake (%s by default)\n"
    (if default.flat_stake then "set" else "unset")

let parse_itv lo hi k =
  let lo = int_of_string lo in
  let hi = int_of_string hi in
  if hi < lo then (
    Format.printf "invalid degree window;" ;
    exit 1) ;
  k {lo; hi}

let rec parse_options list acc =
  match list with
  | [] -> acc
  | "--prod-deg" :: lo :: hi :: rest ->
      parse_itv lo hi @@ fun prod_deg ->
      parse_options rest {acc with prod_deg = Some prod_deg}
  | "--att-deg" :: lo :: hi :: rest ->
      parse_itv lo hi @@ fun att_deg ->
      parse_options rest {acc with att_deg = Some att_deg}
  | "--cons-deg" :: lo :: hi :: rest ->
      parse_itv lo hi @@ fun cons_deg ->
      parse_options rest {acc with cons_deg = Some cons_deg}
  | "--ngraphs" :: ngraphs :: rest ->
      parse_options rest {acc with ngraphs = int_of_string ngraphs}
  | "--nroutings" :: nroutings :: rest ->
      parse_options rest {acc with nroutings = int_of_string nroutings}
  | "--autocorr-drop" :: drop :: rest ->
      parse_options rest {acc with autocorrelation_drop = int_of_string drop}
  | "--no-consumers" :: rest ->
      parse_options rest {acc with with_consumers = false}
  | "--min-relayed" :: shards :: rest ->
      parse_options rest {acc with min_relayed_shards = int_of_string shards}
  | "--verbose" :: rest -> parse_options rest {acc with verbose = true}
  | "--message-kb" :: message_kb :: rest ->
      let message_kb = float_of_string message_kb in
      parse_options rest {acc with message_kb}
  | "--data-root" :: data_root :: rest ->
      parse_options rest {acc with data_root}
  | "--analyse" :: name :: rest ->
      parse_options rest {acc with analysis = name :: acc.analysis}
  | "--flat-stake" :: rest -> parse_options rest {acc with flat_stake = true}
  | _ ->
      usage () ;
      exit 1

let parse_options list =
  match list with
  | [] ->
      usage () ;
      exit 0
  | _ -> parse_options list default

let pp_options fmtr opts =
  Format.fprintf
    fmtr
    "@[<v 2>{ prod_deg = %a;@;\
     att_deg = %a;@;\
     cons_deg = %a;@;\
     ngraphs = %d;@;\
     nroutings = %d;@;\
     autocorrelation_drop = %d;@;\
     with_consumers = %b;@;\
     min_relayed_shards = %d;@;\
     verbose = %b;@;\
     message_kb = %.2f@;\
     data_root = %s;@;\
     flat_stake = %b;@;\
     analysis = %a }@]"
    (pp_opt Interval.pp)
    opts.prod_deg
    (pp_opt Interval.pp)
    opts.att_deg
    (pp_opt Interval.pp)
    opts.cons_deg
    opts.ngraphs
    opts.nroutings
    opts.autocorrelation_drop
    opts.with_consumers
    opts.min_relayed_shards
    opts.verbose
    opts.message_kb
    opts.data_root
    opts.flat_stake
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
       Format.pp_print_string)
    opts.analysis

let parse_options () = Sys.argv |> Array.to_list |> List.tl |> parse_options
