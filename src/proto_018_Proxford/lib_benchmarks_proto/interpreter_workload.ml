(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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
module Size = Gas_input_size

(* ------------------------------------------------------------------------- *)

type id = string

let pp_id = Format.pp_print_string

let equal_id = String.equal

(* ------------------------------------------------------------------------- *)
(* Names of IR instructions together with sizes of their operands as
   encountered during evaluation. *)

type instruction_name =
  (* stack ops *)
  | N_IDrop
  | N_IDup
  | N_ISwap
  | N_IPush
  | N_IUnit
  (* pairs *)
  | N_ICons_pair
  | N_ICar
  | N_ICdr
  | N_IUnpair
  (* options *)
  | N_ICons_some
  | N_ICons_none
  | N_IIf_none
  | N_IOpt_map
  (* ors *)
  | N_ILeft
  | N_IRight
  | N_IIf_left
  (* lists *)
  | N_ICons_list
  | N_INil
  | N_IIf_cons
  | N_IList_map
  | N_IList_iter
  | N_IIter
  | N_IList_size
  (* sets *)
  | N_IEmpty_set
  | N_ISet_iter
  | N_ISet_mem
  | N_ISet_update
  | N_ISet_size
  (* maps *)
  | N_IEmpty_map
  | N_IMap_map
  | N_IMap_iter
  | N_IMap_mem
  | N_IMap_get
  | N_IMap_update
  | N_IMap_get_and_update
  | N_IMap_size
  (* big maps *)
  | N_IEmpty_big_map
  | N_IBig_map_mem
  | N_IBig_map_get
  | N_IBig_map_update
  | N_IBig_map_get_and_update
  (* string operations *)
  | N_IConcat_string
  | N_IConcat_string_pair
  | N_ISlice_string
  | N_IString_size
  (* bytes operations *)
  | N_IConcat_bytes
  | N_IConcat_bytes_pair
  | N_ISlice_bytes
  | N_IBytes_size
  | N_IOr_bytes
  | N_IAnd_bytes
  | N_IXor_bytes
  | N_INot_bytes
  | N_ILsl_bytes
  | N_ILsr_bytes
  | N_IBytes_nat
  | N_INat_bytes
  | N_IBytes_int
  | N_IInt_bytes
  (* timestamp operations *)
  | N_IAdd_seconds_to_timestamp
  | N_IAdd_timestamp_to_seconds
  | N_ISub_timestamp_seconds
  | N_IDiff_timestamps
  (* currency operations *)
  | N_IAdd_tez
  | N_ISub_tez
  | N_ISub_tez_legacy
  | N_IMul_teznat
  | N_IMul_nattez
  | N_IEdiv_teznat
  | N_IEdiv_tez
  (* boolean operations - assumed O(1) *)
  | N_IOr
  | N_IAnd
  | N_IXor
  | N_INot
  (* integer operations *)
  | N_IIs_nat
  | N_INeg
  | N_IAbs_int
  | N_IInt_nat
  | N_IAdd_int
  | N_IAdd_nat
  | N_ISub_int
  | N_IMul_int
  | N_IMul_nat
  | N_IEdiv_int
  | N_IEdiv_nat
  | N_ILsl_nat
  | N_ILsr_nat
  | N_IOr_nat
  | N_IAnd_nat
  | N_IAnd_int_nat
  | N_IXor_nat
  | N_INot_int
  (* control *)
  | N_IIf
  | N_ILoop
  | N_ILoop_left
  | N_IDip
  | N_IExec
  | N_IApply
  | N_ILambda
  | N_IFailwith
  (* comparison, warning: ad-hoc polymorphic instruction *)
  | N_ICompare
  (* comparators *)
  | N_IEq
  | N_INeq
  | N_ILt
  | N_IGt
  | N_ILe
  | N_IGe
  (* protocol *)
  | N_IAddress
  | N_IContract
  | N_ITransfer_tokens
  | N_IImplicit_account
  | N_ICreate_contract
  | N_ISet_delegate
  (* time *)
  | N_INow
  | N_IMin_block_time
  (* other *)
  | N_IBalance
  | N_ILevel
  | N_IView
  (* We specialize the check-signature instruction for each crypto scheme. *)
  | N_ICheck_signature_ed25519
  | N_ICheck_signature_secp256k1
  | N_ICheck_signature_p256
  | N_ICheck_signature_bls
  | N_IHash_key
  | N_IPack
  | N_IUnpack
  | N_IBlake2b
  | N_ISha256
  | N_ISha512
  | N_ISource
  | N_ISender
  | N_ISelf
  | N_ISelf_address
  | N_IAmount
  | N_ISapling_empty_state
  | N_ISapling_verify_update
  | N_IDig
  | N_IDug
  | N_IDipN
  | N_IDropN
  | N_IChainId
  | N_INever
  | N_IVoting_power
  | N_ITotal_voting_power
  | N_IKeccak
  | N_ISha3
  (* Elliptic curves *)
  | N_IAdd_bls12_381_g1
  | N_IAdd_bls12_381_g2
  | N_IAdd_bls12_381_fr
  | N_IMul_bls12_381_g1
  | N_IMul_bls12_381_g2
  | N_IMul_bls12_381_fr
  | N_INeg_bls12_381_g1
  | N_INeg_bls12_381_g2
  | N_INeg_bls12_381_fr
  | N_IMul_bls12_381_fr_z
  | N_IMul_bls12_381_z_fr
  | N_IInt_bls12_381_z_fr
  | N_IPairing_check_bls12_381
  (* Combs *)
  | N_IComb
  | N_IUncomb
  | N_IComb_get
  | N_IComb_set
  | N_IDupN
  (* Tickets *)
  | N_ITicket
  | N_IRead_ticket
  | N_ISplit_ticket
  | N_IJoin_tickets
  (* Misc *)
  | N_IHalt
  | N_ILog
  (* Timelock*)
  | N_IOpen_chest
  (* Event *)
  | N_IEmit

type continuation_name =
  | N_KNil
  | N_KCons
  | N_KReturn
  | N_KView_exit
  | N_KMap_head
  | N_KUndip
  | N_KLoop_in
  | N_KLoop_in_left
  | N_KIter
  | N_KList_enter_body
  | N_KList_exit_body
  | N_KMap_enter_body
  | N_KMap_exit_body
  | N_KLog

and instr_or_cont_name =
  | Instr_name of instruction_name
  | Cont_name of continuation_name

(* ------------------------------------------------------------------------- *)
(* Code that ought to be auto-generated *)

let string_of_instruction_name : instruction_name -> string =
 fun ir ->
  match ir with
  | N_IDrop -> "N_IDrop"
  | N_IDup -> "N_IDup"
  | N_ISwap -> "N_ISwap"
  | N_IPush -> "N_IPush"
  | N_IUnit -> "N_IUnit"
  | N_ICons_pair -> "N_ICons_pair"
  | N_ICar -> "N_ICar"
  | N_ICdr -> "N_ICdr"
  | N_ICons_some -> "N_ICons_some"
  | N_ICons_none -> "N_ICons_none"
  | N_IIf_none -> "N_IIf_none"
  | N_IOpt_map -> "N_IOpt_map"
  | N_ILeft -> "N_ILeft"
  | N_IRight -> "N_IRight"
  | N_IIf_left -> "N_IIf_left"
  | N_ICons_list -> "N_ICons_list"
  | N_INil -> "N_INil"
  | N_IIf_cons -> "N_IIf_cons"
  | N_IList_map -> "N_IList_map"
  | N_IList_iter -> "N_IList_iter"
  | N_IIter -> "N_IIter"
  | N_IList_size -> "N_IList_size"
  | N_IEmpty_set -> "N_IEmpty_set"
  | N_ISet_iter -> "N_ISet_iter"
  | N_ISet_mem -> "N_ISet_mem"
  | N_ISet_update -> "N_ISet_update"
  | N_ISet_size -> "N_ISet_size"
  | N_IEmpty_map -> "N_IEmpty_map"
  | N_IMap_map -> "N_IMap_map"
  | N_IMap_iter -> "N_IMap_iter"
  | N_IMap_mem -> "N_IMap_mem"
  | N_IMap_get -> "N_IMap_get"
  | N_IMap_update -> "N_IMap_update"
  | N_IMap_size -> "N_IMap_size"
  | N_IEmpty_big_map -> "N_IEmpty_big_map"
  | N_IBig_map_mem -> "N_IBig_map_mem"
  | N_IBig_map_get -> "N_IBig_map_get"
  | N_IBig_map_update -> "N_IBig_map_update"
  | N_IConcat_string -> "N_IConcat_string"
  | N_IConcat_string_pair -> "N_IConcat_string_pair"
  | N_ISlice_string -> "N_ISlice_string"
  | N_IString_size -> "N_IString_size"
  | N_IConcat_bytes -> "N_IConcat_bytes"
  | N_IConcat_bytes_pair -> "N_IConcat_bytes_pair"
  | N_ISlice_bytes -> "N_ISlice_bytes"
  | N_IBytes_size -> "N_IBytes_size"
  | N_IOr_bytes -> "N_IOr_bytes"
  | N_IAnd_bytes -> "N_IAnd_bytes"
  | N_IXor_bytes -> "N_IXor_bytes"
  | N_INot_bytes -> "N_INot_bytes"
  | N_ILsl_bytes -> "N_ILsl_bytes"
  | N_ILsr_bytes -> "N_ILsr_bytes"
  | N_IBytes_nat -> "N_IBytes_nat"
  | N_INat_bytes -> "N_INat_bytes"
  | N_IBytes_int -> "N_IBytes_int"
  | N_IInt_bytes -> "N_IInt_bytes"
  | N_IAdd_seconds_to_timestamp -> "N_IAdd_seconds_to_timestamp"
  | N_IAdd_timestamp_to_seconds -> "N_IAdd_timestamp_to_seconds"
  | N_ISub_timestamp_seconds -> "N_ISub_timestamp_seconds"
  | N_IDiff_timestamps -> "N_IDiff_timestamps"
  | N_IAdd_tez -> "N_IAdd_tez"
  | N_ISub_tez -> "N_ISub_tez"
  | N_ISub_tez_legacy -> "N_ISub_tez_legacy"
  | N_IMul_teznat -> "N_IMul_teznat"
  | N_IMul_nattez -> "N_IMul_nattez"
  | N_IEdiv_teznat -> "N_IEdiv_teznat"
  | N_IEdiv_tez -> "N_IEdiv_tez"
  | N_IOr -> "N_IOr"
  | N_IAnd -> "N_IAnd"
  | N_IXor -> "N_IXor"
  | N_INot -> "N_INot"
  | N_IIs_nat -> "N_IIs_nat"
  | N_INeg -> "N_INeg"
  | N_IAbs_int -> "N_IAbs_int"
  | N_IInt_nat -> "N_IInt_nat"
  | N_IAdd_int -> "N_IAdd_int"
  | N_IAdd_nat -> "N_IAdd_nat"
  | N_ISub_int -> "N_ISub_int"
  | N_IMul_int -> "N_IMul_int"
  | N_IMul_nat -> "N_IMul_nat"
  | N_IEdiv_int -> "N_IEdiv_int"
  | N_IEdiv_nat -> "N_IEdiv_nat"
  | N_ILsl_nat -> "N_ILsl_nat"
  | N_ILsr_nat -> "N_ILsr_nat"
  | N_IOr_nat -> "N_IOr_nat"
  | N_IAnd_nat -> "N_IAnd_nat"
  | N_IAnd_int_nat -> "N_IAnd_int_nat"
  | N_IXor_nat -> "N_IXor_nat"
  | N_INot_int -> "N_INot_int"
  | N_IIf -> "N_IIf"
  | N_ILoop -> "N_ILoop"
  | N_ILoop_left -> "N_ILoop_left"
  | N_IDip -> "N_IDip"
  | N_IExec -> "N_IExec"
  | N_IApply -> "N_IApply"
  | N_ILambda -> "N_ILambda"
  | N_IFailwith -> "N_IFailwith"
  | N_ICompare -> "N_ICompare"
  | N_IEq -> "N_IEq"
  | N_INeq -> "N_INeq"
  | N_ILt -> "N_ILt"
  | N_IGt -> "N_IGt"
  | N_ILe -> "N_ILe"
  | N_IGe -> "N_IGe"
  | N_IAddress -> "N_IAddress"
  | N_IContract -> "N_IContract"
  | N_ITransfer_tokens -> "N_ITransfer_tokens"
  | N_IImplicit_account -> "N_IImplicit_account"
  | N_ICreate_contract -> "N_ICreate_contract"
  | N_ISet_delegate -> "N_ISet_delegate"
  | N_INow -> "N_INow"
  | N_IMin_block_time -> "N_IMin_block_time"
  | N_IBalance -> "N_IBalance"
  | N_ICheck_signature_ed25519 -> "N_ICheck_signature_ed25519"
  | N_ICheck_signature_secp256k1 -> "N_ICheck_signature_secp256k1"
  | N_ICheck_signature_p256 -> "N_ICheck_signature_p256"
  | N_ICheck_signature_bls -> "N_ICheck_signature_bls"
  | N_IHash_key -> "N_IHash_key"
  | N_IPack -> "N_IPack"
  | N_IUnpack -> "N_IUnpack"
  | N_IBlake2b -> "N_IBlake2b"
  | N_ISha256 -> "N_ISha256"
  | N_ISha512 -> "N_ISha512"
  | N_ISource -> "N_ISource"
  | N_ISender -> "N_ISender"
  | N_ISelf -> "N_ISelf"
  | N_IAmount -> "N_IAmount"
  | N_IDig -> "N_IDig"
  | N_IDug -> "N_IDug"
  | N_IDipN -> "N_IDipN"
  | N_IDropN -> "N_IDropN"
  | N_IDupN -> "N_IDupN"
  | N_IChainId -> "N_IChainId"
  | N_ILevel -> "N_ILevel"
  | N_IView -> "N_IView"
  | N_ISelf_address -> "N_ISelf_address"
  | N_INever -> "N_INever"
  | N_IUnpair -> "N_IUnpair"
  | N_IVoting_power -> "N_IVoting_power"
  | N_ITotal_voting_power -> "N_ITotal_voting_power"
  | N_IKeccak -> "N_IKeccak"
  | N_ISha3 -> "N_ISha3"
  | N_IAdd_bls12_381_g1 -> "N_IAdd_bls12_381_g1"
  | N_IAdd_bls12_381_g2 -> "N_IAdd_bls12_381_g2"
  | N_IAdd_bls12_381_fr -> "N_IAdd_bls12_381_fr"
  | N_IMul_bls12_381_g1 -> "N_IMul_bls12_381_g1"
  | N_IMul_bls12_381_g2 -> "N_IMul_bls12_381_g2"
  | N_IMul_bls12_381_fr -> "N_IMul_bls12_381_fr"
  | N_INeg_bls12_381_g1 -> "N_INeg_bls12_381_g1"
  | N_INeg_bls12_381_g2 -> "N_INeg_bls12_381_g2"
  | N_INeg_bls12_381_fr -> "N_INeg_bls12_381_fr"
  | N_IPairing_check_bls12_381 -> "N_IPairing_check_bls12_381"
  | N_IMul_bls12_381_fr_z -> "N_IMul_bls12_381_fr_z"
  | N_IMul_bls12_381_z_fr -> "N_IMul_bls12_381_z_fr"
  | N_IInt_bls12_381_z_fr -> "N_IInt_bls12_381_z_fr"
  | N_IComb -> "N_IComb"
  | N_IUncomb -> "N_IUncomb"
  | N_IComb_get -> "N_IComb_get"
  | N_IComb_set -> "N_IComb_set"
  | N_ITicket -> "N_ITicket"
  | N_IRead_ticket -> "N_IRead_ticket"
  | N_ISplit_ticket -> "N_ISplit_ticket"
  | N_IJoin_tickets -> "N_IJoin_tickets"
  | N_ISapling_empty_state -> "N_ISapling_empty_state"
  | N_ISapling_verify_update -> "N_ISapling_verify_update"
  | N_IMap_get_and_update -> "N_IMap_get_and_update"
  | N_IBig_map_get_and_update -> "N_IBig_map_get_and_update"
  | N_IHalt -> "N_IHalt"
  | N_ILog -> "N_ILog"
  | N_IOpen_chest -> "N_IOpen_chest"
  | N_IEmit -> "N_IEmit"

let string_of_continuation_name : continuation_name -> string =
 fun c ->
  match c with
  | N_KNil -> "N_KNil"
  | N_KCons -> "N_KCons"
  | N_KReturn -> "N_KReturn"
  | N_KView_exit -> "N_KView_exit"
  | N_KMap_head -> "N_KMap_head"
  | N_KUndip -> "N_KUndip"
  | N_KLoop_in -> "N_KLoop_in"
  | N_KLoop_in_left -> "N_KLoop_in_left"
  | N_KIter -> "N_KIter"
  | N_KList_enter_body -> "N_KList_enter_body"
  | N_KList_exit_body -> "N_KList_exit_body"
  | N_KMap_enter_body -> "N_KMap_enter_body"
  | N_KMap_exit_body -> "N_KMap_exit_body"
  | N_KLog -> "N_KLog"

let string_of_instr_or_cont name =
  match name with
  | Instr_name instr_name -> string_of_instruction_name instr_name
  | Cont_name cont_name -> string_of_continuation_name cont_name

(* ------------------------------------------------------------------------- *)

type args = arg list

and arg = {name : id; arg : Size.t}

let nullary : args = []

let unary xn x : args = [{name = xn; arg = x}]

let binary xn x yn y : args = {name = xn; arg = x} :: unary yn y

let ternary xn x yn y zn z : args = {name = xn; arg = x} :: binary yn y zn z

let quaternary wn w xn x yn y zn z : args =
  {name = wn; arg = w} :: ternary xn x yn y zn z

let pp_arg fmtr {name; arg} = Format.fprintf fmtr "%s = %a" name Size.pp arg

let pp_args fmtr args =
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
    pp_arg
    fmtr
    args

type ir_sized_step = {name : instr_or_cont_name; args : args}

type t = ir_sized_step list

let ir_sized_step instr_name args = {name = Instr_name instr_name; args}

let cont_sized_step cont_name args = {name = Cont_name cont_name; args}

(* ------------------------------------------------------------------------- *)

(* Changing the ordering breaks the workload file compatibility *)
let all_instructions =
  [
    N_IDrop;
    N_IDup;
    N_ISwap;
    N_IPush;
    N_ICons_pair;
    N_ICar;
    N_ICdr;
    N_ICons_some;
    N_ICons_none;
    N_IIf_none;
    N_IOpt_map;
    N_ILeft;
    N_IRight;
    N_IIf_left;
    N_ICons_list;
    N_INil;
    N_IIf_cons;
    N_IList_map;
    N_IList_iter;
    N_IIter;
    N_IList_size;
    N_IEmpty_set;
    N_ISet_iter;
    N_ISet_mem;
    N_ISet_update;
    N_ISet_size;
    N_IEmpty_map;
    N_IMap_map;
    N_IMap_iter;
    N_IMap_mem;
    N_IMap_get;
    N_IMap_update;
    N_IMap_size;
    N_IEmpty_big_map;
    N_IBig_map_mem;
    N_IBig_map_get;
    N_IBig_map_update;
    N_IConcat_string;
    N_IConcat_string_pair;
    N_ISlice_string;
    N_IString_size;
    N_IConcat_bytes;
    N_IConcat_bytes_pair;
    N_ISlice_bytes;
    N_IBytes_size;
    N_IBytes_nat;
    N_INat_bytes;
    N_IBytes_int;
    N_IInt_bytes;
    N_IAdd_seconds_to_timestamp;
    N_IAdd_timestamp_to_seconds;
    N_ISub_timestamp_seconds;
    N_IDiff_timestamps;
    N_IAdd_tez;
    N_ISub_tez;
    N_ISub_tez_legacy;
    N_IMul_teznat;
    N_IMul_nattez;
    N_IEdiv_teznat;
    N_IEdiv_tez;
    N_IOr;
    N_IAnd;
    N_IXor;
    N_INot;
    N_IIs_nat;
    N_INeg;
    N_IAbs_int;
    N_IInt_nat;
    N_IAdd_int;
    N_IAdd_nat;
    N_ISub_int;
    N_IMul_int;
    N_IMul_nat;
    N_IEdiv_int;
    N_IEdiv_nat;
    N_ILsl_nat;
    N_ILsr_nat;
    N_IOr_nat;
    N_IAnd_nat;
    N_IAnd_int_nat;
    N_IXor_nat;
    N_INot_int;
    N_IIf;
    N_ILoop;
    N_ILoop_left;
    N_IDip;
    N_IExec;
    N_IApply;
    N_ILambda;
    N_IFailwith;
    N_ICompare;
    N_IEq;
    N_INeq;
    N_ILt;
    N_IGt;
    N_ILe;
    N_IGe;
    N_IAddress;
    N_IContract;
    N_ITransfer_tokens;
    N_IImplicit_account;
    N_ICreate_contract;
    N_ISet_delegate;
    N_INow;
    N_IMin_block_time;
    N_IBalance;
    N_ICheck_signature_ed25519;
    N_ICheck_signature_secp256k1;
    N_ICheck_signature_p256;
    N_ICheck_signature_bls;
    N_IHash_key;
    N_IPack;
    N_IUnpack;
    N_IBlake2b;
    N_ISha256;
    N_ISha512;
    N_ISource;
    N_ISender;
    N_ISelf;
    N_IAmount;
    N_IDig;
    N_IDug;
    N_IDipN;
    N_IDropN;
    N_IDupN;
    N_IChainId;
    N_ILevel;
    N_IView;
    N_ISelf_address;
    N_INever;
    N_IUnpair;
    N_IVoting_power;
    N_ITotal_voting_power;
    N_IKeccak;
    N_ISha3;
    N_IAdd_bls12_381_g1;
    N_IAdd_bls12_381_g2;
    N_IAdd_bls12_381_fr;
    N_IMul_bls12_381_g1;
    N_IMul_bls12_381_g2;
    N_IMul_bls12_381_fr;
    N_INeg_bls12_381_g1;
    N_INeg_bls12_381_g2;
    N_INeg_bls12_381_fr;
    N_IPairing_check_bls12_381;
    N_IMul_bls12_381_fr_z;
    N_IMul_bls12_381_z_fr;
    N_IInt_bls12_381_z_fr;
    N_IComb;
    N_IUncomb;
    N_IComb_get;
    N_IComb_set;
    N_ITicket;
    N_IRead_ticket;
    N_ISplit_ticket;
    N_IJoin_tickets;
    N_ISapling_empty_state;
    N_ISapling_verify_update;
    N_IMap_get_and_update;
    N_IBig_map_get_and_update;
    N_IHalt;
    N_ILog;
    N_IOpen_chest;
    N_IEmit;
    N_ILsl_bytes;
    N_ILsr_bytes;
    N_IOr_bytes;
    N_IAnd_bytes;
    N_IXor_bytes;
    N_INot_bytes;
    N_IUnit;
  ]

(* Changing the ordering breaks the workload file compatibility *)
let all_continuations =
  [
    N_KNil;
    N_KCons;
    N_KReturn;
    N_KView_exit;
    N_KMap_head;
    N_KUndip;
    N_KLoop_in;
    N_KLoop_in_left;
    N_KIter;
    N_KList_enter_body;
    N_KList_exit_body;
    N_KMap_enter_body;
    N_KMap_exit_body;
    N_KLog;
  ]

let instruction_name_encoding =
  let open Data_encoding in
  def "instruction_name_encoding"
  @@ string_enum
       (List.map
          (fun instr_name ->
            (string_of_instruction_name instr_name, instr_name))
          all_instructions)

let continuation_name_encoding =
  let open Data_encoding in
  def "continuation_name_encoding"
  @@ string_enum
       (List.map
          (fun cont_name -> (string_of_continuation_name cont_name, cont_name))
          all_continuations)

let args_encoding =
  let open Data_encoding in
  def "args_encoding"
  @@ list
       (conv
          (fun {name; arg} -> (name, arg))
          (fun (name, arg) -> {name; arg})
          (tup2 string Size.encoding))

let instr_or_cont_name_encoding =
  let open Data_encoding in
  def "instr_or_cont_name"
  @@ union
       [
         case
           ~title:"instr_name"
           (Tag 0)
           instruction_name_encoding
           (function Instr_name name -> Some name | _ -> None)
           (fun name -> Instr_name name);
         case
           ~title:"cont_name"
           (Tag 1)
           continuation_name_encoding
           (function Cont_name name -> Some name | _ -> None)
           (fun name -> Cont_name name);
       ]

let ir_sized_step_encoding =
  let open Data_encoding in
  def "ir_sized_step_encoding"
  @@ conv
       (fun {name; args} -> (name, args))
       (fun (name, args) -> {name; args})
       (tup2 instr_or_cont_name_encoding args_encoding)

let encoding =
  let open Data_encoding in
  def "interpreter_trace_encoding" @@ list ir_sized_step_encoding

(* ------------------------------------------------------------------------- *)

module Instructions = struct
  let drop = ir_sized_step N_IDrop nullary

  let dup = ir_sized_step N_IDup nullary

  let swap = ir_sized_step N_ISwap nullary

  let push = ir_sized_step N_IPush nullary

  let unit = ir_sized_step N_IUnit nullary

  let cons_pair = ir_sized_step N_ICons_pair nullary

  let car = ir_sized_step N_ICar nullary

  let cdr = ir_sized_step N_ICdr nullary

  let cons_some = ir_sized_step N_ICons_some nullary

  let cons_none = ir_sized_step N_ICons_none nullary

  let if_none = ir_sized_step N_IIf_none nullary

  let opt_map ~is_some =
    ir_sized_step N_IOpt_map (unary "is_some" (if is_some then 1 else 0))

  let left = ir_sized_step N_ILeft nullary

  let right = ir_sized_step N_IRight nullary

  let if_left = ir_sized_step N_IIf_left nullary

  let cons_list = ir_sized_step N_ICons_list nullary

  let nil = ir_sized_step N_INil nullary

  let if_cons = ir_sized_step N_IIf_cons nullary

  let list_map = ir_sized_step N_IList_map nullary

  let list_iter = ir_sized_step N_IList_iter nullary

  let iter = ir_sized_step N_IIter nullary

  let list_size _list = ir_sized_step N_IList_size nullary

  let empty_set = ir_sized_step N_IEmpty_set nullary

  let set_iter set = ir_sized_step N_ISet_iter (unary "set" set)

  let set_mem elt set = ir_sized_step N_ISet_mem (binary "elt" elt "set" set)

  let set_update elt set =
    ir_sized_step N_ISet_update (binary "elt" elt "set" set)

  let set_size _set = ir_sized_step N_ISet_size nullary

  let empty_map = ir_sized_step N_IEmpty_map nullary

  let map_map map = ir_sized_step N_IMap_map (unary "map" map)

  let map_iter map = ir_sized_step N_IMap_iter (unary "map" map)

  let map_mem key map = ir_sized_step N_IMap_mem (binary "key" key "map" map)

  let map_get key map = ir_sized_step N_IMap_get (binary "key" key "map" map)

  let map_update key map =
    ir_sized_step N_IMap_update (binary "key" key "map" map)

  let map_size _map = ir_sized_step N_IMap_size nullary

  let empty_big_map = ir_sized_step N_IEmpty_big_map nullary

  let big_map_mem key big_map =
    ir_sized_step N_IBig_map_mem (binary "key" key "big_map" big_map)

  let big_map_get key big_map =
    ir_sized_step N_IBig_map_get (binary "key" key "big_map" big_map)

  let big_map_update key big_map =
    ir_sized_step N_IBig_map_update (binary "key" key "big_map" big_map)

  let big_map_get_and_update key big_map =
    ir_sized_step N_IBig_map_get_and_update (binary "key" key "big_map" big_map)

  let concat_string total_bytes list =
    ir_sized_step
      N_IConcat_string
      (binary "total_bytes" total_bytes "list" list)

  let concat_string_pair str1 str2 =
    ir_sized_step N_IConcat_string_pair (binary "str1" str1 "str2" str2)

  let slice_string string =
    ir_sized_step N_ISlice_string (unary "string" string)

  let string_size _string = ir_sized_step N_IString_size nullary

  let concat_bytes total_bytes list =
    ir_sized_step N_IConcat_bytes (binary "total_bytes" total_bytes "list" list)

  let concat_bytes_pair str1 str2 =
    ir_sized_step N_IConcat_bytes_pair (binary "str1" str1 "str2" str2)

  let slice_bytes bytes = ir_sized_step N_ISlice_bytes (unary "bytes" bytes)

  let bytes_size = ir_sized_step N_IBytes_size nullary

  let lsl_bytes bytes shift =
    ir_sized_step N_ILsl_bytes (binary "bytes" bytes "shift" shift)

  let lsr_bytes bytes shift =
    ir_sized_step N_ILsr_bytes (binary "bytes" bytes "shift" shift)

  let or_bytes bytes1 bytes2 =
    ir_sized_step N_IOr_bytes (binary "bytes1" bytes1 "bytes2" bytes2)

  let and_bytes bytes1 bytes2 =
    ir_sized_step N_IAnd_bytes (binary "bytes1" bytes1 "bytes2" bytes2)

  let xor_bytes bytes1 bytes2 =
    ir_sized_step N_IXor_bytes (binary "bytes1" bytes1 "bytes2" bytes2)

  let not_bytes bytes = ir_sized_step N_INot_bytes (unary "bytes" bytes)

  let bytes_nat nat = ir_sized_step N_IBytes_nat (unary "nat" nat)

  let nat_bytes bytes = ir_sized_step N_INat_bytes (unary "bytes" bytes)

  let bytes_int int = ir_sized_step N_IBytes_int (unary "int" int)

  let int_bytes bytes = ir_sized_step N_IInt_bytes (unary "bytes" bytes)

  let add_seconds_to_timestamp seconds tstamp =
    ir_sized_step
      N_IAdd_seconds_to_timestamp
      (binary "seconds" seconds "tstamp" tstamp)

  let add_timestamp_to_seconds tstamp seconds =
    ir_sized_step
      N_IAdd_timestamp_to_seconds
      (binary "tstamp" tstamp "seconds" seconds)

  let sub_timestamp_seconds tstamp seconds =
    ir_sized_step
      N_ISub_timestamp_seconds
      (binary "tstamp" tstamp "seconds" seconds)

  let diff_timestamps tstamp1 tstamp2 =
    ir_sized_step
      N_IDiff_timestamps
      (binary "tstamp1" tstamp1 "tstamp2" tstamp2)

  let add_tez _tez1 _tez2 = ir_sized_step N_IAdd_tez nullary

  let sub_tez _tez1 _tez2 = ir_sized_step N_ISub_tez nullary

  let sub_tez_legacy _tez1 _tez2 = ir_sized_step N_ISub_tez_legacy nullary

  let mul_teznat _tez _nat = ir_sized_step N_IMul_teznat nullary

  let mul_nattez _nat _tez = ir_sized_step N_IMul_nattez nullary

  let ediv_teznat _tez _nat = ir_sized_step N_IEdiv_teznat nullary

  let ediv_tez _tez1 _tez2 = ir_sized_step N_IEdiv_tez nullary

  let or_ = ir_sized_step N_IOr nullary

  let and_ = ir_sized_step N_IAnd nullary

  let xor_ = ir_sized_step N_IXor nullary

  let not_ = ir_sized_step N_INot nullary

  let is_nat _int = ir_sized_step N_IIs_nat nullary

  let neg int = ir_sized_step N_INeg (unary "int" int)

  let abs_int int = ir_sized_step N_IAbs_int (unary "int" int)

  let int_nat _nat = ir_sized_step N_IInt_nat nullary

  let add_int int1 int2 =
    ir_sized_step N_IAdd_int (binary "int1" int1 "int2" int2)

  let add_nat nat1 nat2 =
    ir_sized_step N_IAdd_nat (binary "nat1" nat1 "nat2" nat2)

  let sub_int int1 int2 =
    ir_sized_step N_ISub_int (binary "int1" int1 "int2" int2)

  let mul_int int1 int2 =
    ir_sized_step N_IMul_int (binary "int1" int1 "int2" int2)

  let mul_nat nat int = ir_sized_step N_IMul_nat (binary "nat" nat "int" int)

  let ediv_int int1 int2 =
    ir_sized_step N_IEdiv_int (binary "int1" int1 "int2" int2)

  let ediv_nat nat int = ir_sized_step N_IEdiv_nat (binary "nat" nat "int" int)

  let lsl_nat nat1 _shift = ir_sized_step N_ILsl_nat (unary "nat" nat1)

  let lsr_nat nat1 _shift = ir_sized_step N_ILsr_nat (unary "nat" nat1)

  let or_nat nat1 nat2 =
    ir_sized_step N_IOr_nat (binary "nat1" nat1 "nat2" nat2)

  let and_nat nat1 nat2 =
    ir_sized_step N_IAnd_nat (binary "nat1" nat1 "nat2" nat2)

  let and_int_nat int nat =
    ir_sized_step N_IAnd_int_nat (binary "int" int "nat" nat)

  let xor_nat nat1 nat2 =
    ir_sized_step N_IXor_nat (binary "nat1" nat1 "nat2" nat2)

  let not_int int = ir_sized_step N_INot_int (unary "int" int)

  let if_ = ir_sized_step N_IIf nullary

  let loop = ir_sized_step N_ILoop nullary

  let loop_left = ir_sized_step N_ILoop_left nullary

  let dip = ir_sized_step N_IDip nullary

  let exec = ir_sized_step N_IExec nullary

  let apply ~(rec_flag : bool) =
    ir_sized_step N_IApply (unary "rec" (if rec_flag then 1 else 0))

  let lambda ~(rec_flag : bool) =
    ir_sized_step N_ILambda (unary "rec" (if rec_flag then 1 else 0))

  let failwith_ = ir_sized_step N_IFailwith nullary

  let compare arg1 arg2 =
    ir_sized_step N_ICompare (binary "arg1" arg1 "arg2" arg2)

  let eq = ir_sized_step N_IEq nullary

  let neq = ir_sized_step N_INeq nullary

  let lt = ir_sized_step N_ILt nullary

  let gt = ir_sized_step N_IGt nullary

  let le = ir_sized_step N_ILe nullary

  let ge = ir_sized_step N_IGe nullary

  let address = ir_sized_step N_IAddress nullary

  let contract = ir_sized_step N_IContract nullary

  let transfer_tokens = ir_sized_step N_ITransfer_tokens nullary

  let implicit_account = ir_sized_step N_IImplicit_account nullary

  let create_contract = ir_sized_step N_ICreate_contract nullary

  let set_delegate = ir_sized_step N_ISet_delegate nullary

  let now = ir_sized_step N_INow nullary

  let min_block_time = ir_sized_step N_IMin_block_time nullary

  let balance = ir_sized_step N_IBalance nullary

  let check_signature_ed25519 _pk _signature message =
    ir_sized_step N_ICheck_signature_ed25519 (unary "message" message)

  let check_signature_secp256k1 _pk _signature message =
    ir_sized_step N_ICheck_signature_secp256k1 (unary "message" message)

  let check_signature_p256 _pk _signature message =
    ir_sized_step N_ICheck_signature_p256 (unary "message" message)

  let check_signature_bls _pk _signature message =
    ir_sized_step N_ICheck_signature_bls (unary "message" message)

  let hash_key = ir_sized_step N_IHash_key nullary

  let pack (micheline_size : Size.micheline_size) =
    ir_sized_step
      N_IPack
      (ternary
         "micheline_nodes"
         micheline_size.traversal
         "micheline_int_bytes"
         micheline_size.int_bytes
         "micheline_string_bytes"
         micheline_size.string_bytes)

  let unpack = ir_sized_step N_IUnpack nullary

  let blake2b bytes = ir_sized_step N_IBlake2b (unary "bytes" bytes)

  let sha256 bytes = ir_sized_step N_ISha256 (unary "bytes" bytes)

  let sha512 bytes = ir_sized_step N_ISha512 (unary "bytes" bytes)

  let source = ir_sized_step N_ISource nullary

  let sender = ir_sized_step N_ISender nullary

  let self = ir_sized_step N_ISelf nullary

  let amount = ir_sized_step N_IAmount nullary

  let dig depth = ir_sized_step N_IDig (unary "depth" depth)

  let dug depth = ir_sized_step N_IDug (unary "depth" depth)

  let dipn depth = ir_sized_step N_IDipN (unary "depth" depth)

  let dropn depth = ir_sized_step N_IDropN (unary "depth" depth)

  let dupn depth = ir_sized_step N_IDupN (unary "depth" depth)

  let chain_id = ir_sized_step N_IChainId nullary

  let level = ir_sized_step N_ILevel nullary

  let view = ir_sized_step N_IView nullary

  let self_address = ir_sized_step N_ISelf_address nullary

  let never = ir_sized_step N_INever nullary

  let unpair = ir_sized_step N_IUnpair nullary

  let voting_power = ir_sized_step N_IVoting_power nullary

  let total_voting_power = ir_sized_step N_ITotal_voting_power nullary

  let keccak bytes = ir_sized_step N_IKeccak (unary "bytes" bytes)

  let sha3 bytes = ir_sized_step N_ISha3 (unary "bytes" bytes)

  let add_bls12_381_g1 = ir_sized_step N_IAdd_bls12_381_g1 nullary

  let add_bls12_381_g2 = ir_sized_step N_IAdd_bls12_381_g2 nullary

  let add_bls12_381_fr = ir_sized_step N_IAdd_bls12_381_fr nullary

  let mul_bls12_381_g1 = ir_sized_step N_IMul_bls12_381_g1 nullary

  let mul_bls12_381_g2 = ir_sized_step N_IMul_bls12_381_g2 nullary

  let mul_bls12_381_fr = ir_sized_step N_IMul_bls12_381_fr nullary

  let neg_bls12_381_g1 = ir_sized_step N_INeg_bls12_381_g1 nullary

  let neg_bls12_381_g2 = ir_sized_step N_INeg_bls12_381_g2 nullary

  let neg_bls12_381_fr = ir_sized_step N_INeg_bls12_381_fr nullary

  let pairing_check_bls12_381 length =
    ir_sized_step N_IPairing_check_bls12_381 (unary "length" length)

  let mul_bls12_381_fr_z nat =
    ir_sized_step N_IMul_bls12_381_fr_z (unary "nat" nat)

  let mul_bls12_381_z_fr nat =
    ir_sized_step N_IMul_bls12_381_z_fr (unary "nat" nat)

  let int_bls12_381_z_fr = ir_sized_step N_IInt_bls12_381_z_fr nullary

  let comb depth = ir_sized_step N_IComb (unary "depth" depth)

  let uncomb depth = ir_sized_step N_IUncomb (unary "depth" depth)

  let comb_get key = ir_sized_step N_IComb_get (unary "key" key)

  let comb_set key = ir_sized_step N_IComb_set (unary "key" key)

  let ticket = ir_sized_step N_ITicket nullary

  let read_ticket = ir_sized_step N_IRead_ticket nullary

  let split_ticket nat1 nat2 =
    ir_sized_step N_ISplit_ticket (binary "nat1" nat1 "nat2" nat2)

  let join_tickets size1 size2 size3 size4 =
    ir_sized_step
      N_IJoin_tickets
      (quaternary
         "contents1"
         size1
         "contents2"
         size2
         "amount1"
         size3
         "amount2"
         size4)

  let sapling_empty_state = ir_sized_step N_ISapling_empty_state nullary

  let sapling_verify_update inputs outputs _bound_data _state =
    ir_sized_step
      N_ISapling_verify_update
      (binary "inputs" inputs "outputs" outputs)

  let map_get_and_update key_size map_size =
    ir_sized_step
      N_IMap_get_and_update
      (binary "key_size" key_size "map_size" map_size)

  let halt = ir_sized_step N_IHalt nullary

  let log = ir_sized_step N_ILog nullary

  let open_chest log_time size =
    ir_sized_step N_IOpen_chest (binary "log_time" log_time "size" size)

  (** cost model for the EMIT instruction *)
  let emit = ir_sized_step N_IEmit nullary
end

module Control = struct
  let nil = cont_sized_step N_KNil nullary

  let cons = cont_sized_step N_KCons nullary

  let return = cont_sized_step N_KReturn nullary

  let view_exit = cont_sized_step N_KView_exit nullary

  let map_head = cont_sized_step N_KMap_head nullary

  let undip = cont_sized_step N_KUndip nullary

  let loop_in = cont_sized_step N_KLoop_in nullary

  let loop_in_left = cont_sized_step N_KLoop_in_left nullary

  let iter size = cont_sized_step N_KIter (unary "size" size)

  let list_enter_body xs_size ys_size =
    cont_sized_step
      N_KList_enter_body
      (binary "xs_size" xs_size "ys_size" ys_size)

  let list_exit_body = cont_sized_step N_KList_exit_body nullary

  let map_enter_body size =
    cont_sized_step N_KMap_enter_body (unary "size" size)

  let map_exit_body key_size map_size =
    cont_sized_step N_KMap_exit_body (binary "key" key_size "map" map_size)

  let log = cont_sized_step N_KLog nullary
end

(* ------------------------------------------------------------------------- *)

open Script_typed_ir

let extract_compare_sized_step :
    type a. a comparable_ty -> a -> a -> ir_sized_step =
 fun comparable_ty x y ->
  Instructions.compare
    (Size.size_of_comparable_value comparable_ty x)
    (Size.size_of_comparable_value comparable_ty y)

let extract_ir_sized_step :
    type bef_top bef res_top res.
    Alpha_context.t ->
    (bef_top, bef, res_top, res) Script_typed_ir.kinstr ->
    bef_top * bef ->
    ir_sized_step =
 fun ctxt instr stack ->
  let open Script_typed_ir in
  match (instr, stack) with
  | IDrop (_, _), _ -> Instructions.drop
  | IDup (_, _), _ -> Instructions.dup
  | ISwap (_, _), _ -> Instructions.swap
  | IPush (_, _, _, _), _ -> Instructions.push
  | IUnit (_, _), _ -> Instructions.unit
  | ICons_pair (_, _), _ -> Instructions.cons_pair
  | ICar (_, _), _ -> Instructions.car
  | ICdr (_, _), _ -> Instructions.cdr
  | IUnpair (_, _), _ -> Instructions.unpair
  | ICons_some (_, _), _ -> Instructions.cons_some
  | ICons_none (_, _, _), _ -> Instructions.cons_none
  | IIf_none _, _ -> Instructions.if_none
  | IOpt_map _, (opt, _) ->
      let is_some = match opt with None -> false | Some _ -> true in
      Instructions.opt_map ~is_some
  | ICons_left (_, _, _), _ -> Instructions.left
  | ICons_right (_, _, _), _ -> Instructions.right
  | IIf_left _, _ -> Instructions.if_left
  | ICons_list (_, _), _ -> Instructions.cons_list
  | INil (_, _, _), _ -> Instructions.nil
  | IIf_cons _, _ -> Instructions.if_cons
  | IList_iter (_, _, _, _), _ -> Instructions.list_iter
  | IList_map (_, _, _, _), _ -> Instructions.list_map
  | IList_size (_, _), (list, _) -> Instructions.list_size (Size.list list)
  | IEmpty_set (_, _, _), _ -> Instructions.empty_set
  | ISet_iter _, (set, _) -> Instructions.set_iter (Size.set set)
  | ISet_mem (_, _), (v, (set, _)) ->
      let (module S) = Script_set.get set in
      let sz = S.OPS.elt_size v in
      Instructions.set_mem sz (Size.set set)
  | ISet_update (_, _), (v, (_flag, (set, _))) ->
      let (module S) = Script_set.get set in
      let sz = S.OPS.elt_size v in
      Instructions.set_update sz (Size.set set)
  | ISet_size (_, _), (set, _) -> Instructions.set_size (Size.set set)
  | IEmpty_map (_, _, _, _), _ -> Instructions.empty_map
  | IMap_map _, (map, _) -> Instructions.map_map (Size.map map)
  | IMap_iter _, (map, _) -> Instructions.map_iter (Size.map map)
  | IMap_mem (_, _), (v, (map, _)) ->
      let (module Map) = Script_map.get_module map in
      let key_size = Map.OPS.key_size v in
      Instructions.map_mem key_size (Size.map map)
  | IMap_get (_, _), (v, (map, _)) ->
      let (module Map) = Script_map.get_module map in
      let key_size = Map.OPS.key_size v in
      Instructions.map_get key_size (Size.map map)
  | IMap_update (_, _), (v, (_elt_opt, (map, _))) ->
      let (module Map) = Script_map.get_module map in
      let key_size = Map.OPS.key_size v in
      Instructions.map_update key_size (Size.map map)
  | IMap_get_and_update (_, _), (v, (_elt_opt, (map, _))) ->
      let (module Map) = Script_map.get_module map in
      let key_size = Map.OPS.key_size v in
      Instructions.map_get_and_update key_size (Size.map map)
  | IMap_size (_, _), (map, _) -> Instructions.map_size (Size.map map)
  | IEmpty_big_map (_, _, _, _), _ -> Instructions.empty_big_map
  | IBig_map_mem (_, _), (v, (Big_map {diff = {size; _}; key_type; _}, _)) ->
      let key_size = Size.size_of_comparable_value key_type v in
      Instructions.big_map_mem key_size (Size.of_int size)
  | IBig_map_get (_, _), (v, (Big_map {diff = {size; _}; key_type; _}, _)) ->
      let key_size = Size.size_of_comparable_value key_type v in
      Instructions.big_map_get key_size (Size.of_int size)
  | ( IBig_map_update (_, _),
      (v, (_, (Big_map {diff = {size; _}; key_type; _}, _))) ) ->
      let key_size = Size.size_of_comparable_value key_type v in
      Instructions.big_map_update key_size (Size.of_int size)
  | ( IBig_map_get_and_update (_, _),
      (v, (_, (Big_map {diff = {size; _}; key_type; _}, _))) ) ->
      let key_size = Size.size_of_comparable_value key_type v in
      Instructions.big_map_get_and_update key_size (Size.of_int size)
  | IConcat_string (_, _), (ss, _) ->
      let list_size = Size.list ss in
      let total_bytes =
        List.fold_left
          (fun x s -> Size.(add x (script_string s)))
          Size.zero
          ss.elements
      in
      Instructions.concat_string list_size total_bytes
  | IConcat_string_pair (_, _), (s1, (s2, _)) ->
      Instructions.concat_string_pair
        (Size.script_string s1)
        (Size.script_string s2)
  | ISlice_string (_, _), (_off, (_len, (s, _))) ->
      Instructions.slice_string (Size.script_string s)
  | IString_size (_, _), (s, _) ->
      Instructions.string_size (Size.script_string s)
  | IConcat_bytes (_, _), (ss, _) ->
      let list_size = Size.list ss in
      let total_bytes =
        List.fold_left (fun x s -> Size.(add x (bytes s))) Size.zero ss.elements
      in
      Instructions.concat_bytes list_size total_bytes
  | IConcat_bytes_pair (_, _), (s1, (s2, _)) ->
      Instructions.concat_bytes_pair (Size.bytes s1) (Size.bytes s2)
  | ISlice_bytes (_, _), (_off, (_len, (s, _))) ->
      Instructions.slice_bytes (Size.bytes s)
  | IBytes_size (_, _), _ -> Instructions.bytes_size
  | IBytes_nat (_, _), (n, _) -> Instructions.bytes_nat (Size.integer n)
  | INat_bytes (_, _), (b, _) -> Instructions.nat_bytes (Size.bytes b)
  | IBytes_int (_, _), (n, _) -> Instructions.bytes_int (Size.integer n)
  | IInt_bytes (_, _), (b, _) -> Instructions.int_bytes (Size.bytes b)
  | IAdd_seconds_to_timestamp (_, _), (s, (t, _)) ->
      Instructions.add_seconds_to_timestamp (Size.timestamp t) (Size.integer s)
  | IAdd_timestamp_to_seconds (_, _), (t, (s, _)) ->
      Instructions.add_timestamp_to_seconds (Size.timestamp t) (Size.integer s)
  | ISub_timestamp_seconds (_, _), (t, (s, _)) ->
      Instructions.sub_timestamp_seconds (Size.timestamp t) (Size.integer s)
  | IDiff_timestamps (_, _), (t1, (t2, _)) ->
      Instructions.diff_timestamps (Size.timestamp t1) (Size.timestamp t2)
  | IAdd_tez (_, _), (x, (y, _)) ->
      Instructions.add_tez (Size.mutez x) (Size.mutez y)
  | ISub_tez (_, _), (x, (y, _)) ->
      Instructions.sub_tez (Size.mutez x) (Size.mutez y)
  | ISub_tez_legacy (_, _), (x, (y, _)) ->
      Instructions.sub_tez_legacy (Size.mutez x) (Size.mutez y)
  | IMul_teznat (_, _), (x, (y, _)) ->
      Instructions.mul_teznat (Size.mutez x) (Size.integer y)
  | IMul_nattez (_, _), (x, (y, _)) ->
      Instructions.mul_nattez (Size.integer x) (Size.mutez y)
  | IEdiv_teznat (_, _), (x, (y, _)) ->
      Instructions.ediv_teznat (Size.mutez x) (Size.integer y)
  | IEdiv_tez (_, _), (x, (y, _)) ->
      Instructions.ediv_tez (Size.mutez x) (Size.mutez y)
  | IOr (_, _), _ -> Instructions.or_
  | IAnd (_, _), _ -> Instructions.and_
  | IXor (_, _), _ -> Instructions.xor_
  | INot (_, _), _ -> Instructions.not_
  | IIs_nat (_, _), (x, _) -> Instructions.is_nat (Size.integer x)
  | INeg (_, _), (x, _) -> Instructions.neg (Size.integer x)
  | IAbs_int (_, _), (x, _) -> Instructions.abs_int (Size.integer x)
  | IInt_nat (_, _), (x, _) -> Instructions.int_nat (Size.integer x)
  | IAdd_int (_, _), (x, (y, _)) ->
      Instructions.add_int (Size.integer x) (Size.integer y)
  | IAdd_nat (_, _), (x, (y, _)) ->
      Instructions.add_nat (Size.integer x) (Size.integer y)
  | ISub_int (_, _), (x, (y, _)) ->
      Instructions.sub_int (Size.integer x) (Size.integer y)
  | IMul_int (_, _), (x, (y, _)) ->
      Instructions.mul_int (Size.integer x) (Size.integer y)
  | IMul_nat (_, _), (x, (y, _)) ->
      Instructions.mul_nat (Size.integer x) (Size.integer y)
  | IEdiv_int (_, _), (x, (y, _)) ->
      Instructions.ediv_int (Size.integer x) (Size.integer y)
  | IEdiv_nat (_, _), (x, (y, _)) ->
      Instructions.ediv_nat (Size.integer x) (Size.integer y)
  | ILsl_nat (_, _), (x, (y, _)) ->
      Instructions.lsl_nat (Size.integer x) (Size.integer y)
  | ILsr_nat (_, _), (x, (y, _)) ->
      Instructions.lsr_nat (Size.integer x) (Size.integer y)
  | IOr_nat (_, _), (x, (y, _)) ->
      Instructions.or_nat (Size.integer x) (Size.integer y)
  | IAnd_nat (_, _), (x, (y, _)) ->
      Instructions.and_nat (Size.integer x) (Size.integer y)
  | IAnd_int_nat (_, _), (x, (y, _)) ->
      Instructions.and_int_nat (Size.integer x) (Size.integer y)
  | IXor_nat (_, _), (x, (y, _)) ->
      Instructions.xor_nat (Size.integer x) (Size.integer y)
  | INot_int (_, _), (x, _) -> Instructions.not_int (Size.integer x)
  | IIf _, _ -> Instructions.if_
  | ILoop (_, _, _), _ -> Instructions.loop
  | ILoop_left (_, _, _), _ -> Instructions.loop_left
  | IDip (_, _, _, _), _ -> Instructions.dip
  | IExec (_, _, _), _ -> Instructions.exec
  | IApply (_, _, _), (_, (l, _)) ->
      let rec_flag = match l with Lam _ -> false | LamRec _ -> true in
      Instructions.apply ~rec_flag
  | ILambda (_, l, _), _ ->
      let rec_flag = match l with Lam _ -> false | LamRec _ -> true in
      Instructions.lambda ~rec_flag
  | IFailwith (_, _), _ -> Instructions.failwith_
  | ICompare (_, cmp_ty, _), (a, (b, _)) ->
      extract_compare_sized_step cmp_ty a b
  | IEq (_, _), _ -> Instructions.eq
  | INeq (_, _), _ -> Instructions.neq
  | ILt (_, _), _ -> Instructions.lt
  | IGt (_, _), _ -> Instructions.gt
  | ILe (_, _), _ -> Instructions.le
  | IGe (_, _), _ -> Instructions.ge
  | IAddress (_, _), _ -> Instructions.address
  | IContract (_, _, _, _), _ -> Instructions.contract
  | ITransfer_tokens (_, _), _ -> Instructions.transfer_tokens
  | IView (_, _, _, _), _ -> Instructions.view
  | IImplicit_account (_, _), _ -> Instructions.implicit_account
  | ICreate_contract _, _ -> Instructions.create_contract
  | ISet_delegate (_, _), _ -> Instructions.set_delegate
  | INow (_, _), _ -> Instructions.now
  | IBalance (_, _), _ -> Instructions.balance
  | ILevel (_, _), _ -> Instructions.level
  | ICheck_signature (_, _), (public_key, (_signature, (message, _))) -> (
      match public_key with
      | Signature.Ed25519 pk ->
          let pk = Size.of_int (Signature.Ed25519.Public_key.size pk) in
          let signature = Size.of_int Signature.Ed25519.size in
          let message = Size.bytes message in
          Instructions.check_signature_ed25519 pk signature message
      | Signature.Secp256k1 pk ->
          let pk = Size.of_int (Signature.Secp256k1.Public_key.size pk) in
          let signature = Size.of_int Signature.Secp256k1.size in
          let message = Size.bytes message in
          Instructions.check_signature_secp256k1 pk signature message
      | Signature.P256 pk ->
          let pk = Size.of_int (Signature.P256.Public_key.size pk) in
          let signature = Size.of_int Signature.P256.size in
          let message = Size.bytes message in
          Instructions.check_signature_p256 pk signature message
      | Signature.Bls pk ->
          let pk = Size.of_int (Signature.Bls.Public_key.size pk) in
          let signature = Size.of_int Signature.Bls.size in
          let message = Size.bytes message in
          Instructions.check_signature_bls pk signature message)
  | IHash_key (_, _), _ -> Instructions.hash_key
  | IPack (_, ty, _), (v, _) -> (
      let script_res =
        Lwt_main.run (Script_ir_translator.unparse_data ctxt Optimized ty v)
      in
      match script_res with
      | Ok (node, _ctxt) ->
          Instructions.pack (Size.of_micheline (Micheline.root node))
      | Error _ -> Stdlib.failwith "IPack workload: could not unparse")
  | IUnpack (_, _, _), _ -> Instructions.unpack
  | IBlake2b (_, _), (bytes, _) -> Instructions.blake2b (Size.bytes bytes)
  | ISha256 (_, _), (bytes, _) -> Instructions.sha256 (Size.bytes bytes)
  | ISha512 (_, _), (bytes, _) -> Instructions.sha512 (Size.bytes bytes)
  | ISource (_, _), _ -> Instructions.source
  | ISender (_, _), _ -> Instructions.sender
  | ISelf (_, _, _, _), _ -> Instructions.self
  | ISelf_address (_, _), _ -> Instructions.self_address
  | IAmount (_, _), _ -> Instructions.amount
  | ISapling_empty_state (_, _, _), _ -> Instructions.sapling_empty_state
  | ISapling_verify_update (_, _), (transaction, (_state, _)) ->
      let inputs = Size.sapling_transaction_inputs transaction in
      let outputs = Size.sapling_transaction_outputs transaction in
      let bound_data = Size.sapling_transaction_bound_data transaction in
      let state = Size.zero in
      Instructions.sapling_verify_update inputs outputs bound_data state
  | ISapling_verify_update_deprecated (_, _), (transaction, (_state, _)) ->
      let inputs = List.length transaction.inputs in
      let outputs = List.length transaction.outputs in
      let bound_data = Size.zero in
      let state = Size.zero in
      Instructions.sapling_verify_update inputs outputs bound_data state
  | IDig (_, n, _, _), _ -> Instructions.dig (Size.of_int n)
  | IDug (_, n, _, _), _ -> Instructions.dug (Size.of_int n)
  | IDipn (_, n, _, _, _), _ -> Instructions.dipn (Size.of_int n)
  | IDropn (_, n, _, _), _ -> Instructions.dropn (Size.of_int n)
  | IChainId (_, _), _ -> Instructions.chain_id
  | INever _, _ -> .
  | IVoting_power (_, _), _ -> Instructions.voting_power
  | ITotal_voting_power (_, _), _ -> Instructions.total_voting_power
  | IKeccak (_, _), (bytes, _) -> Instructions.keccak (Size.bytes bytes)
  | ISha3 (_, _), (bytes, _) -> Instructions.sha3 (Size.bytes bytes)
  | IAdd_bls12_381_g1 (_, _), _ -> Instructions.add_bls12_381_g1
  | IAdd_bls12_381_g2 (_, _), _ -> Instructions.add_bls12_381_g2
  | IAdd_bls12_381_fr (_, _), _ -> Instructions.add_bls12_381_fr
  | IMul_bls12_381_g1 (_, _), _ -> Instructions.mul_bls12_381_g1
  | IMul_bls12_381_g2 (_, _), _ -> Instructions.mul_bls12_381_g2
  | IMul_bls12_381_fr (_, _), _ -> Instructions.mul_bls12_381_fr
  | IMul_bls12_381_z_fr (_, _), (_fr, (z, _)) ->
      Instructions.mul_bls12_381_z_fr (Size.integer z)
  | IMul_bls12_381_fr_z (_, _), (z, _) ->
      Instructions.mul_bls12_381_fr_z (Size.integer z)
  | IInt_bls12_381_fr (_, _), _ -> Instructions.int_bls12_381_z_fr
  | INeg_bls12_381_g1 (_, _), _ -> Instructions.neg_bls12_381_g1
  | INeg_bls12_381_g2 (_, _), _ -> Instructions.neg_bls12_381_g2
  | INeg_bls12_381_fr (_, _), _ -> Instructions.neg_bls12_381_fr
  | IPairing_check_bls12_381 (_, _), (list, _) ->
      Instructions.pairing_check_bls12_381 (Size.list list)
  | IComb (_, n, _, _), _ -> Instructions.comb (Size.of_int n)
  | IUncomb (_, n, _, _), _ -> Instructions.uncomb (Size.of_int n)
  | IComb_get (_, n, _, _), _ -> Instructions.comb_get (Size.of_int n)
  | IComb_set (_, n, _, _), _ -> Instructions.comb_set (Size.of_int n)
  | IDup_n (_, n, _, _), _ -> Instructions.dupn (Size.of_int n)
  | ITicket (_, _, _), _ | ITicket_deprecated (_, _, _), _ ->
      Instructions.ticket
  | IRead_ticket (_, _, _), _ -> Instructions.read_ticket
  | ISplit_ticket (_, _), (_ticket, ((amount_a, amount_b), _)) ->
      Instructions.split_ticket (Size.integer amount_a) (Size.integer amount_b)
  | IJoin_tickets (_, cmp_ty, _), ((ticket1, ticket2), _) ->
      let size1 = Size.size_of_comparable_value cmp_ty ticket1.contents in
      let size2 = Size.size_of_comparable_value cmp_ty ticket2.contents in
      let tez1 = Size.integer (ticket1.amount :> Script_int.n Script_int.num) in
      let tez2 = Size.integer (ticket2.amount :> Script_int.n Script_int.num) in
      Instructions.join_tickets size1 size2 tez1 tez2
  | IHalt _, _ -> Instructions.halt
  | ILog _, _ -> Instructions.log
  | IOpen_chest (_, _), (_, (chest, (time, _))) ->
      let plaintext_size =
        Script_timelock.get_plaintext_size chest - 1 |> Size.of_int
      in
      let log_time = Z.log2 Z.(one + Script_int.to_zint time) |> Size.of_int in
      Instructions.open_chest log_time plaintext_size
  | IMin_block_time _, _ -> Instructions.min_block_time
  | IEmit _, _ -> Instructions.emit
  | ILsl_bytes (_, _), (x, (y, _)) ->
      let y =
        match Script_int.to_int y with
        | Some y -> y
        | None -> (* overflow *) assert false
      in
      Instructions.lsl_bytes (Size.bytes x) y
  | ILsr_bytes (_, _), (x, (y, _)) ->
      let y =
        match Script_int.to_int y with
        | Some y -> y
        | None -> (* overflow *) assert false
      in
      Instructions.lsr_bytes (Size.bytes x) y
  | IOr_bytes (_, _), (x, (y, _)) ->
      Instructions.or_bytes (Size.bytes x) (Size.bytes y)
  | IAnd_bytes (_, _), (x, (y, _)) ->
      Instructions.and_bytes (Size.bytes x) (Size.bytes y)
  | IXor_bytes (_, _), (x, (y, _)) ->
      Instructions.xor_bytes (Size.bytes x) (Size.bytes y)
  | INot_bytes (_, _), (x, _) -> Instructions.not_bytes (Size.bytes x)

let extract_control_trace (type bef_top bef aft_top aft)
    (cont : (bef_top, bef, aft_top, aft) Script_typed_ir.continuation) =
  match cont with
  | KNil -> Control.nil
  | KCons _ -> Control.cons
  | KReturn _ -> Control.return
  | KMap_head (_, _) -> Control.map_head
  | KUndip _ -> Control.undip
  | KLoop_in _ -> Control.loop_in
  | KLoop_in_left _ -> Control.loop_in_left
  | KIter (_, _, xs, _) -> Control.iter (Size.of_int (List.length xs))
  | KList_enter_body (_, xs, ys, _, _, _) ->
      Control.list_enter_body
        (Size.of_int (List.length xs))
        (Size.of_int (Script_list.length ys))
  | KList_exit_body (_, _, _, _, _, _) -> Control.list_exit_body
  | KMap_enter_body (_, xs, _, _, _) ->
      Control.map_enter_body (Size.of_int (List.length xs))
  | KMap_exit_body (_, _, map, k, _, _) ->
      let (module Map) = Script_map.get_module map in
      let key_size = Map.OPS.key_size k in
      Control.map_exit_body key_size (Size.map map)
  | KView_exit _ -> Control.view_exit
  | KLog _ -> Control.log

(** [Stop_bench] gets raised when a [IFailwith] would be the next instruction.
    This allows us to recover the full execution trace, including the trace of
    the [IFailwith].

    The actual benchmark will follow the same execution branch, but instead will
    raise an [error] which will be ignored. Thus it is safe to end a benchmark
    with [IFailwith], but timings are expected to be different from ending with
    [IHalt]. This means that, if we choose to include this behavior in any
    benchmark, [IFailwith] must be benched. *)
exception Stop_bench

let extract_deps (type bef_top bef aft_top aft) ctxt step_constants
    (sty : (bef_top, bef) Script_typed_ir.stack_ty)
    (kinstr : (bef_top, bef, aft_top, aft) Script_typed_ir.kinstr)
    (stack : bef_top * bef) =
  let trace = ref [] in
  (* Logger definition *)
  let logger =
    Script_interpreter_logging.make
      (module struct
        let log_interp _instr _ctxt _log _stack_ty _stack = ()

        let log_entry :
            type a s b f. (a, s, b, f, a, s) Script_typed_ir.logging_function =
         fun kinstr ctxt _loc _stack_ty stack ->
          trace := extract_ir_sized_step ctxt kinstr stack :: !trace ;
          match kinstr with IFailwith _ -> raise Stop_bench | _ -> ()

        let log_control kont = trace := extract_control_trace kont :: !trace

        let log_exit _instr _ctxt _log _stack_ty _stack = ()

        let get_log () = Environment.Error_monad.return_none
      end)
  in
  try
    let res =
      Lwt_main.run
        (Script_interpreter.Internals.kstep
           (Some logger)
           ctxt
           step_constants
           sty
           kinstr
           (fst stack)
           (snd stack))
    in
    match Environment.wrap_tzresult res with
    | Error errs ->
        Format.eprintf "%a@." Error_monad.pp_print_trace errs ;
        raise (Failure "Interpreter_workload.extract_deps: error in step")
    | Ok (_aft_top, _aft, _ctxt) ->
        (* ((aft_top, aft), List.rev !trace, ctxt) *)
        List.rev !trace
  with Stop_bench -> List.rev !trace

let extract_deps_continuation (type bef_top bef aft_top aft) ctxt step_constants
    (stack_type : (bef_top, bef) stack_ty)
    (cont : (bef_top, bef, aft_top, aft) Script_typed_ir.continuation)
    (stack : bef_top * bef) =
  let trace = ref [] in
  (* Logger definition *)
  let logger =
    Script_interpreter_logging.make
      (module struct
        let log_interp _instr _ctxt _log _stack_ty _stack = ()

        let log_entry :
            type a s b f. (a, s, b, f, a, s) Script_typed_ir.logging_function =
         fun kinstr ctxt _loc _stack_ty stack ->
          trace := extract_ir_sized_step ctxt kinstr stack :: !trace ;
          match kinstr with IFailwith _ -> raise Stop_bench | _ -> ()

        let log_control kont = trace := extract_control_trace kont :: !trace

        let log_exit _instr _ctxt _log _stack_ty _stack = ()

        let get_log () = Environment.Error_monad.return_none
      end)
  in
  try
    let res =
      let _gas_counter, outdated_ctxt =
        Local_gas_counter.local_gas_counter_and_outdated_context ctxt
      in
      Lwt_main.run
        (Script_interpreter.Internals.next
           (Some logger)
           (outdated_ctxt, step_constants)
           (Local_gas_counter 0xFF_FF_FF_FF)
           stack_type
           cont
           (fst stack)
           (snd stack))
    in
    match Environment.wrap_tzresult res with
    | Error errs ->
        Format.eprintf "%a@." Error_monad.pp_print_trace errs ;
        raise (Failure "Interpreter_workload.extract_deps: error in step")
    | Ok (_aft_top, _aft, _outdated_ctxt, _gas) ->
        (* ((aft_top, aft), List.rev !trace, outdated_ctxt, gas) *)
        List.rev !trace
  with Stop_bench -> List.rev !trace

let sized_step_to_sparse_vec {name; args} =
  let s = string_of_instr_or_cont name in
  match args with
  | [] -> Sparse_vec.String.of_list [(s, float_of_int 1)]
  | _ ->
      List.fold_left
        (fun acc {name; arg} ->
          Sparse_vec.String.(
            add acc (of_list [(s ^ "_" ^ name, float_of_int (Size.to_int arg))])))
        Sparse_vec.String.zero
        args

let trace_to_sparse_vec trace =
  List.fold_left
    (fun acc step -> Sparse_vec.String.add acc (sized_step_to_sparse_vec step))
    Sparse_vec.String.zero
    trace
