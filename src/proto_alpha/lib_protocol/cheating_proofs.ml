(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
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

open Alpha_context

type error +=
  | (* `Permanent *)
      Inconsistent_evidence of {
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }
  | (* `Permanent *)
      Outdated_evidence of {
      level : Raw_level.t;
      last : Raw_level.t;
    }
  | (* `Temporary *)
      Too_early_evidence of {
      level : Raw_level.t;
      current : Raw_level.t;
    }
  | (* `Permanent *)
      Invalid_double_baking_evidence of {
      hash1 : Block_hash.t;
      level1 : Raw_level.t;
      hash2 : Block_hash.t;
      level2 : Raw_level.t;
    }
  | (* `Permanent *)
      Invalid_double_endorsement_evidence of {
      hash1 : Operation_hash.t;
      level1 : Raw_level.t;
      hash2 : Operation_hash.t;
      level2 : Raw_level.t;
    }

let check_level_bounds ctxt level =
  let current_level = Level.current ctxt in
  fail_unless
    Level.(level < current_level)
    (Too_early_evidence {level = level.level; current = current_level.level})
  >>=? fun () ->
  let oldest_level = Level.last_allowed_fork_level ctxt in
  fail_unless
    Raw_level.(oldest_level <= level.level)
    (Outdated_evidence {level = level.level; last = oldest_level})

let prove_double_baking ctxt chain_id (Double_baking_evidence {bh1; bh2}) =
  let hash1 = Block_header.hash bh1 in
  let hash2 = Block_header.hash bh2 in
  Lwt.return @@ Raw_level.of_int32 bh1.shell.level
  >>=? fun level1 ->
  Lwt.return @@ Raw_level.of_int32 bh2.shell.level
  >>=? fun level2 ->
  fail_unless
    (Raw_level.(level1 = level2) && not (Block_hash.equal hash1 hash2))
    (Invalid_double_baking_evidence {hash1; level1; hash2; level2})
  >>=? fun () ->
  let level = Level.from_raw ctxt level1 in
  check_level_bounds ctxt level
  >>=? fun () ->
  Roll.baking_rights_owner
    ctxt
    level
    ~priority:bh1.protocol_data.contents.priority
  >>=? fun delegate1 ->
  Baking.check_signature bh1 chain_id delegate1
  >>=? fun () ->
  Roll.baking_rights_owner
    ctxt
    level
    ~priority:bh2.protocol_data.contents.priority
  >>=? fun delegate2 ->
  Baking.check_signature bh2 chain_id delegate2
  >>=? fun () ->
  let (delegate1, delegate2) =
    (Signature.Public_key.hash delegate1, Signature.Public_key.hash delegate2)
  in
  fail_unless
    (Signature.Public_key_hash.equal delegate1 delegate2)
    (Inconsistent_evidence {delegate1; delegate2})
  >>=? fun () -> return (level, delegate1)

let prove_double_endorsement ctxt chain_id
    (Double_endorsement_evidence {op1; op2}) =
  let (Single (Endorsement e1), Single (Endorsement e2)) =
    (op1.protocol_data.contents, op2.protocol_data.contents)
  in
  let hash1 = Operation.hash op1 in
  let hash2 = Operation.hash op2 in
  let level1 = e1.level in
  let level2 = e2.level in
  fail_unless
    ( Raw_level.(level1 = level2)
    && not (Block_hash.equal op1.shell.branch op2.shell.branch) )
    (Invalid_double_endorsement_evidence {hash1; level1; hash2; level2})
  >>=? fun () ->
  let level = Level.from_raw ctxt level1 in
  check_level_bounds ctxt level
  >>=? fun () ->
  Baking.check_endorsement_rights ctxt chain_id op1
  >>=? fun (delegate1, _, _) ->
  Baking.check_endorsement_rights ctxt chain_id op2
  >>=? fun (delegate2, _, _) ->
  fail_unless
    (Signature.Public_key_hash.equal delegate1 delegate2)
    (Inconsistent_evidence {delegate1; delegate2})
  >>=? fun () -> return (level, delegate1)

let () =
  register_error_kind
    `Permanent
    ~id:"proving.inconsistent_evidence"
    ~title:"Inconsistent evidence"
    ~description:"The evidence is inconsistent (two distinct delegates)"
    ~pp:(fun ppf (delegate1, delegate2) ->
      Format.fprintf
        ppf
        "Inconsistent evidence (distinct delegate: %a and %a)"
        Signature.Public_key_hash.pp_short
        delegate1
        Signature.Public_key_hash.pp_short
        delegate2)
    Data_encoding.(
      obj2
        (req "delegate1" Signature.Public_key_hash.encoding)
        (req "delegate2" Signature.Public_key_hash.encoding))
    (function
      | Inconsistent_evidence {delegate1; delegate2} ->
          Some (delegate1, delegate2)
      | _ ->
          None)
    (fun (delegate1, delegate2) ->
      Inconsistent_evidence {delegate1; delegate2}) ;
  register_error_kind
    `Temporary
    ~id:"proving.too_early_evidence"
    ~title:"Too early evidence"
    ~description:"An evidence is in the future"
    ~pp:(fun ppf (level, current) ->
      Format.fprintf
        ppf
        "The evidence is in the future  (current level: %a, evidence level: %a)"
        Raw_level.pp
        current
        Raw_level.pp
        level)
    Data_encoding.(
      obj2 (req "level" Raw_level.encoding) (req "current" Raw_level.encoding))
    (function
      | Too_early_evidence {level; current} ->
          Some (level, current)
      | _ ->
          None)
    (fun (level, current) -> Too_early_evidence {level; current}) ;
  register_error_kind
    `Permanent
    ~id:"proving.outdated_evidence"
    ~title:"Outdated evidence"
    ~description:"An evidence is outdated."
    ~pp:(fun ppf (level, last) ->
      Format.fprintf
        ppf
        "The evidence is outdated (last acceptable level: %a, evidence level: \
         %a)"
        Raw_level.pp
        last
        Raw_level.pp
        level)
    Data_encoding.(
      obj2 (req "level" Raw_level.encoding) (req "last" Raw_level.encoding))
    (function
      | Outdated_evidence {level; last} -> Some (level, last) | _ -> None)
    (fun (level, last) -> Outdated_evidence {level; last}) ;
  register_error_kind
    `Permanent
    ~id:"proof.invalid_double_baking_evidence"
    ~title:"Invalid double baking evidence"
    ~description:
      "A double-baking evidence is inconsistent (two distinct levels)"
    ~pp:(fun ppf (hash1, level1, hash2, level2) ->
      Format.fprintf
        ppf
        "Invalid double-baking evidence (hash: %a and %a, levels: %a and %a)"
        Block_hash.pp
        hash1
        Block_hash.pp
        hash2
        Raw_level.pp
        level1
        Raw_level.pp
        level2)
    Data_encoding.(
      obj4
        (req "hash1" Block_hash.encoding)
        (req "level1" Raw_level.encoding)
        (req "hash2" Block_hash.encoding)
        (req "level2" Raw_level.encoding))
    (function
      | Invalid_double_baking_evidence {hash1; level1; hash2; level2} ->
          Some (hash1, level1, hash2, level2)
      | _ ->
          None)
    (fun (hash1, level1, hash2, level2) ->
      Invalid_double_baking_evidence {hash1; level1; hash2; level2}) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_double_endorsement_evidence"
    ~title:"Invalid double endorsement evidence"
    ~description:"A double-endorsement evidence is malformed"
    ~pp:(fun ppf (hash1, level1, hash2, level2) ->
      Format.fprintf
        ppf
        "Malformed double-endorsement evidence (hash: %a and %a, levels: %a \
         and %a)"
        Operation_hash.pp
        hash1
        Operation_hash.pp
        hash2
        Raw_level.pp
        level1
        Raw_level.pp
        level2)
    Data_encoding.(
      obj4
        (req "hash1" Operation_hash.encoding)
        (req "level1" Raw_level.encoding)
        (req "hash2" Operation_hash.encoding)
        (req "level2" Raw_level.encoding))
    (function
      | Invalid_double_endorsement_evidence {hash1; level1; hash2; level2} ->
          Some (hash1, level1, hash2, level2)
      | _ ->
          None)
    (fun (hash1, level1, hash2, level2) ->
      Invalid_double_endorsement_evidence {hash1; level1; hash2; level2})
