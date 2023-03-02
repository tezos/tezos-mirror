(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Block = struct
  type t = {
    number : int32;
    hash : string option;
    parent : string;
    nonce : string;
    sha3Uncles : string;
    logsBloom : string option;
    transactionRoot : string;
    stateRoot : string;
    receiptRoot : string;
    miner : string;
    difficulty : int64;
    totalDifficulty : int64;
    extraData : string;
    size : int32;
    gasLimit : int32;
    gasUsed : int32;
    timestamp : int32;
    transactions : string list;
    uncles : string list;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             number;
             hash;
             parent;
             nonce;
             sha3Uncles;
             logsBloom;
             transactionRoot;
             stateRoot;
             receiptRoot;
             miner;
             difficulty;
             totalDifficulty;
             extraData;
             size;
             gasLimit;
             gasUsed;
             timestamp;
             transactions;
             uncles;
           } ->
        ( ( number,
            hash,
            parent,
            nonce,
            sha3Uncles,
            logsBloom,
            transactionRoot,
            stateRoot,
            receiptRoot,
            miner ),
          ( difficulty,
            totalDifficulty,
            extraData,
            size,
            gasLimit,
            gasUsed,
            timestamp,
            transactions,
            uncles ) ))
      (fun ( ( number,
               hash,
               parent,
               nonce,
               sha3Uncles,
               logsBloom,
               transactionRoot,
               stateRoot,
               receiptRoot,
               miner ),
             ( difficulty,
               totalDifficulty,
               extraData,
               size,
               gasLimit,
               gasUsed,
               timestamp,
               transactions,
               uncles ) ) ->
        {
          number;
          hash;
          parent;
          nonce;
          sha3Uncles;
          logsBloom;
          transactionRoot;
          stateRoot;
          receiptRoot;
          miner;
          difficulty;
          totalDifficulty;
          extraData;
          size;
          gasLimit;
          gasUsed;
          timestamp;
          transactions;
          uncles;
        })
      (merge_objs
         (obj10
            (req "number" int32)
            (req "hash" (option string))
            (req "parent" string)
            (req "nonce" string)
            (req "sha3Uncles" string)
            (req "logsBloom" (option string))
            (req "transactionRoot" string)
            (req "stateRoot" string)
            (req "receiptRoot" string)
            (req "miner" string))
         (obj9
            (req "difficulty" int64)
            (req "totalDifficulty" int64)
            (req "extraData" string)
            (req "size" int32)
            (req "gasLimit" int32)
            (req "gasUsed" int32)
            (req "timestamp" int32)
            (req "transactions" (list string))
            (req "uncles" (list string))))
end
