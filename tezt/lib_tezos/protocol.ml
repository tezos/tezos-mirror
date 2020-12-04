type t = Alpha | Delphi | Carthage

let name = function
  | Alpha ->
      "Alpha"
  | Delphi ->
      "Delphi"
  | Carthage ->
      "Carthage"

(* Test tags must be lowercase. *)
let tag protocol = String.lowercase_ascii (name protocol)

let hash = function
  | Alpha ->
      "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  | Delphi ->
      "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo"
  | Carthage ->
      "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"

let parameter_file = function
  | Alpha ->
      "src/proto_alpha/parameters/sandbox-parameters.json"
  | Delphi ->
      "src/proto_007_PsDELPH1/parameters/sandbox-parameters.json"
  | Carthage ->
      "src/proto_006_PsCARTHA/parameters/sandbox-parameters.json"

let accuser = function
  | Alpha ->
      "./tezos-accuser-alpha"
  | Delphi ->
      "./tezos-accuser-007-PsDELPH1"
  | Carthage ->
      "./tezos-accuser-006-PsCARTHA"
