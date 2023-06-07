(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(* Using vanilla Data_encoding.  Easier to parse but less efficient *)
module Make_V1 (Conf : sig
  val entries : int
end) =
struct
  open Tezos_context_sigs.Context.Proof_types
  open Data_encoding

  let entries = Conf.entries

  (* The code only supports branching factors 2 and 32 *)
  let () = assert (entries = 2 || entries = 32)

  let value_encoding : value Data_encoding.t = bytes

  let length_field = req "length" (conv Int64.of_int Int64.to_int int64)

  let step_encoding : step Data_encoding.t = Bounded.string' Hex 255

  let hash_encoding = Context_hash.encoding

  let index_encoding = uint8

  (* This works for entries <= 32 but specialized for entries = 32 *)
  let segment_encoding_32 =
    (* The segment int is in 5bits. *)
    (* Format

       * Required bytes = (n * 5 + 8) / 8
       * Maximum length allowed: 406
       * 10* is filled at the end of the bytes

       ex: Encoding of [aaaaa; bbbbb; ccccc; ddddd; eeeee; ..; zzzzz]

               |76543210|76543210|7654.. ..       |76543210|
               |aaaaabbb|bbcccccd|ddde.. ..        zzzzz100|

               |76543210|76543210|7654.. ..  43210|76543210|
               |aaaaabbb|bbcccccd|ddde.. ..  yzzzz|z1000000|

               |76543210|76543210|7654.. .. 543210|76543210|
               |aaaaabbb|bbcccccd|ddde.. .. yzzzzz|10000000|
    *)
    let encode is =
      let buf = Buffer.create 0 in
      let push c = Buffer.add_char buf @@ Char.chr c in
      let close c bit = push (c lor (1 lsl (7 - bit))) in
      let write c bit i =
        if bit < 3 then (c lor (i lsl (3 - bit)), bit + 5)
        else
          let i = i lsl (11 - bit) in
          push (c lor (i / 256)) ;
          (i land 255, bit - 3)
      in
      let rec f c bit = function
        | [] -> close c bit
        | i :: is ->
            let c, bit = write c bit i in
            f c bit is
      in
      f 0 0 is ;
      Buffer.to_bytes buf
    in
    let decode b =
      let open Result_syntax in
      let error = Error "invalid 5bit list" in
      let* l =
        let sl = Bytes.length b in
        if sl = 0 then error
        else
          let c = Char.code @@ Bytes.get b (sl - 1) in
          let* last_bit =
            if c = 0 then error
            else
              let rec aux i =
                if c land (1 lsl i) = 0 then aux (i + 1) else i + 1
              in
              Ok (aux 0)
          in
          let bits = (sl * 8) - last_bit in
          if bits mod 5 = 0 then Ok (bits / 5) else error
      in
      let s = Bytes.to_seq b in
      let head s =
        (* This assertion won't fail even with malformed input. *)
        match s () with
        | Seq.Nil -> assert false
        | Seq.Cons (c, s) -> (Char.code c, s)
      in
      let rec read c rembit l s =
        if l = 0 then []
        else
          let c, s, rembit =
            if rembit >= 5 then (c, s, rembit)
            else
              let c', s = head s in
              ((c * 256) + c', s, rembit + 8)
          in
          let rembit = rembit - 5 in
          let i = c lsr rembit in
          let c = c land ((1 lsl rembit) - 1) in
          i :: read c rembit (l - 1) s
      in
      Ok (read 0 0 l s)
    in
    conv_with_guard encode decode (Bounded.bytes 255)

  (* This works only for entries = 2 *)
  let segment_encoding_2 =
    (* Format

       * Required bytes = (n + 8) / 8
       * Maximum length allowed: 2032
       * 10* is filled at the end of the bytes
    *)
    let encode is =
      let buf = Buffer.create 0 in
      let push c = Buffer.add_char buf @@ Char.chr c in
      let close c bit = push (c lor (1 lsl (7 - bit))) in
      let write c bit i =
        if bit < 7 then (c lor (i lsl (7 - bit)), bit + 1)
        else
          let i = i lsl (15 - bit) in
          push (c lor (i / 256)) ;
          (i land 255, bit - 7)
      in
      let rec f c bit = function
        | [] -> close c bit
        | i :: is ->
            let c, bit = write c bit i in
            f c bit is
      in
      f 0 0 is ;
      Buffer.to_bytes buf
    in
    let decode b =
      let open Result_syntax in
      let error = Error "invalid binary list" in
      let* l =
        let sl = Bytes.length b in
        if sl = 0 then error
        else
          let c = Char.code @@ Bytes.get b (sl - 1) in
          let* last_bit =
            if c = 0 then error
            else
              let rec aux i =
                if c land (1 lsl i) = 0 then aux (i + 1) else i + 1
              in
              Ok (aux 0)
          in
          let bits = (sl * 8) - last_bit in
          Ok bits
      in
      let s = Bytes.to_seq b in
      let head s =
        (* This assertion won't fail even with malformed input. *)
        match s () with
        | Seq.Nil -> assert false
        | Seq.Cons (c, s) -> (Char.code c, s)
      in
      let rec read c rembit l s =
        if l = 0 then []
        else
          let c, s, rembit =
            if rembit >= 1 then (c, s, rembit)
            else
              let c', s = head s in
              ((c * 256) + c', s, rembit + 8)
          in
          let rembit = rembit - 1 in
          let i = c lsr rembit in
          let c = c land ((1 lsl rembit) - 1) in
          i :: read c rembit (l - 1) s
      in
      Ok (read 0 0 l s)
    in
    conv_with_guard encode decode (Bounded.bytes 255)

  let segment_encoding =
    match entries with
    | 32 -> segment_encoding_32
    | 2 -> segment_encoding_2
    | _ ->
        (* Unsupported *)
        assert false

  let inode_proofs_encoding_2 a =
    conv_with_guard
      (function
        | [(0, x); (1, y)] -> (Some x, Some y)
        | [(0, x)] -> (Some x, None)
        | [(1, y)] -> (None, Some y)
        | [] -> invalid_arg "cannot encode ill-formed Merkle proof"
        | _ -> invalid_arg "cannot encode non binary proof tree")
      (function
        | Some x, Some y -> Ok [(0, x); (1, y)]
        | Some x, None -> Ok [(0, x)]
        | None, Some y -> Ok [(1, y)]
        | None, None -> Error "cannot decode ill-formed Merkle proof")
      (tup2 a a)

  let inode_proofs_encoding_gen a =
    (* When the number of proofs is large enough (>= Context.Conf.entries / 2),
       proofs are encoded as `array` instead of `list` for compactness. *)
    (* This encode assumes that proofs are ordered by its index. *)
    let encode_type v =
      if Compare.List_length_with.(v >= entries / 2) then `Array else `List
    in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"sparse_proof"
          (Tag 0)
          (obj1
             (req
                "sparse_proof"
                (conv_with_guard
                   (fun v -> List.map (fun (i, d) -> (i, Some d)) v)
                   (fun v ->
                     List.fold_right_e
                       (fun (i, d) acc ->
                         match d with
                         | None -> Error "cannot decode ill-formed Merkle proof"
                         | Some d -> Ok ((i, d) :: acc))
                       v
                       [])
                   (list (tup2 index_encoding a)))))
          (fun v -> if encode_type v = `List then Some v else None)
          (fun v -> v);
        case
          ~title:"dense_proof"
          (Tag 1)
          (obj1 (req "dense_proof" (Fixed.array entries a)))
          (fun v ->
            if encode_type v = `Array then (
              let arr = Array.make entries None in
              (* The `a` passed to this encoding will be
                 `option_inode_tree_encoding` and `option_inode_tree_encoding`,
                 both encode `option` tags with its variant tag,
                 thus this `option` wrapping won't increase the encoding size. *)
              List.iter (fun (i, d) -> arr.(i) <- Some d) v ;
              Some arr)
            else None)
          (fun v ->
            let res = ref [] in
            for i = Array.length v - 1 downto 0 do
              match v.(i) with None -> () | Some d -> res := (i, d) :: !res
            done ;
            !res);
      ]

  let inode_proofs_encoding =
    match entries with
    | 2 -> inode_proofs_encoding_2
    | _ -> inode_proofs_encoding_gen

  let inode_encoding a =
    conv
      (fun {length; proofs} -> (length, proofs))
      (fun (length, proofs) -> {length; proofs})
    @@ obj2 length_field (req "proofs" (inode_proofs_encoding a))

  let inode_extender_encoding a =
    conv
      (fun {length; segment; proof} -> (length, segment, proof))
      (fun (length, segment, proof) -> {length; segment; proof})
    @@ obj3 length_field (req "segment" segment_encoding) (req "proof" a)

  (* data-encoding.0.4/test/mu.ml for building mutually recursive data_encodings *)
  let _inode_tree_encoding, tree_encoding =
    let unoptionize enc =
      conv_with_guard
        (fun v -> Some v)
        (function
          | Some v -> Ok v
          | None -> Error "cannot decode ill-formed Merkle proof")
        enc
    in
    let mu_option_inode_tree_encoding tree_encoding =
      mu "inode_tree" (fun option_inode_tree_encoding ->
          let inode_tree_encoding = unoptionize option_inode_tree_encoding in
          union
            [
              case
                ~title:"Blinded_inode"
                (Tag 0)
                (obj1 (req "blinded_inode" hash_encoding))
                (function Some (Blinded_inode h) -> Some h | _ -> None)
                (fun h -> Some (Blinded_inode h));
              case
                ~title:"Inode_values"
                (Tag 1)
                (obj1
                   (req
                      "inode_values"
                      (list (tup2 step_encoding tree_encoding))))
                (function Some (Inode_values xs) -> Some xs | _ -> None)
                (fun xs -> Some (Inode_values xs));
              case
                ~title:"Inode_tree"
                (Tag 2)
                (obj1
                   (req
                      "inode_tree"
                      (inode_encoding option_inode_tree_encoding)))
                (function Some (Inode_tree i) -> Some i | _ -> None)
                (fun i -> Some (Inode_tree i));
              case
                ~title:"Inode_extender"
                (Tag 3)
                (obj1
                   (req
                      "inode_extender"
                      (inode_extender_encoding inode_tree_encoding)))
                (function
                  | Some (Inode_extender i : inode_tree) -> Some i | _ -> None)
                (fun i : inode_tree option -> Some (Inode_extender i));
              case
                ~title:"None"
                (Tag 4)
                (obj1 (req "none" null))
                (function None -> Some () | Some _ -> None)
                (fun () -> None);
            ])
    in
    let mu_option_tree_encoding : tree option encoding =
      mu "tree_encoding" (fun option_tree_encoding ->
          let tree_encoding = unoptionize option_tree_encoding in
          let option_inode_tree_encoding =
            mu_option_inode_tree_encoding tree_encoding
          in
          let inode_tree_encoding = unoptionize option_inode_tree_encoding in
          union
            [
              case
                ~title:"Value"
                (Tag 0)
                (obj1 (req "value" value_encoding))
                (function Some (Value v : tree) -> Some v | _ -> None)
                (fun v -> Some (Value v));
              case
                ~title:"Blinded_value"
                (Tag 1)
                (obj1 (req "blinded_value" hash_encoding))
                (function Some (Blinded_value hash) -> Some hash | _ -> None)
                (fun hash -> Some (Blinded_value hash));
              case
                ~title:"Node"
                (Tag 2)
                (obj1 (req "node" (list (tup2 step_encoding tree_encoding))))
                (function Some (Node sts : tree) -> Some sts | _ -> None)
                (fun sts -> Some (Node sts));
              case
                ~title:"Blinded_node"
                (Tag 3)
                (obj1 (req "blinded_node" hash_encoding))
                (function Some (Blinded_node hash) -> Some hash | _ -> None)
                (fun hash -> Some (Blinded_node hash));
              case
                ~title:"Inode"
                (Tag 4)
                (obj1 (req "inode" (inode_encoding option_inode_tree_encoding)))
                (function Some (Inode i : tree) -> Some i | _ -> None)
                (fun i -> Some (Inode i));
              case
                ~title:"Extender"
                (Tag 5)
                (obj1
                   (req
                      "extender"
                      (inode_extender_encoding inode_tree_encoding)))
                (function Some (Extender i) -> Some i | _ -> None)
                (fun i -> Some (Extender i));
              case
                ~title:"None"
                (Tag 6)
                (obj1 (req "none" null))
                (function None -> Some () | Some _ -> None)
                (fun () -> None);
            ])
    in
    let tree_encoding = unoptionize mu_option_tree_encoding in
    let inode_tree_encoding =
      unoptionize @@ mu_option_inode_tree_encoding tree_encoding
    in
    (inode_tree_encoding, tree_encoding)

  let kinded_hash_encoding =
    union
      [
        case
          ~title:"Value"
          (Tag 0)
          (obj1 (req "value" hash_encoding))
          (function `Value ch -> Some ch | _ -> None)
          (fun ch -> `Value ch);
        case
          ~title:"Node"
          (Tag 1)
          (obj1 (req "node" hash_encoding))
          (function `Node ch -> Some ch | _ -> None)
          (fun ch -> `Node ch);
      ]

  let elt_encoding =
    let open Stream in
    union
      [
        case
          ~title:"Value"
          (Tag 0)
          (obj1 (req "value" value_encoding))
          (function Value v -> Some v | _ -> None)
          (fun v -> Value v);
        case
          ~title:"Node"
          (Tag 1)
          (obj1 (req "node" (list (tup2 step_encoding kinded_hash_encoding))))
          (function Node sks -> Some sks | _ -> None)
          (fun sks -> Node sks);
        case
          ~title:"Inode"
          (Tag 2)
          (* This option wrapping increases the encoding size.
             But stream encoding is basically larger than proof encoding,
             so I temporarily won't mind this increment. *)
          (obj1 (req "inode" (inode_encoding (option hash_encoding))))
          (function Inode hinode -> Some hinode | _ -> None)
          (fun hinode -> Inode hinode);
        case
          ~title:"Inode_extender"
          (Tag 3)
          (obj1 (req "inode_extender" (inode_extender_encoding hash_encoding)))
          (function Inode_extender e -> Some e | _ -> None)
          (fun e -> Inode_extender e);
      ]

  let stream_encoding = conv List.of_seq List.to_seq (list elt_encoding)

  let encoding a =
    conv
      (fun {version; before; after; state} -> (version, before, after, state))
      (fun (version, before, after, state) -> {version; before; after; state})
    @@ obj4
         (req "version" int16)
         (req "before" kinded_hash_encoding)
         (req "after" kinded_hash_encoding)
         (req "state" a)

  let tree_proof_encoding = encoding tree_encoding

  let stream_proof_encoding = encoding stream_encoding
end

(* Using `Data_encoding.Compact`. Harder to parse but more efficient *)
module Make_V2 (Conf : sig
  val entries : int
end) =
struct
  open Tezos_context_sigs.Context.Proof_types
  module V1 = Make_V1 (Conf)
  open Data_encoding

  let entries = Conf.entries

  (* The code only supports branching factors 2 and 32 *)
  let () = assert (entries = 2 || entries = 32)

  let value_encoding : value Compact.t =
    let open Compact in
    let bytes_case name a b =
      let check_length s =
        let l = Bytes.length s in
        a <= l && l < b
      in
      case
        ~title:name
        (payload @@ Bounded.bytes (b - 1))
        (fun s -> if check_length s then Some s else None)
        (fun s ->
          if check_length s then s
          else invalid_arg "cannot decode ill-formed Merkle proof")
    in
    union
      ~union_tag_bits:2
      ~cases_tag_bits:0
      [
        bytes_case "short_bytes" 0 256;
        bytes_case "medium_bytes" 256 (256 * 256);
        (* The following case will be introduced
           when uint24 field is implemented in Data_enopding. *)
        (* bytes_case "long_bytes" (256 * 256 * 256); *)
        void_case ~title:"long_bytes";
        (let check_length s = 256 * 256 <= Bytes.length s in
         case
           ~title:"unlimited_bytes"
           (payload bytes)
           (fun s -> if check_length s then Some s else None)
           (fun s ->
             if check_length s then s
             else invalid_arg "cannot decode ill-formed Merkle proof"));
      ]

  let length_field : int Compact.field =
    let open Compact in
    req "length" @@ conv Int64.of_int Int64.to_int int64

  let step_encoding : step Data_encoding.t = V1.step_encoding

  let hash_encoding = Compact.payload V1.hash_encoding

  let segment_encoding = Compact.payload V1.segment_encoding

  let inode_proofs_encoding_2 a =
    let open Compact in
    conv
      (function
        | [(0, x); (1, y)] -> (Some x, Some y)
        | [(0, x)] -> (Some x, None)
        | [(1, y)] -> (None, Some y)
        | [] -> invalid_arg "cannot encode ill-formed Merkle proof"
        | _ -> invalid_arg "cannot encode non binary proof tree")
      (function
        | Some x, Some y -> [(0, x); (1, y)]
        | Some x, None -> [(0, x)]
        | None, Some y -> [(1, y)]
        | None, None -> invalid_arg "cannot decode ill-formed Merkle proof")
      (tup2 a a)

  let inode_proofs_encoding_32 a =
    (* When the number of proofs is large enough (>= Context.Conf.entries / 2),
       proofs are encoded as `array` instead of `list` for compactness. *)
    (* Due to the limitation of tag bits,
       we encode lists whose length is 15 as an array. *)
    (* This encode assumes that proofs are ordered by its index. *)
    let encode_type v =
      if Compare.List_length_with.(v < 15) then `List else `Array
    in
    let open Compact in
    (* When mu is introduced to `Data_encoding.Compact`,
       we may better use 5bit union as index_encoding. *)
    let index_encoding = uint8 in
    let a = make ~tag_size:(if tag_bit_count a = 0 then `Uint0 else `Uint8) a in
    union
      ~union_tag_bits:1
      ~cases_tag_bits:4
      [
        case
          ~title:"sparse_proof"
          (obj1
             (req
                "sparse_proof"
                (conv
                   (fun v -> List.map (fun (i, d) -> (i, Some d)) v)
                   (fun v ->
                     List.fold_right
                       (fun (i, d) acc ->
                         match d with
                         | None ->
                             invalid_arg "cannot decode ill-formed Merkle proof"
                         | Some d -> (i, d) :: acc)
                       v
                       [])
                   (* 4bit share tag is required to store the length of sparse proof (0..14) *)
                   (list ~bits:4 Data_encoding.(tup2 index_encoding a)))))
          (fun v -> if encode_type v = `List then Some v else None)
          (fun v -> v);
        case
          ~title:"dense_proof"
          (obj1 (req "dense_proof" (payload @@ Fixed.array entries a)))
          (fun v ->
            if encode_type v = `Array then (
              let arr = Array.make entries None in
              (* The `a` passed to this encoding will be
                 `option_inode_tree_encoding` and `option_inode_tree_encoding`,
                 both encode `option` tags with its variant tag,
                 thus this `option` wrapping won't increase the encoding size. *)
              List.iter (fun (i, d) -> arr.(i) <- Some d) v ;
              Some arr)
            else None)
          (fun v ->
            let res = ref [] in
            for i = Array.length v - 1 downto 0 do
              match v.(i) with None -> () | Some d -> res := (i, d) :: !res
            done ;
            !res);
      ]

  let inode_proofs_encoding =
    match entries with
    | 2 -> inode_proofs_encoding_2
    | 32 -> inode_proofs_encoding_32
    | _ ->
        (* Unsupported *)
        assert false

  let inode_encoding a =
    let open Compact in
    conv
      (fun {length; proofs} -> (length, proofs))
      (fun (length, proofs) -> {length; proofs})
    @@ obj2 length_field (req "proofs" (inode_proofs_encoding a))

  let inode_extender_encoding a =
    let open Compact in
    conv
      (fun {length; segment; proof} -> (length, segment, proof))
      (fun (length, segment, proof) -> {length; segment; proof})
    @@ obj3 length_field (req "segment" segment_encoding) (req "proof" a)

  let bits_of_node_list =
    match entries with
    | 32 -> 6 (* 6bit :: 0 - 62 *)
    | 2 -> 2 (* 2bit :: 0 - 2 *)
    | _ ->
        (* Unsupported *)
        assert false

  (* data-encoding.0.4/test/mu.ml for building mutually recursive data_encodings *)
  let _inode_tree_encoding, tree_encoding =
    let unoptionize enc =
      conv_with_guard
        (fun v -> Some v)
        (function
          | Some v -> Ok v
          | None -> Error "cannot decode ill-formed Merkle proof")
        enc
    in
    let mu_option_inode_tree_encoding tree_encoding =
      mu "inode_tree" (fun option_inode_tree_encoding ->
          let inode_tree_encoding = unoptionize option_inode_tree_encoding in
          let open Compact in
          (* Variant layouts and required shared tag bits.
             0xxxxxxx :: Inode_tree     (requires 7bits)
             10xxxxxx :: Inode_values   (requires 6bits)
             1100xxxx :: Blinded_inode  (requires 0bits)
             1101xxxx :: Inode_extender (requires 2bits)
             1110xxxx :: None           (requires 0bits) *)
          make ~tag_size:`Uint8
          @@ union
               ~union_tag_bits:1
               ~cases_tag_bits:7
               [
                 case
                   ~title:"Inode_tree"
                   (obj1
                      (req
                         "inode_tree"
                         (inode_encoding (payload option_inode_tree_encoding))))
                   (function Some (Inode_tree i) -> Some i | _ -> None)
                   (fun i -> Some (Inode_tree i));
                 case
                   ~title:"other_inode_trees"
                   (obj1
                      (req
                         "other_inode_trees"
                         (union
                            ~union_tag_bits:1
                            ~cases_tag_bits:6
                            [
                              case
                                ~title:"Inode_values"
                                (obj1
                                   (req
                                      "inode_values"
                                      (list
                                         ~bits:bits_of_node_list
                                         Data_encoding.(
                                           tup2 step_encoding tree_encoding))))
                                (function
                                  | Some (Inode_values xs) -> Some xs
                                  | _ -> None)
                                (fun xs -> Some (Inode_values xs));
                              case
                                ~title:"other_inode_trees"
                                (obj1
                                   (req
                                      "other_inode_trees"
                                      (union
                                         ~union_tag_bits:2
                                         ~cases_tag_bits:4
                                         [
                                           case
                                             ~title:"Blinded_inode"
                                             (obj1
                                                (req
                                                   "blinded_inode"
                                                   hash_encoding))
                                             (function
                                               | Some (Blinded_inode h) ->
                                                   Some h
                                               | _ -> None)
                                             (fun h -> Some (Blinded_inode h));
                                           case
                                             ~title:"Inode_extender"
                                             (obj1
                                                (req
                                                   "inode_extender"
                                                   (inode_extender_encoding
                                                      (payload
                                                         inode_tree_encoding))))
                                             (function
                                               | Some
                                                   (Inode_extender i :
                                                     inode_tree) ->
                                                   Some i
                                               | _ -> None)
                                             (fun i : inode_tree option ->
                                               Some (Inode_extender i));
                                           case
                                             (obj1 (req "none" unit))
                                             ~title:"None"
                                             (function
                                               | None -> Some ()
                                               | Some _ -> None)
                                             (fun () -> None);
                                         ])))
                                (function
                                  | Some (Inode_values _) -> None | v -> Some v)
                                (fun v -> v);
                            ])))
                   (function Some (Inode_tree _) -> None | v -> Some v)
                   (fun v -> v);
               ])
    in
    let mu_option_tree_encoding : tree option encoding =
      mu "tree_encoding" (fun option_tree_encoding ->
          let tree_encoding = unoptionize option_tree_encoding in
          let option_inode_tree_encoding =
            mu_option_inode_tree_encoding tree_encoding
          in
          let inode_tree_encoding = unoptionize option_inode_tree_encoding in
          let open Compact in
          (* Variant layouts and required shared tag bits.
             0xxxxxxx :: Inode         (requires 7bits)
             10xxxxxx :: Node          (requires 6bits)
             11000xxx :: Value         (requires 2bits)
             11001xxx :: Blinded_value (requires 0bits)
             11010xxx :: Blinded_node  (requires 0bits)
             11011xxx :: Extender      (requires 2bits)
             11100xxx :: None          (requires 0bits) *)
          make ~tag_size:`Uint8
          @@ union
               ~union_tag_bits:1
               ~cases_tag_bits:7
               [
                 case
                   ~title:"Inode"
                   (obj1
                      (req
                         "inode"
                         (inode_encoding (payload option_inode_tree_encoding))))
                   (function Some (Inode i : tree) -> Some i | _ -> None)
                   (fun i -> Some (Inode i));
                 case
                   ~title:"other_trees"
                   (obj1
                      (req
                         "other_trees"
                         (union
                            ~union_tag_bits:1
                            ~cases_tag_bits:6
                            [
                              case
                                ~title:"Node"
                                (obj1
                                   (req
                                      "node"
                                      (list
                                         ~bits:bits_of_node_list
                                         Data_encoding.(
                                           tup2 step_encoding tree_encoding))))
                                (function
                                  | Some (Node sts : tree) -> Some sts
                                  | _ -> None)
                                (fun sts -> Some (Node sts));
                              case
                                ~title:"other_trees"
                                (obj1
                                   (req
                                      "other_trees"
                                      (union
                                         ~union_tag_bits:3
                                         ~cases_tag_bits:3
                                         [
                                           case
                                             ~title:"Value"
                                             (obj1 (req "value" value_encoding))
                                             (function
                                               | Some (Value v : tree) -> Some v
                                               | _ -> None)
                                             (fun v -> Some (Value v));
                                           case
                                             ~title:"Blinded_value"
                                             (obj1
                                                (req
                                                   "blinded_value"
                                                   hash_encoding))
                                             (function
                                               | Some (Blinded_value hash) ->
                                                   Some hash
                                               | _ -> None)
                                             (fun hash ->
                                               Some (Blinded_value hash));
                                           case
                                             ~title:"Blinded_node"
                                             (obj1
                                                (req
                                                   "blinded_node"
                                                   hash_encoding))
                                             (function
                                               | Some (Blinded_node hash) ->
                                                   Some hash
                                               | _ -> None)
                                             (fun hash ->
                                               Some (Blinded_node hash));
                                           case
                                             ~title:"Extender"
                                             (obj1
                                                (req
                                                   "extender"
                                                   (inode_extender_encoding
                                                      (payload
                                                         inode_tree_encoding))))
                                             (function
                                               | Some (Extender i) -> Some i
                                               | _ -> None)
                                             (fun i -> Some (Extender i));
                                           case
                                             (obj1 (req "none" unit))
                                             ~title:"None"
                                             (function
                                               | None -> Some ()
                                               | Some _ -> None)
                                             (fun () -> None);
                                         ])))
                                (function Some (Node _) -> None | v -> Some v)
                                (fun v -> v);
                            ])))
                   (function Some (Inode _) -> None | v -> Some v)
                   (fun v -> v);
               ])
    in
    let tree_encoding = unoptionize mu_option_tree_encoding in
    let inode_tree_encoding =
      unoptionize @@ mu_option_inode_tree_encoding tree_encoding
    in
    (inode_tree_encoding, tree_encoding)

  let kinded_hash_encoding =
    let open Compact in
    union
      ~union_tag_bits:1
      ~cases_tag_bits:0
      [
        case
          ~title:"Value"
          (obj1 (req "value" hash_encoding))
          (function `Value ch -> Some ch | _ -> None)
          (fun ch -> `Value ch);
        case
          ~title:"Node"
          (obj1 (req "node" hash_encoding))
          (function `Node ch -> Some ch | _ -> None)
          (fun ch -> `Node ch);
      ]

  let elt_encoding =
    let open Stream in
    let open Compact in
    (* Variant layouts and required shared tag bits.
       0xxxxxxx :: Inode          (requires 7bits)
       10xxxxxx :: Node           (requires 6bits)
       11000xxx :: Value          (requires 2bits)
       11011xxx :: Inode_extender (requires 2bits) *)
    union
      ~union_tag_bits:1
      ~cases_tag_bits:7
      [
        case
          ~title:"Inode"
          (obj1 (req "inode" (inode_encoding (option hash_encoding))))
          (function Inode hinode -> Some hinode | _ -> None)
          (fun hinode -> Inode hinode)
        (* This option wrapping increases the encoding size when `entries = 32`.
           But stream encoding is basically larger than proof encoding,
           so I temporarily won't mind this increment. *);
        case
          ~title:"other_elts"
          (obj1
             (req
                "other_elts"
                (union
                   ~union_tag_bits:1
                   ~cases_tag_bits:6
                   [
                     case
                       ~title:"Node"
                       (obj1
                          (req
                             "node"
                             (list
                                ~bits:bits_of_node_list
                                Data_encoding.(
                                  tup2
                                    step_encoding
                                    (make ~tag_size:`Uint8 kinded_hash_encoding)))))
                       (function Node sks -> Some sks | _ -> None)
                       (fun sks -> Node sks);
                     case
                       ~title:"other_elts"
                       (obj1
                          (req
                             "other_elts"
                             (union
                                ~union_tag_bits:1
                                ~cases_tag_bits:5
                                [
                                  case
                                    ~title:"Value"
                                    (obj1 (req "value" value_encoding))
                                    (function Value v -> Some v | _ -> None)
                                    (fun v -> Value v);
                                  case
                                    ~title:"Inode_extender"
                                    (obj1
                                       (req
                                          "inode_extender"
                                          (inode_extender_encoding
                                             hash_encoding)))
                                    (function
                                      | Inode_extender e -> Some e | _ -> None)
                                    (fun e -> Inode_extender e);
                                ])))
                       (function Node _ -> None | e -> Some e)
                       (fun e -> e);
                   ])))
          (function Inode _ -> None | e -> Some e)
          (fun e -> e);
      ]

  let stream_encoding =
    (* Encoding method should be revisited after the actual stream length is determined. *)
    conv
      List.of_seq
      List.to_seq
      (list @@ Compact.make ~tag_size:`Uint8 elt_encoding)

  let encoding a =
    let open Compact in
    conv
      (fun {version; before; after; state} -> (version, before, after, state))
      (fun (version, before, after, state) -> {version; before; after; state})
    @@ obj4
         (req "version" (payload int16))
         (req "before" kinded_hash_encoding)
         (req "after" kinded_hash_encoding)
         (req "state" a)

  let tree_proof_encoding =
    let open Compact in
    make ~tag_size:`Uint8 @@ encoding (payload tree_encoding)

  let stream_proof_encoding =
    let open Compact in
    make ~tag_size:`Uint8 @@ encoding (payload stream_encoding)
end

module V1 = struct
  module Tree32 = Make_V1 (struct
    let entries = 32
  end)

  module Tree2 = Make_V1 (struct
    let entries = 2
  end)
end

module V2 = struct
  module Tree32 = Make_V2 (struct
    let entries = 32
  end)

  module Tree2 = Make_V2 (struct
    let entries = 2
  end)
end
