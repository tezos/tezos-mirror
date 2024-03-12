meta:
  id: id_015__ptlimapt__operation__contents_list
  endian: be
  imports:
  - block_header__shell
  - operation__shell_header
doc: ! 'Encoding id: 015-PtLimaPt.operation.contents_list'
types:
  activate_account:
    seq:
    - id: pkh
      size: 20
    - id: secret
      size: 20
  amount:
    seq:
    - id: amount_tag
      type: u1
      enum: amount_tag
    - id: case__0
      type: u1
      if: (amount_tag == amount_tag::case__0)
    - id: case__1
      type: u2
      if: (amount_tag == amount_tag::case__1)
    - id: case__2
      type: s4
      if: (amount_tag == amount_tag::case__2)
    - id: case__3
      type: s8
      if: (amount_tag == amount_tag::case__3)
  args:
    seq:
    - id: args_entries
      type: args_entries
      repeat: eos
  args_0:
    seq:
    - id: len_args
      type: u4
      valid:
        max: 1073741823
    - id: args
      type: args
      size: len_args
  args_entries:
    seq:
    - id: args_elt
      type: micheline__015__ptlimapt__michelson_v1__expression
  ballot:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: period
      type: s4
    - id: proposal
      size: 32
    - id: ballot
      type: s1
  bh1:
    seq:
    - id: id_015__ptlimapt__block_header__alpha__full_header
      type: id_015__ptlimapt__block_header__alpha__full_header
  bh1_0:
    seq:
    - id: len_bh1
      type: u4
      valid:
        max: 1073741823
    - id: bh1
      type: bh1
      size: len_bh1
  bh2:
    seq:
    - id: id_015__ptlimapt__block_header__alpha__full_header
      type: id_015__ptlimapt__block_header__alpha__full_header
  bh2_0:
    seq:
    - id: len_bh2
      type: u4
      valid:
        max: 1073741823
    - id: bh2
      type: bh2
      size: len_bh2
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  case__0:
    seq:
    - id: case__0_field0
      type: s2
    - id: case__0_field1
      size: 32
      doc: context_hash
    - id: case__0_field2
      size: 32
      doc: context_hash
    - id: case__0_field3
      type: tree_encoding
      doc: tree_encoding
  case__0_0:
    seq:
    - id: case__0_field0
      type: s2
    - id: case__0_field1
      size: 32
      doc: context_hash
    - id: case__0_field2
      size: 32
      doc: context_hash
    - id: case__0_field3
      type: tree_encoding
  case__1:
    seq:
    - id: case__1_field0
      type: s2
    - id: case__1_field1
      size: 32
      doc: context_hash
    - id: case__1_field2
      size: 32
      doc: context_hash
    - id: case__1_field3
      type: tree_encoding
  case__10:
    seq:
    - id: case__10_field0
      type: s4
    - id: case__10_field1
      type: case__10_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: case__10_field1_entries
  case__10_field1_entries:
    seq:
    - id: case__10_field1_elt_field0
      type: u1
    - id: case__10_field1_elt_field1
      type: inode_tree
  case__11:
    seq:
    - id: case__11_field0
      type: s8
    - id: case__11_field1
      type: case__11_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: case__11_field1_entries
  case__11_field1_entries:
    seq:
    - id: case__11_field1_elt_field0
      type: u1
    - id: case__11_field1_elt_field1
      type: inode_tree
  case__12:
    seq:
    - id: case__12_field0
      type: u1
    - id: case__12_field1
      type: case__12_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: case__12_field1_entries
  case__129_elt_field0:
    seq:
    - id: case__129_elt_field0
      size-eos: true
  case__129_elt_field0_0:
    seq:
    - id: len_case__129_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__129_elt_field0
      type: case__129_elt_field0
      size: len_case__129_elt_field0
  case__129_entries:
    seq:
    - id: case__129_elt_field0
      type: case__129_elt_field0_0
    - id: case__129_elt_field1
      type: tree_encoding
  case__12_field1_entries:
    seq:
    - id: case__12_field1_elt_field0
      type: u1
    - id: case__12_field1_elt_field1
      type: inode_tree
  case__13:
    seq:
    - id: case__13_field0
      type: u2
    - id: case__13_field1
      type: case__13_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: case__13_field1_entries
  case__130_elt_field0:
    seq:
    - id: case__130_elt_field0
      size-eos: true
  case__130_elt_field0_0:
    seq:
    - id: len_case__130_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__130_elt_field0
      type: case__130_elt_field0
      size: len_case__130_elt_field0
  case__130_entries:
    seq:
    - id: case__130_elt_field0
      type: case__130_elt_field0_0
    - id: case__130_elt_field1
      type: tree_encoding
  case__131_elt_field0:
    seq:
    - id: case__131_elt_field0
      size-eos: true
  case__131_elt_field0_0:
    seq:
    - id: len_case__131_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__131_elt_field0
      type: case__131_elt_field0
      size: len_case__131_elt_field0
  case__131_entries:
    seq:
    - id: case__131_elt_field0
      type: case__131_elt_field0_0
    - id: case__131_elt_field1
      type: tree_encoding
  case__132_elt_field0:
    seq:
    - id: case__132_elt_field0
      size-eos: true
  case__132_elt_field0_0:
    seq:
    - id: len_case__132_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__132_elt_field0
      type: case__132_elt_field0
      size: len_case__132_elt_field0
  case__132_entries:
    seq:
    - id: case__132_elt_field0
      type: case__132_elt_field0_0
    - id: case__132_elt_field1
      type: tree_encoding
  case__133_elt_field0:
    seq:
    - id: case__133_elt_field0
      size-eos: true
  case__133_elt_field0_0:
    seq:
    - id: len_case__133_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__133_elt_field0
      type: case__133_elt_field0
      size: len_case__133_elt_field0
  case__133_entries:
    seq:
    - id: case__133_elt_field0
      type: case__133_elt_field0_0
    - id: case__133_elt_field1
      type: tree_encoding
  case__134_elt_field0:
    seq:
    - id: case__134_elt_field0
      size-eos: true
  case__134_elt_field0_0:
    seq:
    - id: len_case__134_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__134_elt_field0
      type: case__134_elt_field0
      size: len_case__134_elt_field0
  case__134_entries:
    seq:
    - id: case__134_elt_field0
      type: case__134_elt_field0_0
    - id: case__134_elt_field1
      type: tree_encoding
  case__135_elt_field0:
    seq:
    - id: case__135_elt_field0
      size-eos: true
  case__135_elt_field0_0:
    seq:
    - id: len_case__135_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__135_elt_field0
      type: case__135_elt_field0
      size: len_case__135_elt_field0
  case__135_entries:
    seq:
    - id: case__135_elt_field0
      type: case__135_elt_field0_0
    - id: case__135_elt_field1
      type: tree_encoding
  case__136_elt_field0:
    seq:
    - id: case__136_elt_field0
      size-eos: true
  case__136_elt_field0_0:
    seq:
    - id: len_case__136_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__136_elt_field0
      type: case__136_elt_field0
      size: len_case__136_elt_field0
  case__136_entries:
    seq:
    - id: case__136_elt_field0
      type: case__136_elt_field0_0
    - id: case__136_elt_field1
      type: tree_encoding
  case__137_elt_field0:
    seq:
    - id: case__137_elt_field0
      size-eos: true
  case__137_elt_field0_0:
    seq:
    - id: len_case__137_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__137_elt_field0
      type: case__137_elt_field0
      size: len_case__137_elt_field0
  case__137_entries:
    seq:
    - id: case__137_elt_field0
      type: case__137_elt_field0_0
    - id: case__137_elt_field1
      type: tree_encoding
  case__138_elt_field0:
    seq:
    - id: case__138_elt_field0
      size-eos: true
  case__138_elt_field0_0:
    seq:
    - id: len_case__138_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__138_elt_field0
      type: case__138_elt_field0
      size: len_case__138_elt_field0
  case__138_entries:
    seq:
    - id: case__138_elt_field0
      type: case__138_elt_field0_0
    - id: case__138_elt_field1
      type: tree_encoding
  case__139_elt_field0:
    seq:
    - id: case__139_elt_field0
      size-eos: true
  case__139_elt_field0_0:
    seq:
    - id: len_case__139_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__139_elt_field0
      type: case__139_elt_field0
      size: len_case__139_elt_field0
  case__139_entries:
    seq:
    - id: case__139_elt_field0
      type: case__139_elt_field0_0
    - id: case__139_elt_field1
      type: tree_encoding
  case__13_field1_entries:
    seq:
    - id: case__13_field1_elt_field0
      type: u1
    - id: case__13_field1_elt_field1
      type: inode_tree
  case__14:
    seq:
    - id: case__14_field0
      type: s4
    - id: case__14_field1
      type: case__14_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: case__14_field1_entries
  case__140_elt_field0:
    seq:
    - id: case__140_elt_field0
      size-eos: true
  case__140_elt_field0_0:
    seq:
    - id: len_case__140_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__140_elt_field0
      type: case__140_elt_field0
      size: len_case__140_elt_field0
  case__140_entries:
    seq:
    - id: case__140_elt_field0
      type: case__140_elt_field0_0
    - id: case__140_elt_field1
      type: tree_encoding
  case__141_elt_field0:
    seq:
    - id: case__141_elt_field0
      size-eos: true
  case__141_elt_field0_0:
    seq:
    - id: len_case__141_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__141_elt_field0
      type: case__141_elt_field0
      size: len_case__141_elt_field0
  case__141_entries:
    seq:
    - id: case__141_elt_field0
      type: case__141_elt_field0_0
    - id: case__141_elt_field1
      type: tree_encoding
  case__142_elt_field0:
    seq:
    - id: case__142_elt_field0
      size-eos: true
  case__142_elt_field0_0:
    seq:
    - id: len_case__142_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__142_elt_field0
      type: case__142_elt_field0
      size: len_case__142_elt_field0
  case__142_entries:
    seq:
    - id: case__142_elt_field0
      type: case__142_elt_field0_0
    - id: case__142_elt_field1
      type: tree_encoding
  case__143_elt_field0:
    seq:
    - id: case__143_elt_field0
      size-eos: true
  case__143_elt_field0_0:
    seq:
    - id: len_case__143_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__143_elt_field0
      type: case__143_elt_field0
      size: len_case__143_elt_field0
  case__143_entries:
    seq:
    - id: case__143_elt_field0
      type: case__143_elt_field0_0
    - id: case__143_elt_field1
      type: tree_encoding
  case__144_elt_field0:
    seq:
    - id: case__144_elt_field0
      size-eos: true
  case__144_elt_field0_0:
    seq:
    - id: len_case__144_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__144_elt_field0
      type: case__144_elt_field0
      size: len_case__144_elt_field0
  case__144_entries:
    seq:
    - id: case__144_elt_field0
      type: case__144_elt_field0_0
    - id: case__144_elt_field1
      type: tree_encoding
  case__145_elt_field0:
    seq:
    - id: case__145_elt_field0
      size-eos: true
  case__145_elt_field0_0:
    seq:
    - id: len_case__145_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__145_elt_field0
      type: case__145_elt_field0
      size: len_case__145_elt_field0
  case__145_entries:
    seq:
    - id: case__145_elt_field0
      type: case__145_elt_field0_0
    - id: case__145_elt_field1
      type: tree_encoding
  case__146_elt_field0:
    seq:
    - id: case__146_elt_field0
      size-eos: true
  case__146_elt_field0_0:
    seq:
    - id: len_case__146_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__146_elt_field0
      type: case__146_elt_field0
      size: len_case__146_elt_field0
  case__146_entries:
    seq:
    - id: case__146_elt_field0
      type: case__146_elt_field0_0
    - id: case__146_elt_field1
      type: tree_encoding
  case__147_elt_field0:
    seq:
    - id: case__147_elt_field0
      size-eos: true
  case__147_elt_field0_0:
    seq:
    - id: len_case__147_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__147_elt_field0
      type: case__147_elt_field0
      size: len_case__147_elt_field0
  case__147_entries:
    seq:
    - id: case__147_elt_field0
      type: case__147_elt_field0_0
    - id: case__147_elt_field1
      type: tree_encoding
  case__148_elt_field0:
    seq:
    - id: case__148_elt_field0
      size-eos: true
  case__148_elt_field0_0:
    seq:
    - id: len_case__148_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__148_elt_field0
      type: case__148_elt_field0
      size: len_case__148_elt_field0
  case__148_entries:
    seq:
    - id: case__148_elt_field0
      type: case__148_elt_field0_0
    - id: case__148_elt_field1
      type: tree_encoding
  case__149_elt_field0:
    seq:
    - id: case__149_elt_field0
      size-eos: true
  case__149_elt_field0_0:
    seq:
    - id: len_case__149_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__149_elt_field0
      type: case__149_elt_field0
      size: len_case__149_elt_field0
  case__149_entries:
    seq:
    - id: case__149_elt_field0
      type: case__149_elt_field0_0
    - id: case__149_elt_field1
      type: tree_encoding
  case__14_field1_entries:
    seq:
    - id: case__14_field1_elt_field0
      type: u1
    - id: case__14_field1_elt_field1
      type: inode_tree
  case__15:
    seq:
    - id: case__15_field0
      type: s8
    - id: case__15_field1
      type: case__15_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: case__15_field1_entries
  case__150_elt_field0:
    seq:
    - id: case__150_elt_field0
      size-eos: true
  case__150_elt_field0_0:
    seq:
    - id: len_case__150_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__150_elt_field0
      type: case__150_elt_field0
      size: len_case__150_elt_field0
  case__150_entries:
    seq:
    - id: case__150_elt_field0
      type: case__150_elt_field0_0
    - id: case__150_elt_field1
      type: tree_encoding
  case__151_elt_field0:
    seq:
    - id: case__151_elt_field0
      size-eos: true
  case__151_elt_field0_0:
    seq:
    - id: len_case__151_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__151_elt_field0
      type: case__151_elt_field0
      size: len_case__151_elt_field0
  case__151_entries:
    seq:
    - id: case__151_elt_field0
      type: case__151_elt_field0_0
    - id: case__151_elt_field1
      type: tree_encoding
  case__152_elt_field0:
    seq:
    - id: case__152_elt_field0
      size-eos: true
  case__152_elt_field0_0:
    seq:
    - id: len_case__152_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__152_elt_field0
      type: case__152_elt_field0
      size: len_case__152_elt_field0
  case__152_entries:
    seq:
    - id: case__152_elt_field0
      type: case__152_elt_field0_0
    - id: case__152_elt_field1
      type: tree_encoding
  case__153_elt_field0:
    seq:
    - id: case__153_elt_field0
      size-eos: true
  case__153_elt_field0_0:
    seq:
    - id: len_case__153_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__153_elt_field0
      type: case__153_elt_field0
      size: len_case__153_elt_field0
  case__153_entries:
    seq:
    - id: case__153_elt_field0
      type: case__153_elt_field0_0
    - id: case__153_elt_field1
      type: tree_encoding
  case__154_elt_field0:
    seq:
    - id: case__154_elt_field0
      size-eos: true
  case__154_elt_field0_0:
    seq:
    - id: len_case__154_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__154_elt_field0
      type: case__154_elt_field0
      size: len_case__154_elt_field0
  case__154_entries:
    seq:
    - id: case__154_elt_field0
      type: case__154_elt_field0_0
    - id: case__154_elt_field1
      type: tree_encoding
  case__155_elt_field0:
    seq:
    - id: case__155_elt_field0
      size-eos: true
  case__155_elt_field0_0:
    seq:
    - id: len_case__155_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__155_elt_field0
      type: case__155_elt_field0
      size: len_case__155_elt_field0
  case__155_entries:
    seq:
    - id: case__155_elt_field0
      type: case__155_elt_field0_0
    - id: case__155_elt_field1
      type: tree_encoding
  case__156_elt_field0:
    seq:
    - id: case__156_elt_field0
      size-eos: true
  case__156_elt_field0_0:
    seq:
    - id: len_case__156_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__156_elt_field0
      type: case__156_elt_field0
      size: len_case__156_elt_field0
  case__156_entries:
    seq:
    - id: case__156_elt_field0
      type: case__156_elt_field0_0
    - id: case__156_elt_field1
      type: tree_encoding
  case__157_elt_field0:
    seq:
    - id: case__157_elt_field0
      size-eos: true
  case__157_elt_field0_0:
    seq:
    - id: len_case__157_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__157_elt_field0
      type: case__157_elt_field0
      size: len_case__157_elt_field0
  case__157_entries:
    seq:
    - id: case__157_elt_field0
      type: case__157_elt_field0_0
    - id: case__157_elt_field1
      type: tree_encoding
  case__158_elt_field0:
    seq:
    - id: case__158_elt_field0
      size-eos: true
  case__158_elt_field0_0:
    seq:
    - id: len_case__158_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__158_elt_field0
      type: case__158_elt_field0
      size: len_case__158_elt_field0
  case__158_entries:
    seq:
    - id: case__158_elt_field0
      type: case__158_elt_field0_0
    - id: case__158_elt_field1
      type: tree_encoding
  case__159_elt_field0:
    seq:
    - id: case__159_elt_field0
      size-eos: true
  case__159_elt_field0_0:
    seq:
    - id: len_case__159_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__159_elt_field0
      type: case__159_elt_field0
      size: len_case__159_elt_field0
  case__159_entries:
    seq:
    - id: case__159_elt_field0
      type: case__159_elt_field0_0
    - id: case__159_elt_field1
      type: tree_encoding
  case__15_field1_entries:
    seq:
    - id: case__15_field1_elt_field0
      type: u1
    - id: case__15_field1_elt_field1
      type: inode_tree
  case__16:
    seq:
    - id: case__16_field0
      type: u1
    - id: case__16_field1
      type: case__16_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: case__16_field1_entries
  case__160_elt_field0:
    seq:
    - id: case__160_elt_field0
      size-eos: true
  case__160_elt_field0_0:
    seq:
    - id: len_case__160_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__160_elt_field0
      type: case__160_elt_field0
      size: len_case__160_elt_field0
  case__160_entries:
    seq:
    - id: case__160_elt_field0
      type: case__160_elt_field0_0
    - id: case__160_elt_field1
      type: tree_encoding
  case__161_elt_field0:
    seq:
    - id: case__161_elt_field0
      size-eos: true
  case__161_elt_field0_0:
    seq:
    - id: len_case__161_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__161_elt_field0
      type: case__161_elt_field0
      size: len_case__161_elt_field0
  case__161_entries:
    seq:
    - id: case__161_elt_field0
      type: case__161_elt_field0_0
    - id: case__161_elt_field1
      type: tree_encoding
  case__162_elt_field0:
    seq:
    - id: case__162_elt_field0
      size-eos: true
  case__162_elt_field0_0:
    seq:
    - id: len_case__162_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__162_elt_field0
      type: case__162_elt_field0
      size: len_case__162_elt_field0
  case__162_entries:
    seq:
    - id: case__162_elt_field0
      type: case__162_elt_field0_0
    - id: case__162_elt_field1
      type: tree_encoding
  case__163_elt_field0:
    seq:
    - id: case__163_elt_field0
      size-eos: true
  case__163_elt_field0_0:
    seq:
    - id: len_case__163_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__163_elt_field0
      type: case__163_elt_field0
      size: len_case__163_elt_field0
  case__163_entries:
    seq:
    - id: case__163_elt_field0
      type: case__163_elt_field0_0
    - id: case__163_elt_field1
      type: tree_encoding
  case__164_elt_field0:
    seq:
    - id: case__164_elt_field0
      size-eos: true
  case__164_elt_field0_0:
    seq:
    - id: len_case__164_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__164_elt_field0
      type: case__164_elt_field0
      size: len_case__164_elt_field0
  case__164_entries:
    seq:
    - id: case__164_elt_field0
      type: case__164_elt_field0_0
    - id: case__164_elt_field1
      type: tree_encoding
  case__165_elt_field0:
    seq:
    - id: case__165_elt_field0
      size-eos: true
  case__165_elt_field0_0:
    seq:
    - id: len_case__165_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__165_elt_field0
      type: case__165_elt_field0
      size: len_case__165_elt_field0
  case__165_entries:
    seq:
    - id: case__165_elt_field0
      type: case__165_elt_field0_0
    - id: case__165_elt_field1
      type: tree_encoding
  case__166_elt_field0:
    seq:
    - id: case__166_elt_field0
      size-eos: true
  case__166_elt_field0_0:
    seq:
    - id: len_case__166_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__166_elt_field0
      type: case__166_elt_field0
      size: len_case__166_elt_field0
  case__166_entries:
    seq:
    - id: case__166_elt_field0
      type: case__166_elt_field0_0
    - id: case__166_elt_field1
      type: tree_encoding
  case__167_elt_field0:
    seq:
    - id: case__167_elt_field0
      size-eos: true
  case__167_elt_field0_0:
    seq:
    - id: len_case__167_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__167_elt_field0
      type: case__167_elt_field0
      size: len_case__167_elt_field0
  case__167_entries:
    seq:
    - id: case__167_elt_field0
      type: case__167_elt_field0_0
    - id: case__167_elt_field1
      type: tree_encoding
  case__168_elt_field0:
    seq:
    - id: case__168_elt_field0
      size-eos: true
  case__168_elt_field0_0:
    seq:
    - id: len_case__168_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__168_elt_field0
      type: case__168_elt_field0
      size: len_case__168_elt_field0
  case__168_entries:
    seq:
    - id: case__168_elt_field0
      type: case__168_elt_field0_0
    - id: case__168_elt_field1
      type: tree_encoding
  case__169_elt_field0:
    seq:
    - id: case__169_elt_field0
      size-eos: true
  case__169_elt_field0_0:
    seq:
    - id: len_case__169_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__169_elt_field0
      type: case__169_elt_field0
      size: len_case__169_elt_field0
  case__169_entries:
    seq:
    - id: case__169_elt_field0
      type: case__169_elt_field0_0
    - id: case__169_elt_field1
      type: tree_encoding
  case__16_field1_entries:
    seq:
    - id: case__16_field1_elt_field0
      type: u1
    - id: case__16_field1_elt_field1
      type: inode_tree
  case__17:
    seq:
    - id: case__17_field0
      type: u2
    - id: case__17_field1
      type: case__17_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: case__17_field1_entries
  case__170_elt_field0:
    seq:
    - id: case__170_elt_field0
      size-eos: true
  case__170_elt_field0_0:
    seq:
    - id: len_case__170_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__170_elt_field0
      type: case__170_elt_field0
      size: len_case__170_elt_field0
  case__170_entries:
    seq:
    - id: case__170_elt_field0
      type: case__170_elt_field0_0
    - id: case__170_elt_field1
      type: tree_encoding
  case__171_elt_field0:
    seq:
    - id: case__171_elt_field0
      size-eos: true
  case__171_elt_field0_0:
    seq:
    - id: len_case__171_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__171_elt_field0
      type: case__171_elt_field0
      size: len_case__171_elt_field0
  case__171_entries:
    seq:
    - id: case__171_elt_field0
      type: case__171_elt_field0_0
    - id: case__171_elt_field1
      type: tree_encoding
  case__172_elt_field0:
    seq:
    - id: case__172_elt_field0
      size-eos: true
  case__172_elt_field0_0:
    seq:
    - id: len_case__172_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__172_elt_field0
      type: case__172_elt_field0
      size: len_case__172_elt_field0
  case__172_entries:
    seq:
    - id: case__172_elt_field0
      type: case__172_elt_field0_0
    - id: case__172_elt_field1
      type: tree_encoding
  case__173_elt_field0:
    seq:
    - id: case__173_elt_field0
      size-eos: true
  case__173_elt_field0_0:
    seq:
    - id: len_case__173_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__173_elt_field0
      type: case__173_elt_field0
      size: len_case__173_elt_field0
  case__173_entries:
    seq:
    - id: case__173_elt_field0
      type: case__173_elt_field0_0
    - id: case__173_elt_field1
      type: tree_encoding
  case__174_elt_field0:
    seq:
    - id: case__174_elt_field0
      size-eos: true
  case__174_elt_field0_0:
    seq:
    - id: len_case__174_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__174_elt_field0
      type: case__174_elt_field0
      size: len_case__174_elt_field0
  case__174_entries:
    seq:
    - id: case__174_elt_field0
      type: case__174_elt_field0_0
    - id: case__174_elt_field1
      type: tree_encoding
  case__175_elt_field0:
    seq:
    - id: case__175_elt_field0
      size-eos: true
  case__175_elt_field0_0:
    seq:
    - id: len_case__175_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__175_elt_field0
      type: case__175_elt_field0
      size: len_case__175_elt_field0
  case__175_entries:
    seq:
    - id: case__175_elt_field0
      type: case__175_elt_field0_0
    - id: case__175_elt_field1
      type: tree_encoding
  case__176_elt_field0:
    seq:
    - id: case__176_elt_field0
      size-eos: true
  case__176_elt_field0_0:
    seq:
    - id: len_case__176_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__176_elt_field0
      type: case__176_elt_field0
      size: len_case__176_elt_field0
  case__176_entries:
    seq:
    - id: case__176_elt_field0
      type: case__176_elt_field0_0
    - id: case__176_elt_field1
      type: tree_encoding
  case__177_elt_field0:
    seq:
    - id: case__177_elt_field0
      size-eos: true
  case__177_elt_field0_0:
    seq:
    - id: len_case__177_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__177_elt_field0
      type: case__177_elt_field0
      size: len_case__177_elt_field0
  case__177_entries:
    seq:
    - id: case__177_elt_field0
      type: case__177_elt_field0_0
    - id: case__177_elt_field1
      type: tree_encoding
  case__178_elt_field0:
    seq:
    - id: case__178_elt_field0
      size-eos: true
  case__178_elt_field0_0:
    seq:
    - id: len_case__178_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__178_elt_field0
      type: case__178_elt_field0
      size: len_case__178_elt_field0
  case__178_entries:
    seq:
    - id: case__178_elt_field0
      type: case__178_elt_field0_0
    - id: case__178_elt_field1
      type: tree_encoding
  case__179_elt_field0:
    seq:
    - id: case__179_elt_field0
      size-eos: true
  case__179_elt_field0_0:
    seq:
    - id: len_case__179_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__179_elt_field0
      type: case__179_elt_field0
      size: len_case__179_elt_field0
  case__179_entries:
    seq:
    - id: case__179_elt_field0
      type: case__179_elt_field0_0
    - id: case__179_elt_field1
      type: tree_encoding
  case__17_field1_entries:
    seq:
    - id: case__17_field1_elt_field0
      type: u1
    - id: case__17_field1_elt_field1
      type: inode_tree
  case__18:
    seq:
    - id: case__18_field0
      type: s4
    - id: case__18_field1
      type: case__18_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: case__18_field1_entries
  case__180_elt_field0:
    seq:
    - id: case__180_elt_field0
      size-eos: true
  case__180_elt_field0_0:
    seq:
    - id: len_case__180_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__180_elt_field0
      type: case__180_elt_field0
      size: len_case__180_elt_field0
  case__180_entries:
    seq:
    - id: case__180_elt_field0
      type: case__180_elt_field0_0
    - id: case__180_elt_field1
      type: tree_encoding
  case__181_elt_field0:
    seq:
    - id: case__181_elt_field0
      size-eos: true
  case__181_elt_field0_0:
    seq:
    - id: len_case__181_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__181_elt_field0
      type: case__181_elt_field0
      size: len_case__181_elt_field0
  case__181_entries:
    seq:
    - id: case__181_elt_field0
      type: case__181_elt_field0_0
    - id: case__181_elt_field1
      type: tree_encoding
  case__182_elt_field0:
    seq:
    - id: case__182_elt_field0
      size-eos: true
  case__182_elt_field0_0:
    seq:
    - id: len_case__182_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__182_elt_field0
      type: case__182_elt_field0
      size: len_case__182_elt_field0
  case__182_entries:
    seq:
    - id: case__182_elt_field0
      type: case__182_elt_field0_0
    - id: case__182_elt_field1
      type: tree_encoding
  case__183_elt_field0:
    seq:
    - id: case__183_elt_field0
      size-eos: true
  case__183_elt_field0_0:
    seq:
    - id: len_case__183_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__183_elt_field0
      type: case__183_elt_field0
      size: len_case__183_elt_field0
  case__183_entries:
    seq:
    - id: case__183_elt_field0
      type: case__183_elt_field0_0
    - id: case__183_elt_field1
      type: tree_encoding
  case__184_elt_field0:
    seq:
    - id: case__184_elt_field0
      size-eos: true
  case__184_elt_field0_0:
    seq:
    - id: len_case__184_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__184_elt_field0
      type: case__184_elt_field0
      size: len_case__184_elt_field0
  case__184_entries:
    seq:
    - id: case__184_elt_field0
      type: case__184_elt_field0_0
    - id: case__184_elt_field1
      type: tree_encoding
  case__185_elt_field0:
    seq:
    - id: case__185_elt_field0
      size-eos: true
  case__185_elt_field0_0:
    seq:
    - id: len_case__185_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__185_elt_field0
      type: case__185_elt_field0
      size: len_case__185_elt_field0
  case__185_entries:
    seq:
    - id: case__185_elt_field0
      type: case__185_elt_field0_0
    - id: case__185_elt_field1
      type: tree_encoding
  case__186_elt_field0:
    seq:
    - id: case__186_elt_field0
      size-eos: true
  case__186_elt_field0_0:
    seq:
    - id: len_case__186_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__186_elt_field0
      type: case__186_elt_field0
      size: len_case__186_elt_field0
  case__186_entries:
    seq:
    - id: case__186_elt_field0
      type: case__186_elt_field0_0
    - id: case__186_elt_field1
      type: tree_encoding
  case__187_elt_field0:
    seq:
    - id: case__187_elt_field0
      size-eos: true
  case__187_elt_field0_0:
    seq:
    - id: len_case__187_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__187_elt_field0
      type: case__187_elt_field0
      size: len_case__187_elt_field0
  case__187_entries:
    seq:
    - id: case__187_elt_field0
      type: case__187_elt_field0_0
    - id: case__187_elt_field1
      type: tree_encoding
  case__188_elt_field0:
    seq:
    - id: case__188_elt_field0
      size-eos: true
  case__188_elt_field0_0:
    seq:
    - id: len_case__188_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__188_elt_field0
      type: case__188_elt_field0
      size: len_case__188_elt_field0
  case__188_entries:
    seq:
    - id: case__188_elt_field0
      type: case__188_elt_field0_0
    - id: case__188_elt_field1
      type: tree_encoding
  case__189_elt_field0:
    seq:
    - id: case__189_elt_field0
      size-eos: true
  case__189_elt_field0_0:
    seq:
    - id: len_case__189_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__189_elt_field0
      type: case__189_elt_field0
      size: len_case__189_elt_field0
  case__189_entries:
    seq:
    - id: case__189_elt_field0
      type: case__189_elt_field0_0
    - id: case__189_elt_field1
      type: tree_encoding
  case__18_field1_entries:
    seq:
    - id: case__18_field1_elt_field0
      type: u1
    - id: case__18_field1_elt_field1
      type: inode_tree
  case__19:
    seq:
    - id: case__19_field0
      type: s8
    - id: case__19_field1
      type: case__19_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: case__19_field1_entries
  case__190_elt_field0:
    seq:
    - id: case__190_elt_field0
      size-eos: true
  case__190_elt_field0_0:
    seq:
    - id: len_case__190_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__190_elt_field0
      type: case__190_elt_field0
      size: len_case__190_elt_field0
  case__190_entries:
    seq:
    - id: case__190_elt_field0
      type: case__190_elt_field0_0
    - id: case__190_elt_field1
      type: tree_encoding
  case__191:
    seq:
    - id: case__191_entries
      type: case__191_entries
      repeat: eos
  case__191_0:
    seq:
    - id: len_case__191
      type: u4
      valid:
        max: 1073741823
    - id: case__191
      type: case__191
      size: len_case__191
  case__191_1:
    seq:
    - id: len_case__191
      type: u4
      valid:
        max: 1073741823
    - id: case__191
      type: case__191
      size: len_case__191
  case__191_elt_field0:
    seq:
    - id: case__191_elt_field0
      size-eos: true
  case__191_elt_field0_0:
    seq:
    - id: len_case__191_elt_field0
      type: u1
      valid:
        max: 255
    - id: case__191_elt_field0
      type: case__191_elt_field0
      size: len_case__191_elt_field0
  case__191_entries:
    seq:
    - id: case__191_elt_field0
      type: case__191_elt_field0_0
    - id: case__191_elt_field1
      type: tree_encoding
  case__192:
    seq:
    - id: case__192
      size-eos: true
  case__192_0:
    seq:
    - id: len_case__192
      type: u1
      valid:
        max: 255
    - id: case__192
      type: case__192
      size: len_case__192
  case__193:
    seq:
    - id: case__193
      size-eos: true
  case__193_0:
    seq:
    - id: len_case__193
      type: u2
      valid:
        max: 65535
    - id: case__193
      type: case__193
      size: len_case__193
  case__19_field1_entries:
    seq:
    - id: case__19_field1_elt_field0
      type: u1
    - id: case__19_field1_elt_field1
      type: inode_tree
  case__2:
    seq:
    - id: case__2_field0
      type: s2
    - id: case__2_field1
      size: 32
      doc: context_hash
    - id: case__2_field2
      size: 32
      doc: context_hash
    - id: case__2_field3
      type: tree_encoding
  case__20:
    seq:
    - id: case__20_field0
      type: u1
    - id: case__20_field1
      type: case__20_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: case__20_field1_entries
  case__208:
    seq:
    - id: case__208_field0
      type: u1
    - id: case__208_field1
      type: case__208_field1_0
    - id: case__208_field2
      type: inode_tree
  case__208_field1:
    seq:
    - id: case__208_field1
      size-eos: true
  case__208_field1_0:
    seq:
    - id: len_case__208_field1
      type: u1
      valid:
        max: 255
    - id: case__208_field1
      type: case__208_field1
      size: len_case__208_field1
  case__209:
    seq:
    - id: case__209_field0
      type: u2
    - id: case__209_field1
      type: case__209_field1_0
    - id: case__209_field2
      type: inode_tree
  case__209_field1:
    seq:
    - id: case__209_field1
      size-eos: true
  case__209_field1_0:
    seq:
    - id: len_case__209_field1
      type: u1
      valid:
        max: 255
    - id: case__209_field1
      type: case__209_field1
      size: len_case__209_field1
  case__20_field1_entries:
    seq:
    - id: case__20_field1_elt_field0
      type: u1
    - id: case__20_field1_elt_field1
      type: inode_tree
  case__21:
    seq:
    - id: case__21_field0
      type: u2
    - id: case__21_field1
      type: case__21_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: case__21_field1_entries
  case__210:
    seq:
    - id: case__210_field0
      type: s4
    - id: case__210_field1
      type: case__210_field1_0
    - id: case__210_field2
      type: inode_tree
  case__210_field1:
    seq:
    - id: case__210_field1
      size-eos: true
  case__210_field1_0:
    seq:
    - id: len_case__210_field1
      type: u1
      valid:
        max: 255
    - id: case__210_field1
      type: case__210_field1
      size: len_case__210_field1
  case__211:
    seq:
    - id: case__211_field0
      type: s8
    - id: case__211_field1
      type: case__211_field1_0
    - id: case__211_field2
      type: inode_tree
  case__211_field1:
    seq:
    - id: case__211_field1
      size-eos: true
  case__211_field1_0:
    seq:
    - id: len_case__211_field1
      type: u1
      valid:
        max: 255
    - id: case__211_field1
      type: case__211_field1
      size: len_case__211_field1
  case__216:
    seq:
    - id: case__216_field0
      type: u1
    - id: case__216_field1
      type: case__216_field1_0
    - id: case__216_field2
      type: inode_tree
  case__216_field1:
    seq:
    - id: case__216_field1
      size-eos: true
  case__216_field1_0:
    seq:
    - id: len_case__216_field1
      type: u1
      valid:
        max: 255
    - id: case__216_field1
      type: case__216_field1
      size: len_case__216_field1
  case__217:
    seq:
    - id: case__217_field0
      type: u2
    - id: case__217_field1
      type: case__217_field1_0
    - id: case__217_field2
      type: inode_tree
  case__217_field1:
    seq:
    - id: case__217_field1
      size-eos: true
  case__217_field1_0:
    seq:
    - id: len_case__217_field1
      type: u1
      valid:
        max: 255
    - id: case__217_field1
      type: case__217_field1
      size: len_case__217_field1
  case__218:
    seq:
    - id: case__218_field0
      type: s4
    - id: case__218_field1
      type: case__218_field1_0
    - id: case__218_field2
      type: inode_tree
  case__218_field1:
    seq:
    - id: case__218_field1
      size-eos: true
  case__218_field1_0:
    seq:
    - id: len_case__218_field1
      type: u1
      valid:
        max: 255
    - id: case__218_field1
      type: case__218_field1
      size: len_case__218_field1
  case__219:
    seq:
    - id: case__219_field0
      type: s8
    - id: case__219_field1
      type: case__219_field1_0
    - id: case__219_field2
      type: inode_tree
  case__219_field1:
    seq:
    - id: case__219_field1
      size-eos: true
  case__219_field1_0:
    seq:
    - id: len_case__219_field1
      type: u1
      valid:
        max: 255
    - id: case__219_field1
      type: case__219_field1
      size: len_case__219_field1
  case__21_field1_entries:
    seq:
    - id: case__21_field1_elt_field0
      type: u1
    - id: case__21_field1_elt_field1
      type: inode_tree
  case__22:
    seq:
    - id: case__22_field0
      type: s4
    - id: case__22_field1
      type: case__22_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: case__22_field1_entries
  case__22_field1_entries:
    seq:
    - id: case__22_field1_elt_field0
      type: u1
    - id: case__22_field1_elt_field1
      type: inode_tree
  case__23:
    seq:
    - id: case__23_field0
      type: s8
    - id: case__23_field1
      type: case__23_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: case__23_field1_entries
  case__23_field1_entries:
    seq:
    - id: case__23_field1_elt_field0
      type: u1
    - id: case__23_field1_elt_field1
      type: inode_tree
  case__24:
    seq:
    - id: case__24_field0
      type: u1
    - id: case__24_field1
      type: case__24_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: case__24_field1_entries
  case__24_field1_entries:
    seq:
    - id: case__24_field1_elt_field0
      type: u1
    - id: case__24_field1_elt_field1
      type: inode_tree
  case__25:
    seq:
    - id: case__25_field0
      type: u2
    - id: case__25_field1
      type: case__25_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: case__25_field1_entries
  case__25_field1_entries:
    seq:
    - id: case__25_field1_elt_field0
      type: u1
    - id: case__25_field1_elt_field1
      type: inode_tree
  case__26:
    seq:
    - id: case__26_field0
      type: s4
    - id: case__26_field1
      type: case__26_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: case__26_field1_entries
  case__26_field1_entries:
    seq:
    - id: case__26_field1_elt_field0
      type: u1
    - id: case__26_field1_elt_field1
      type: inode_tree
  case__27:
    seq:
    - id: case__27_field0
      type: s8
    - id: case__27_field1
      type: case__27_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: case__27_field1_entries
  case__27_field1_entries:
    seq:
    - id: case__27_field1_elt_field0
      type: u1
    - id: case__27_field1_elt_field1
      type: inode_tree
  case__28:
    seq:
    - id: case__28_field0
      type: u1
    - id: case__28_field1
      type: case__28_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: case__28_field1_entries
  case__28_field1_entries:
    seq:
    - id: case__28_field1_elt_field0
      type: u1
    - id: case__28_field1_elt_field1
      type: inode_tree
  case__29:
    seq:
    - id: case__29_field0
      type: u2
    - id: case__29_field1
      type: case__29_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: case__29_field1_entries
  case__29_field1_entries:
    seq:
    - id: case__29_field1_elt_field0
      type: u1
    - id: case__29_field1_elt_field1
      type: inode_tree
  case__3:
    seq:
    - id: case__3_field0
      type: s2
    - id: case__3_field1
      size: 32
      doc: context_hash
    - id: case__3_field2
      size: 32
      doc: context_hash
    - id: case__3_field3
      type: tree_encoding
  case__30:
    seq:
    - id: case__30_field0
      type: s4
    - id: case__30_field1
      type: case__30_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: case__30_field1_entries
  case__30_field1_entries:
    seq:
    - id: case__30_field1_elt_field0
      type: u1
    - id: case__30_field1_elt_field1
      type: inode_tree
  case__31:
    seq:
    - id: case__31_field0
      type: s8
    - id: case__31_field1
      type: case__31_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: case__31_field1_entries
  case__31_field1_entries:
    seq:
    - id: case__31_field1_elt_field0
      type: u1
    - id: case__31_field1_elt_field1
      type: inode_tree
  case__32:
    seq:
    - id: case__32_field0
      type: u1
    - id: case__32_field1
      type: case__32_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: case__32_field1_entries
  case__32_field1_entries:
    seq:
    - id: case__32_field1_elt_field0
      type: u1
    - id: case__32_field1_elt_field1
      type: inode_tree
  case__33:
    seq:
    - id: case__33_field0
      type: u2
    - id: case__33_field1
      type: case__33_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: case__33_field1_entries
  case__33_field1_entries:
    seq:
    - id: case__33_field1_elt_field0
      type: u1
    - id: case__33_field1_elt_field1
      type: inode_tree
  case__34:
    seq:
    - id: case__34_field0
      type: s4
    - id: case__34_field1
      type: case__34_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: case__34_field1_entries
  case__34_field1_entries:
    seq:
    - id: case__34_field1_elt_field0
      type: u1
    - id: case__34_field1_elt_field1
      type: inode_tree
  case__35:
    seq:
    - id: case__35_field0
      type: s8
    - id: case__35_field1
      type: case__35_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: case__35_field1_entries
  case__35_field1_entries:
    seq:
    - id: case__35_field1_elt_field0
      type: u1
    - id: case__35_field1_elt_field1
      type: inode_tree
  case__36:
    seq:
    - id: case__36_field0
      type: u1
    - id: case__36_field1
      type: case__36_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: case__36_field1_entries
  case__36_field1_entries:
    seq:
    - id: case__36_field1_elt_field0
      type: u1
    - id: case__36_field1_elt_field1
      type: inode_tree
  case__37:
    seq:
    - id: case__37_field0
      type: u2
    - id: case__37_field1
      type: case__37_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: case__37_field1_entries
  case__37_field1_entries:
    seq:
    - id: case__37_field1_elt_field0
      type: u1
    - id: case__37_field1_elt_field1
      type: inode_tree
  case__38:
    seq:
    - id: case__38_field0
      type: s4
    - id: case__38_field1
      type: case__38_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: case__38_field1_entries
  case__38_field1_entries:
    seq:
    - id: case__38_field1_elt_field0
      type: u1
    - id: case__38_field1_elt_field1
      type: inode_tree
  case__39:
    seq:
    - id: case__39_field0
      type: s8
    - id: case__39_field1
      type: case__39_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: case__39_field1_entries
  case__39_field1_entries:
    seq:
    - id: case__39_field1_elt_field0
      type: u1
    - id: case__39_field1_elt_field1
      type: inode_tree
  case__4:
    seq:
    - id: case__4_field0
      type: u1
    - id: case__4_field1
      type: case__4_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: case__4_field1_entries
  case__40:
    seq:
    - id: case__40_field0
      type: u1
    - id: case__40_field1
      type: case__40_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: case__40_field1_entries
  case__40_field1_entries:
    seq:
    - id: case__40_field1_elt_field0
      type: u1
    - id: case__40_field1_elt_field1
      type: inode_tree
  case__41:
    seq:
    - id: case__41_field0
      type: u2
    - id: case__41_field1
      type: case__41_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: case__41_field1_entries
  case__41_field1_entries:
    seq:
    - id: case__41_field1_elt_field0
      type: u1
    - id: case__41_field1_elt_field1
      type: inode_tree
  case__42:
    seq:
    - id: case__42_field0
      type: s4
    - id: case__42_field1
      type: case__42_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: case__42_field1_entries
  case__42_field1_entries:
    seq:
    - id: case__42_field1_elt_field0
      type: u1
    - id: case__42_field1_elt_field1
      type: inode_tree
  case__43:
    seq:
    - id: case__43_field0
      type: s8
    - id: case__43_field1
      type: case__43_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: case__43_field1_entries
  case__43_field1_entries:
    seq:
    - id: case__43_field1_elt_field0
      type: u1
    - id: case__43_field1_elt_field1
      type: inode_tree
  case__44:
    seq:
    - id: case__44_field0
      type: u1
    - id: case__44_field1
      type: case__44_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: case__44_field1_entries
  case__44_field1_entries:
    seq:
    - id: case__44_field1_elt_field0
      type: u1
    - id: case__44_field1_elt_field1
      type: inode_tree
  case__45:
    seq:
    - id: case__45_field0
      type: u2
    - id: case__45_field1
      type: case__45_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: case__45_field1_entries
  case__45_field1_entries:
    seq:
    - id: case__45_field1_elt_field0
      type: u1
    - id: case__45_field1_elt_field1
      type: inode_tree
  case__46:
    seq:
    - id: case__46_field0
      type: s4
    - id: case__46_field1
      type: case__46_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: case__46_field1_entries
  case__46_field1_entries:
    seq:
    - id: case__46_field1_elt_field0
      type: u1
    - id: case__46_field1_elt_field1
      type: inode_tree
  case__47:
    seq:
    - id: case__47_field0
      type: s8
    - id: case__47_field1
      type: case__47_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: case__47_field1_entries
  case__47_field1_entries:
    seq:
    - id: case__47_field1_elt_field0
      type: u1
    - id: case__47_field1_elt_field1
      type: inode_tree
  case__48:
    seq:
    - id: case__48_field0
      type: u1
    - id: case__48_field1
      type: case__48_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: case__48_field1_entries
  case__48_field1_entries:
    seq:
    - id: case__48_field1_elt_field0
      type: u1
    - id: case__48_field1_elt_field1
      type: inode_tree
  case__49:
    seq:
    - id: case__49_field0
      type: u2
    - id: case__49_field1
      type: case__49_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: case__49_field1_entries
  case__49_field1_entries:
    seq:
    - id: case__49_field1_elt_field0
      type: u1
    - id: case__49_field1_elt_field1
      type: inode_tree
  case__4_0:
    seq:
    - id: case__4_field0
      type: u1
    - id: case__4_field1
      type: case__4_field1_entries_0
      repeat: expr
      repeat-expr: 1
      doc: case__4_field1_entries
  case__4_field1_entries:
    seq:
    - id: case__4_field1_elt_field0
      type: u1
    - id: case__4_field1_elt_field1
      type: inode_tree
  case__4_field1_entries_0:
    seq:
    - id: case__4_field1_elt_field0
      type: u1
    - id: case__4_field1_elt_field1
      type: inode_tree
      doc: inode_tree
  case__5:
    seq:
    - id: case__5_field0
      type: u2
    - id: case__5_field1
      type: case__5_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: case__5_field1_entries
  case__50:
    seq:
    - id: case__50_field0
      type: s4
    - id: case__50_field1
      type: case__50_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: case__50_field1_entries
  case__50_field1_entries:
    seq:
    - id: case__50_field1_elt_field0
      type: u1
    - id: case__50_field1_elt_field1
      type: inode_tree
  case__51:
    seq:
    - id: case__51_field0
      type: s8
    - id: case__51_field1
      type: case__51_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: case__51_field1_entries
  case__51_field1_entries:
    seq:
    - id: case__51_field1_elt_field0
      type: u1
    - id: case__51_field1_elt_field1
      type: inode_tree
  case__52:
    seq:
    - id: case__52_field0
      type: u1
    - id: case__52_field1
      type: case__52_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: case__52_field1_entries
  case__52_field1_entries:
    seq:
    - id: case__52_field1_elt_field0
      type: u1
    - id: case__52_field1_elt_field1
      type: inode_tree
  case__53:
    seq:
    - id: case__53_field0
      type: u2
    - id: case__53_field1
      type: case__53_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: case__53_field1_entries
  case__53_field1_entries:
    seq:
    - id: case__53_field1_elt_field0
      type: u1
    - id: case__53_field1_elt_field1
      type: inode_tree
  case__54:
    seq:
    - id: case__54_field0
      type: s4
    - id: case__54_field1
      type: case__54_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: case__54_field1_entries
  case__54_field1_entries:
    seq:
    - id: case__54_field1_elt_field0
      type: u1
    - id: case__54_field1_elt_field1
      type: inode_tree
  case__55:
    seq:
    - id: case__55_field0
      type: s8
    - id: case__55_field1
      type: case__55_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: case__55_field1_entries
  case__55_field1_entries:
    seq:
    - id: case__55_field1_elt_field0
      type: u1
    - id: case__55_field1_elt_field1
      type: inode_tree
  case__56:
    seq:
    - id: case__56_field0
      type: u1
    - id: case__56_field1
      type: case__56_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: case__56_field1_entries
  case__56_field1_entries:
    seq:
    - id: case__56_field1_elt_field0
      type: u1
    - id: case__56_field1_elt_field1
      type: inode_tree
  case__57:
    seq:
    - id: case__57_field0
      type: u2
    - id: case__57_field1
      type: case__57_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: case__57_field1_entries
  case__57_field1_entries:
    seq:
    - id: case__57_field1_elt_field0
      type: u1
    - id: case__57_field1_elt_field1
      type: inode_tree
  case__58:
    seq:
    - id: case__58_field0
      type: s4
    - id: case__58_field1
      type: case__58_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: case__58_field1_entries
  case__58_field1_entries:
    seq:
    - id: case__58_field1_elt_field0
      type: u1
    - id: case__58_field1_elt_field1
      type: inode_tree
  case__59:
    seq:
    - id: case__59_field0
      type: s8
    - id: case__59_field1
      type: case__59_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: case__59_field1_entries
  case__59_field1_entries:
    seq:
    - id: case__59_field1_elt_field0
      type: u1
    - id: case__59_field1_elt_field1
      type: inode_tree
  case__5_field1_entries:
    seq:
    - id: case__5_field1_elt_field0
      type: u1
    - id: case__5_field1_elt_field1
      type: inode_tree
  case__6:
    seq:
    - id: case__6_field0
      type: s4
    - id: case__6_field1
      type: case__6_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: case__6_field1_entries
  case__60:
    seq:
    - id: case__60_field0
      type: u1
    - id: case__60_field1
      type: case__60_field1_0
  case__60_field1:
    seq:
    - id: case__60_field1_entries
      type: case__60_field1_entries
      repeat: eos
  case__60_field1_0:
    seq:
    - id: len_case__60_field1
      type: u4
      valid:
        max: 1073741823
    - id: case__60_field1
      type: case__60_field1
      size: len_case__60_field1
  case__60_field1_entries:
    seq:
    - id: case__60_field1_elt_field0
      type: u1
    - id: case__60_field1_elt_field1
      type: inode_tree
  case__61:
    seq:
    - id: case__61_field0
      type: u2
    - id: case__61_field1
      type: case__61_field1_0
  case__61_field1:
    seq:
    - id: case__61_field1_entries
      type: case__61_field1_entries
      repeat: eos
  case__61_field1_0:
    seq:
    - id: len_case__61_field1
      type: u4
      valid:
        max: 1073741823
    - id: case__61_field1
      type: case__61_field1
      size: len_case__61_field1
  case__61_field1_entries:
    seq:
    - id: case__61_field1_elt_field0
      type: u1
    - id: case__61_field1_elt_field1
      type: inode_tree
  case__62:
    seq:
    - id: case__62_field0
      type: s4
    - id: case__62_field1
      type: case__62_field1_0
  case__62_field1:
    seq:
    - id: case__62_field1_entries
      type: case__62_field1_entries
      repeat: eos
  case__62_field1_0:
    seq:
    - id: len_case__62_field1
      type: u4
      valid:
        max: 1073741823
    - id: case__62_field1
      type: case__62_field1
      size: len_case__62_field1
  case__62_field1_entries:
    seq:
    - id: case__62_field1_elt_field0
      type: u1
    - id: case__62_field1_elt_field1
      type: inode_tree
  case__63:
    seq:
    - id: case__63_field0
      type: s8
    - id: case__63_field1
      type: case__63_field1_0
  case__63_field1:
    seq:
    - id: case__63_field1_entries
      type: case__63_field1_entries
      repeat: eos
  case__63_field1_0:
    seq:
    - id: len_case__63_field1
      type: u4
      valid:
        max: 1073741823
    - id: case__63_field1
      type: case__63_field1
      size: len_case__63_field1
  case__63_field1_entries:
    seq:
    - id: case__63_field1_elt_field0
      type: u1
    - id: case__63_field1_elt_field1
      type: inode_tree
  case__64:
    seq:
    - id: case__64_field0
      type: u1
    - id: case__64_field1
      type: case__64_field1_entries
      repeat: expr
      repeat-expr: 32
      doc: case__64_field1_entries
  case__64_field1_entries:
    seq:
    - id: case__64_field1_elt
      type: inode_tree
  case__65:
    seq:
    - id: case__65_field0
      type: u2
    - id: case__65_field1
      type: case__65_field1_entries
      repeat: expr
      repeat-expr: 32
      doc: case__65_field1_entries
  case__65_field1_entries:
    seq:
    - id: case__65_field1_elt
      type: inode_tree
  case__66:
    seq:
    - id: case__66_field0
      type: s4
    - id: case__66_field1
      type: case__66_field1_entries
      repeat: expr
      repeat-expr: 32
      doc: case__66_field1_entries
  case__66_field1_entries:
    seq:
    - id: case__66_field1_elt
      type: inode_tree
  case__67:
    seq:
    - id: case__67_field0
      type: s8
    - id: case__67_field1
      type: case__67_field1_entries
      repeat: expr
      repeat-expr: 32
      doc: case__67_field1_entries
  case__67_field1_entries:
    seq:
    - id: case__67_field1_elt
      type: inode_tree
  case__6_field1_entries:
    seq:
    - id: case__6_field1_elt_field0
      type: u1
    - id: case__6_field1_elt_field1
      type: inode_tree
  case__7:
    seq:
    - id: case__7_field0
      type: s8
    - id: case__7_field1
      type: case__7_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: case__7_field1_entries
  case__7_field1_entries:
    seq:
    - id: case__7_field1_elt_field0
      type: u1
    - id: case__7_field1_elt_field1
      type: inode_tree
  case__8:
    seq:
    - id: case__8_field0
      type: u1
    - id: case__8_field1
      type: case__8_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: case__8_field1_entries
  case__8_field1_entries:
    seq:
    - id: case__8_field1_elt_field0
      type: u1
    - id: case__8_field1_elt_field1
      type: inode_tree
  case__9:
    seq:
    - id: case__9_field0
      type: u2
    - id: case__9_field1
      type: case__9_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: case__9_field1_entries
  case__9_field1_entries:
    seq:
    - id: case__9_field1_elt_field0
      type: u1
    - id: case__9_field1_elt_field1
      type: inode_tree
  circuits_info:
    seq:
    - id: circuits_info_entries
      type: circuits_info_entries
      repeat: eos
  circuits_info_0:
    seq:
    - id: len_circuits_info
      type: u4
      valid:
        max: 1073741823
    - id: circuits_info
      type: circuits_info
      size: len_circuits_info
  circuits_info_entries:
    seq:
    - id: circuits_info_elt_field0
      type: bytes_dyn_uint30
    - id: circuits_info_elt_field1
      type: u1
      enum: bool
  commitment:
    seq:
    - id: level
      type: s4
    - id: messages
      type: messages_0
    - id: predecessor
      type: predecessor
    - id: inbox_merkle_root
      size: 32
  commitment_0:
    seq:
    - id: compressed_state
      size: 32
    - id: inbox_level
      type: s4
    - id: predecessor
      size: 32
    - id: number_of_ticks
      type: s8
  dal_publish_slot_header:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: slot
      type: slot
  dal_slot_availability:
    seq:
    - id: endorser
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: endorsement
      type: z
  delegation:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
  deposit:
    seq:
    - id: sender
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: destination
      size: 20
    - id: ticket_hash
      size: 32
    - id: amount
      type: amount
  dissection:
    seq:
    - id: dissection_entries
      type: dissection_entries
      repeat: eos
  dissection_0:
    seq:
    - id: len_dissection
      type: u4
      valid:
        max: 1073741823
    - id: dissection
      type: dissection
      size: len_dissection
  dissection_entries:
    seq:
    - id: state_tag
      type: u1
      enum: bool
    - id: state
      size: 32
      if: (state_tag == bool::true)
    - id: tick
      type: n
  double_baking_evidence:
    seq:
    - id: bh1
      type: bh1_0
    - id: bh2
      type: bh2_0
  double_endorsement_evidence:
    seq:
    - id: op1
      type: op1_0
    - id: op2
      type: op2_0
  double_preendorsement_evidence:
    seq:
    - id: op1
      type: op1_2
    - id: op2
      type: op2_2
  drain_delegate:
    seq:
    - id: consensus_key
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: destination
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  endorsement:
    seq:
    - id: slot
      type: u2
    - id: level
      type: s4
    - id: round
      type: s4
    - id: block_payload_hash
      size: 32
  id_015__ptlimapt__block_header__alpha__full_header:
    seq:
    - id: id_015__ptlimapt__block_header__alpha__full_header
      type: block_header__shell
    - id: id_015__ptlimapt__block_header__alpha__signed_contents
      type: id_015__ptlimapt__block_header__alpha__signed_contents
  id_015__ptlimapt__block_header__alpha__signed_contents:
    seq:
    - id: id_015__ptlimapt__block_header__alpha__unsigned_contents
      type: id_015__ptlimapt__block_header__alpha__unsigned_contents
    - id: signature
      size: 64
  id_015__ptlimapt__block_header__alpha__unsigned_contents:
    seq:
    - id: payload_hash
      size: 32
    - id: payload_round
      type: s4
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
    - id: liquidity_baking_toggle_vote
      type: id_015__ptlimapt__liquidity_baking_toggle_vote
  id_015__ptlimapt__contract_id:
    seq:
    - id: id_015__ptlimapt__contract_id_tag
      type: u1
      enum: id_015__ptlimapt__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_015__ptlimapt__contract_id_tag == id_015__ptlimapt__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_015__ptlimapt__contract_id_tag == id_015__ptlimapt__contract_id_tag::originated)
  id_015__ptlimapt__contract_id__originated:
    seq:
    - id: id_015__ptlimapt__contract_id__originated_tag
      type: u1
      enum: id_015__ptlimapt__contract_id__originated_tag
    - id: originated
      type: originated
      if: (id_015__ptlimapt__contract_id__originated_tag == id_015__ptlimapt__contract_id__originated_tag::originated)
  id_015__ptlimapt__entrypoint:
    seq:
    - id: id_015__ptlimapt__entrypoint_tag
      type: u1
      enum: id_015__ptlimapt__entrypoint_tag
    - id: named
      type: named_0
      if: (id_015__ptlimapt__entrypoint_tag == id_015__ptlimapt__entrypoint_tag::named)
  id_015__ptlimapt__inlined__endorsement:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement
      type: operation__shell_header
    - id: operations
      type: id_015__ptlimapt__inlined__endorsement_mempool__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_015__ptlimapt__inlined__endorsement_mempool__contents:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement_mempool__contents_tag
      type: u1
      enum: id_015__ptlimapt__inlined__endorsement_mempool__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_015__ptlimapt__inlined__endorsement_mempool__contents_tag == id_015__ptlimapt__inlined__endorsement_mempool__contents_tag::endorsement)
  id_015__ptlimapt__inlined__preendorsement:
    seq:
    - id: id_015__ptlimapt__inlined__preendorsement
      type: operation__shell_header
    - id: operations
      type: id_015__ptlimapt__inlined__preendorsement__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_015__ptlimapt__inlined__preendorsement__contents:
    seq:
    - id: id_015__ptlimapt__inlined__preendorsement__contents_tag
      type: u1
      enum: id_015__ptlimapt__inlined__preendorsement__contents_tag
    - id: preendorsement
      type: preendorsement
      if: (id_015__ptlimapt__inlined__preendorsement__contents_tag == id_015__ptlimapt__inlined__preendorsement__contents_tag::preendorsement)
  id_015__ptlimapt__liquidity_baking_toggle_vote:
    seq:
    - id: id_015__ptlimapt__liquidity_baking_toggle_vote
      type: s1
  id_015__ptlimapt__michelson__v1__primitives:
    seq:
    - id: id_015__ptlimapt__michelson__v1__primitives
      type: u1
      enum: id_015__ptlimapt__michelson__v1__primitives
  id_015__ptlimapt__mutez:
    seq:
    - id: id_015__ptlimapt__mutez
      type: n
  id_015__ptlimapt__operation__alpha__contents:
    seq:
    - id: id_015__ptlimapt__operation__alpha__contents_tag
      type: u1
      enum: id_015__ptlimapt__operation__alpha__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::endorsement)
    - id: preendorsement
      type: preendorsement
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::preendorsement)
    - id: dal_slot_availability
      type: dal_slot_availability
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::dal_slot_availability)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: vdf_revelation
      type: solution
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::vdf_revelation)
    - id: double_endorsement_evidence
      type: double_endorsement_evidence
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::double_endorsement_evidence)
    - id: double_preendorsement_evidence
      type: double_preendorsement_evidence
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::double_preendorsement_evidence)
    - id: double_baking_evidence
      type: double_baking_evidence
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::double_baking_evidence)
    - id: activate_account
      type: activate_account
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::proposals)
    - id: ballot
      type: ballot
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::ballot)
    - id: reveal
      type: reveal
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::reveal)
    - id: transaction
      type: transaction
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::transaction)
    - id: origination
      type: origination
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::origination)
    - id: delegation
      type: delegation
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::delegation)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::set_deposits_limit)
    - id: increase_paid_storage
      type: increase_paid_storage
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::increase_paid_storage)
    - id: update_consensus_key
      type: update_consensus_key
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::update_consensus_key)
    - id: drain_delegate
      type: drain_delegate
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::drain_delegate)
    - id: failing_noop
      type: bytes_dyn_uint30
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::failing_noop)
    - id: register_global_constant
      type: register_global_constant
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::register_global_constant)
    - id: tx_rollup_origination
      type: tx_rollup_origination
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_origination)
    - id: tx_rollup_submit_batch
      type: tx_rollup_submit_batch
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_submit_batch)
    - id: tx_rollup_commit
      type: tx_rollup_commit
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_commit)
    - id: tx_rollup_return_bond
      type: tx_rollup_return_bond
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_return_bond)
    - id: tx_rollup_finalize_commitment
      type: tx_rollup_finalize_commitment
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_finalize_commitment)
    - id: tx_rollup_remove_commitment
      type: tx_rollup_remove_commitment
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_remove_commitment)
    - id: tx_rollup_rejection
      type: tx_rollup_rejection
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_rejection)
    - id: tx_rollup_dispatch_tickets
      type: tx_rollup_dispatch_tickets
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_dispatch_tickets)
    - id: transfer_ticket
      type: transfer_ticket
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::transfer_ticket)
    - id: dal_publish_slot_header
      type: dal_publish_slot_header
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::dal_publish_slot_header)
    - id: sc_rollup_originate
      type: sc_rollup_originate
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_originate)
    - id: sc_rollup_add_messages
      type: sc_rollup_add_messages
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_add_messages)
    - id: sc_rollup_cement
      type: sc_rollup_cement
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_cement)
    - id: sc_rollup_publish
      type: sc_rollup_publish
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_publish)
    - id: sc_rollup_refute
      type: sc_rollup_refute
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_refute)
    - id: sc_rollup_timeout
      type: sc_rollup_timeout
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_timeout)
    - id: sc_rollup_execute_outbox_message
      type: sc_rollup_execute_outbox_message
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_execute_outbox_message)
    - id: sc_rollup_recover_bond
      type: sc_rollup_recover_bond
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_recover_bond)
    - id: sc_rollup_dal_slot_subscribe
      type: sc_rollup_dal_slot_subscribe
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_dal_slot_subscribe)
    - id: zk_rollup_origination
      type: zk_rollup_origination
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::zk_rollup_origination)
    - id: zk_rollup_publish
      type: zk_rollup_publish
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::zk_rollup_publish)
  id_015__ptlimapt__operation__contents_list_entries:
    seq:
    - id: id_015__ptlimapt__operation__alpha__contents
      type: id_015__ptlimapt__operation__alpha__contents
  id_015__ptlimapt__rollup_address:
    seq:
    - id: id_015__ptlimapt__rollup_address
      type: bytes_dyn_uint30
  id_015__ptlimapt__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  id_015__ptlimapt__tx_rollup_id:
    seq:
    - id: rollup_hash
      size: 20
  inbox__proof:
    seq:
    - id: level
      type: s4
    - id: message_counter
      type: n
    - id: serialized_proof
      type: bytes_dyn_uint30
  increase_paid_storage:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: z
    - id: destination
      type: id_015__ptlimapt__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
  init_state:
    seq:
    - id: init_state_entries
      type: init_state_entries
      repeat: eos
  init_state_0:
    seq:
    - id: len_init_state
      type: u4
      valid:
        max: 1073741823
    - id: init_state
      type: init_state
      size: len_init_state
  init_state_entries:
    seq:
    - id: init_state_elt
      size: 32
  inode_tree:
    seq:
    - id: inode_tree_tag
      type: u1
      enum: inode_tree_tag
    - id: case__0
      type: u1
      if: (inode_tree_tag == inode_tree_tag::case__0)
    - id: case__4
      type: case__4
      if: (inode_tree_tag == inode_tree_tag::case__4)
    - id: case__8
      type: case__8
      if: (inode_tree_tag == inode_tree_tag::case__8)
    - id: case__12
      type: case__12
      if: (inode_tree_tag == inode_tree_tag::case__12)
    - id: case__16
      type: case__16
      if: (inode_tree_tag == inode_tree_tag::case__16)
    - id: case__20
      type: case__20
      if: (inode_tree_tag == inode_tree_tag::case__20)
    - id: case__24
      type: case__24
      if: (inode_tree_tag == inode_tree_tag::case__24)
    - id: case__28
      type: case__28
      if: (inode_tree_tag == inode_tree_tag::case__28)
    - id: case__32
      type: case__32
      if: (inode_tree_tag == inode_tree_tag::case__32)
    - id: case__36
      type: case__36
      if: (inode_tree_tag == inode_tree_tag::case__36)
    - id: case__40
      type: case__40
      if: (inode_tree_tag == inode_tree_tag::case__40)
    - id: case__44
      type: case__44
      if: (inode_tree_tag == inode_tree_tag::case__44)
    - id: case__48
      type: case__48
      if: (inode_tree_tag == inode_tree_tag::case__48)
    - id: case__52
      type: case__52
      if: (inode_tree_tag == inode_tree_tag::case__52)
    - id: case__56
      type: case__56
      if: (inode_tree_tag == inode_tree_tag::case__56)
    - id: case__60
      type: case__60
      if: (inode_tree_tag == inode_tree_tag::case__60)
    - id: case__64
      type: case__64
      if: (inode_tree_tag == inode_tree_tag::case__64)
    - id: case__1
      type: u2
      if: (inode_tree_tag == inode_tree_tag::case__1)
    - id: case__5
      type: case__5
      if: (inode_tree_tag == inode_tree_tag::case__5)
    - id: case__9
      type: case__9
      if: (inode_tree_tag == inode_tree_tag::case__9)
    - id: case__13
      type: case__13
      if: (inode_tree_tag == inode_tree_tag::case__13)
    - id: case__17
      type: case__17
      if: (inode_tree_tag == inode_tree_tag::case__17)
    - id: case__21
      type: case__21
      if: (inode_tree_tag == inode_tree_tag::case__21)
    - id: case__25
      type: case__25
      if: (inode_tree_tag == inode_tree_tag::case__25)
    - id: case__29
      type: case__29
      if: (inode_tree_tag == inode_tree_tag::case__29)
    - id: case__33
      type: case__33
      if: (inode_tree_tag == inode_tree_tag::case__33)
    - id: case__37
      type: case__37
      if: (inode_tree_tag == inode_tree_tag::case__37)
    - id: case__41
      type: case__41
      if: (inode_tree_tag == inode_tree_tag::case__41)
    - id: case__45
      type: case__45
      if: (inode_tree_tag == inode_tree_tag::case__45)
    - id: case__49
      type: case__49
      if: (inode_tree_tag == inode_tree_tag::case__49)
    - id: case__53
      type: case__53
      if: (inode_tree_tag == inode_tree_tag::case__53)
    - id: case__57
      type: case__57
      if: (inode_tree_tag == inode_tree_tag::case__57)
    - id: case__61
      type: case__61
      if: (inode_tree_tag == inode_tree_tag::case__61)
    - id: case__65
      type: case__65
      if: (inode_tree_tag == inode_tree_tag::case__65)
    - id: case__2
      type: s4
      if: (inode_tree_tag == inode_tree_tag::case__2)
    - id: case__6
      type: case__6
      if: (inode_tree_tag == inode_tree_tag::case__6)
    - id: case__10
      type: case__10
      if: (inode_tree_tag == inode_tree_tag::case__10)
    - id: case__14
      type: case__14
      if: (inode_tree_tag == inode_tree_tag::case__14)
    - id: case__18
      type: case__18
      if: (inode_tree_tag == inode_tree_tag::case__18)
    - id: case__22
      type: case__22
      if: (inode_tree_tag == inode_tree_tag::case__22)
    - id: case__26
      type: case__26
      if: (inode_tree_tag == inode_tree_tag::case__26)
    - id: case__30
      type: case__30
      if: (inode_tree_tag == inode_tree_tag::case__30)
    - id: case__34
      type: case__34
      if: (inode_tree_tag == inode_tree_tag::case__34)
    - id: case__38
      type: case__38
      if: (inode_tree_tag == inode_tree_tag::case__38)
    - id: case__42
      type: case__42
      if: (inode_tree_tag == inode_tree_tag::case__42)
    - id: case__46
      type: case__46
      if: (inode_tree_tag == inode_tree_tag::case__46)
    - id: case__50
      type: case__50
      if: (inode_tree_tag == inode_tree_tag::case__50)
    - id: case__54
      type: case__54
      if: (inode_tree_tag == inode_tree_tag::case__54)
    - id: case__58
      type: case__58
      if: (inode_tree_tag == inode_tree_tag::case__58)
    - id: case__62
      type: case__62
      if: (inode_tree_tag == inode_tree_tag::case__62)
    - id: case__66
      type: case__66
      if: (inode_tree_tag == inode_tree_tag::case__66)
    - id: case__3
      type: s8
      if: (inode_tree_tag == inode_tree_tag::case__3)
    - id: case__7
      type: case__7
      if: (inode_tree_tag == inode_tree_tag::case__7)
    - id: case__11
      type: case__11
      if: (inode_tree_tag == inode_tree_tag::case__11)
    - id: case__15
      type: case__15
      if: (inode_tree_tag == inode_tree_tag::case__15)
    - id: case__19
      type: case__19
      if: (inode_tree_tag == inode_tree_tag::case__19)
    - id: case__23
      type: case__23
      if: (inode_tree_tag == inode_tree_tag::case__23)
    - id: case__27
      type: case__27
      if: (inode_tree_tag == inode_tree_tag::case__27)
    - id: case__31
      type: case__31
      if: (inode_tree_tag == inode_tree_tag::case__31)
    - id: case__35
      type: case__35
      if: (inode_tree_tag == inode_tree_tag::case__35)
    - id: case__39
      type: case__39
      if: (inode_tree_tag == inode_tree_tag::case__39)
    - id: case__43
      type: case__43
      if: (inode_tree_tag == inode_tree_tag::case__43)
    - id: case__47
      type: case__47
      if: (inode_tree_tag == inode_tree_tag::case__47)
    - id: case__51
      type: case__51
      if: (inode_tree_tag == inode_tree_tag::case__51)
    - id: case__55
      type: case__55
      if: (inode_tree_tag == inode_tree_tag::case__55)
    - id: case__59
      type: case__59
      if: (inode_tree_tag == inode_tree_tag::case__59)
    - id: case__63
      type: case__63
      if: (inode_tree_tag == inode_tree_tag::case__63)
    - id: case__67
      type: case__67
      if: (inode_tree_tag == inode_tree_tag::case__67)
    - id: case__129
      type: case__129_entries
      if: (inode_tree_tag == inode_tree_tag::case__129)
    - id: case__130
      type: case__130_entries
      if: (inode_tree_tag == inode_tree_tag::case__130)
    - id: case__131
      type: case__131_entries
      if: (inode_tree_tag == inode_tree_tag::case__131)
    - id: case__132
      type: case__132_entries
      if: (inode_tree_tag == inode_tree_tag::case__132)
    - id: case__133
      type: case__133_entries
      if: (inode_tree_tag == inode_tree_tag::case__133)
    - id: case__134
      type: case__134_entries
      if: (inode_tree_tag == inode_tree_tag::case__134)
    - id: case__135
      type: case__135_entries
      if: (inode_tree_tag == inode_tree_tag::case__135)
    - id: case__136
      type: case__136_entries
      if: (inode_tree_tag == inode_tree_tag::case__136)
    - id: case__137
      type: case__137_entries
      if: (inode_tree_tag == inode_tree_tag::case__137)
    - id: case__138
      type: case__138_entries
      if: (inode_tree_tag == inode_tree_tag::case__138)
    - id: case__139
      type: case__139_entries
      if: (inode_tree_tag == inode_tree_tag::case__139)
    - id: case__140
      type: case__140_entries
      if: (inode_tree_tag == inode_tree_tag::case__140)
    - id: case__141
      type: case__141_entries
      if: (inode_tree_tag == inode_tree_tag::case__141)
    - id: case__142
      type: case__142_entries
      if: (inode_tree_tag == inode_tree_tag::case__142)
    - id: case__143
      type: case__143_entries
      if: (inode_tree_tag == inode_tree_tag::case__143)
    - id: case__144
      type: case__144_entries
      if: (inode_tree_tag == inode_tree_tag::case__144)
    - id: case__145
      type: case__145_entries
      if: (inode_tree_tag == inode_tree_tag::case__145)
    - id: case__146
      type: case__146_entries
      if: (inode_tree_tag == inode_tree_tag::case__146)
    - id: case__147
      type: case__147_entries
      if: (inode_tree_tag == inode_tree_tag::case__147)
    - id: case__148
      type: case__148_entries
      if: (inode_tree_tag == inode_tree_tag::case__148)
    - id: case__149
      type: case__149_entries
      if: (inode_tree_tag == inode_tree_tag::case__149)
    - id: case__150
      type: case__150_entries
      if: (inode_tree_tag == inode_tree_tag::case__150)
    - id: case__151
      type: case__151_entries
      if: (inode_tree_tag == inode_tree_tag::case__151)
    - id: case__152
      type: case__152_entries
      if: (inode_tree_tag == inode_tree_tag::case__152)
    - id: case__153
      type: case__153_entries
      if: (inode_tree_tag == inode_tree_tag::case__153)
    - id: case__154
      type: case__154_entries
      if: (inode_tree_tag == inode_tree_tag::case__154)
    - id: case__155
      type: case__155_entries
      if: (inode_tree_tag == inode_tree_tag::case__155)
    - id: case__156
      type: case__156_entries
      if: (inode_tree_tag == inode_tree_tag::case__156)
    - id: case__157
      type: case__157_entries
      if: (inode_tree_tag == inode_tree_tag::case__157)
    - id: case__158
      type: case__158_entries
      if: (inode_tree_tag == inode_tree_tag::case__158)
    - id: case__159
      type: case__159_entries
      if: (inode_tree_tag == inode_tree_tag::case__159)
    - id: case__160
      type: case__160_entries
      if: (inode_tree_tag == inode_tree_tag::case__160)
    - id: case__161
      type: case__161_entries
      if: (inode_tree_tag == inode_tree_tag::case__161)
    - id: case__162
      type: case__162_entries
      if: (inode_tree_tag == inode_tree_tag::case__162)
    - id: case__163
      type: case__163_entries
      if: (inode_tree_tag == inode_tree_tag::case__163)
    - id: case__164
      type: case__164_entries
      if: (inode_tree_tag == inode_tree_tag::case__164)
    - id: case__165
      type: case__165_entries
      if: (inode_tree_tag == inode_tree_tag::case__165)
    - id: case__166
      type: case__166_entries
      if: (inode_tree_tag == inode_tree_tag::case__166)
    - id: case__167
      type: case__167_entries
      if: (inode_tree_tag == inode_tree_tag::case__167)
    - id: case__168
      type: case__168_entries
      if: (inode_tree_tag == inode_tree_tag::case__168)
    - id: case__169
      type: case__169_entries
      if: (inode_tree_tag == inode_tree_tag::case__169)
    - id: case__170
      type: case__170_entries
      if: (inode_tree_tag == inode_tree_tag::case__170)
    - id: case__171
      type: case__171_entries
      if: (inode_tree_tag == inode_tree_tag::case__171)
    - id: case__172
      type: case__172_entries
      if: (inode_tree_tag == inode_tree_tag::case__172)
    - id: case__173
      type: case__173_entries
      if: (inode_tree_tag == inode_tree_tag::case__173)
    - id: case__174
      type: case__174_entries
      if: (inode_tree_tag == inode_tree_tag::case__174)
    - id: case__175
      type: case__175_entries
      if: (inode_tree_tag == inode_tree_tag::case__175)
    - id: case__176
      type: case__176_entries
      if: (inode_tree_tag == inode_tree_tag::case__176)
    - id: case__177
      type: case__177_entries
      if: (inode_tree_tag == inode_tree_tag::case__177)
    - id: case__178
      type: case__178_entries
      if: (inode_tree_tag == inode_tree_tag::case__178)
    - id: case__179
      type: case__179_entries
      if: (inode_tree_tag == inode_tree_tag::case__179)
    - id: case__180
      type: case__180_entries
      if: (inode_tree_tag == inode_tree_tag::case__180)
    - id: case__181
      type: case__181_entries
      if: (inode_tree_tag == inode_tree_tag::case__181)
    - id: case__182
      type: case__182_entries
      if: (inode_tree_tag == inode_tree_tag::case__182)
    - id: case__183
      type: case__183_entries
      if: (inode_tree_tag == inode_tree_tag::case__183)
    - id: case__184
      type: case__184_entries
      if: (inode_tree_tag == inode_tree_tag::case__184)
    - id: case__185
      type: case__185_entries
      if: (inode_tree_tag == inode_tree_tag::case__185)
    - id: case__186
      type: case__186_entries
      if: (inode_tree_tag == inode_tree_tag::case__186)
    - id: case__187
      type: case__187_entries
      if: (inode_tree_tag == inode_tree_tag::case__187)
    - id: case__188
      type: case__188_entries
      if: (inode_tree_tag == inode_tree_tag::case__188)
    - id: case__189
      type: case__189_entries
      if: (inode_tree_tag == inode_tree_tag::case__189)
    - id: case__190
      type: case__190_entries
      if: (inode_tree_tag == inode_tree_tag::case__190)
    - id: case__191
      type: case__191_0
      if: (inode_tree_tag == inode_tree_tag::case__191)
    - id: case__192
      size: 32
      if: (inode_tree_tag == inode_tree_tag::case__192)
    - id: case__208
      type: case__208
      if: (inode_tree_tag == inode_tree_tag::case__208)
    - id: case__209
      type: case__209
      if: (inode_tree_tag == inode_tree_tag::case__209)
    - id: case__210
      type: case__210
      if: (inode_tree_tag == inode_tree_tag::case__210)
    - id: case__211
      type: case__211
      if: (inode_tree_tag == inode_tree_tag::case__211)
  input_proof:
    seq:
    - id: input_proof_tag
      type: u1
      enum: input_proof_tag
    - id: inbox__proof
      type: inbox__proof
      if: (input_proof_tag == input_proof_tag::inbox__proof)
    - id: reveal__proof
      type: reveal_proof
      if: (input_proof_tag == input_proof_tag::reveal__proof)
  int31:
    seq:
    - id: int31
      type: s4
      valid:
        min: -1073741824
        max: 1073741823
  message:
    seq:
    - id: message_tag
      type: u1
      enum: message_tag
    - id: batch
      type: bytes_dyn_uint30
      if: (message_tag == message_tag::batch)
    - id: deposit
      type: deposit
      if: (message_tag == message_tag::deposit)
  message_0:
    seq:
    - id: message_entries
      type: message_entries
      repeat: eos
  message_1:
    seq:
    - id: len_message
      type: u4
      valid:
        max: 1073741823
    - id: message
      type: message_0
      size: len_message
  message_entries:
    seq:
    - id: message_elt
      type: bytes_dyn_uint30
  message_path:
    seq:
    - id: message_path_entries
      type: message_path_entries
      repeat: eos
  message_path_0:
    seq:
    - id: len_message_path
      type: u4
      valid:
        max: 1073741823
    - id: message_path
      type: message_path
      size: len_message_path
  message_path_entries:
    seq:
    - id: inbox_list_hash
      size: 32
  message_result_path:
    seq:
    - id: message_result_path_entries
      type: message_result_path_entries
      repeat: eos
  message_result_path_0:
    seq:
    - id: len_message_result_path
      type: u4
      valid:
        max: 1073741823
    - id: message_result_path
      type: message_result_path
      size: len_message_result_path
  message_result_path_entries:
    seq:
    - id: message_result_list_hash
      size: 32
  messages:
    seq:
    - id: messages_entries
      type: messages_entries
      repeat: eos
  messages_0:
    seq:
    - id: len_messages
      type: u4
      valid:
        max: 1073741823
    - id: messages
      type: messages
      size: len_messages
  messages_entries:
    seq:
    - id: message_result_hash
      size: 32
  micheline__015__ptlimapt__michelson_v1__expression:
    seq:
    - id: micheline__015__ptlimapt__michelson_v1__expression_tag
      type: u1
      enum: micheline__015__ptlimapt__michelson_v1__expression_tag
    - id: int
      type: z
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::int)
    - id: string
      type: bytes_dyn_uint30
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::string)
    - id: sequence
      type: sequence_0
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::sequence)
    - id: prim__no_args__no_annots
      type: id_015__ptlimapt__michelson__v1__primitives
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__no_args__no_annots)
    - id: prim__no_args__some_annots
      type: prim__no_args__some_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: prim__1_arg__no_annots
      type: prim__1_arg__no_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: prim__1_arg__some_annots
      type: prim__1_arg__some_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: prim__2_args__no_annots
      type: prim__2_args__no_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: prim__2_args__some_annots
      type: prim__2_args__some_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: prim__generic
      type: prim__generic
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__generic)
    - id: bytes
      type: bytes_dyn_uint30
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::bytes)
  n:
    seq:
    - id: n
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
  named:
    seq:
    - id: named
      size-eos: true
  named_0:
    seq:
    - id: len_named
      type: u1
      valid:
        max: 31
    - id: named
      type: named
      size: len_named
  op:
    seq:
    - id: op_entries
      type: op_entries
      repeat: eos
  op1:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement
      type: id_015__ptlimapt__inlined__endorsement
  op1_0:
    seq:
    - id: len_op1
      type: u4
      valid:
        max: 1073741823
    - id: op1
      type: op1
      size: len_op1
  op1_1:
    seq:
    - id: id_015__ptlimapt__inlined__preendorsement
      type: id_015__ptlimapt__inlined__preendorsement
  op1_2:
    seq:
    - id: len_op1
      type: u4
      valid:
        max: 1073741823
    - id: op1
      type: op1_1
      size: len_op1
  op2:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement
      type: id_015__ptlimapt__inlined__endorsement
  op2_0:
    seq:
    - id: len_op2
      type: u4
      valid:
        max: 1073741823
    - id: op2
      type: op2
      size: len_op2
  op2_1:
    seq:
    - id: id_015__ptlimapt__inlined__preendorsement
      type: id_015__ptlimapt__inlined__preendorsement
  op2_2:
    seq:
    - id: len_op2
      type: u4
      valid:
        max: 1073741823
    - id: op2
      type: op2_1
      size: len_op2
  op_0:
    seq:
    - id: len_op
      type: u4
      valid:
        max: 1073741823
    - id: op
      type: op
      size: len_op
  op_elt_field0:
    seq:
    - id: op_code
      type: int31
    - id: price
      type: price
    - id: l1_dst
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: rollup_id
      size: 20
    - id: payload
      type: payload_0
  op_elt_field1:
    seq:
    - id: op_elt_field1_tag
      type: u1
      enum: op_elt_field1_tag
    - id: some
      type: some
      if: (op_elt_field1_tag == op_elt_field1_tag::some)
  op_entries:
    seq:
    - id: op_elt_field0
      type: op_elt_field0
    - id: op_elt_field1
      type: op_elt_field1
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: id_015__ptlimapt__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: script
      type: id_015__ptlimapt__scripted__contracts
  parameters:
    seq:
    - id: entrypoint
      type: id_015__ptlimapt__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
  payload:
    seq:
    - id: payload_entries
      type: payload_entries
      repeat: eos
  payload_0:
    seq:
    - id: len_payload
      type: u4
      valid:
        max: 1073741823
    - id: payload
      type: payload
      size: len_payload
  payload_entries:
    seq:
    - id: payload_elt
      size: 32
  predecessor:
    seq:
    - id: predecessor_tag
      type: u1
      enum: predecessor_tag
    - id: some
      size: 32
      if: (predecessor_tag == predecessor_tag::some)
  preendorsement:
    seq:
    - id: slot
      type: u2
    - id: level
      type: s4
    - id: round
      type: s4
    - id: block_payload_hash
      size: 32
  previous_message_result:
    seq:
    - id: context_hash
      size: 32
    - id: withdraw_list_hash
      size: 32
  previous_message_result_path:
    seq:
    - id: previous_message_result_path_entries
      type: previous_message_result_path_entries
      repeat: eos
  previous_message_result_path_0:
    seq:
    - id: len_previous_message_result_path
      type: u4
      valid:
        max: 1073741823
    - id: previous_message_result_path
      type: previous_message_result_path
      size: len_previous_message_result_path
  previous_message_result_path_entries:
    seq:
    - id: message_result_list_hash
      size: 32
  price:
    seq:
    - id: id
      size: 32
    - id: amount
      type: z
  prim__1_arg__no_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg
      type: micheline__015__ptlimapt__michelson_v1__expression
  prim__1_arg__some_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__2_args__no_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg1
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: arg2
      type: micheline__015__ptlimapt__michelson_v1__expression
  prim__2_args__some_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg1
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: arg2
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__generic:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: args
      type: args_0
    - id: annots
      type: bytes_dyn_uint30
  prim__no_args__some_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: annots
      type: bytes_dyn_uint30
  proof:
    seq:
    - id: proof_tag
      type: u1
      enum: proof_tag
    - id: case__0
      type: case__0
      if: (proof_tag == proof_tag::case__0)
    - id: case__2
      type: case__2
      if: (proof_tag == proof_tag::case__2)
    - id: case__1
      type: case__1
      if: (proof_tag == proof_tag::case__1)
    - id: case__3
      type: case__3
      if: (proof_tag == proof_tag::case__3)
  proof_0:
    seq:
    - id: proof_tag
      type: u1
      enum: proof_tag
    - id: case__0
      type: case__0_0
      if: (proof_tag == proof_tag::case__0)
    - id: case__2
      type: case__2
      if: (proof_tag == proof_tag::case__2)
    - id: case__1
      type: case__1
      if: (proof_tag == proof_tag::case__1)
    - id: case__3
      type: case__3
      if: (proof_tag == proof_tag::case__3)
  proof_1:
    seq:
    - id: pvm_step
      type: pvm_step
    - id: input_proof_tag
      type: u1
      enum: bool
    - id: input_proof
      type: input_proof
      if: (input_proof_tag == bool::true)
  proposals:
    seq:
    - id: proposals_entries
      type: proposals_entries
      repeat: eos
  proposals_0:
    seq:
    - id: len_proposals
      type: u4
      valid:
        max: 640
    - id: proposals
      type: proposals
      size: len_proposals
  proposals_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: period
      type: s4
    - id: proposals
      type: proposals_0
  proposals_entries:
    seq:
    - id: protocol_hash
      size: 32
  public_key:
    seq:
    - id: public_key_tag
      type: u1
      enum: public_key_tag
    - id: ed25519
      size: 32
      if: (public_key_tag == public_key_tag::ed25519)
    - id: secp256k1
      size: 33
      if: (public_key_tag == public_key_tag::secp256k1)
    - id: p256
      size: 33
      if: (public_key_tag == public_key_tag::p256)
  public_key_hash:
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
  public_parameters:
    seq:
    - id: public_parameters_field0
      type: bytes_dyn_uint30
    - id: public_parameters_field1
      type: bytes_dyn_uint30
  pvm_step:
    seq:
    - id: pvm_step_tag
      type: u1
      enum: pvm_step_tag
    - id: arithmetic__pvm__with__proof
      type: proof
      if: (pvm_step_tag == pvm_step_tag::arithmetic__pvm__with__proof)
    - id: wasm__2__0__0__pvm__with__proof
      type: proof_0
      if: (pvm_step_tag == pvm_step_tag::wasm__2__0__0__pvm__with__proof)
  refutation:
    seq:
    - id: choice
      type: n
    - id: step
      type: step
  register_global_constant:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: value
      type: bytes_dyn_uint30
  reveal:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  reveal_proof:
    seq:
    - id: reveal_proof_tag
      type: u1
      enum: reveal_proof_tag
    - id: raw__data__proof
      type: bytes_dyn_uint30
      if: (reveal_proof_tag == reveal_proof_tag::raw__data__proof)
  sc_rollup_add_messages:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: message
      type: message_1
  sc_rollup_cement:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      size: 32
  sc_rollup_dal_slot_subscribe:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: slot_index
      type: u1
  sc_rollup_execute_outbox_message:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: cemented_commitment
      size: 32
    - id: output_proof
      type: bytes_dyn_uint30
  sc_rollup_originate:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pvm_kind
      type: u1
      enum: pvm_kind
    - id: boot_sector
      type: bytes_dyn_uint30
    - id: origination_proof
      type: bytes_dyn_uint30
    - id: parameters_ty
      type: bytes_dyn_uint30
  sc_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      type: commitment_0
  sc_rollup_recover_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
  sc_rollup_refute:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: opponent
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: refutation_tag
      type: u1
      enum: bool
    - id: refutation
      type: refutation
      if: (refutation_tag == bool::true)
  sc_rollup_timeout:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: stakers
      type: stakers
  seed_nonce_revelation:
    seq:
    - id: level
      type: s4
    - id: nonce
      size: 32
  sequence:
    seq:
    - id: sequence_entries
      type: sequence_entries
      repeat: eos
  sequence_0:
    seq:
    - id: len_sequence
      type: u4
      valid:
        max: 1073741823
    - id: sequence
      type: sequence
      size: len_sequence
  sequence_entries:
    seq:
    - id: sequence_elt
      type: micheline__015__ptlimapt__michelson_v1__expression
  set_deposits_limit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: limit_tag
      type: u1
      enum: bool
    - id: limit
      type: id_015__ptlimapt__mutez
      if: (limit_tag == bool::true)
  slot:
    seq:
    - id: level
      type: s4
    - id: index
      type: u1
    - id: header
      size: 48
  solution:
    seq:
    - id: solution_field0
      size: 100
    - id: solution_field1
      size: 100
  some:
    seq:
    - id: contents
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: ty
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: ticketer
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
  stakers:
    seq:
    - id: alice
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: bob
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  step:
    seq:
    - id: step_tag
      type: u1
      enum: step_tag
    - id: dissection
      type: dissection_0
      if: (step_tag == step_tag::dissection)
    - id: proof
      type: proof_1
      if: (step_tag == step_tag::proof)
  tickets_info:
    seq:
    - id: tickets_info_entries
      type: tickets_info_entries
      repeat: eos
  tickets_info_0:
    seq:
    - id: len_tickets_info
      type: u4
      valid:
        max: 1073741823
    - id: tickets_info
      type: tickets_info
      size: len_tickets_info
  tickets_info_entries:
    seq:
    - id: contents
      type: bytes_dyn_uint30
    - id: ty
      type: bytes_dyn_uint30
    - id: ticketer
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: amount
      type: amount
    - id: claimer
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  transaction:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: id_015__ptlimapt__mutez
    - id: destination
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  transfer_ticket:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: ticket_contents
      type: bytes_dyn_uint30
    - id: ticket_ty
      type: bytes_dyn_uint30
    - id: ticket_ticketer
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: bytes_dyn_uint30
  tree_encoding:
    seq:
    - id: tree_encoding_tag
      type: u1
      enum: tree_encoding_tag
    - id: case__0
      type: u1
      if: (tree_encoding_tag == tree_encoding_tag::case__0)
    - id: case__4
      type: case__4_0
      if: (tree_encoding_tag == tree_encoding_tag::case__4)
    - id: case__8
      type: case__8
      if: (tree_encoding_tag == tree_encoding_tag::case__8)
    - id: case__12
      type: case__12
      if: (tree_encoding_tag == tree_encoding_tag::case__12)
    - id: case__16
      type: case__16
      if: (tree_encoding_tag == tree_encoding_tag::case__16)
    - id: case__20
      type: case__20
      if: (tree_encoding_tag == tree_encoding_tag::case__20)
    - id: case__24
      type: case__24
      if: (tree_encoding_tag == tree_encoding_tag::case__24)
    - id: case__28
      type: case__28
      if: (tree_encoding_tag == tree_encoding_tag::case__28)
    - id: case__32
      type: case__32
      if: (tree_encoding_tag == tree_encoding_tag::case__32)
    - id: case__36
      type: case__36
      if: (tree_encoding_tag == tree_encoding_tag::case__36)
    - id: case__40
      type: case__40
      if: (tree_encoding_tag == tree_encoding_tag::case__40)
    - id: case__44
      type: case__44
      if: (tree_encoding_tag == tree_encoding_tag::case__44)
    - id: case__48
      type: case__48
      if: (tree_encoding_tag == tree_encoding_tag::case__48)
    - id: case__52
      type: case__52
      if: (tree_encoding_tag == tree_encoding_tag::case__52)
    - id: case__56
      type: case__56
      if: (tree_encoding_tag == tree_encoding_tag::case__56)
    - id: case__60
      type: case__60
      if: (tree_encoding_tag == tree_encoding_tag::case__60)
    - id: case__64
      type: case__64
      if: (tree_encoding_tag == tree_encoding_tag::case__64)
    - id: case__1
      type: u2
      if: (tree_encoding_tag == tree_encoding_tag::case__1)
    - id: case__5
      type: case__5
      if: (tree_encoding_tag == tree_encoding_tag::case__5)
    - id: case__9
      type: case__9
      if: (tree_encoding_tag == tree_encoding_tag::case__9)
    - id: case__13
      type: case__13
      if: (tree_encoding_tag == tree_encoding_tag::case__13)
    - id: case__17
      type: case__17
      if: (tree_encoding_tag == tree_encoding_tag::case__17)
    - id: case__21
      type: case__21
      if: (tree_encoding_tag == tree_encoding_tag::case__21)
    - id: case__25
      type: case__25
      if: (tree_encoding_tag == tree_encoding_tag::case__25)
    - id: case__29
      type: case__29
      if: (tree_encoding_tag == tree_encoding_tag::case__29)
    - id: case__33
      type: case__33
      if: (tree_encoding_tag == tree_encoding_tag::case__33)
    - id: case__37
      type: case__37
      if: (tree_encoding_tag == tree_encoding_tag::case__37)
    - id: case__41
      type: case__41
      if: (tree_encoding_tag == tree_encoding_tag::case__41)
    - id: case__45
      type: case__45
      if: (tree_encoding_tag == tree_encoding_tag::case__45)
    - id: case__49
      type: case__49
      if: (tree_encoding_tag == tree_encoding_tag::case__49)
    - id: case__53
      type: case__53
      if: (tree_encoding_tag == tree_encoding_tag::case__53)
    - id: case__57
      type: case__57
      if: (tree_encoding_tag == tree_encoding_tag::case__57)
    - id: case__61
      type: case__61
      if: (tree_encoding_tag == tree_encoding_tag::case__61)
    - id: case__65
      type: case__65
      if: (tree_encoding_tag == tree_encoding_tag::case__65)
    - id: case__2
      type: s4
      if: (tree_encoding_tag == tree_encoding_tag::case__2)
    - id: case__6
      type: case__6
      if: (tree_encoding_tag == tree_encoding_tag::case__6)
    - id: case__10
      type: case__10
      if: (tree_encoding_tag == tree_encoding_tag::case__10)
    - id: case__14
      type: case__14
      if: (tree_encoding_tag == tree_encoding_tag::case__14)
    - id: case__18
      type: case__18
      if: (tree_encoding_tag == tree_encoding_tag::case__18)
    - id: case__22
      type: case__22
      if: (tree_encoding_tag == tree_encoding_tag::case__22)
    - id: case__26
      type: case__26
      if: (tree_encoding_tag == tree_encoding_tag::case__26)
    - id: case__30
      type: case__30
      if: (tree_encoding_tag == tree_encoding_tag::case__30)
    - id: case__34
      type: case__34
      if: (tree_encoding_tag == tree_encoding_tag::case__34)
    - id: case__38
      type: case__38
      if: (tree_encoding_tag == tree_encoding_tag::case__38)
    - id: case__42
      type: case__42
      if: (tree_encoding_tag == tree_encoding_tag::case__42)
    - id: case__46
      type: case__46
      if: (tree_encoding_tag == tree_encoding_tag::case__46)
    - id: case__50
      type: case__50
      if: (tree_encoding_tag == tree_encoding_tag::case__50)
    - id: case__54
      type: case__54
      if: (tree_encoding_tag == tree_encoding_tag::case__54)
    - id: case__58
      type: case__58
      if: (tree_encoding_tag == tree_encoding_tag::case__58)
    - id: case__62
      type: case__62
      if: (tree_encoding_tag == tree_encoding_tag::case__62)
    - id: case__66
      type: case__66
      if: (tree_encoding_tag == tree_encoding_tag::case__66)
    - id: case__3
      type: s8
      if: (tree_encoding_tag == tree_encoding_tag::case__3)
    - id: case__7
      type: case__7
      if: (tree_encoding_tag == tree_encoding_tag::case__7)
    - id: case__11
      type: case__11
      if: (tree_encoding_tag == tree_encoding_tag::case__11)
    - id: case__15
      type: case__15
      if: (tree_encoding_tag == tree_encoding_tag::case__15)
    - id: case__19
      type: case__19
      if: (tree_encoding_tag == tree_encoding_tag::case__19)
    - id: case__23
      type: case__23
      if: (tree_encoding_tag == tree_encoding_tag::case__23)
    - id: case__27
      type: case__27
      if: (tree_encoding_tag == tree_encoding_tag::case__27)
    - id: case__31
      type: case__31
      if: (tree_encoding_tag == tree_encoding_tag::case__31)
    - id: case__35
      type: case__35
      if: (tree_encoding_tag == tree_encoding_tag::case__35)
    - id: case__39
      type: case__39
      if: (tree_encoding_tag == tree_encoding_tag::case__39)
    - id: case__43
      type: case__43
      if: (tree_encoding_tag == tree_encoding_tag::case__43)
    - id: case__47
      type: case__47
      if: (tree_encoding_tag == tree_encoding_tag::case__47)
    - id: case__51
      type: case__51
      if: (tree_encoding_tag == tree_encoding_tag::case__51)
    - id: case__55
      type: case__55
      if: (tree_encoding_tag == tree_encoding_tag::case__55)
    - id: case__59
      type: case__59
      if: (tree_encoding_tag == tree_encoding_tag::case__59)
    - id: case__63
      type: case__63
      if: (tree_encoding_tag == tree_encoding_tag::case__63)
    - id: case__67
      type: case__67
      if: (tree_encoding_tag == tree_encoding_tag::case__67)
    - id: case__129
      type: case__129_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__129)
    - id: case__130
      type: case__130_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__130)
    - id: case__131
      type: case__131_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__131)
    - id: case__132
      type: case__132_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__132)
    - id: case__133
      type: case__133_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__133)
    - id: case__134
      type: case__134_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__134)
    - id: case__135
      type: case__135_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__135)
    - id: case__136
      type: case__136_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__136)
    - id: case__137
      type: case__137_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__137)
    - id: case__138
      type: case__138_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__138)
    - id: case__139
      type: case__139_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__139)
    - id: case__140
      type: case__140_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__140)
    - id: case__141
      type: case__141_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__141)
    - id: case__142
      type: case__142_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__142)
    - id: case__143
      type: case__143_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__143)
    - id: case__144
      type: case__144_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__144)
    - id: case__145
      type: case__145_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__145)
    - id: case__146
      type: case__146_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__146)
    - id: case__147
      type: case__147_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__147)
    - id: case__148
      type: case__148_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__148)
    - id: case__149
      type: case__149_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__149)
    - id: case__150
      type: case__150_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__150)
    - id: case__151
      type: case__151_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__151)
    - id: case__152
      type: case__152_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__152)
    - id: case__153
      type: case__153_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__153)
    - id: case__154
      type: case__154_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__154)
    - id: case__155
      type: case__155_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__155)
    - id: case__156
      type: case__156_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__156)
    - id: case__157
      type: case__157_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__157)
    - id: case__158
      type: case__158_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__158)
    - id: case__159
      type: case__159_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__159)
    - id: case__160
      type: case__160_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__160)
    - id: case__161
      type: case__161_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__161)
    - id: case__162
      type: case__162_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__162)
    - id: case__163
      type: case__163_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__163)
    - id: case__164
      type: case__164_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__164)
    - id: case__165
      type: case__165_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__165)
    - id: case__166
      type: case__166_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__166)
    - id: case__167
      type: case__167_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__167)
    - id: case__168
      type: case__168_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__168)
    - id: case__169
      type: case__169_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__169)
    - id: case__170
      type: case__170_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__170)
    - id: case__171
      type: case__171_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__171)
    - id: case__172
      type: case__172_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__172)
    - id: case__173
      type: case__173_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__173)
    - id: case__174
      type: case__174_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__174)
    - id: case__175
      type: case__175_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__175)
    - id: case__176
      type: case__176_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__176)
    - id: case__177
      type: case__177_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__177)
    - id: case__178
      type: case__178_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__178)
    - id: case__179
      type: case__179_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__179)
    - id: case__180
      type: case__180_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__180)
    - id: case__181
      type: case__181_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__181)
    - id: case__182
      type: case__182_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__182)
    - id: case__183
      type: case__183_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__183)
    - id: case__184
      type: case__184_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__184)
    - id: case__185
      type: case__185_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__185)
    - id: case__186
      type: case__186_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__186)
    - id: case__187
      type: case__187_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__187)
    - id: case__188
      type: case__188_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__188)
    - id: case__189
      type: case__189_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__189)
    - id: case__190
      type: case__190_entries
      if: (tree_encoding_tag == tree_encoding_tag::case__190)
    - id: case__191
      type: case__191_1
      if: (tree_encoding_tag == tree_encoding_tag::case__191)
    - id: case__192
      type: case__192_0
      if: (tree_encoding_tag == tree_encoding_tag::case__192)
    - id: case__193
      type: case__193_0
      if: (tree_encoding_tag == tree_encoding_tag::case__193)
    - id: case__195
      type: bytes_dyn_uint30
      if: (tree_encoding_tag == tree_encoding_tag::case__195)
    - id: case__200
      size: 32
      if: (tree_encoding_tag == tree_encoding_tag::case__200)
    - id: case__208
      size: 32
      if: (tree_encoding_tag == tree_encoding_tag::case__208)
    - id: case__216
      type: case__216
      if: (tree_encoding_tag == tree_encoding_tag::case__216)
    - id: case__217
      type: case__217
      if: (tree_encoding_tag == tree_encoding_tag::case__217)
    - id: case__218
      type: case__218
      if: (tree_encoding_tag == tree_encoding_tag::case__218)
    - id: case__219
      type: case__219
      if: (tree_encoding_tag == tree_encoding_tag::case__219)
  tx_rollup_commit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: commitment
      type: commitment
  tx_rollup_dispatch_tickets:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: tx_rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4
    - id: context_hash
      size: 32
    - id: message_index
      type: int31
    - id: message_result_path
      type: message_result_path_0
    - id: tickets_info
      type: tickets_info_0
  tx_rollup_finalize_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
  tx_rollup_rejection:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4
    - id: message
      type: message
    - id: message_position
      type: n
    - id: message_path
      type: message_path_0
    - id: message_result_hash
      size: 32
    - id: message_result_path
      type: message_result_path_0
    - id: previous_message_result
      type: previous_message_result
    - id: previous_message_result_path
      type: previous_message_result_path_0
    - id: proof
      type: bytes_dyn_uint30
  tx_rollup_remove_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_return_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_submit_batch:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: content
      type: bytes_dyn_uint30
    - id: burn_limit_tag
      type: u1
      enum: bool
    - id: burn_limit
      type: id_015__ptlimapt__mutez
      if: (burn_limit_tag == bool::true)
  update_consensus_key:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  z:
    seq:
    - id: has_tail
      type: b1be
    - id: sign
      type: b1be
    - id: payload
      type: b6be
    - id: tail
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
      if: has_tail.as<bool>
  zk_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_parameters
      type: public_parameters
    - id: circuits_info
      type: circuits_info_0
    - id: init_state
      type: init_state_0
    - id: nb_ops
      type: int31
  zk_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: op
      type: op_0
enums:
  amount_tag:
    0: case__0
    1: case__1
    2: case__2
    3: case__3
  bool:
    0: false
    255: true
  id_015__ptlimapt__contract_id__originated_tag:
    1: originated
  id_015__ptlimapt__contract_id_tag:
    0: implicit
    1: originated
  id_015__ptlimapt__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    5: deposit
    255: named
  id_015__ptlimapt__inlined__endorsement_mempool__contents_tag:
    21: endorsement
  id_015__ptlimapt__inlined__preendorsement__contents_tag:
    20: preendorsement
  id_015__ptlimapt__michelson__v1__primitives:
    0: parameter
    1: storage
    2: code
    3:
      id: false
      doc: False
    4:
      id: elt
      doc: Elt
    5:
      id: left
      doc: Left
    6:
      id: none_0
      doc: None
    7:
      id: pair_1
      doc: Pair
    8:
      id: right
      doc: Right
    9:
      id: some_0
      doc: Some
    10:
      id: true
      doc: True
    11:
      id: unit_0
      doc: Unit
    12:
      id: pack
      doc: PACK
    13:
      id: unpack
      doc: UNPACK
    14:
      id: blake2b
      doc: BLAKE2B
    15:
      id: sha256
      doc: SHA256
    16:
      id: sha512
      doc: SHA512
    17:
      id: abs
      doc: ABS
    18:
      id: add
      doc: ADD
    19:
      id: amount
      doc: AMOUNT
    20:
      id: and
      doc: AND
    21:
      id: balance
      doc: BALANCE
    22:
      id: car
      doc: CAR
    23:
      id: cdr
      doc: CDR
    24:
      id: check_signature
      doc: CHECK_SIGNATURE
    25:
      id: compare
      doc: COMPARE
    26:
      id: concat
      doc: CONCAT
    27:
      id: cons
      doc: CONS
    28:
      id: create_account
      doc: CREATE_ACCOUNT
    29:
      id: create_contract
      doc: CREATE_CONTRACT
    30:
      id: implicit_account
      doc: IMPLICIT_ACCOUNT
    31:
      id: dip
      doc: DIP
    32:
      id: drop
      doc: DROP
    33:
      id: dup
      doc: DUP
    34:
      id: ediv
      doc: EDIV
    35:
      id: empty_map
      doc: EMPTY_MAP
    36:
      id: empty_set
      doc: EMPTY_SET
    37:
      id: eq
      doc: EQ
    38:
      id: exec
      doc: EXEC
    39:
      id: failwith
      doc: FAILWITH
    40:
      id: ge
      doc: GE
    41:
      id: get
      doc: GET
    42:
      id: gt
      doc: GT
    43:
      id: hash_key
      doc: HASH_KEY
    44:
      id: if
      doc: IF
    45:
      id: if_cons
      doc: IF_CONS
    46:
      id: if_left
      doc: IF_LEFT
    47:
      id: if_none
      doc: IF_NONE
    48:
      id: int_0
      doc: INT
    49:
      id: lambda_0
      doc: LAMBDA
    50:
      id: le
      doc: LE
    51:
      id: left_0
      doc: LEFT
    52:
      id: loop
      doc: LOOP
    53:
      id: lsl
      doc: LSL
    54:
      id: lsr
      doc: LSR
    55:
      id: lt
      doc: LT
    56:
      id: map_0
      doc: MAP
    57:
      id: mem
      doc: MEM
    58:
      id: mul
      doc: MUL
    59:
      id: neg
      doc: NEG
    60:
      id: neq
      doc: NEQ
    61:
      id: nil
      doc: NIL
    62:
      id: none
      doc: NONE
    63:
      id: not
      doc: NOT
    64:
      id: now
      doc: NOW
    65:
      id: or_0
      doc: OR
    66:
      id: pair_0
      doc: PAIR
    67:
      id: push
      doc: PUSH
    68:
      id: right_0
      doc: RIGHT
    69:
      id: size
      doc: SIZE
    70:
      id: some
      doc: SOME
    71:
      id: source
      doc: SOURCE
    72:
      id: sender
      doc: SENDER
    73:
      id: self
      doc: SELF
    74:
      id: steps_to_quota
      doc: STEPS_TO_QUOTA
    75:
      id: sub
      doc: SUB
    76:
      id: swap
      doc: SWAP
    77:
      id: transfer_tokens
      doc: TRANSFER_TOKENS
    78:
      id: set_delegate
      doc: SET_DELEGATE
    79:
      id: unit_1
      doc: UNIT
    80:
      id: update
      doc: UPDATE
    81:
      id: xor
      doc: XOR
    82:
      id: iter
      doc: ITER
    83:
      id: loop_left
      doc: LOOP_LEFT
    84:
      id: address_0
      doc: ADDRESS
    85:
      id: contract_0
      doc: CONTRACT
    86:
      id: isnat
      doc: ISNAT
    87:
      id: cast
      doc: CAST
    88:
      id: rename
      doc: RENAME
    89: bool
    90: contract
    91: int
    92: key
    93: key_hash
    94: lambda
    95: list
    96: map
    97: big_map
    98: nat
    99: option
    100: or
    101: pair
    102: set
    103: signature
    104: string
    105: bytes
    106: mutez
    107: timestamp
    108: unit
    109: operation
    110: address
    111:
      id: slice
      doc: SLICE
    112:
      id: dig
      doc: DIG
    113:
      id: dug
      doc: DUG
    114:
      id: empty_big_map
      doc: EMPTY_BIG_MAP
    115:
      id: apply
      doc: APPLY
    116: chain_id
    117:
      id: chain_id_0
      doc: CHAIN_ID
    118:
      id: level
      doc: LEVEL
    119:
      id: self_address
      doc: SELF_ADDRESS
    120: never
    121:
      id: never_0
      doc: NEVER
    122:
      id: unpair
      doc: UNPAIR
    123:
      id: voting_power
      doc: VOTING_POWER
    124:
      id: total_voting_power
      doc: TOTAL_VOTING_POWER
    125:
      id: keccak
      doc: KECCAK
    126:
      id: sha3
      doc: SHA3
    127:
      id: pairing_check
      doc: PAIRING_CHECK
    128: bls12_381_g1
    129: bls12_381_g2
    130: bls12_381_fr
    131: sapling_state
    132: sapling_transaction_deprecated
    133:
      id: sapling_empty_state
      doc: SAPLING_EMPTY_STATE
    134:
      id: sapling_verify_update
      doc: SAPLING_VERIFY_UPDATE
    135: ticket
    136:
      id: ticket_deprecated
      doc: TICKET_DEPRECATED
    137:
      id: read_ticket
      doc: READ_TICKET
    138:
      id: split_ticket
      doc: SPLIT_TICKET
    139:
      id: join_tickets
      doc: JOIN_TICKETS
    140:
      id: get_and_update
      doc: GET_AND_UPDATE
    141: chest
    142: chest_key
    143:
      id: open_chest
      doc: OPEN_CHEST
    144:
      id: view_0
      doc: VIEW
    145: view
    146: constant
    147:
      id: sub_mutez
      doc: SUB_MUTEZ
    148: tx_rollup_l2_address
    149:
      id: min_block_time
      doc: MIN_BLOCK_TIME
    150: sapling_transaction
    151:
      id: emit
      doc: EMIT
    152:
      id: lambda_rec
      doc: Lambda_rec
    153:
      id: lambda_rec_0
      doc: LAMBDA_REC
    154:
      id: ticket_0
      doc: TICKET
  id_015__ptlimapt__operation__alpha__contents_tag:
    1: seed_nonce_revelation
    2: double_endorsement_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    7: double_preendorsement_evidence
    8: vdf_revelation
    9: drain_delegate
    17: failing_noop
    20: preendorsement
    21: endorsement
    22: dal_slot_availability
    107: reveal
    108: transaction
    109: origination
    110: delegation
    111: register_global_constant
    112: set_deposits_limit
    113: increase_paid_storage
    114: update_consensus_key
    150: tx_rollup_origination
    151: tx_rollup_submit_batch
    152: tx_rollup_commit
    153: tx_rollup_return_bond
    154: tx_rollup_finalize_commitment
    155: tx_rollup_remove_commitment
    156: tx_rollup_rejection
    157: tx_rollup_dispatch_tickets
    158: transfer_ticket
    200: sc_rollup_originate
    201: sc_rollup_add_messages
    202: sc_rollup_cement
    203: sc_rollup_publish
    204: sc_rollup_refute
    205: sc_rollup_timeout
    206: sc_rollup_execute_outbox_message
    207: sc_rollup_recover_bond
    208: sc_rollup_dal_slot_subscribe
    230: dal_publish_slot_header
    250: zk_rollup_origination
    251: zk_rollup_publish
  inode_tree_tag:
    0: case__0
    1: case__1
    2: case__2
    3: case__3
    4: case__4
    5: case__5
    6: case__6
    7: case__7
    8: case__8
    9: case__9
    10: case__10
    11: case__11
    12: case__12
    13: case__13
    14: case__14
    15: case__15
    16: case__16
    17: case__17
    18: case__18
    19: case__19
    20: case__20
    21: case__21
    22: case__22
    23: case__23
    24: case__24
    25: case__25
    26: case__26
    27: case__27
    28: case__28
    29: case__29
    30: case__30
    31: case__31
    32: case__32
    33: case__33
    34: case__34
    35: case__35
    36: case__36
    37: case__37
    38: case__38
    39: case__39
    40: case__40
    41: case__41
    42: case__42
    43: case__43
    44: case__44
    45: case__45
    46: case__46
    47: case__47
    48: case__48
    49: case__49
    50: case__50
    51: case__51
    52: case__52
    53: case__53
    54: case__54
    55: case__55
    56: case__56
    57: case__57
    58: case__58
    59: case__59
    60: case__60
    61: case__61
    62: case__62
    63: case__63
    64: case__64
    65: case__65
    66: case__66
    67: case__67
    128: case__128
    129: case__129
    130: case__130
    131: case__131
    132: case__132
    133: case__133
    134: case__134
    135: case__135
    136: case__136
    137: case__137
    138: case__138
    139: case__139
    140: case__140
    141: case__141
    142: case__142
    143: case__143
    144: case__144
    145: case__145
    146: case__146
    147: case__147
    148: case__148
    149: case__149
    150: case__150
    151: case__151
    152: case__152
    153: case__153
    154: case__154
    155: case__155
    156: case__156
    157: case__157
    158: case__158
    159: case__159
    160: case__160
    161: case__161
    162: case__162
    163: case__163
    164: case__164
    165: case__165
    166: case__166
    167: case__167
    168: case__168
    169: case__169
    170: case__170
    171: case__171
    172: case__172
    173: case__173
    174: case__174
    175: case__175
    176: case__176
    177: case__177
    178: case__178
    179: case__179
    180: case__180
    181: case__181
    182: case__182
    183: case__183
    184: case__184
    185: case__185
    186: case__186
    187: case__187
    188: case__188
    189: case__189
    190: case__190
    191: case__191
    192: case__192
    208: case__208
    209: case__209
    210: case__210
    211: case__211
    224: case__224
  input_proof_tag:
    0: inbox__proof
    1: reveal__proof
  message_tag:
    0: batch
    1: deposit
  micheline__015__ptlimapt__michelson_v1__expression_tag:
    0: int
    1: string
    2: sequence
    3:
      id: prim__no_args__no_annots
      doc: Primitive with no arguments and no annotations
    4:
      id: prim__no_args__some_annots
      doc: Primitive with no arguments and some annotations
    5:
      id: prim__1_arg__no_annots
      doc: Primitive with one argument and no annotations
    6:
      id: prim__1_arg__some_annots
      doc: Primitive with one argument and some annotations
    7:
      id: prim__2_args__no_annots
      doc: Primitive with two arguments and no annotations
    8:
      id: prim__2_args__some_annots
      doc: Primitive with two arguments and some annotations
    9:
      id: prim__generic
      doc: Generic primitive (any number of args with or without annotations)
    10: bytes
  op_elt_field1_tag:
    0: none
    1: some
  predecessor_tag:
    0: none
    1: some
  proof_tag:
    0: case__0
    1: case__1
    2: case__2
    3: case__3
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
  pvm_kind:
    0: arith_pvm_kind
    1: wasm_2_0_0_pvm_kind
  pvm_step_tag:
    0: arithmetic__pvm__with__proof
    1: wasm__2__0__0__pvm__with__proof
    255: unencodable
  reveal_proof_tag:
    0: raw__data__proof
  step_tag:
    0: dissection
    1: proof
  tree_encoding_tag:
    0: case__0
    1: case__1
    2: case__2
    3: case__3
    4: case__4
    5: case__5
    6: case__6
    7: case__7
    8: case__8
    9: case__9
    10: case__10
    11: case__11
    12: case__12
    13: case__13
    14: case__14
    15: case__15
    16: case__16
    17: case__17
    18: case__18
    19: case__19
    20: case__20
    21: case__21
    22: case__22
    23: case__23
    24: case__24
    25: case__25
    26: case__26
    27: case__27
    28: case__28
    29: case__29
    30: case__30
    31: case__31
    32: case__32
    33: case__33
    34: case__34
    35: case__35
    36: case__36
    37: case__37
    38: case__38
    39: case__39
    40: case__40
    41: case__41
    42: case__42
    43: case__43
    44: case__44
    45: case__45
    46: case__46
    47: case__47
    48: case__48
    49: case__49
    50: case__50
    51: case__51
    52: case__52
    53: case__53
    54: case__54
    55: case__55
    56: case__56
    57: case__57
    58: case__58
    59: case__59
    60: case__60
    61: case__61
    62: case__62
    63: case__63
    64: case__64
    65: case__65
    66: case__66
    67: case__67
    128: case__128
    129: case__129
    130: case__130
    131: case__131
    132: case__132
    133: case__133
    134: case__134
    135: case__135
    136: case__136
    137: case__137
    138: case__138
    139: case__139
    140: case__140
    141: case__141
    142: case__142
    143: case__143
    144: case__144
    145: case__145
    146: case__146
    147: case__147
    148: case__148
    149: case__149
    150: case__150
    151: case__151
    152: case__152
    153: case__153
    154: case__154
    155: case__155
    156: case__156
    157: case__157
    158: case__158
    159: case__159
    160: case__160
    161: case__161
    162: case__162
    163: case__163
    164: case__164
    165: case__165
    166: case__166
    167: case__167
    168: case__168
    169: case__169
    170: case__170
    171: case__171
    172: case__172
    173: case__173
    174: case__174
    175: case__175
    176: case__176
    177: case__177
    178: case__178
    179: case__179
    180: case__180
    181: case__181
    182: case__182
    183: case__183
    184: case__184
    185: case__185
    186: case__186
    187: case__187
    188: case__188
    189: case__189
    190: case__190
    191: case__191
    192: case__192
    193: case__193
    195: case__195
    200: case__200
    208: case__208
    216: case__216
    217: case__217
    218: case__218
    219: case__219
    224: case__224
seq:
- id: id_015__ptlimapt__operation__contents_list_entries
  type: id_015__ptlimapt__operation__contents_list_entries
  repeat: eos
