meta:
  id: id_024__pttallin__smart_rollup__game
  endian: be
doc: ! 'Encoding id: 024-PtTALLiN.smart_rollup.game'
types:
  agreed_start_chunk:
    seq:
    - id: state_tag
      type: u1
      enum: bool
    - id: state
      size: 32
      if: (state_tag == bool::true)
    - id: tick
      type: n
  back_pointers:
    seq:
    - id: back_pointers_entries
      type: back_pointers_entries
      repeat: eos
  back_pointers_0:
    seq:
    - id: len_back_pointers
      type: u4be
      valid:
        max: 1073741823
    - id: back_pointers
      type: back_pointers
      size: len_back_pointers
  back_pointers_1:
    seq:
    - id: back_pointers_entries
      type: back_pointers_entries_0
      repeat: eos
  back_pointers_2:
    seq:
    - id: len_back_pointers
      type: u4be
      valid:
        max: 1073741823
    - id: back_pointers
      type: back_pointers_1
      size: len_back_pointers
  back_pointers_entries:
    seq:
    - id: smart_rollup_inbox_hash
      size: 32
  back_pointers_entries_0:
    seq:
    - id: dal_skip_list_pointer
      size: 32
  content:
    seq:
    - id: hash
      size: 32
    - id: level
      type: s4be
  content_0:
    seq:
    - id: content_tag
      type: u1
      enum: content_tag
    - id: unpublished
      type: unpublished
      if: (content_tag == content_tag::unpublished)
    - id: published
      type: published
      if: (content_tag == content_tag::published)
  dal_snapshot:
    seq:
    - id: dal_snapshot_tag
      type: u1
      enum: dal_snapshot_tag
    - id: dal_skip_list_legacy
      size: 57
      if: (dal_snapshot_tag == dal_snapshot_tag::dal_skip_list_legacy)
    - id: dal_skip_list
      type: skip_list
      if: (dal_snapshot_tag == dal_snapshot_tag::dal_skip_list)
  dissecting:
    seq:
    - id: dissection
      type: dissection_0
    - id: default_number_of_sections
      type: u1
  dissection:
    seq:
    - id: dissection_entries
      type: dissection_entries
      repeat: eos
  dissection_0:
    seq:
    - id: len_dissection
      type: u4be
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
  final_move:
    seq:
    - id: agreed_start_chunk
      type: agreed_start_chunk
    - id: refuted_stop_chunk
      type: refuted_stop_chunk
  game_state:
    seq:
    - id: game_state_tag
      type: u1
      enum: game_state_tag
    - id: dissecting
      type: dissecting
      if: (game_state_tag == game_state_tag::dissecting)
    - id: final_move
      type: final_move
      if: (game_state_tag == game_state_tag::final_move)
  id_024__pttallin__contract_id:
    seq:
    - id: id_024__pttallin__contract_id_tag
      type: u1
      enum: id_024__pttallin__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_024__pttallin__contract_id_tag == id_024__pttallin__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (id_024__pttallin__contract_id_tag == id_024__pttallin__contract_id_tag::originated)
  inbox_snapshot:
    seq:
    - id: index
      type: n
    - id: content
      type: content
    - id: back_pointers
      type: back_pointers_0
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
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
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
    - id: bls
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::bls)
  published:
    seq:
    - id: publisher
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: is_proto_attested
      type: u1
      enum: bool
    - id: attested_shards
      type: u2be
    - id: total_shards
      type: u2be
    - id: published_tag
      type: u1
      enum: published_tag
    - id: v0
      type: v0
      if: (published_tag == published_tag::v0)
  refuted_stop_chunk:
    seq:
    - id: state_tag
      type: u1
      enum: bool
    - id: state
      size: 32
      if: (state_tag == bool::true)
    - id: tick
      type: n
  skip_list:
    seq:
    - id: index
      type: n
    - id: content
      type: content_0
    - id: back_pointers
      type: back_pointers_2
  unpublished:
    seq:
    - id: level
      type: s4be
    - id: index
      type: u1
  v0:
    seq:
    - id: level
      type: s4be
    - id: index
      type: u1
    - id: commitment
      size: 48
enums:
  bool:
    0: false
    255: true
  content_tag:
    2: unpublished
    3: published
  dal_snapshot_tag:
    0: dal_skip_list_legacy
    1: dal_skip_list
  game_state_tag:
    0: dissecting
    1: final_move
  id_024__pttallin__contract_id_tag:
    0: implicit
    1: originated
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  published_tag:
    0: v0
  turn_tag:
    0: alice
    1: bob
seq:
- id: turn
  type: u1
  enum: turn_tag
- id: inbox_snapshot
  type: inbox_snapshot
- id: dal_snapshot
  type: dal_snapshot
- id: start_level
  type: s4be
- id: inbox_level
  type: s4be
- id: game_state
  type: game_state
