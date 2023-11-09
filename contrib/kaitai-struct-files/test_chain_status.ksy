meta:
  id: test_chain_status
  endian: be
doc: ! >-
  The status of the test chain: not_running (there is no test chain at the moment),
  forking (the test chain is being setup), running (the test chain is running).
types:
  test_chain_status_forking:
    seq:
    - id: protocol
      size: 32
    - id: expiration
      type: s8
      doc: ! 'A timestamp as seen by the protocol: second-level precision, epoch based.'
  test_chain_status_running:
    seq:
    - id: chain_id
      size: 4
    - id: genesis
      size: 32
    - id: protocol
      size: 32
    - id: expiration
      type: s8
      doc: ! 'A timestamp as seen by the protocol: second-level precision, epoch based.'
enums:
  test_chain_status_tag:
    0: not_running
    1: forking
    2: running
seq:
- id: test_chain_status_tag
  type: u1
  enum: test_chain_status_tag
- id: test_chain_status_forking
  type: test_chain_status_forking
  if: (test_chain_status_tag == test_chain_status_tag::forking)
- id: test_chain_status_running
  type: test_chain_status_running
  if: (test_chain_status_tag == test_chain_status_tag::running)
