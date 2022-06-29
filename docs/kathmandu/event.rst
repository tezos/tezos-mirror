Contract event logging
======================

Contract event logging is a way for contracts to deliver event-like information to external applications.
This mechanism allows off-chain applications to react to Tezos contracts execution.

Event address
-------------
On the Tezos chain, an event is uniquely identified with a 53-byte long base58 address starting with ``ev1``.
This address is computed from an event schema and an event tag (see below for details).
An event schema is a Micheline expression denoting the Michelson type of the event data attachment.
For instance, this schema could be ``or (nat %int) (string %str)`` in Micheline.
An event tag is an identifier represented by a string, such as ``writeOut`` or ``mintToken``.

Computing an event address
--------------------------
An address for a event is computed from a Base58 32-byte hash of the binary serialization of
the original Michelson node, with all annotations, separated by a ``0x05`` byte.

For instance, the following event schema declaration produces a unique address of
``ev12m5E1yW14mc9rsrcdGAWVfDSdmRGuctykrVU55bHZBGv9kmdhW``, which is a Base58-encoded Blake2b hash of
a ``0x05`` byte and then followed by the binary serialization of ``or (nat %int) (string %str)``
with all the annotations ``%int`` and ``%str``.

::
    %tag1 (or (nat %int) (string %str));

Alternatively, there is a RPC exposed by the node for computing this address.
Make a ``POST`` request to ``helpers/scripts/event_address`` with a JSON payload of shape
``{"type": "<Michelson data type>"}``, and it will return a JSON response of shape
``{"address": "ev1..."}``.

Sending events
--------------
Contract events can be emitted by invoking the Michelson instruction ``EMIT``.
``EMIT %tag ty`` pops an item of type ``ty`` off the stack and pushes an ``operation`` onto the stack.

``EMIT`` has the following typing rule.

::
    EMIT %tag ty :: 'ty : 'S -> operation : 'S

To actually send out the events, most importantly, the produced ``operation``\s must be included into the list of
operations that the main contract code returns along with other ``operation``\s that this contract wants to effect.

Event
-----
A contract event in Tezos consists of the following data.

- An event ``data`` of a certain type ``event``.
- An event address associated with a certain event tag and data type.

Each successful contract execution attaches into the transaction receipt a list of contract events
arranged in the order of appearance in the resultant list of operations.
There, the events are made ready for consumption by services observing the chain.

Example
-------
Suppose a contract wants to emit events with the following type for the data attachment.

::
    or (nat %int) (string %str);

Then, this contract may generate an event emission operation with the following instructions.
Note that the ``EMIT`` instruction will emit an event with a tag ``notify_client``.

::
    PUSH string "right";
    RIGHT nat;
    EMIT %notify_client (or (nat %int) (string %str));


Retrieving events
-----------------
Events successfully emitted can be read off directly from transaction results.
This is typically achieved by making JSON RPCs to the block service.
It will return a list of operations, each including the event entries with the information above.

Here is a sample result from a call.

::

    {
      "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
      "hash": "opNX59asPwNZGu2kFiHFVJzn7QwtyaExoLtxdZSm3Q3o4jDdSmo",
      // ... fields elided for brevity
      "contents": [
        {
          "kind": "transaction",
          // ... fields elided for brevity
          "metadata": {
            // ... fields elided for brevity
            "internal_operation_results": [
              {
                "status": "applied",
                // ... fields elided for brevity
                "source": "KT...",
                "destination": "ev1...",                            // <~
                "parameters": {                                     // <~
                  "entrypoint": "event_tag...",                     // <~
                  "value": {                                        // <~
                    "prim": "Right",                                // <~
                    "args": [                                       // <~
                      {                                             // <~
                        "string": "right"                           // <~
                      }                                             // <~
                    ]                                               // <~
                  }                                                 // <~
                }                                                   // <~
              }
            ]
          }
        }
      ]
    }

Note that the ``operation`` produced by ``EMIT`` does not constitute a call to any other contract.
Events emitted with ``EMIT`` is optimized to avoid calls to external contracts to reduce gas usage.
