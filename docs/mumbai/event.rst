Contract events
===============

Contract events provide a way for contracts to deliver event-like information to external applications.
This mechanism allows off-chain applications to react to Tezos contracts execution.


Sending events
--------------
Contract events can be emitted by invoking the Michelson instruction ``EMIT``.
``EMIT %tag ty`` pops an item of type ``ty`` off the stack and pushes an ``operation`` onto the stack.
The type ascription ``ty`` is special since annotations in ``ty`` are preserved and eventually transmitted
in the event so that indexers have a choice to present the event body with appropriate labels.
Both the ``ty`` type ascription and ``%tag`` are optional.
If ``ty`` is absent, this type is inferred from the type of the top element of the stack.
If ``%tag`` is absent, no tag is attached to the event.

To actually send out the events, most importantly, the produced ``operation``\s must be included in the list of
operations, along with ``TRANSFER_TOKEN`` operations for example, that the contract wants to effect.

Event
-----

Each successful contract execution attaches into the transaction receipt a list of contract events
arranged in the order of appearance in the resultant list of operations.
There, the events are made ready for consumption by services observing the chain.

Example
-------
Suppose a contract wants to emit events with the following type:

::

    or (nat %int) (string %str)

Then, this contract may generate an event emission operation with the following instructions.
Note that the ``EMIT`` instruction will emit an event with a tag ``notify_client``.
In addition, it allows indexers to recognise the two variants as ``int`` and ``str`` and,
therefore, be able to present the values with the appropriate variant labels.

::

    PUSH string "right";
    RIGHT nat;
    EMIT %notify_client (or (nat %int) (string %str));


Retrieving events
-----------------
Events successfully emitted can be read off directly from transaction results.
This is typically achieved by making JSON RPCs to the block service.
It will return a list of operations, each including the event entries with the information above.

Here is a sample result from a call, corresponding to the example above.

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
                "kind": "event",
                // ... fields elided for brevity
                "source": "KT1M1ynE3YXkM7qLZoMppq6szMbBvxX9yQVL",
                "nonce": 0,
                "type": {
                   "prim": "or",
                   "args": [
                     {
                       "prim": "nat",
                       "annots": [
                         "%int"
                       ]
                     },
                     {
                       "prim": "string",
                       "annots": [
                         "%str"
                       ]
                     }
                   ]
                },
                "tag": "notify_client",
                "payload": {
                  "prim": "Right",
                  "args": [
                    {
                      "string": "right"
                    }
                  ]
                },
                "result": {
                  "status": "applied",
                  "consumed_milligas": "1000000"
                }
              }
            ]
          }
        }
      ]
    }

Note that the ``operation`` produced by ``EMIT`` does not constitute a call to any other contract.
