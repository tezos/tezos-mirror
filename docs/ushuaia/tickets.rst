Tickets
=======

Tickets are a special class of assets on Tezos that contracts can construct and store, and that can then be transferred between contracts, user accounts, or rollups.
They are useful to model ownership of some digital artefact, existing in a limited number of copies (or instances); this can further serve as a basis for restricting access to the artefact and permissions to use it.
In this document, we will explain the semantics and lifetime of tickets, including creation, transfer, and destruction.


Semantics
---------

A ticket represents a digital artefact existing in a limited number of copies (or instances).
As such, a ticket has the following four attributes:

#. ``ticketer`` is the creator of the ticket, which is always a Tezos smart contract.

#. ``contents`` is the :doc:`Michelson <./michelson>` expression attached to the ticket.

#. ``content_ty`` is the :doc:`Michelson <./michelson>` type of the ``contents`` data. It can be any comparable type.

#. ``amount`` is the amount of the ticket. It is a strictly positive whole number.

Furthermore, a `ticket <https://tezos.gitlab.io/michelson-reference/#type-ticket>`__ cannot be duplicated
with the `DUP <https://tezos.gitlab.io/michelson-reference/#instr-DUP>`__
or the `DUP n <https://tezos.gitlab.io/michelson-reference/#instr-DUPN>`__ instructions.


Operations on tickets
---------------------

Creating
~~~~~~~~

Tickets can be constructed by smart contracts. To construct tickets, smart contracts use the ``TICKET``
`instruction <https://tezos.gitlab.io/michelson-reference/#instr-TICKET>`__ with some ticket amount,
ticket content type, and value as inputs.
Smart contracts then may store them in contract storage or transfer them along to either
other contracts, rollups, or user accounts.

Tickets cannot be constructed with instructions for duplication, such as ``DUP``.
Therefore, the issuance of tickets is completely within the control of the ticketer contract by
invoking ``TICKET`` instructions.

Splitting
~~~~~~~~~

A ticket can be split into two tickets with the same ``contents`` and ``ticketer`` values,
as long as the sum of the ``amount`` of the two tickets is equal to that of the original.
This operation allows tickets to be spent across several transactions
by breaking tickets into smaller tickets.
Smart contracts may split tickets by invoking the `SPLIT_TICKET <https://tezos.gitlab.io/michelson-reference/#instr-SLIT_TICKET>`__ instruction.
The ticket splitting is done automatically when a user account transfers part of a ticket.

Joining
~~~~~~~

Tickets issued by the same ``ticketer`` contract with the same ``contents`` data of the
same ``contents_ty`` type are considered of the same kind.
Therefore, two such tickets can be **joined** into one ticket and the output ``amount``
will be the sum of those of the two input tickets.
Smart contracts can join tickets via the `JOIN_TICKETS <https://tezos.gitlab.io/michelson-reference/#instr-JOIN_TICKETS>`__ instruction.
Tickets of the same kind are automatically joined when they belong to the same user account.

Transferring
~~~~~~~~~~~~

Once a ticket has been constructed by a smart contract, it may be transferred to other contracts as follows:

- *Smart contract to user account*: Smart contracts can transfer a ticket to user accounts
  via `TRANSFER_TOKENS <https://tezos.gitlab.io/michelson-reference/#instr-TRANSFER_TOKENS>`__.
  To do so, the contract needs to cast the address of the target user account to type ``contract (ticket cty)`` where ``cty`` is the type of the content of the ticket to be sent. This can be done using ``CONTRACT (ticket cty)``.
  The rest is the same as making a contract call.
  The following Michelson snippet is an example sending a ticket of amount ``10`` with a ``string`` content
  ``"some ticket"`` to a user account address made available at the top of the stack.

::

    # Stack: address :: S
    CONTRACT (ticket string) ;
    # Stack: option (contract (ticket string)) :: S
    ASSERT_SOME ;
    # Stack: contract (ticket string) :: S
    PUSH mutez 0 ;
    # Stack: mutez :: contract (ticket string) :: S
    PUSH nat 10 ;
    # Stack: nat :: mutez :: contract (ticket string) :: S
    PUSH string "some ticket" ;
    # Stack: string :: nat :: mutez :: contract (ticket string) :: S
    TICKET ;
    # Stack: option (ticket string) :: mutez :: contract (ticket string) :: S
    ASSERT_SOME ;
    # Stack: ticket string :: mutez :: contract (ticket string) :: S
    TRANSFER_TOKENS ;
    # Stack: operation :: S

- *Between smart contracts*: Contracts can send tickets to other contracts via regular contract
  calls using the instruction ``TRANFSER_TOKENS``,
  as long as target contracts accept tickets of matching content type in their ``parameter``\s.

- *Between user accounts*: User accounts can transfer existing tickets they own to other user accounts
  with ``Transfer_ticket`` operation from their wallets.
  For instance, ``octez-client`` can be invoked in the following way by a user account holder ``alice``
  to transfer a ticket of amount ``10``, type ``string``, content ``"some ticket"`` and ticketer ``ticketer``
  to another user account held by ``bob``.

::

    octez-client transfer 10 tickets from alice to bob with entrypoint default and type string and content '"some ticket"' and ticketer 'ticketer'

If the amount of the ticket ``"some ticket"`` owned by ``alice`` was greater than 10,
this transfer would cause an automatic ticket split,
after which ``alice`` would continue to own the remaining amount.

Note that the above command uses the expression "transfer 10 tickets" of the given kind,
instead of "transfer a ticket of amount 10".
Indeed, by virtue of automatic joining of tickets of the same kind belonging to the same user account,
this formulation does not introduce any ambiguity:
the user account cannot hold several distinct tickets of this kind.
Of course, these expressions would not be interchangeable for tickets belonging to a smart contract.

- *User account to smart contract*: Using the same ``Transfer_ticket`` operation, user accounts
  can also send their tickets to smart contracts.
  In this case, the entrypoint, as defined by the specification of the target contract, must accept this kind of tickets.
  Here is an example using ``octez-client`` to transfer a ticket of amount ``10``, type ``string``,
  content ``"some ticket"`` and ticketer ``ticketer`` owned by ``alice`` to a contract ``receiver`` accepting tickets
  at the entrypoint ``save``.

::

    octez-client transfer 10 tickets from alice to receiver with entrypoint save and type string and content '"some ticket"' and ticketer 'ticketer'

In this case, too, automatic ticket splitting may happen, under the same circumstances.

.. note::

   This page does not cover transfers of tickets to/from rollups. For that, refer to the documentation pages of particular rollups (e.g. :doc:`./smart_rollups`).


Destroying
~~~~~~~~~~~

Only smart contracts can destroy tickets, by simply dropping them.
That is, by not storing them anymore
in the contract storage and not sending them to other contracts.
User accounts, on the other hand, cannot destroy any ticket in their possession.

For instance, a user account ``A`` may receive a ``string`` ticket of amount ``2`` with
content ``Lorem ipsum`` created by a smart contract ``B``.
``A`` may send amount ``1`` of it to a smart contract ``C``. This automatically splits the ticket into two tickets of amount ``1``.
Now ``C`` may destroy this ticket by dropping it and ``A`` will still hold a ticket of amount ``1``
with a ``string`` content ``Lorem ipsum`` created by ``B``.
It will remain in the possession of ``A`` until ``A`` sends it to another user account or smart contract.
