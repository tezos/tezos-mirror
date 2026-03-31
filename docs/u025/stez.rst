.. note::

   Protocol Proposal U introduces sTEZ under a feature flag disabled
   by default on Mainnet. Parameters are under discussion and will be
   adjusted based on community feedback and testnet results.

===============================
Enshrined Liquid Staking (sTEZ)
===============================

sTEZ is a protocol-native liquid staking mechanism that complements
the existing :doc:`staking mechanism <staking>`. With staking, users
freeze tez towards a :ref:`delegate's <def_delegate_u025>` security
deposit to earn :ref:`participation rewards <adaptive_rewards_u025>`,
with the frozen funds subject to :doc:`slashing
<adaptive_slashing>`. sTEZ offers a liquid alternative: users deposit
tez into a protocol-managed contract and receive transferable `FA2.1
<https://tzip.tezosagora.org/proposal/tzip-26/>`__ tokens in return.
The token quantity stays constant in the account's balance, and its
value accrues through an :ref:`exchange rate <exchange_rate_u025>`,
simplifying integration with DeFi protocols and accounting tools. sTEZ
tokens can also be bridged to :doc:`smart rollups
<../shell/smart_rollup_node>` via protocol-native tickets (see
:ref:`FA2.1 <stez_fa21_u025>`).

The mechanism is enshrined in the protocol, i.e., its parameters can
only be modified through :doc:`on-chain governance <voting>`, and the
system requires no multisig wallet or admin key. The protocol
automatically :ref:`distributes deposited staking power
<stez_staking_power_u025>` across registered delegates each cycle.
Direct stakers retain priority over liquid stakers, so sTEZ is
designed to coexist with direct staking, not to replace it. Unlike
staked or delegated funds, sTEZ stake holdings carry no :doc:`voting
<voting>` power.

How It Works
------------

This section describes the core mechanisms behind sTEZ: the
:ref:`internal ledgers <stez_staking_ledger_u025>` that track funds,
the :ref:`exchange rate <exchange_rate_u025>` that determines token
value, the :ref:`fee auction <stez_global_fee_u025>` that determines
delegate fees, the :ref:`allocation algorithm
<stez_staking_power_u025>` that distributes staking power, and the
:ref:`rules and constants <stez_safeguards_u025>` that govern the
system.

.. _stez_staking_ledger_u025:

sTEZ Staking and Unstaked Frozen Ledgers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The sTEZ system uses two internal ledgers to track funds at different
stages of their lifecycle. The spendable balance of the sTEZ FA2.1
contract is always zero (except during internal transfers between
ledgers and the account's spendable balance) -- all tez held by the
contract are accounted for in one of these two ledgers.

**sTEZ Staking Ledger**: When a user deposits tez via the sTEZ FA2.1
contract, the funds are moved from the user's spendable balance to the
**sTEZ staking ledger**. This ledger holds the tez backing all sTEZ
tokens in circulation. The protocol handles :ref:`staking rewards
<adaptive_rewards_u025>` and :doc:`slashing <adaptive_slashing>` for
the sTEZ staking ledger automatically -- rewards increase it and
slashing decreases it, which in turn affects the :ref:`exchange rate
<exchange_rate_u025>`. In :ref:`receipts <stez_receipts_u025>`, this
ledger appears as ``stez_deposits``.

**sTEZ Unstaked Frozen Ledger**: When a user redeems their sTEZ
tokens, the corresponding tez are moved from the sTEZ staking ledger
to the **sTEZ unstaked frozen ledger**. This ledger is indexed by
``(redeemer, cycle)`` and tracks redeemed funds that remain **frozen**
and **slashable** for ``UNSTAKE_FINALIZATION_DELAY + 1`` cycles. After
this delay, the funds are no longer frozen nor slashable and can be
transferred back to the redeemer's spendable balance by calling the
``finalize_redeem`` entrypoint of the sTEZ contract (see
:ref:`Finalizing a Redemption <stez_user_operations_u025>`). In
:ref:`receipts <stez_receipts_u025>`, this ledger appears as
``stez_redeemed_deposits``.

.. _exchange_rate_u025:

Exchange Rate
^^^^^^^^^^^^^

The exchange rate determines how many tez each sTEZ token is worth.
It is computed as:

::

   exchange_rate = total_amount_of_tez / total_supply

where ``total_amount_of_tez`` is the total tez held in the :ref:`sTEZ
staking ledger <stez_staking_ledger_u025>` and ``total_supply`` is the total number of sTEZ tokens
in existence. Both values can be queried via the :ref:`sTEZ RPCs
<stez_rpc_u025>`.

If no sTEZ has been issued yet, the exchange rate starts at 1:1 (one
sTEZ equals one tez) and evolves as follows:

- **Staking rewards**: when staking rewards are added to the sTEZ
  staking ledger, ``total_amount_of_tez`` increases while
  ``total_supply`` remains unchanged, causing the exchange rate to
  **increase** -- the tez value of each sTEZ token grows.

- **Slashing events**: when a delegate with an sTEZ stake is slashed,
  ``total_amount_of_tez`` decreases while ``total_supply`` remains
  unchanged, causing the exchange rate to **decrease** -- the tez
  value of each sTEZ token shrinks.

- **Deposits and redemptions**: when users deposit tez or redeem sTEZ
  tokens, both ``total_amount_of_tez`` and ``total_supply`` are
  adjusted proportionally, leaving the exchange rate **unchanged**.

**Example (rewards)**: A user deposits 100 tez on day 1 when the
exchange rate is 1.00, receiving 100 sTEZ. After a year of accruing
staking rewards, the exchange rate rises to approximately 1.05. The
user still holds 100 sTEZ, but they are now worth approximately 105
tez. The user can either redeem their sTEZ to receive ~105 tez, or
sell the sTEZ tokens directly on an exchange.


.. _stez_global_fee_u025:

Global Fee Mechanism
^^^^^^^^^^^^^^^^^^^^

sTEZ uses a **uniform price auction**: delegates declare a fee as a
competitive bid, the protocol selects the lowest-fee delegates
(respecting capacity limits), and all of them receive the **same** fee
rate -- the **global fee**, defined as the highest declared fee among
all selected delegates in a given cycle. The remainder of sTEZ staking
rewards (after the global fee) goes to the :ref:`sTEZ staking ledger
<stez_staking_ledger_u025>`,
increasing the :ref:`exchange rate <exchange_rate_u025>` for all sTEZ
holders.

This auction design creates a self-correcting dynamic: delegates are
incentivized to set lower fees to increase their chances of being
selected, but the effective fee is always determined by the
least competitive selected delegate. If the global fee rises too high,
sTEZ holders receive a smaller share of staking rewards, reducing the
attractiveness of the token and creating redemption pressure, which in
turn pressures delegates to lower their fees.

For details on how :ref:`participation rewards
<adaptive_rewards_u025>` are computed and distributed, see
:doc:`adaptive_issuance`.

.. _stez_staking_power_u025:

Staking Power Distribution Across Delegates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

At the end of each cycle, the protocol allocates sTEZ staking power
across :ref:`registered <stez_delegate_operations_u025>`
delegates. sTEZ stake contributes to a delegate's :doc:`baking power
<baking_power>` but **not** their :doc:`voting <voting>` power.

All delegates must respect a **maximum allowed external stake** of
``own_staked × GLOBAL_LIMIT_OF_STAKING_OVER_BAKING``, but each
delegate can cap how much of this maximum capacity (possibly none) can
be filled by sTEZ. Allocation follows a priority model: direct stakers
fill capacity first, then sTEZ fills the remainder.

**Allocation rules:**

1. **Compute maximum allowed external stake**: ``own_staked ×
   GLOBAL_LIMIT_OF_STAKING_OVER_BAKING``.
2. **Direct stakers first**: direct stakers fill capacity up to
   the delegate's ``limit_of_staking_over_baking`` (capped by
   ``GLOBAL_LIMIT_OF_STAKING_OVER_BAKING``).
3. **sTEZ fills the remainder**: the maximum sTEZ allocation is the
   smaller of the remaining room under the maximum allowed external
   stake and the delegate's declared capacity (capped by
   ``GLOBAL_STEZ_BAKER_CAPACITY``) applied as a percentage of the
   maximum allowed external stake.

**Allocation pseudocode**::

   # Step 1: compute maximum allowed external stake
   max_external_stake = own_staked * global_limit_of_staking_over_baking

   # Step 2: direct stakers fill capacity first, subject to limit_of_staking_over_baking
   direct_staker_limit = min(limit_of_staking_over_baking,
                             global_limit_of_staking_over_baking)
   max_allowed_direct = own_staked * direct_staker_limit
   direct_staked_after_limits = min(direct_staked, max_allowed_direct)

   # Step 3: sTEZ fills the remainder
   # remaining_stez_stake: portion of the total sTEZ ledger not yet
   # allocated to higher-priority (lower-fee) delegates
   max_allowed_stez = min(max_external_stake - direct_staked_after_limits,
                          min(capacity, global_stez_baker_capacity)
                            * max_external_stake)
   stez_stake = min(remaining_stez_stake, max_allowed_stez)

**Example**: A delegate has ``1000`` tez of own stake, ``500`` tez of
direct stake, declared a ``capacity`` of ``15%`` and a
``limit_of_staking_over_baking`` of ``5``.
``GLOBAL_LIMIT_OF_STAKING_OVER_BAKING`` is ``9`` and
``GLOBAL_STEZ_BAKER_CAPACITY`` is ``20%``.

::

   # Step 1: compute maximum allowed external stake
   max_external_stake = 1000 * 9 = 9000

   # Step 2: direct stakers first (limit_of_staking_over_baking = 5)
   direct_staker_limit = min(5, 9) = 5
   max_allowed_direct = 1000 * 5 = 5000
   direct_staked_after_limits = min(500, 5000) = 500

   # Step 3: sTEZ fills the remainder
   max_allowed_stez = min(9000 - 500,                   # 8500 (remaining room)
                          min(0.15, 0.20) * 9000)      # 1350 (capacity cap)
                    = 1350
   stez_stake = min(remaining_stez_stake, 1350)

In this case the binding constraint is the delegate's declared
capacity (``15%`` of maximum allowed external stake = ``1350``
tez). If the delegate raised their capacity to the global maximum
(``20%``), up to ``1800`` tez of sTEZ could have their rights
allocated to them.

.. _stez_safeguards_u025:

Protocol Rules and Constants
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following protocol rules and constants govern the sTEZ system.
They can be adjusted through on-chain governance as conditions change:

- **Delegate allocation maximum** (``GLOBAL_STEZ_BAKER_CAPACITY``):
  the maximum proportion of a delegate's :ref:`maximum allowed
  external stake <stez_staking_power_u025>` that can be filled by
  sTEZ staking power, expressed in billionths. Delegates can declare a
  lower capacity but cannot exceed this global maximum.
- **Delegate fee maximum** (``GLOBAL_STEZ_BAKER_EDGE``): the maximum
  fee bid a delegate can declare, expressed in millionths (a ratio
  between 0 and 1). The effective fee determines the fraction of sTEZ
  staking rewards that accrues to the delegate's frozen deposit; the
  remainder goes to the :ref:`sTEZ staking ledger
  <stez_staking_ledger_u025>`. See :ref:`Global Fee
  <stez_global_fee_u025>` for how bids determine delegate selection
  and the effective fee.
- **Eligibility**: delegates must maintain a clean slashing history to
  receive sTEZ staking power allocations. Deactivated delegates are
  automatically excluded and may re-register once they are active
  again.

.. _stez_constants_u025:

.. list-table:: sTEZ protocol constants
   :widths: 50 30 20
   :header-rows: 1

   * - Constant
     - Value
     - Notes
   * - ``GLOBAL_STEZ_BAKER_CAPACITY``
     - TBD
     - Max % of delegate's maximum allowed external stake allocable to sTEZ
   * - ``GLOBAL_STEZ_BAKER_EDGE``
     - TBD
     - Max fee delegates can charge on sTEZ rewards

.. note::

   Values are under discussion and will be adjusted based on community
   feedback and testnet results.

Protocol constants can be queried via::

   octez-client rpc get /chains/<chain>/blocks/<block>/context/constants

.. _stez_user_operations_u025:

User Operations
---------------

This section describes the operations available to sTEZ users. For
background on how staking works in general, see :doc:`staking`.

sTEZ user operations -- **deposit**, **redeem**, and **finalize
redeem** -- mirror the ``stake``, ``unstake``, and
``finalize_unstake`` operations of :doc:`staking <staking>`. Staking
uses :ref:`pseudo-operations <pseudo_operations_u025>`
(self-transfers) where funds remain within the staker's account. sTEZ
operations are entrypoints of the sTEZ FA2.1 contract that transfer
tez between the user's account and the contract and back. The protocol
automatically distributes deposited staking power across registered
delegates, so the user does not need to have a delegate. If they have
a delegate, the delegate is only used for delegation and direct
staking.

.. list-table:: Comparison with staking commands
   :widths: 30 35 35
   :header-rows: 1

   * - Action
     - Staking
     - sTEZ
   * - Choose delegate
     - ``octez-client set delegate for <src> to <dlg>``
     - *Not needed*
   * - Lock funds
     - ``octez-client stake <amount> for <staker>``
     - ``octez-client stez deposit <amount> for <src>``
   * - Unlock funds
     - ``octez-client unstake <amount> for <staker>``
     - ``octez-client stez redeem <amount> for <src>``
   * - Finalize
     - ``octez-client finalize unstake for <staker>``
     - ``octez-client stez finalize redeem for <redeemer>``

Similarly to :doc:`staking <staking>`, any call to ``deposit`` or
``redeem`` implicitly finalizes all currently finalizable pending
redeem requests for the caller.

Depositing Tez
^^^^^^^^^^^^^^

The ``deposit`` entrypoint deposits tez into the :ref:`sTEZ staking
ledger <stez_staking_ledger_u025>`
and mints an equivalent value of sTEZ tokens (based on the current
:ref:`exchange rate <exchange_rate_u025>`), which are fully
transferable.

The specified amount of tez is transferred from the user's spendable
balance to the sTEZ contract.

::

   octez-client transfer <amount> from <src> to <stez_contract> --entrypoint "deposit"

or more conveniently::

   octez-client stez deposit <amount> for <src>

Redeeming sTEZ
^^^^^^^^^^^^^^

The ``redeem`` entrypoint initiates a redemption request by burning
the specified amount of sTEZ tokens and transferring the equivalent
tez (computed using the current :ref:`exchange rate
<exchange_rate_u025>`) from the :ref:`sTEZ staking ledger
<stez_staking_ledger_u025>` to the :ref:`sTEZ Unstaked Frozen Ledger
<stez_staking_ledger_u025>`. The amount is specified in **sTEZ
tokens** (not tez).

The redeemed funds remain **frozen and slashable** for at least
``UNSTAKE_FINALIZATION_DELAY+1`` (currently 4) cycles (the same delay
as :ref:`staking unstake requests
<staked_funds_management_u025>`). During this period, the funds may
be subject to :doc:`slashing <adaptive_slashing>` if an sTEZ delegate
misbehaves. This is also similar to direct staking, but the impact of
slashing is likely much smaller in this case, because weighted by the
proportion of the misbehaving baker's stake with respect to the global
sTEZ stake. Such slashing can be also viewed as a slight decrease in
the exchange rate that was applied when the funds were withdrawn.


::

   octez-client transfer 0 from <src> to <stez_contract> --entrypoint "redeem" --arg "<amount_in_stez>"

or more conveniently::

   octez-client stez redeem <amount> for <src>

Finalizing a Redemption
^^^^^^^^^^^^^^^^^^^^^^^

The ``finalize_redeem`` entrypoint triggers the transfer of **all**
finalizable funds back to the redeemer's spendable balance. Anyone can
trigger finalization on behalf of a redeemer: the unfrozen funds
always go to the original redeemer, without any ownership
transfer. This enables off-chain finalization bots to automate the
process.

::

   octez-client transfer 0 from <any_account> to <stez_contract> --entrypoint "finalize_redeem" --arg "<redeemer>"

or more conveniently::

   octez-client stez finalize redeem for <redeemer>

.. _stez_delegate_operations_u025:

Delegate Operations
-------------------

Delegates can opt in to the sTEZ system to receive additional staking
power from the protocol-managed :ref:`sTEZ staking ledger
<stez_staking_ledger_u025>`. Delegates
register with the sTEZ contract, declare their parameters, and the
protocol handles allocation automatically.

Delegates must satisfy the eligibility criteria defined in
:ref:`Protocol Rules and Constants <stez_safeguards_u025>`.

.. list-table:: Comparison with staking delegate commands
   :widths: 30 35 35
   :header-rows: 1

   * - Action
     - Staking
     - sTEZ
   * - Register
     - ``octez-client register key <dlg> as delegate``
     - ``octez-client stez register key <dlg> as delegate with initial fee <fee> and capacity <capacity>``
   * - Update parameters
     - ``octez-client update delegate parameters for <dlg> [--limit-of-staking-over-baking <limit>] [--edge-of-baking-over-staking <edge>]``
     - ``octez-client stez update delegate parameters for <dlg> with fee <fee> and capacity <capacity>``
   * - Unregister
     - *Delegate cannot unregister*
     - ``octez-client stez unregister key <dlg> as delegate``

Registering
^^^^^^^^^^^

The ``register_delegate`` entrypoint opts a delegate into sTEZ. The
registration takes effect after ``CONSENSUS_RIGHTS_DELAY`` (currently
2) cycles. The delegate specifies two parameters:

- **Fee**: the delegate's fee bid, expressed in millionths (range
  ``0`` -- ``GLOBAL_STEZ_BAKER_EDGE``). This is a competitive bid, not
  the actual fee charged. See :ref:`Global Fee
  <stez_global_fee_u025>` for how bids determine delegate selection
  and the effective fee.

- **Capacity**: the maximum proportion of the delegate's maximum
  allowed external stake that can be filled by sTEZ, expressed in
  billionths (e.g., ``150_000_000`` = 15%), capped by
  ``GLOBAL_STEZ_BAKER_CAPACITY``. See :ref:`Staking Power Distribution
  <stez_staking_power_u025>` for how capacity is used in the
  allocation.

::

   octez-client transfer 0 from <delegate> to <stez_contract> --entrypoint "register_delegate" --arg "Some (Pair <fee> <capacity>)"

To register with default parameters::

   octez-client transfer 0 from <delegate> to <stez_contract> --entrypoint "register_delegate" --arg "None"

or more conveniently::

   octez-client stez register key <delegate> as delegate with initial fee <fee> and capacity <capacity>

Updating Parameters
^^^^^^^^^^^^^^^^^^^

The ``update_delegate_parameters`` entrypoint allows a delegate to
modify their fee and capacity. Updated parameters take effect
after ``CONSENSUS_RIGHTS_DELAY`` (currently 2) cycles.

::

   octez-client transfer 0 from <delegate> to <stez_contract> --entrypoint "update_delegate_parameters" --arg "Pair <fee> <capacity>"

or more conveniently::

   octez-client stez update delegate parameters for <delegate> with fee <fee> and capacity <capacity>

Unregistering
^^^^^^^^^^^^^

The ``unregister_delegate`` entrypoint opts a delegate out of
sTEZ. The unregistration takes effect after ``CONSENSUS_RIGHTS_DELAY``
(currently 2) cycles. During the delay period, the delegate continues
to receive sTEZ staking power allocations under their existing
parameters.

::

   octez-client transfer 0 from <delegate> to <stez_contract> --entrypoint "unregister_delegate"

or more conveniently::

   octez-client stez unregister key <delegate> as delegate

.. _stez_fa21_u025:

FA2.1 Token Standard
--------------------

The sTEZ contract implements the `FA2.1 token standard
<https://tzip.tezosagora.org/proposal/tzip-26/>`__, enabling seamless
integration with wallets, block explorers, decentralized exchanges,
and other tools and applications.

The contract exposes entrypoints for transfers and approvals, ticket
operations for rollup bridging, and a callback for FA2 backwards
compatibility. It also provides on-chain views for read-only queries.

**Transfer and approval entrypoints**:

- ``%transfer`` -- transfer sTEZ tokens between accounts.
- ``%approve`` -- authorize others to withdraw tokens from the user's
  account, similar to `FA1.2
  <https://tzip.tezosagora.org/proposal/tzip-7/>`__ with a finite
  allowance mechanism.
- ``%update_operators`` -- approve others to withdraw tokens from the
  user's account (infinite allowance), maintaining backwards
  compatibility with `FA2
  <https://tzip.tezosagora.org/proposal/tzip-12/>`__. An operator is a
  Tezos address that performs token transfer operations on behalf of
  the token owner.

**Ticket entrypoints**:

- ``%export_ticket`` -- export sTEZ tokens as tickets outside of the
  contract.
- ``%import_ticket`` -- import sTEZ tokens from tickets back into the
  contract.
- ``%lambda_export`` -- similar to ``%export_ticket``, but also
  executes a lambda in a sandbox for custom behavior.

**On-chain views**: ``get_balance``, ``get_total_supply``,
``get_allowance``, ``is_operator``, ``get_token_metadata`` (`TZIP-21
<https://tzip.tezosagora.org/proposal/tzip-21/>`__), and ``is_token``.

**Callback view**: ``%balance_of`` (batch balance query with callback,
for backwards compatibility with `FA2
<https://tzip.tezosagora.org/proposal/tzip-12/>`__).

Entrypoints emit events (``transfer_event``, ``balance_update``,
``total_supply_update``, ``operator_update``, ``allowance_update``,
``token_metadata_update``) to support indexer discoverability.

.. _stez_reference_u025:

Reference
---------

This section provides the RPC endpoints and receipt formats for
interacting with and monitoring the sTEZ system.

.. _stez_rpc_u025:

RPC Endpoints
^^^^^^^^^^^^^

sTEZ RPCs for users and delegates mirror the existing :doc:`staking
<staking>` RPCs. The tables below show the correspondence, followed by
the full endpoint reference.

sTEZ Info
"""""""""

Global sTEZ state: exchange rate, total supply, registered delegates,
and per-cycle allocation data.

Base path: ``/chains/<chain>/blocks/<block>/context/stez``

.. list-table::
   :widths: 35 65
   :header-rows: 1

   * - Endpoint
     - Description
   * - ``../contract_hash``
     - sTEZ contract hash
   * - ``../total_supply``
     - Total sTEZ token supply
   * - ``../total_amount_of_tez``
     - Total tez in sTEZ staking ledger (in mutez)
   * - ``../exchange_rate``
     - Exchange rate as ``{numerator, denominator}``; returns ``{1,
       1}`` if supply is 0
   * - ``../bakers``
     - List of delegates registered with sTEZ together with their
       ``fee`` and ``capacity``
   * - ``../stez_staking_power?cycle=n``
     - List of delegates with their staking power from sTEZ for the
       given cycle ``n``; defaults to the current cycle
   * - ``../global_fee?cycle=n``
     - Maximum fee among the selected (lowest-fee first) eligible
       delegates for the given cycle ``n``; defaults to the current
       cycle

User Info
"""""""""

sTEZ user RPCs correspond to the staking user RPCs as follows:

.. list-table:: Comparison with staking user RPCs
   :widths: 50 50
   :header-rows: 1

   * - Staking
     - sTEZ
   * - ``../staked_balance``
     - ``../stez_balance``
   * - ``../unstake_requests``
     - ``../stez_redeem_requests``
   * - ``../unstaked_finalizable_balance``
     - ``../stez_redeemed_finalizable_balance``
   * - ``../unstaked_frozen_balance``
     - ``../stez_redeemed_frozen_balance``

Base path: ``/chains/<chain>/blocks/<block>/context/contracts/<contract>``

.. list-table::
   :widths: 40 60
   :header-rows: 1

   * - Endpoint
     - Description
   * - ``../stez_balance``
     - sTEZ token balance of the contract. Returns 0 if the contract
       is originated or does not hold any sTEZ tokens.
   * - ``../stez_ticket_balance``
     - sTEZ ticket balance of the contract (for tokens exported as
       tickets).
   * - ``../stez_redeem_requests``
     - Redemption requests of the contract. Returns an object with
       ``finalizable`` and ``unfinalizable`` lists, each containing
       ``{cycle, amount}`` entries. Requests in the ``finalizable``
       list can be finalized — these funds are no longer frozen and
       can be transferred to the contract's spendable balance with any
       ``deposit``, ``redeem``, or ``finalize_redeem``
       operation. Requests in the ``unfinalizable`` list are still
       frozen for the standard unstake finalization delay. Returns
       ``null`` for originated contracts or if no redemption requests
       are pending.

       Example output::

         {"finalizable":
           [{"cycle": <cycle_x>, "amount": "<amount_x>"}, ...],
          "unfinalizable":
           [{"cycle": <cycle_y>, "amount": "<amount_y>"}, ...]}
   * - ``../stez_redeemed_frozen_balance``
     - Balance (in mutez) of funds that were requested for a
       ``redeem`` operation but are still frozen for the standard
       unstake finalization delay. Returns ``null`` for originated
       contracts.
   * - ``../stez_redeemed_finalizable_balance``
     - Balance (in mutez) of funds that were requested for a
       ``redeem`` operation and are no longer frozen. These can be
       transferred to the contract's spendable balance with any
       ``deposit``, ``redeem``, or ``finalize_redeem``
       operation. Returns ``null`` for originated contracts.

Delegate Info
"""""""""""""

sTEZ delegate RPCs correspond to the staking delegate RPCs as follows:

.. list-table:: Comparison with staking delegate RPCs
   :widths: 50 50
   :header-rows: 1

   * - Staking
     - sTEZ
   * - ``../active_staking_parameters``
     - ``../active_stez_parameters``
   * - ``../pending_staking_parameters``
     - ``../pending_stez_parameters``
   * - ``../own_staked``
     - ``../own_staked``
   * - ``../stakers``
     - ``../stez_registered``
   * - ``../external_staked``
     - ``../stez_staking_power``

Base path: ``/chains/<chain>/blocks/<block>/context/delegates/<delegate>``

.. list-table::
   :widths: 35 65
   :header-rows: 1

   * - Endpoint
     - Description
   * - ``../stez_registered``
     - Whether the delegate is registered with sTEZ. Returns ``true``
       if registered, ``false`` otherwise.
   * - ``../active_stez_parameters``
     - Currently active delegate parameters for sTEZ (fee and capacity),
       or ``null`` if the delegate is not registered.
   * - ``../pending_stez_parameters``
     - Pending parameter updates with their activation cycles.
   * - ``../stez_staking_power``
     - Current staking power from sTEZ allocated to this delegate.

.. _stez_receipts_u025:

Receipts
^^^^^^^^

Staking rewards and slashing penalties are distributed automatically
by the protocol. Receipts for these events appear in the block
metadata and can be queried with::

   octez-client rpc get /chains/<chain>/blocks/<block>/metadata

In addition to the existing staking receipt categories, sTEZ
introduces two new balance update categories.  Both use ``"kind":
"freezer"``:

- ``stez_deposits`` -- the global sTEZ staking ledger (frozen tez
  backing all sTEZ tokens). Credits increase the :ref:`exchange rate
  <exchange_rate_u025>` and debits (from slashing) decrease it.
- ``stez_redeemed_deposits`` -- per-user redeemed funds still frozen
  during the finalization delay, tagged with the staker address and
  cycle. These funds remain slashable until the finalization delay
  elapses.

Staking Rewards Receipts
""""""""""""""""""""""""

Staking rewards are split into six components. The two sTEZ-specific
parts (in **bold**) are new in Protocol Proposal U:

- the part that goes to the delegate's spendable balance from
  delegation,
- the part that goes to the delegate's frozen deposits from its own
  frozen stake,
- the part that goes to the delegate's frozen deposits from its edge
  on stakers rewards,
- **the part that goes to the delegate's frozen deposits from its fee
  on sTEZ staking rewards,**
- the parts shared amongst the delegate's stakers,
- **the part that goes to the sTEZ staking ledger.**

.. code-block:: json

   [
     { "kind": "minted", "category": "baking rewards",
       "change": "-<amount_delegation>", "origin": "block" },
     { "kind": "contract", "contract": "<delegate_pkh>",
       "change": "<amount_delegation>", "origin": "block" },

     { "kind": "minted", "category": "baking rewards",
       "change": "-<amount_baker_share>", "origin": "block" },
     { "kind": "freezer", "category": "deposits",
       "staker": { "baker_own_stake": "<delegate_pkh>" },
       "change": "<amount_baker_share>", "origin": "block" },

     { "kind": "minted", "category": "baking rewards",
       "change": "-<amount_edge>", "origin": "block" },
     { "kind": "freezer", "category": "deposits",
       "staker": { "baker_edge": "<delegate_pkh>" },
       "change": "<amount_edge>", "origin": "block" },

     { "kind": "minted", "category": "baking rewards",
       "change": "-<amount_stez_fee>", "origin": "block" },
     { "kind": "freezer", "category": "deposits",
       "staker": { "baker_stez_fee": "<delegate_pkh>" },
       "change": "<amount_stez_fee>", "origin": "block" },

     { "kind": "minted", "category": "baking rewards",
       "change": "-<amount_stakers>", "origin": "block" },
     { "kind": "freezer", "category": "deposits",
       "staker": { "delegate": "<delegate_pkh>" },
       "change": "<amount_stakers>", "origin": "block" },

     { "kind": "minted", "category": "baking rewards",
       "change": "-<amount_stez_staking>", "origin": "block" },
     { "kind": "freezer", "category": "stez_deposits",
       "change": "<amount_stez_staking>", "origin": "block" }
   ]

Where:

- ``amount_delegation`` -- rewards to the delegate's spendable balance
  from delegation
- ``amount_baker_share`` -- rewards to the delegate's frozen deposits
  from its own frozen stake
- ``amount_edge`` -- rewards to the delegate's frozen deposits from
  its edge on stakers rewards
- ``amount_stez_fee`` -- **rewards to the delegate's frozen deposits
  from its fee on sTEZ staking rewards**
- ``amount_stakers`` -- rewards shared amongst the delegate's stakers
- ``amount_stez_staking`` -- **rewards to the sTEZ staking ledger**

Denunciation and Slashing Receipts
""""""""""""""""""""""""""""""""""

Slashing receipts include the denunciation reward and the slashing of
frozen and unstaked deposits of the offending delegate, its stakers,
and its **sTEZ staking power**. The two sTEZ-specific categories are
``stez_deposits`` and ``stez_redeemed_deposits``:

.. code-block:: json

   [
     { "kind": "freezer", "category": "deposits",
       "staker": { "baker_own_stake": "<offending_delegate_pkh>" },
       "change": "-<amount_slashed_from_baker_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "deposits",
       "staker": { "delegate": "<offending_delegate_pkh>" },
       "change": "-<amount_slashed_from_stakers_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "stez_deposits",
       "change": "-<amount_slashed_from_stez_staking_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "unstaked_deposits",
       "staker": { "delegate": "<offending_delegate_pkh>" },
       "cycle": "<cycle>",
       "change": "-<amount_slashed_from_unstake_stakers_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "stez_redeemed_deposits",
       "cycle": "<cycle>",
       "change": "-<amount_slashed_from_stez_redeemed_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "burned", "category": "punishments",
       "change": "<total_amount_slashed>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },

     { "kind": "freezer", "category": "deposits",
       "staker": { "baker_own_stake": "<offending_delegate_pkh>" },
       "change": "-<amount_rewarded_from_baker_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "deposits",
       "staker": { "delegate": "<offending_delegate_pkh>" },
       "change": "-<amount_rewarded_from_stakers_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "stez_deposits",
       "change": "-<amount_rewarded_from_stez_staking_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "unstaked_deposits",
       "staker": { "delegate": "<offending_delegate_pkh>" },
       "cycle": "<cycle>",
       "change": "-<amount_rewarded_from_unstake_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "freezer", "category": "stez_redeemed_deposits",
       "cycle": "<cycle>",
       "change": "-<amount_rewarded_from_stez_redeemed_deposits>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" },
     { "kind": "contract", "contract": "<denunciator_pkh>",
       "change": "<total_amount_rewarded>",
       "origin": "delayed_operation",
       "delayed_operation_hash": "<denunciation_op_hash>" }
   ]

Where:

- ``amount_slashed_from_baker_deposits`` -- tez slashed from the
  offending delegate's frozen deposits (proportional to its
  contribution, rounded up)
- ``amount_slashed_from_stakers_deposits`` -- tez slashed from the
  offending delegate's stakers' frozen deposits (proportional, rounded
  down)
- ``amount_slashed_from_stez_staking_deposits`` -- **tez slashed from
  the sTEZ staking ledger (proportional, rounded down)**
- ``amount_slashed_from_unstake_stakers_deposits`` -- tez slashed from
  unstaked frozen deposits of the delegate and its stakers
- ``amount_slashed_from_stez_redeemed_deposits`` -- **tez slashed from
  redeemed frozen deposits of the sTEZ share**
- ``total_amount_slashed`` -- total tez slashed
- ``amount_rewarded_from_*`` -- denunciation rewards taken from each
  category (proportional to amounts slashed, rounded down)
- ``total_amount_rewarded`` -- total tez rewarded to the denunciator
