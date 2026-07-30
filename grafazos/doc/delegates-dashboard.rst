Octez Delegates Dashboard
=========================

Overview
--------

The Octez Delegates Dashboard monitors the health, participation and staking
of one or several delegates (bakers), using the ``octez_delegate_*``
Prometheus metrics.

Unlike the other Grafazos dashboards, these metrics are not exposed by the
Octez node itself: they are extracted from the node's
``/chains/main/blocks/head/context/delegates/<pkh>`` RPC endpoint by an
external exporter (for instance a
`json-exporter <https://github.com/prometheus-community/json_exporter>`__
probing the RPC for each monitored delegate).

Metrics contract
----------------

The dashboard expects one metric per numeric field of the delegates RPC
response, named ``octez_delegate_`` followed by the flattened
(dot-to-underscore) JSON path of the field. For example:

- ``.baking_power`` → ``octez_delegate_baking_power``
- ``.participation.missed_slots`` → ``octez_delegate_participation_missed_slots``
- ``.dal_participation.sufficient_dal_participation`` → ``octez_delegate_dal_participation_sufficient_dal_participation``

Boolean fields are exported as ``0``/``1`` gauges. Balance fields are in
mutez (the dashboard divides by ``1e6`` where it displays XTZ).

Each series must carry two labels identifying the delegate:

- ``delegate``: the delegate public key hash (address)
- ``delegate_name``: a friendly alias, used in legends and in the
  ``Delegate`` dashboard variable

plus the instance label configured at build time with
``NODE_INSTANCE_LABEL`` (default ``instance``), used by the
``Node Instance`` variable.

Dashboard sections
------------------

Overview
~~~~~~~~

- **Delegate Status**: ``deactivated`` (Active/Deactivated)
- **Forbidden Status**: ``is_forbidden`` (Allowed/Forbidden)
- **DAL Participation / DAL Denounced**: current-cycle DAL sufficiency and
  denunciation flags
- **Grace Period (cycle)**: cycle at which the delegate gets deactivated
  unless it participates again
- **Baking Power (XTZ)**

Staking & Balances
~~~~~~~~~~~~~~~~~~

- **Total Staked / Total Delegated / Own Full Balance / Min Delegated in
  Current Cycle** stats
- **Staking Composition**: own vs. external staked over time
- **Delegation Composition**: own vs. external delegated over time

Participation (current cycle)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- **Attested Slots (%)**: share of assigned attestation slots not missed;
  falling below the minimal cycle activity (~2/3) forfeits attesting rewards
- **Remaining Allowed Missed Slots** and **Missed Levels**
- **Expected Attesting Rewards (XTZ)**
- **Participation Rate Over Time** and **Participation Activity**
  (expected / minimal / missed slots)

DAL Participation (current cycle)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- **DAL Attested Slots (%)**: attested vs. attestable DAL slots
- **Expected Assigned Shards per Slot** and **Expected DAL Rewards (XTZ)**
- **DAL Slots** and **DAL Participation Status** over time

Voting & Governance
~~~~~~~~~~~~~~~~~~~

- **Voting Power (XTZ)**: ``voting_power`` and ``current_voting_power``
- **Remaining Proposals** in the current voting period

Staking Parameters & Risk
~~~~~~~~~~~~~~~~~~~~~~~~~

- **Limit of Staking over Baking**: maximum external stake accepted, as a
  multiple of the delegate's own stake
- **Edge of Baking over Staking**: share of the stakers' rewards kept by the
  delegate
- **Staking Denominator (XTZ)**
- **Pending Slashed Amount (XTZ)**: turns red above zero

Build
-----

.. code-block:: shell

    cd grafazos && make delegates

The dashboard is generated in ``output/octez-delegates.json`` and can be
imported into Grafana. The usual Grafazos build options apply
(``NODE_INSTANCE_LABEL``, ``UID``, ``DATASOURCE_SELECTION``,
``DATASOURCE_DEFAULT``).

Variables
---------

- **Node Instance**: instances exposing ``octez_delegate_*`` metrics
  (one per monitored network in a typical deployment)
- **Delegate**: multi-select over the ``delegate_name`` label, defaults to all
