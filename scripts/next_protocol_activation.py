#!/usr/bin/env python3
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
"""Estimate when the next Tezos protocol activation will happen, in the best case.

The tool queries a running Octez node (read-only), finds the current voting
period and the candidate protocol on track to activate (even if voting is not
finished), and projects the activation date/time:

  * best case  -- every block produced at round 0 (minimal block delay), and
  * with drift -- a configurable fraction of higher-round (slower) blocks,
                  e.g. 5% of blocks at round 1.

"Best case" assumes the candidate passes every remaining governance vote; the
tool projects timing, it does not predict the vote outcome.

Governance order: Proposal -> Exploration -> Cooldown -> Promotion -> Adoption.
Activation happens at the end of the Adoption period.
"""

import argparse
import json
import sys
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone

# Exit codes.
EXIT_OK = 0
EXIT_NO_CANDIDATE = 1
EXIT_BAD_ARGS = 2
EXIT_NODE_ERROR = 3

# Voting period kinds in chronological order. Activation fires at the end of the
# last one (Adoption). Confirmed in src/proto_alpha/lib_protocol:
#   voting_period_repr.mli (kind variant) and amendment.ml (Adoption -> activate).
PERIOD_ORDER = ["proposal", "exploration", "cooldown", "promotion", "adoption"]
ADOPTION_INDEX = len(PERIOD_ORDER) - 1

DEFAULT_ENDPOINT = "http://localhost:8732"
RPC_TIMEOUT_SECONDS = 10


# --------------------------------------------------------------------------- #
# Node access (read-only)
# --------------------------------------------------------------------------- #
class NodeError(Exception):
    """Raised when the node cannot be reached or returns unusable data."""


def rpc_get(endpoint, path):
    """GET a JSON RPC from the node. Read-only; raises NodeError on failure."""
    url = endpoint.rstrip("/") + path
    try:
        with urllib.request.urlopen(url, timeout=RPC_TIMEOUT_SECONDS) as resp:
            if resp.status != 200:
                raise NodeError(f"{url}: HTTP {resp.status}")
            raw = resp.read()
    except urllib.error.HTTPError as exc:
        raise NodeError(f"{url}: HTTP {exc.code}") from exc
    except (urllib.error.URLError, OSError) as exc:
        raise NodeError(f"cannot reach {url}: {exc}") from exc
    try:
        return json.loads(raw)
    except json.JSONDecodeError as exc:
        raise NodeError(f"{url}: invalid JSON response ({exc})") from exc


def parse_timestamp(ts):
    """Parse an RFC3339 timestamp (e.g. '2026-06-15T08:37:01Z') as tz-aware UTC."""
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00"))
    except ValueError as exc:
        raise NodeError(f"unparseable block timestamp {ts!r}: {exc}") from exc


def load_constants(endpoint):
    """Read timing/governance constants from the node.

    Everything used by the estimate comes from the node, so the tool stays
    correct across protocols with different parameters. The voting period
    length is taken from a directly-provided ``blocks_per_voting_period`` when
    present, otherwise derived from ``cycles_per_voting_period * blocks_per_cycle``
    (protocols expose one or the other depending on their version).
    """
    c = rpc_get(endpoint, "/chains/main/blocks/head/context/constants")
    try:
        mbd = int(c["minimal_block_delay"])
        incr = int(c["delay_increment_per_round"])
        blocks_per_cycle = int(c["blocks_per_cycle"])
    except (KeyError, ValueError, TypeError) as exc:
        raise NodeError(f"unexpected constants payload: {exc}") from exc

    blocks_per_voting_period = None
    cycles_per_voting_period = None
    try:
        if c.get("blocks_per_voting_period") is not None:
            blocks_per_voting_period = int(c["blocks_per_voting_period"])
        if c.get("cycles_per_voting_period") is not None:
            cycles_per_voting_period = int(c["cycles_per_voting_period"])
    except (ValueError, TypeError) as exc:
        raise NodeError(f"unexpected voting-period constants: {exc}") from exc

    if blocks_per_voting_period is None:
        if cycles_per_voting_period is None:
            raise NodeError(
                "constants expose neither blocks_per_voting_period nor "
                "cycles_per_voting_period"
            )
        blocks_per_voting_period = cycles_per_voting_period * blocks_per_cycle

    if mbd <= 0 or incr < 0 or blocks_per_cycle <= 0 or blocks_per_voting_period <= 0:
        raise NodeError("constants out of range (non-positive timing values)")

    return {
        "minimal_block_delay": mbd,
        "delay_increment_per_round": incr,
        "blocks_per_cycle": blocks_per_cycle,
        "cycles_per_voting_period": cycles_per_voting_period,
        "blocks_per_voting_period": blocks_per_voting_period,
    }


def load_head(endpoint):
    """Read the head level and timestamp (the projection anchor)."""
    shell = rpc_get(endpoint, "/chains/main/blocks/head/header/shell")
    try:
        return {
            "level": int(shell["level"]),
            "timestamp": parse_timestamp(shell["timestamp"]),
        }
    except (KeyError, ValueError, TypeError) as exc:
        raise NodeError(f"unexpected header payload: {exc}") from exc


def _proposal_hash(entry):
    """A /votes/proposals entry is either a hash or a [hash, weight] pair."""
    if isinstance(entry, (list, tuple)) and entry:
        return entry[0]
    return entry


def load_voting_state(endpoint):
    """Read the current voting period, candidate proposal and informational tallies."""
    period = rpc_get(endpoint, "/chains/main/blocks/head/votes/current_period")
    current_proposal = rpc_get(
        endpoint, "/chains/main/blocks/head/votes/current_proposal"
    )
    proposals = rpc_get(endpoint, "/chains/main/blocks/head/votes/proposals")
    # Informational only -- best effort, never fatal.
    try:
        ballots = rpc_get(endpoint, "/chains/main/blocks/head/votes/ballots")
    except NodeError:
        ballots = None
    try:
        quorum = rpc_get(endpoint, "/chains/main/blocks/head/votes/current_quorum")
    except NodeError:
        quorum = None

    try:
        vp = period["voting_period"]
        kind = vp["kind"]
        start_position = int(vp["start_position"])
        position = int(period["position"])
        remaining = int(period["remaining"])
    except (KeyError, ValueError, TypeError) as exc:
        raise NodeError(f"unexpected voting period payload: {exc}") from exc

    if kind not in PERIOD_ORDER:
        raise NodeError(f"unknown governance period kind: {kind!r}")
    if remaining < 0:
        raise NodeError(f"negative blocks remaining in period: {remaining}")

    candidates = [_proposal_hash(p) for p in proposals] if proposals else []
    return {
        "kind": kind,
        "index": PERIOD_ORDER.index(kind),
        "start_position": start_position,
        "position": position,
        "remaining": remaining,
        "current_proposal": current_proposal,  # hash string or None
        "proposals": candidates,
        "ballots": ballots,
        "quorum": quorum,
    }


# --------------------------------------------------------------------------- #
# Pure timing math (verified against src/proto_alpha/lib_protocol/round_repr.ml)
# --------------------------------------------------------------------------- #
def level_offset(r, minimal_block_delay, delay_increment_per_round):
    """Cumulative time to reach round r from the start of a level (round 0 = 0).

    level_offset(r) = r*mbd + (r*(r-1)/2)*incr  (round_repr.ml raw_level_offset_of_round)
    """
    return r * minimal_block_delay + (r * (r - 1) // 2) * delay_increment_per_round


def avg_block_time(round_pcts, minimal_block_delay, delay_increment_per_round):
    """Average inter-block time given a fraction of blocks at each higher round.

    An isolated round-r block (predecessor at round 0) takes
    `minimal_block_delay + level_offset(r)` seconds, i.e. `level_offset(r)` more
    than a round-0 block. With fraction p_r of blocks at round r (r >= 1):

        avg = minimal_block_delay + sum_{r>=1} p_r * level_offset(r)
    """
    avg = float(minimal_block_delay)
    for r, frac in round_pcts.items():
        avg += frac * level_offset(r, minimal_block_delay, delay_increment_per_round)
    return avg


def blocks_to_activation(voting_state, head_level, blocks_per_voting_period):
    """Blocks from head to activation, plus the relevant levels.

    blocks = remaining(current period) + (ADOPTION_INDEX - index) * blocks_per_voting_period
    """
    subsequent_full = ADOPTION_INDEX - voting_state["index"]
    blocks = voting_state["remaining"] + subsequent_full * blocks_per_voting_period
    last_adoption_level = head_level + blocks
    return {
        "blocks": blocks,
        "subsequent_full_periods": subsequent_full,
        "last_adoption_level": last_adoption_level,
        "new_protocol_level": last_adoption_level + 1,
    }


def scenario_eta(head_timestamp, blocks, avg_seconds):
    """Project the activation datetime for a given average block time."""
    total_seconds = blocks * avg_seconds
    eta = head_timestamp + timedelta(seconds=total_seconds)
    return eta, total_seconds / 86400.0


# --------------------------------------------------------------------------- #
# CLI argument handling
# --------------------------------------------------------------------------- #
def parse_round_pcts(round1_pct, round_pct_args):
    """Build {round -> fraction} from --round1-pct and repeated --round-pct R:PCT.

    Percentages are validated to be in [0, 100] and to sum to <= 100. Returns a
    dict of fractions (0..1) for rounds >= 1. Raises ValueError on bad input.
    """
    pcts = {}
    if round1_pct is not None:
        pcts[1] = float(round1_pct)
    for spec in round_pct_args or []:
        if ":" not in spec:
            raise ValueError(f"--round-pct expects R:PCT, got {spec!r}")
        r_str, pct_str = spec.split(":", 1)
        try:
            r = int(r_str)
            pct = float(pct_str)
        except ValueError as exc:
            raise ValueError(f"--round-pct {spec!r}: {exc}") from exc
        if r < 1:
            raise ValueError(f"--round-pct round must be >= 1, got {r}")
        pcts[r] = pct  # explicit --round-pct overrides --round1-pct for round 1

    fractions = {}
    total = 0.0
    for r, pct in pcts.items():
        if pct < 0 or pct > 100:
            raise ValueError(f"round-{r} percentage {pct} out of range [0, 100]")
        total += pct
        if pct > 0:
            fractions[r] = pct / 100.0
    if total > 100.0 + 1e-9:
        raise ValueError(f"round percentages sum to {total} > 100")
    return fractions


def drift_label(round_pcts):
    """Human label for a drift scenario, e.g. '5% round-1, 1% round-2'."""
    if not round_pcts:
        return "no drift"
    parts = [
        f"{frac * 100:g}% round-{r}" for r, frac in sorted(round_pcts.items())
    ]
    return ", ".join(parts)


def build_parser():
    p = argparse.ArgumentParser(
        description="Estimate the next Tezos protocol activation date/time.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument(
        "--endpoint",
        default=DEFAULT_ENDPOINT,
        help="Base RPC URL of the Octez node",
    )
    p.add_argument(
        "--round1-pct",
        type=float,
        default=5.0,
        help="Percentage of blocks assumed produced at round 1 (drift)",
    )
    p.add_argument(
        "--round-pct",
        action="append",
        metavar="R:PCT",
        help="Assume PCT%% of blocks at round R (R>=1); repeatable for higher rounds",
    )
    p.add_argument(
        "--selftest",
        action="store_true",
        help="Run inline checks on the timing math and exit",
    )
    return p


# --------------------------------------------------------------------------- #
# Self-test (SC-004 and the timing math)
# --------------------------------------------------------------------------- #
def selftest():
    # level_offset against round_repr.ml: offset(1)=6, offset(2)=2*6+1*3=15.
    assert level_offset(0, 6, 3) == 0
    assert level_offset(1, 6, 3) == 6
    assert level_offset(2, 6, 3) == 15
    # Round-0 only -> minimal block delay.
    assert avg_block_time({}, 6, 3) == 6
    # 5% round-1 -> 6 + 0.05*6 = 6.3.
    assert abs(avg_block_time({1: 0.05}, 6, 3) - 6.3) < 1e-9
    # blocks_to_activation for a promotion fixture: remaining + 1 full period.
    vs = {"index": PERIOD_ORDER.index("promotion"), "remaining": 7682}
    res = blocks_to_activation(vs, head_level=100, blocks_per_voting_period=201600)
    assert res["blocks"] == 7682 + 201600
    assert res["last_adoption_level"] == 100 + 7682 + 201600
    assert res["new_protocol_level"] == res["last_adoption_level"] + 1
    # 0% drift equals best case (SC-004).
    assert avg_block_time({}, 6, 3) == avg_block_time(parse_round_pcts(0, None), 6, 3)
    print("selftest: OK")
    return EXIT_OK


# --------------------------------------------------------------------------- #
# Reporting
# --------------------------------------------------------------------------- #
def format_ballots(ballots):
    if not ballots:
        return "ballots: unavailable"
    try:
        yay = int(ballots.get("yay", 0))
        nay = int(ballots.get("nay", 0))
        pas = int(ballots.get("pass", 0))
    except (ValueError, TypeError, AttributeError):
        return f"ballots: {ballots}"
    decisive = yay + nay
    ratio = f"{100.0 * yay / decisive:.2f}% yay of yay+nay" if decisive else "no yay/nay"
    return f"ballots: yay={yay} nay={nay} pass={pas} ({ratio})"


def format_quorum(quorum):
    if quorum is None:
        return "quorum: unavailable"
    try:
        return f"quorum: {quorum} (~{int(quorum) / 100:.2f}%)"
    except (ValueError, TypeError):
        return f"quorum: {quorum}"


def report(constants, head, voting, candidate, activation, scenarios):
    bpvp = constants["blocks_per_voting_period"]
    print("Tezos next protocol activation estimate")
    print("=" * 60)
    print(
        f"Current period : {voting['kind']} (index {voting['index']} of "
        f"{ADOPTION_INDEX}), starts at level {voting['start_position']}"
    )
    print(
        f"  position {voting['position']}, {voting['remaining']} blocks "
        f"remaining in this period (period = {bpvp} blocks)"
    )
    print(f"  {format_ballots(voting['ballots'])}")
    print(f"  {format_quorum(voting['quorum'])}")
    print()
    print(f"Candidate protocol : {candidate}")
    print("  NOTE: 'best case' assumes this candidate passes ALL remaining votes.")
    print()
    print(f"Head : level {head['level']} at {head['timestamp'].isoformat()}")
    print(
        f"Blocks until activation : {activation['blocks']} "
        f"(= {voting['remaining']} + {activation['subsequent_full_periods']} "
        f"full period(s) x {bpvp})"
    )
    print(f"  Last Adoption block level   : {activation['last_adoption_level']}")
    print(f"  New protocol first block lvl : {activation['new_protocol_level']}")
    print()
    print("Scenarios:")
    print(
        f"  {'scenario':<22} {'avg block':>10} {'days':>7}   activation (UTC)"
    )
    for label, avg, eta, days in scenarios:
        print(
            f"  {label:<22} {avg:>9.3f}s {days:>7.2f}   "
            f"{eta.strftime('%Y-%m-%d %H:%M:%S UTC')}"
        )


# --------------------------------------------------------------------------- #
# Main
# --------------------------------------------------------------------------- #
def main(argv=None):
    args = build_parser().parse_args(argv)

    if args.selftest:
        return selftest()

    try:
        round_pcts = parse_round_pcts(args.round1_pct, args.round_pct)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return EXIT_BAD_ARGS

    try:
        constants = load_constants(args.endpoint)
        head = load_head(args.endpoint)
        voting = load_voting_state(args.endpoint)
    except NodeError as exc:
        print(f"node error: {exc}", file=sys.stderr)
        return EXIT_NODE_ERROR

    # Candidate resolution: current_proposal (during/after Exploration) or the
    # first submitted proposal (during the Proposal period).
    candidate = voting["current_proposal"]
    if not candidate and voting["proposals"]:
        candidate = voting["proposals"][0]
    if not candidate:
        print(
            f"No protocol is currently on track to activate "
            f"(period: {voting['kind']}, no current proposal)."
        )
        return EXIT_NO_CANDIDATE

    activation = blocks_to_activation(
        voting, head["level"], constants["blocks_per_voting_period"]
    )

    mbd = constants["minimal_block_delay"]
    incr = constants["delay_increment_per_round"]

    scenarios = []
    best_avg = avg_block_time({}, mbd, incr)
    best_eta, best_days = scenario_eta(head["timestamp"], activation["blocks"], best_avg)
    scenarios.append(("best case", best_avg, best_eta, best_days))

    drift_avg = avg_block_time(round_pcts, mbd, incr)
    drift_eta, drift_days = scenario_eta(
        head["timestamp"], activation["blocks"], drift_avg
    )
    scenarios.append((drift_label(round_pcts), drift_avg, drift_eta, drift_days))

    report(constants, head, voting, candidate, activation, scenarios)
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
