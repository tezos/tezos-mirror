#!/usr/bin/env python3
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
"""Estimate the next protocol activation using an empirically-measured drift.

This combines the two sibling tools:

  * `next_protocol_activation.py` computes how many blocks (N) remain until the
    next protocol activation and projects the activation date/time, and
  * `round_distribution.py` measures the per-round block distribution over a
    window of blocks from tzkt.

It measures the round distribution over the trailing window of the SAME length
as the distance to activation (the last N blocks, by default) and feeds that
observed distribution as the drift -- i.e. "assume the next N blocks behave like
the last N blocks". The best-case and empirical activation ETAs are reported
together.

The functions are reused by import; `next_protocol_activation.py` keeps no tzkt
dependency. The tool is read-only.
"""

import argparse
import sys
from pathlib import Path

# Reuse the sibling scripts by import (they live in this directory and are
# guarded by `if __name__ == "__main__"`, so importing has no side effects).
sys.path.insert(0, str(Path(__file__).resolve().parent))

import next_protocol_activation as npa  # noqa: E402
import round_distribution as rd  # noqa: E402

EXIT_OK = 0
EXIT_NO_CANDIDATE = 1
EXIT_BAD_ARGS = 2
EXIT_DATA_ERROR = 3

DEFAULT_ENDPOINT = "http://localhost:8732"

# load_constants/head/voting raise npa.NodeError; resolve_tzkt_base raises
# rd.NodeError; tally_rounds raises rd.TzktError. Treat all as data errors.
NODE_ERRORS = (npa.NodeError, rd.NodeError)


# --------------------------------------------------------------------------- #
# Pure helper (covered by --selftest)
# --------------------------------------------------------------------------- #
def window_range(end, length, floor=1):
    """Trailing window of `length` blocks ending at `end`, clamped to `floor`.

    Returns (start, total, clamped).
    """
    start = end - length + 1
    clamped = start < floor
    if clamped:
        start = floor
    return start, end - start + 1, clamped


def fetch_previous_activation(tzkt_base):
    """(level, hash) of the most recent protocol activation (its first level)."""
    rows = rd.http_get_json(
        tzkt_base + "/v1/protocols?sort.desc=firstLevel&limit=1"
        "&select=firstLevel,hash",
        rd.TzktError,
    )
    if not rows:
        raise rd.TzktError("no protocols returned by tzkt")
    try:
        return int(rows[0]["firstLevel"]), rows[0].get("hash")
    except (KeyError, ValueError, TypeError) as exc:
        raise rd.TzktError(f"unexpected /v1/protocols payload: {exc}") from exc


# --------------------------------------------------------------------------- #
# Reporting
# --------------------------------------------------------------------------- #
def report(chain_id, tzkt_base, voting, candidate, activation, head, best,
           measurements):
    print("Activation estimate with empirical round drift")
    print("=" * 72)
    net = chain_id if chain_id else "(via --tzkt-base)"
    print(f"Network  : {net}  |  tzkt: {tzkt_base}")
    print(f"Period   : {voting['kind']} (index {voting['index']})")
    print(f"Candidate: {candidate}")
    print("  NOTE: best/empirical all assume the candidate passes all votes.")
    print(f"Head     : level {head['level']} at {head['timestamp'].isoformat()}")
    print(f"Blocks until activation : {activation['blocks']}")
    print(f"  Last Adoption block level    : {activation['last_adoption_level']}")
    print(f"  New protocol first block lvl : {activation['new_protocol_level']}")

    for m in measurements:
        print()
        clamp_note = "  (clamped to earliest available)" if m["clamped"] else ""
        print(f"Window @ {m['label']}")
        print(f"  levels {m['start']}..{m['end']}  ({m['total']} blocks){clamp_note}")
        print(f"  {'round':>5}  {'count':>10}  {'percent':>10}")
        print("  " + "-" * 29)
        for rnd in sorted(m["counts"]):
            n = m["counts"][rnd]
            pct = rd.fmt_sig(100.0 * n / m["total"]) if m["total"] else "0"
            print(f"  {rnd:>5}  {n:>10}  {pct:>9}%")
        print(f"  Measured drift : --drift '{m['drift_text']}'")

    print()
    print("Scenarios (horizon = blocks-to-activation):")
    print(f"  {'scenario':<22} {'avg block':>10} {'days':>7}   activation (UTC)")
    rows = [("best case", *best)]
    rows += [(f"empirical ({m['short']})", m["avg"], m["eta"], m["days"])
             for m in measurements]
    for label, avg, eta, days in rows:
        print(f"  {label:<22} {avg:>9.3f}s {days:>7.2f}   "
              f"{eta.strftime('%Y-%m-%d %H:%M:%S UTC')}")


# --------------------------------------------------------------------------- #
# Self-test
# --------------------------------------------------------------------------- #
def selftest():
    # window_range: normal and clamped.
    assert window_range(100, 10) == (91, 10, False)
    assert window_range(5, 10) == (1, 5, True)
    assert window_range(100, 1) == (100, 1, False)
    # Reproducibility: the rounded list parses back to the fractions used.
    counts = {0: 199669, 1: 2909, 2: 101, 3: 3, 4: 1}
    total = 202683
    text = rd.build_drift_list(counts, total)
    pcts = npa.parse_drift_list(text)
    assert set(pcts) == {1, 2, 3, 4}, pcts
    # Re-parsing the same text is stable (display == ETA inputs).
    assert npa.parse_drift_list(text) == pcts
    # Round-0-only window -> empty drift -> best case.
    assert npa.parse_drift_list(rd.build_drift_list({0: 100}, 100)) == {}
    print("selftest: OK")
    return EXIT_OK


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #
def build_parser():
    p = argparse.ArgumentParser(
        description="Estimate the next protocol activation using the round "
        "distribution measured over the last N blocks (N = blocks-to-activation).",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("--endpoint", default=DEFAULT_ENDPOINT,
                   help="Node RPC: voting/activation data + network detection")
    p.add_argument("--tzkt-base", default=None,
                   help="tzkt API base for round data; overrides auto-detection")
    p.add_argument("--window-blocks", type=int, default=None,
                   help="Trailing window length in blocks "
                        "(default: blocks-to-activation)")
    p.add_argument("--anchor", choices=["both", "head", "previous-activation"],
                   default="both",
                   help="Which measurement window(s) to report: both the current "
                        "head and the previous protocol activation block (default), "
                        "or just one. Each window has length N and ends at the "
                        "chosen anchor")
    p.add_argument("--selftest", action="store_true",
                   help="Run inline checks on the new logic and exit")
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)

    if args.selftest:
        return selftest()

    if args.window_blocks is not None and args.window_blocks < 1:
        print(f"error: --window-blocks must be >= 1, got {args.window_blocks}",
              file=sys.stderr)
        return EXIT_BAD_ARGS

    try:
        constants = npa.load_constants(args.endpoint)
        head = npa.load_head(args.endpoint)
        voting = npa.load_voting_state(args.endpoint)
    except NODE_ERRORS as exc:
        print(f"node error: {exc}", file=sys.stderr)
        return EXIT_DATA_ERROR

    candidate = npa.resolve_candidate(voting)
    if not candidate:
        print(f"No protocol is currently on track to activate "
              f"(period: {voting['kind']}, no current proposal).")
        return EXIT_NO_CANDIDATE

    activation = npa.blocks_to_activation(
        voting, head["level"], constants["blocks_per_voting_period"]
    )
    horizon = activation["blocks"]
    length = args.window_blocks if args.window_blocks is not None else horizon

    try:
        tzkt_base, chain_id = rd.resolve_tzkt_base(args.endpoint, args.tzkt_base)
    except NODE_ERRORS as exc:
        print(f"node error: {exc}", file=sys.stderr)
        return EXIT_DATA_ERROR

    mbd = constants["minimal_block_delay"]
    incr = constants["delay_increment_per_round"]

    # Each window has length N and ends at the chosen anchor (head and/or the
    # previous activation block). The ETA always projects from now over the
    # horizon N -- only the drift-source window moves.
    wanted = (["head", "previous-activation"] if args.anchor == "both"
              else [args.anchor])
    measurements = []
    try:
        for mode in wanted:
            if mode == "previous-activation":
                end, proto_hash = fetch_previous_activation(tzkt_base)
                label = f"previous activation ({proto_hash}, level {end})"
                short = "prev-act"
            else:
                end = head["level"]
                label = f"current head (level {end})"
                short = "head"
            start, total, clamped = window_range(end, length)
            counts = rd.tally_rounds(tzkt_base, start, end, total)
            # Build the drift list (rounded) and parse it back, so the displayed
            # list and the empirical ETA use identical numbers (reproducibility).
            drift_text = rd.build_drift_list(counts, total)
            round_pcts = npa.parse_drift_list(drift_text)
            avg = npa.avg_block_time(round_pcts, mbd, incr)
            eta, days = npa.scenario_eta(head["timestamp"], horizon, avg)
            measurements.append({
                "short": short, "label": label, "start": start, "end": end,
                "total": total, "clamped": clamped, "counts": counts,
                "drift_text": drift_text, "avg": avg, "eta": eta, "days": days,
            })
    except rd.TzktError as exc:
        print(f"tzkt error: {exc}", file=sys.stderr)
        return EXIT_DATA_ERROR

    best_avg = npa.avg_block_time({}, mbd, incr)
    best = (best_avg, *npa.scenario_eta(head["timestamp"], horizon, best_avg))

    report(chain_id, tzkt_base, voting, candidate, activation, head, best,
           measurements)
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
