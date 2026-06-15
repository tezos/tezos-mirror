#!/usr/bin/env python3
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
"""Measure the per-round block distribution over a recent window, from tzkt.

For a window of blocks (default: the last two weeks in block time, up to the
current head) the tool counts how often blocks were produced at each consensus
round and prints:

  * a human-readable summary (per-round counts and percentages), and
  * a ready-to-use list for `scripts/next_protocol_activation.py --drift`,
    e.g. `[1 : 1.44, 2 : 0.0498, 3 : 0.00148]`.

Round data comes from the tzkt indexer (field `blockRound`). Because blocks are
contiguous (one per level), the total in a level range is the level span, so
only the rare higher-round blocks are enumerated; round 0 is the remainder.

The tzkt network is auto-detected from a node's chain id (--endpoint), or set
explicitly with --tzkt-base. The tool is read-only.
"""

import argparse
import json
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone

EXIT_OK = 0
EXIT_BAD_ARGS = 2
EXIT_DATA_ERROR = 3

DEFAULT_ENDPOINT = "http://localhost:8732"
DEFAULT_DAYS = 14.0
HTTP_TIMEOUT_SECONDS = 30
TZKT_PAGE_LIMIT = 10000

# Chain id -> tzkt API base (mirrors scripts/baker_round_versions.py).
TZKT_BASE_BY_CHAIN = {
    "NetXdQprcVkpaWU": "https://api.tzkt.io",  # Mainnet
    "NetXnHfVqm9iesp": "https://api.ghostnet.tzkt.io",  # Ghostnet
}


class NodeError(Exception):
    """Fatal: the node cannot be reached or returned unusable data."""


class TzktError(Exception):
    """Fatal: the tzkt indexer cannot be reached or returned unusable data."""


# --------------------------------------------------------------------------- #
# HTTP (read-only)
# --------------------------------------------------------------------------- #
def http_get_json(url, error_cls):
    """GET a JSON document. Raise error_cls on any failure."""
    try:
        with urllib.request.urlopen(url, timeout=HTTP_TIMEOUT_SECONDS) as resp:
            if resp.status != 200:
                raise error_cls(f"{url}: HTTP {resp.status}")
            raw = resp.read()
    except urllib.error.HTTPError as exc:
        raise error_cls(f"{url}: HTTP {exc.code}") from exc
    except (urllib.error.URLError, OSError) as exc:
        raise error_cls(f"cannot reach {url}: {exc}") from exc
    try:
        return json.loads(raw)
    except json.JSONDecodeError as exc:
        raise error_cls(f"{url}: invalid JSON response ({exc})") from exc


# --------------------------------------------------------------------------- #
# Pure helpers (covered by --selftest)
# --------------------------------------------------------------------------- #
def tzkt_base_for_chain(chain_id, override=None):
    """Resolve the tzkt base URL: explicit override wins, else map the chain id.

    Returns None for an unknown chain with no override.
    """
    if override:
        return override.rstrip("/")
    return TZKT_BASE_BY_CHAIN.get(chain_id)


def fmt_sig(x, sig=3):
    """Format a percentage to ~`sig` significant figures.

    Never returns "0" for a strictly positive value (an observed round must
    stay visible). Trailing zeros and any trailing dot are trimmed.
    """
    if x == 0:
        return "0"
    formatted = f"{x:.{sig}g}"
    # %g can still emit "0" for extremely small magnitudes; widen precision.
    if float(formatted) == 0.0:
        formatted = repr(x)
    return formatted


def build_drift_list(counts, total):
    """Build the `[r : pct, ...]` text for rounds >= 1 (ascending); `[]` if none."""
    rounds = sorted(r for r in counts if r >= 1 and counts[r] > 0)
    if not rounds or total <= 0:
        return "[]"
    parts = [f"{r} : {fmt_sig(100.0 * counts[r] / total)}" for r in rounds]
    return "[" + ", ".join(parts) + "]"


def parse_timestamp(ts):
    """Parse an RFC3339 timestamp as tz-aware UTC."""
    return datetime.fromisoformat(ts.replace("Z", "+00:00"))


# --------------------------------------------------------------------------- #
# Network resolution
# --------------------------------------------------------------------------- #
def resolve_tzkt_base(endpoint, override):
    """Pick the tzkt base: --tzkt-base override, else node chain id -> mapping."""
    if override:
        return override.rstrip("/"), None
    try:
        chain_id = http_get_json(
            endpoint.rstrip("/") + "/chains/main/chain_id", NodeError
        )
    except NodeError as exc:
        raise NodeError(
            f"could not detect network from node ({exc}); pass --tzkt-base"
        ) from exc
    base = tzkt_base_for_chain(chain_id)
    if base is None:
        raise NodeError(
            f"no known tzkt base for chain {chain_id!r}; pass --tzkt-base"
        )
    return base, chain_id


# --------------------------------------------------------------------------- #
# Window resolution
# --------------------------------------------------------------------------- #
def tzkt_head(tzkt_base):
    head = http_get_json(tzkt_base + "/v1/head", TzktError)
    try:
        return int(head["level"]), parse_timestamp(head["timestamp"])
    except (KeyError, ValueError, TypeError) as exc:
        raise TzktError(f"unexpected /v1/head payload: {exc}") from exc


def first_level_at_or_after(tzkt_base, ts):
    """First block level with timestamp >= ts (tz-aware UTC)."""
    iso = ts.astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    url = (f"{tzkt_base}/v1/blocks?timestamp.ge={iso}"
           "&limit=1&sort.asc=level&select=level")
    rows = http_get_json(url, TzktError)
    if not rows:
        raise TzktError(f"no block found at or after {iso}")
    return int(rows[0])


def resolve_window(tzkt_base, days, start_level, end_level):
    """Resolve (start_level, end_level, start_time, end_time, total_blocks)."""
    head_level, head_time = tzkt_head(tzkt_base)
    end = end_level if end_level is not None else head_level
    if start_level is not None:
        start = start_level
    else:
        start = first_level_at_or_after(tzkt_base, head_time - timedelta(days=days))
    if start > end:
        raise ValueError(
            f"empty window: start level {start} is after end level {end}"
        )
    start_time = level_time(tzkt_base, start)
    end_time = level_time(tzkt_base, end)
    return start, end, start_time, end_time, end - start + 1


def level_time(tzkt_base, level):
    rows = http_get_json(
        f"{tzkt_base}/v1/blocks?level={level}&select=timestamp", TzktError
    )
    if not rows:
        raise TzktError(f"no block at level {level}")
    return parse_timestamp(rows[0])


# --------------------------------------------------------------------------- #
# Round tally
# --------------------------------------------------------------------------- #
def tally_rounds(tzkt_base, start, end, total):
    """Count blocks per round in [start, end]; round 0 = total - higher rounds.

    Only blocks with blockRound > 0 are enumerated (paged with a level cursor).
    """
    counts = {}
    cursor = start - 1
    while True:
        url = (f"{tzkt_base}/v1/blocks?level.gt={cursor}&level.le={end}"
               f"&blockRound.gt=0&select=level,blockRound"
               f"&sort.asc=level&limit={TZKT_PAGE_LIMIT}")
        rows = http_get_json(url, TzktError)
        if not rows:
            break
        for row in rows:
            try:
                rnd = int(row["blockRound"])
            except (KeyError, ValueError, TypeError) as exc:
                raise TzktError(f"unexpected block payload: {exc}") from exc
            counts[rnd] = counts.get(rnd, 0) + 1
            cursor = int(row["level"])
        if len(rows) < TZKT_PAGE_LIMIT:
            break
    higher = sum(counts.values())
    counts[0] = total - higher
    return counts


# --------------------------------------------------------------------------- #
# Reporting
# --------------------------------------------------------------------------- #
def report(chain_id, tzkt_base, start, end, start_time, end_time, total, counts):
    print("Round distribution")
    print("=" * 60)
    net = chain_id if chain_id else "(via --tzkt-base)"
    print(f"Network : {net}  |  tzkt: {tzkt_base}")
    print(f"Window  : levels {start}..{end}  ({total} blocks)")
    print(f"          {start_time.isoformat()}  ->  {end_time.isoformat()}")
    print()
    print(f"  {'round':>5}  {'count':>10}  {'percent':>10}")
    print("  " + "-" * 29)
    for rnd in sorted(counts):
        n = counts[rnd]
        pct = fmt_sig(100.0 * n / total) if total else "0"
        print(f"  {rnd:>5}  {n:>10}  {pct:>9}%")
    print()
    print(f"--drift '{build_drift_list(counts, total)}'")


# --------------------------------------------------------------------------- #
# Self-test
# --------------------------------------------------------------------------- #
def selftest():
    assert tzkt_base_for_chain("NetXdQprcVkpaWU") == "https://api.tzkt.io"
    assert tzkt_base_for_chain("NetXnHfVqm9iesp") == "https://api.ghostnet.tzkt.io"
    assert tzkt_base_for_chain("NetXunknown") is None
    assert tzkt_base_for_chain("NetXunknown", "https://x/") == "https://x"

    assert fmt_sig(1.435) == "1.44"
    assert fmt_sig(0.0) == "0"
    # A tiny but positive value must not collapse to "0".
    assert float(fmt_sig(0.0004934)) > 0
    assert fmt_sig(0.0004934) == "0.000493"

    # Worked example over a real ~2-week window.
    counts = {0: 199669, 1: 2909, 2: 101, 3: 3, 4: 1}
    total = 202683
    assert build_drift_list(counts, total) == \
        "[1 : 1.44, 2 : 0.0498, 3 : 0.00148, 4 : 0.000493]", \
        build_drift_list(counts, total)
    # No higher rounds -> empty list.
    assert build_drift_list({0: 100}, 100) == "[]"
    print("selftest: OK")
    return EXIT_OK


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #
def build_parser():
    p = argparse.ArgumentParser(
        description="Measure the per-round block distribution over a window "
        "(from tzkt) and emit a --drift list for next_protocol_activation.py.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("--days", type=float, default=DEFAULT_DAYS,
                   help="Window length in days (block time), ending at head; "
                        "ignored if --start-level is given")
    p.add_argument("--start-level", type=int, default=None,
                   help="Explicit window start level (overrides --days)")
    p.add_argument("--end-level", type=int, default=None,
                   help="Explicit window end level (default: head)")
    p.add_argument("--endpoint", default=DEFAULT_ENDPOINT,
                   help="Node RPC, used only to detect the network (chain id)")
    p.add_argument("--tzkt-base", default=None,
                   help="tzkt API base; overrides network auto-detection")
    p.add_argument("--list-only", action="store_true",
                   help="Print only the bare drift list (for --drift substitution)")
    p.add_argument("--selftest", action="store_true",
                   help="Run inline checks on the pure helpers and exit")
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)

    if args.selftest:
        return selftest()

    if args.days <= 0:
        print(f"error: --days must be > 0, got {args.days}", file=sys.stderr)
        return EXIT_BAD_ARGS
    for name, val in (("--start-level", args.start_level),
                      ("--end-level", args.end_level)):
        if val is not None and val < 0:
            print(f"error: {name} must be >= 0, got {val}", file=sys.stderr)
            return EXIT_BAD_ARGS

    try:
        tzkt_base, chain_id = resolve_tzkt_base(args.endpoint, args.tzkt_base)
    except NodeError as exc:
        print(f"node error: {exc}", file=sys.stderr)
        return EXIT_DATA_ERROR

    try:
        start, end, start_time, end_time, total = resolve_window(
            tzkt_base, args.days, args.start_level, args.end_level
        )
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return EXIT_BAD_ARGS
    except TzktError as exc:
        print(f"tzkt error: {exc}", file=sys.stderr)
        return EXIT_DATA_ERROR

    try:
        counts = tally_rounds(tzkt_base, start, end, total)
    except TzktError as exc:
        print(f"tzkt error: {exc}", file=sys.stderr)
        return EXIT_DATA_ERROR

    if args.list_only:
        print(build_drift_list(counts, total))
    else:
        report(chain_id, tzkt_base, start, end, start_time, end_time, total, counts)
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
