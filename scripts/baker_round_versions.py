#!/usr/bin/env python3
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
"""List the bakers assigned to rounds [0; n] of a block, with their Octez version.

For a given block (default: head) and a maximum round n, the tool asks a running
Octez node which baker holds the baking right at each round 0..n, then annotates
each baker with its name and the Octez version it currently runs, as reported by
the tzkt indexer (auto-selected to match the node's network).

The tzkt lookup is best-effort: if tzkt is unreachable, a baker is unknown
there, or it is disabled, the round->baker listing still prints and the exit
code is unaffected.

Name and version are keyed on the delegate (baker account), not its consensus
key.
"""

import argparse
import json
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone

# Exit codes.
EXIT_OK = 0
EXIT_NO_RIGHTS = 1
EXIT_BAD_ARGS = 2
EXIT_NODE_ERROR = 3

DEFAULT_ENDPOINT = "http://localhost:8732"
HTTP_TIMEOUT_SECONDS = 15

# Chain id -> tzkt API base. Extend as needed; unknown chains require --tzkt-base.
TZKT_BASE_BY_CHAIN = {
    "NetXdQprcVkpaWU": "https://api.tzkt.io",  # Mainnet
    "NetXnHfVqm9iesp": "https://api.ghostnet.tzkt.io",  # Ghostnet
}


class NodeError(Exception):
    """Fatal: the node cannot be reached or returned unusable data."""


class TzktError(Exception):
    """Non-fatal: the version source failed; versions become unavailable."""


class NoRightsError(Exception):
    """The requested level has no computable baking rights (beyond the horizon)."""


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


def node_get(endpoint, path):
    return http_get_json(endpoint.rstrip("/") + path, NodeError)


def detect_horizon_error(body):
    """If an error body signals a level beyond the rights horizon, describe it.

    The node answers HTTP 500 with a Tezos error list (e.g. `...seed.unknown_seed`
    with `requested`/`latest` cycles) when the level is too far ahead. Return a
    human message in that case, else None.
    """
    try:
        errors = json.loads(body)
    except (json.JSONDecodeError, TypeError):
        return None
    if not isinstance(errors, list):
        return None
    for err in errors:
        if not isinstance(err, dict):
            continue
        err_id = err.get("id", "")
        if "unknown_seed" in err_id or "seed" in err_id:
            req, latest = err.get("requested"), err.get("latest")
            if req is not None and latest is not None:
                return (f"level beyond the rights horizon "
                        f"(requested cycle {req}, latest available {latest})")
            return "level beyond the rights horizon"
    return None


# --------------------------------------------------------------------------- #
# Pure helpers (covered by --selftest)
# --------------------------------------------------------------------------- #
def classify_block_ref(raw):
    """Classify a block reference into ('head'|'hash'|'level', level_or_None).

    An all-digits reference is a level (must be >= 0); anything else is treated
    as 'head' or a block hash and used verbatim in the RPC path.
    """
    if raw is None or raw == "head":
        return ("head", None)
    if raw.isdigit():
        level = int(raw)
        return ("level", level)
    if raw.lstrip("-").isdigit():
        raise ValueError(f"block level must be non-negative, got {raw}")
    return ("hash", None)


def tzkt_base_for_chain(chain_id, override=None):
    """Resolve the tzkt base URL: explicit override wins, else map the chain id.

    Returns None for an unknown chain with no override (caller disables versions).
    """
    if override:
        return override.rstrip("/")
    return TZKT_BASE_BY_CHAIN.get(chain_id)


def unique_delegates(assignments):
    """First-seen-ordered de-duplication of delegate addresses (FR-007)."""
    seen = set()
    ordered = []
    for a in assignments:
        d = a["delegate"]
        if d not in seen:
            seen.add(d)
            ordered.append(d)
    return ordered


def parse_timestamp(ts):
    """Parse an RFC3339 timestamp as tz-aware UTC, or return None."""
    if not ts:
        return None
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00"))
    except (ValueError, AttributeError):
        return None


# --------------------------------------------------------------------------- #
# Node: baking rights
# --------------------------------------------------------------------------- #
def fetch_baking_rights(endpoint, block_ref, max_round):
    """Return RoundAssignment dicts for rounds 0..max_round of the target block.

    Integer block -> query head with ?level=L (works for past and near-future
    levels); 'head'/hash -> use it as the path block.
    """
    if max_round < 0:
        raise ValueError(f"max round must be >= 0, got {max_round}")
    kind, level = classify_block_ref(block_ref)
    query = f"max_round={max_round}"
    if kind == "level":
        path = f"/chains/main/blocks/head/helpers/baking_rights?level={level}&{query}"
        block_label = f"level {level}"
    else:
        block = "head" if kind == "head" else block_ref
        path = f"/chains/main/blocks/{block}/helpers/baking_rights?{query}"
        block_label = block

    url = endpoint.rstrip("/") + path
    try:
        with urllib.request.urlopen(url, timeout=HTTP_TIMEOUT_SECONDS) as resp:
            if resp.status != 200:
                raise NodeError(f"{url}: HTTP {resp.status}")
            raw = resp.read()
    except urllib.error.HTTPError as exc:
        horizon = detect_horizon_error(exc.read())
        if horizon:
            raise NoRightsError(f"{block_label}: {horizon}") from exc
        raise NodeError(f"{url}: HTTP {exc.code}") from exc
    except (urllib.error.URLError, OSError) as exc:
        raise NodeError(f"cannot reach {url}: {exc}") from exc
    try:
        rights = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise NodeError(f"{url}: invalid JSON response ({exc})") from exc
    assignments = []
    try:
        for entry in rights:
            assignments.append(
                {
                    "round": int(entry["round"]),
                    "delegate": entry["delegate"],
                    "consensus_key": entry.get("consensus_key", entry["delegate"]),
                    "estimated_time": parse_timestamp(entry.get("estimated_time")),
                }
            )
    except (KeyError, ValueError, TypeError) as exc:
        raise NodeError(f"unexpected baking_rights payload: {exc}") from exc
    assignments.sort(key=lambda a: a["round"])
    return assignments, block_label


# --------------------------------------------------------------------------- #
# Network resolution + tzkt versions
# --------------------------------------------------------------------------- #
def resolve_network(endpoint, tzkt_override, versions_requested):
    """Build the network config: chain id, tzkt base, whether versions are on."""
    chain_id = node_get(endpoint, "/chains/main/chain_id")
    if not isinstance(chain_id, str):
        raise NodeError(f"unexpected chain_id payload: {chain_id!r}")
    if not versions_requested:
        return {"chain_id": chain_id, "tzkt_base": None, "versions_enabled": False,
                "note": "version lookup disabled (--no-versions)"}
    base = tzkt_base_for_chain(chain_id, tzkt_override)
    if base is None:
        return {"chain_id": chain_id, "tzkt_base": None, "versions_enabled": False,
                "note": f"no known tzkt base for chain {chain_id}; pass --tzkt-base"}
    return {"chain_id": chain_id, "tzkt_base": base, "versions_enabled": True,
            "note": None}


def fetch_versions(tzkt_base, delegates):
    """Map each delegate -> BakerVersion dict via one batched tzkt request.

    Each record carries the baker's tzkt name (``alias``, or None), its Octez
    ``version`` and the date that version was observed. Any tzkt/network failure
    yields 'unavailable' for every delegate (non-fatal). Addresses absent from
    tzkt, or with null/empty software, yield 'unknown'.
    """
    if not delegates:
        return {}
    csv = ",".join(delegates)
    url = (
        f"{tzkt_base}/v1/accounts?address.in={urllib.parse.quote(csv)}"
        "&select=address,alias,software"
    )
    try:
        rows = http_get_json(url, TzktError)
    except TzktError as exc:
        return {d: {"status": "unavailable", "name": None, "version": None,
                    "observed": None, "error": str(exc)} for d in delegates}

    by_addr = {}
    for row in rows if isinstance(rows, list) else []:
        addr = row.get("address")
        name = row.get("alias") or None
        software = row.get("software") or {}
        version = software.get("version")
        if version:
            by_addr[addr] = {
                "status": "known",
                "name": name,
                "version": version,
                "observed": parse_timestamp(software.get("date")),
                "error": None,
            }
        else:
            by_addr[addr] = {"status": "unknown", "name": name, "version": None,
                             "observed": parse_timestamp(software.get("date")),
                             "error": None}
    # Delegates not returned by tzkt are unknown there.
    for d in delegates:
        by_addr.setdefault(
            d, {"status": "unknown", "name": None, "version": None,
                "observed": None, "error": None}
        )
    return by_addr


# --------------------------------------------------------------------------- #
# Reporting
# --------------------------------------------------------------------------- #
def version_cell(bv):
    if bv is None or bv["status"] == "unavailable":
        return "unavailable"
    if bv["status"] == "unknown":
        return "unknown"
    return bv["version"]


def name_cell(bv):
    """The baker's tzkt name, or '-' when none is known."""
    if bv and bv.get("name"):
        return bv["name"]
    return "-"


def report(block_label, network, assignments, versions):
    print(f"Bakers per round for block {block_label}")
    print("=" * 72)
    base = network["tzkt_base"] or "-"
    print(f"Network : chain {network['chain_id']}  |  tzkt: {base}")
    if network["note"]:
        print(f"  ({network['note']})")
    rounds = [a["round"] for a in assignments]
    print(f"Rounds  : {min(rounds)}..{max(rounds)}")
    print()
    header = (f"{'round':>5}  {'delegate':<37} {'baker':<20} {'version':<12} "
              f"observed / consensus key")
    print(header)
    print("-" * len(header))
    for a in assignments:
        bv = versions.get(a["delegate"]) if versions else None
        name = name_cell(bv)
        if len(name) > 20:
            name = name[:19] + "…"
        ver = version_cell(bv)
        observed = ""
        if bv and bv.get("observed"):
            observed = bv["observed"].strftime("%Y-%m-%d")
        extra = observed
        if a["consensus_key"] != a["delegate"]:
            ck = f"consensus={a['consensus_key']}"
            extra = f"{observed}  {ck}" if observed else ck
        print(f"{a['round']:>5}  {a['delegate']:<37} {name:<20} {ver:<12} {extra}")


# --------------------------------------------------------------------------- #
# Self-test
# --------------------------------------------------------------------------- #
def selftest():
    assert classify_block_ref("head") == ("head", None)
    assert classify_block_ref(None) == ("head", None)
    assert classify_block_ref("123") == ("level", 123)
    assert classify_block_ref("BMabc123") == ("hash", None)
    try:
        classify_block_ref("-4")
        raise AssertionError("expected ValueError for negative level")
    except ValueError:
        pass

    assert tzkt_base_for_chain("NetXdQprcVkpaWU") == "https://api.tzkt.io"
    assert tzkt_base_for_chain("NetXnHfVqm9iesp") == "https://api.ghostnet.tzkt.io"
    assert tzkt_base_for_chain("NetXunknown") is None
    assert tzkt_base_for_chain("NetXunknown", "https://x/") == "https://x"

    sample = [
        {"delegate": "tzA"}, {"delegate": "tzB"}, {"delegate": "tzA"},
        {"delegate": "tzC"}, {"delegate": "tzB"},
    ]
    assert unique_delegates(sample) == ["tzA", "tzB", "tzC"]
    print("selftest: OK")
    return EXIT_OK


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #
def build_parser():
    p = argparse.ArgumentParser(
        description="List bakers assigned to rounds [0; n] of a block, with Octez "
        "versions.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("--endpoint", default=DEFAULT_ENDPOINT,
                   help="Base RPC URL of the Octez node")
    p.add_argument("--block", default="head",
                   help="Block reference: head, a block hash, or a level (integer)")
    p.add_argument("-n", "--max-round", type=int, default=5,
                   help="Maximum round; lists rounds 0..N")
    p.add_argument("--tzkt-base", default=None,
                   help="tzkt API base; overrides network auto-detection")
    p.add_argument("--no-versions", action="store_true",
                   help="Node-only mode: list rounds/bakers without version lookup")
    p.add_argument("--selftest", action="store_true",
                   help="Run inline checks on the pure helpers and exit")
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)

    if args.selftest:
        return selftest()

    if args.max_round < 0:
        print(f"error: max round must be >= 0, got {args.max_round}", file=sys.stderr)
        return EXIT_BAD_ARGS
    try:
        classify_block_ref(args.block)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return EXIT_BAD_ARGS

    try:
        assignments, block_label = fetch_baking_rights(
            args.endpoint, args.block, args.max_round
        )
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return EXIT_BAD_ARGS
    except NoRightsError as exc:
        print(f"No baking rights for block {exc}")
        return EXIT_NO_RIGHTS
    except NodeError as exc:
        print(f"node error: {exc}", file=sys.stderr)
        return EXIT_NODE_ERROR

    if not assignments:
        print(f"No baking rights for block {block_label} "
              f"(beyond the rights horizon?).")
        return EXIT_NO_RIGHTS

    try:
        network = resolve_network(
            args.endpoint, args.tzkt_base, not args.no_versions
        )
    except NodeError as exc:
        print(f"node error: {exc}", file=sys.stderr)
        return EXIT_NODE_ERROR

    versions = {}
    if network["versions_enabled"]:
        versions = fetch_versions(
            network["tzkt_base"], unique_delegates(assignments)
        )

    report(block_label, network, assignments, versions)
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
