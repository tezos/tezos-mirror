#!/bin/bash

# Usage: ./verify_dumps.sh [dumps_directory] [report_directory]
#
# Read-only integrity and content check of the monthly dump .bz2 files
# produced by dump_month.sh. One decompression pass per file extracts:
#   - bzip2 stream validity (truncation/corruption)
#   - presence of the pg_dump end-of-dump trailer
#   - per-table INSERT counts
# then cross-checks each month's .dump / .dump_tmp_tables pair and flags
# .dump files still containing temp_ table names (failed rename).
#
# Safe to run while an export is in progress: files modified less than
# an hour ago are skipped. Run it from local disk, not the dumps mount.

set -u

DIR="${1:-/mnt/s3bucket/mainnet/monthly}"
OUT="${2:-$HOME/dump_check}"
mkdir -p "$OUT"
REPORT="$OUT/report-$(date +%Y%m%d-%H%M).tsv"

if type -P pbzip2 > /dev/null 2>&1; then
  DCMP="pbzip2 -dc"
else
  DCMP="bzcat"
fi

mtime() {
  stat -c%Y "$1" 2> /dev/null || stat -f%m "$1"
}

printf 'file\tsize_bytes\tbzip_ok\ttrailer\ttables\ttotal_inserts\tper_table\n' > "$REPORT"

for f in "$DIR"/*.bz2; do
  [ -e "$f" ] || continue
  base=$(basename "$f")
  size=$(wc -c < "$f")
  age=$(($(date +%s) - $(mtime "$f")))
  if [ "$age" -lt 3600 ]; then
    printf '%s\t%s\tSKIPPED\trecent(<1h)\t-\t-\t-\n' "$base" "$size" | tee -a "$REPORT"
    continue
  fi
  echo ">>> $base ($size bytes) - $(date)" >&2

  $DCMP "$f" 2> "$OUT/.stderr" | LC_ALL=C awk '
    index($0, "INSERT INTO") == 1 {
      t = $3; sub(/^[^.]*\./, "", t); gsub(/"/, "", t)
      ins[t]++; total++
    }
    index($0, "-- PostgreSQL database dump complete") == 1 { trailer = "YES" }
    END {
      n = 0; pt = ""
      for (t in ins) { n++; pt = pt t "=" ins[t] ";" }
      if (pt == "") pt = "-"
      print (trailer ? trailer : "NO") "\t" n "\t" total + 0 "\t" pt
    }' > "$OUT/.awkout"
  bz=${PIPESTATUS[0]}

  IFS=$'\t' read -r trailer ntables total per < "$OUT/.awkout"
  if [ "$bz" -eq 0 ]; then
    bz_ok="OK"
  else
    bz_ok="CORRUPT(rc=$bz)"
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$base" "$size" "$bz_ok" "$trailer" "$ntables" "$total" "$per" | tee -a "$REPORT"
done

echo
echo "================ SUMMARY ================"

echo "-- bzip2 corruption / missing trailer:"
awk -F'\t' 'NR>1 && ($3 ~ /CORRUPT/ || $4 == "NO") {print "  " $1 " : bzip=" $3 " trailer=" $4}' "$REPORT"

echo "-- .dump.bz2 still containing temp_ table names (rename failed):"
awk -F'\t' 'NR>1 && $1 ~ /\.dump\.bz2$/ && $7 ~ /temp_monthly_view_/ {print "  " $1}' "$REPORT"

echo "-- pair consistency (total inserts: .dump vs .dump_tmp_tables):"
awk -F'\t' '
  NR>1 && $1 ~ /\.dump\.bz2$/ { m = $1; sub(/\.dump\.bz2$/, "", m); d[m] = $6 }
  NR>1 && $1 ~ /\.dump_tmp_tables\.bz2$/ { m = $1; sub(/\.dump_tmp_tables\.bz2$/, "", m); t[m] = $6 }
  END {
    for (m in d)
      if (m in t) printf "  %s : dump=%s tmp=%s %s\n", m, d[m], t[m], (d[m] == t[m] ? "OK" : "** MISMATCH **")
    for (m in d)
      if (!(m in t)) printf "  %s : tmp_tables variant MISSING\n", m
    for (m in t)
      if (!(m in d)) printf "  %s : dump variant MISSING\n", m
  }' "$REPORT" | sort

echo
echo "Full report: $REPORT"
