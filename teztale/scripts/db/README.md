# DB scripts

This directory contains small shell scripts for extracting month-based data from the project database and verifying the resulting dumps.

Requirements: `jq`, `psql`, `pg_dump`, `bzip2` (`pbzip2` is used instead when installed — recommended, compression is parallelized).

## list_months.sh
- Purpose: list the months for which data exists (one month per line, e.g. `2023-06`).
- Invocation: `./list_months.sh`
- Args: none (or a single optional DB connection argument if the script supports it).
- Output: prints available months to stdout for piping or scripting.
- Notes: make executable with `chmod +x list_months.sh`. The script uses the repository's DB configuration or standard env vars (PGHOST/PGPORT/PGUSER/PGPASSWORD/PGDATABASE) if applicable.

Example:
```
./list_months.sh         # prints months like 2023-05, 2023-06
./list_months.sh | less
```

Output from list_months can be used to feed dump_month.sh, cf. examples below.

## dump_month.sh
- Purpose: dump the data for a specific month.
- Invocation: `./dump_month.sh <YYYY-MM> <output-dir>`
- Args:
    - `<YYYY-MM>` — required month identifier.
    - `<output-dir>` — directory where the final `.bz2` artifacts are published.
- Output: three compressed files per month in `<output-dir>`:
    - `teztale_<db>_dump_<YYYY-MM>.dump.bz2` — restore-ready data dump (original table names),
    - `teztale_<db>_dump_<YYYY-MM>.dump_tmp_tables.bz2` — same data with the `temp_monthly_view_` table names,
    - `teztale_<db>_dump_<YYYY-MM>.schema.bz2` — full schema dump.
- Environment variables:
    - `TEZTALE_CONFIG` — teztale config file holding the DB connection string (default `/nomadic/conf/teztale-config.json`),
    - `LOCAL_DIR` — local staging directory (default `~/teztale_exports/staging`),
    - `FORCE=1` — re-dump a month whose three outputs already exist (skipped otherwise),
    - `ALLOW_UNCOVERED=1` — proceed even if the DB contains tables not covered by the monthly views.

The dump is streamed through the compressor: the uncompressed SQL (tens of GB for recent months) never lands on any disk. Artifacts are staged in `LOCAL_DIR` and only the final `.bz2` files are copied (with size verification) to `<output-dir>`. This matters when `<output-dir>` is a FUSE-mounted object store (s3fs): writing large files there directly stages them in the local cache and fails with ENOSPC once the backing disk is full. Keep `LOCAL_DIR`, the working directory and any log files on local disk, not on the mount.

The script aborts up front if the DB contains tables not covered by the monthly views, so newly added tables cannot be silently missing from the dumps; add a view (and temp table) for the new table, following the `dal_shard_assignments` example.

Examples:
```
./dump_month.sh 2023-06 target-directory
```

## verify_dumps.sh
- Purpose: read-only integrity and content check of already-produced dump files.
- Invocation: `./verify_dumps.sh [dumps-dir] [report-dir]` (defaults: `/mnt/s3bucket/mainnet/monthly`, `~/dump_check`)
- Checks, in a single decompression pass per file:
    - bzip2 stream validity (truncation/corruption),
    - presence of the pg_dump end-of-dump trailer,
    - per-table INSERT counts,
  then cross-checks each month's `.dump` / `.dump_tmp_tables` pair and flags `.dump` files still containing `temp_` table names.
- Notes: decompresses every file entirely — expect hours for large dump sets; run it under `tmux`/`nohup` from local disk. Files modified less than an hour ago are skipped, so it is safe to run while an export is in progress. A `tables` count lower than expected is not necessarily an error: a month with no rows for a table (e.g. `missing_blocks`) produces no INSERT lines for it.

# Examples:

customize the config file before calling dump_month.sh for 2024-11, to be stored on a specific directory
```
time TEZTALE_CONFIG=/<foobarnet>/config.json ./dump_month.sh 2024-11 /mnt/s3bucket/<foobarnet>/
```

```
# define the TEZTALE_CONFIG file location
export TEZTALE_CONFIG=/<foobarnet>/config.json

# create a list of the existing months to be exported
./list_months.sh > list_to_export.txt

## preliminary test (echo the dump commands)
time for i in $(cat list_to_export.txt) ; do echo "######## $i - $(date) ########"; echo time ./dump_month.sh $i /mnt/s3bucket/<foobarnet>/; date ; done

## run the dump commands (run from a local directory: the log must not land on the s3fs mount)
time for i in $(cat list_to_export.txt) ; do echo "######## $i - $(date) ########"; time ./dump_month.sh $i /mnt/s3bucket/<foobarnet>/; date ; done 2>&1 | tee export-$(date +%Y%m%d-%H%M).log
```
