# DB scripts

This directory contains two small shell scripts for extracting month-based data from the project database.

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
- Purpose: dump the data for a specific month to stdout or a file.
- Invocation: `./dump_month.sh <YYYY-MM> [output-file]`
- Args:
    - `<YYYY-MM>` — required month identifier (format used by the project).
    - `[output-dir]` — path where to write the dump;
- Output: a consistent database dump for the given month.
- Notes: make executable with `chmod +x dump_month.sh`. The script uses the teztale configuration file to determine the DB connection string.

Examples:
```
./dump_month.sh 2023-06 target-directory
```

If you need the exact DB connection options or output format, open the top of each script — it documents the supported environment variables and flags.



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

## run the dump commands
time for i in $(cat list_to_export.txt) ; do echo "######## $i - $(date) ########"; time ./dump_month.sh $i /mnt/s3bucket/<foobarnet>/; date ; done
```
