These are the scripts used to run on a regular basis all the benchmarks for the Tezos protocol on the latest commit of the master branch. For motivation, see https://gitlab.com/tezos/tezos/-/milestones/143#tab-issues.

## Setup

- Benchmarks are run on the reference machine (163.172.52.82).
- We use Cron to run the benchmarks twice a week, at 8pm on Tuesdays and Fridays.
- The benchmarks Cron process is setup for a dedicated user named `mclaren`, whose password is not set. Any sudoer can modify this process by switching to `mclaren` (for instance with `sudo su mclaren`).

The actual script being run by Cron can be displayed with `crontab -l` from `mclaren`, and edited with `crontab -e`. Right now, it is:
```
* 20 * * fri bash /data/tezos-benchmarks/cronjob.sh
* 20 * * tue bash /data/tezos-benchmarks/cronjob.sh
```

## Directory structure

On the reference machine, the benchmarks directory should look like this:
```
/data/tezos-benchmarks/
  - cronjob.sh
  - run_all_benchmarks_on_latest_master.sh
  - rustup-init.sh
  - zcash_params
  - snoop_results/
      - _snoop_<DATE>_<COMMIT>/
          - benchmark_results/
          - inference_results/
          - cron_res
          - cron_res_errors
  - tezos/
      - _snoop/
          - michelson_data/
          - sapling_data/
          - benchmark_results/
          - inference_results/
      - everything else we have in the Octez git repo
  - cron_res
  - cron_res_errors
```

- `cronjob.sh` is the main script run by Cron. Its sources are in this repository and needs to be copied to the reference machine whenever it is updated.
- `run_all_benchmarks_on_latest_master.sh` is the script actually launching the benchmarks and called by `cronjob.sh`. Its sources are in this repository and needs to be copied to the reference machine whenever it is updated.
- `rustup-init.sh` and `zcash_params` handle some external dependencies. How to maintain these files is under discussion.
- `snoop_results` contains the result of the benchmarks, with one sub-directory per benchmarks run.
- `tezos` is a clone of https://gitlab.com/tezos/tezos. It is updated automatically in `run_all_benchmarks_on_latest_master.sh`, but it needs to be created prior to the very first Cron job.
  - `michelson_data` and `sapling_data` contain data that are very long to generate but rarely change between two benchmarks runs. For now, we decided not to regenerate them, and keep the same data from one run to the other. When to update them is under discussion.
  - Also note that in this directory, `benchmark_results` and `inference_results` only exist during a run of the benchmarks. They are moved to `snoop_results` at the end.
- `cron_res` and `cron_res_errors` are the log files of a benchmarks run. They are only present during a run, and moved to `snoop_results` at the end.

`cronjob.sh` and `run_all_benchmarks_on_latest_master.sh` are two different scripts because we want to create log files early, but only know their final destination after fetching the most recent `master` commit.
