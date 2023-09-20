These are the scripts used to run on a regular basis all the benchmarks for the Tezos protocol on the latest commit of the master branch.

## Setup

- Benchmarks are run on the reference machine (`163.172.52.82`).
- We use Cron to run the benchmarks twice a week, at 8pm CET on Tuesdays and Fridays.
- The benchmarks Cron process is setup for a dedicated user named `mclaren`, whose password is not set. Any sudoer can modify this process by switching to `mclaren` (for instance with `sudo su mclaren`).

The actual script being run by Cron can be displayed with `crontab -l` from `mclaren`, and edited with `crontab -e`. Right now, it is:
```
0 20 * * fri bash /data/tezos-benchmarks/cronjob.sh
0 20 * * tue bash /data/tezos-benchmarks/cronjob.sh
```

- Regressions are detected on another machine (`163.172.34.197`).
- We use Cron to run the detection every 10 minutes.
- The Cron detection process is setup for a dedicated user named `redbull`, whose password is not set. Any sudoer can modify this process by switching to `redbull` (for instance with `sudo su redbull`).
- The regression detection relies on the [`gas_parameter_diff`](https://gitlab.com/tezos/tezos/-/tree/master/devtools/gas_parameter_diff) tool.

The actual script being run by Cron can be displayed with `crontab -l` from `redbull`, and edited with `crontab -e`. Right now, it is:
```
*/10 * * * * cd /data/redbull/tezos && eval $(opam env) && /data/redbull/tezos/devtools/benchmarks-tools/watch_regressions.sh
```

## Script details

### Running all benchmarks on the reference machine

On `163.172.52.82`, `cronjob.sh` does the following:
* it checks that the `cron_res` file doesn't exist. Otherwise, this would mean that the previous run is in progress, or has failed and wasn't properly cleaned up;
* it runs `run_all_benchmarks_on_latest_master.sh`, redirecting outputs to `cron_res` and errors to `cron_res_errors`;
* when `run_all_benchmarks_on_latest_master.sh` completes, it moves `cron_res` and `cron_res_errors` to the appropriate directory, depending on whether it succeeded or failed;
* it reports its activity to a dedicated Slack channel.

Then, `run_all_benchmarks_on_latest_master.sh` operates as follows:
* it pulls the current branch of its local copy of [Tezos](https://gitlab.com/tezos/tezos) at `/data/tezos-benchmarks/tezos`;
* it creates the directory `/data/tezos-benchmarks/snoop_results/_snoop_<YYYYMMDD>_<HHMM>_<version-and-hash>` where Snoop result files will be stored;
* it `make`s, also building dependencies;
* it runs all benchmarks by calling `dune exec tezt/snoop/main.exe`;
* it moves Snoop result files to the directory created above;
* finally, it uploads Snoop result files to an AWS remote storage.

About the directory with Snoop result files: `run_all_benchmarks_on_latest_master.sh` creates a `current_run_dir` file that contains the name of the directory, and moves it to a `last_run_dir` file upon success. `cronjob.sh` uses these files to fetch the directory where to store `cron_res` and `cron_res_errors`.

### Sending alerts when regressions are found

On `163.172.34.197`, `watch_regressions.sh` does the following:
* it fetches the last Snoop result directory that it treated through the `last_known_dir` file;
* it fetches the most recent Snoop result directory on the AWS remote storage;
* it continues only if they are different and the one from AWS was successfull;
* it downloads the Snoop `CSV` result files from AWS for all successfull runs;
* it considers the oldest to be the first one, but this can be set differently by the user;
* it also considers the first one to be the reference run, but this can be set differently by the user as well;
* it runs `gas_parameter_diff` on all the `CSV` files *of the most recent run*, registering regression alerts, and creating comparison files:
  * with all runs from the first directory, including the reference run;
  * with the previous run;
  * with the reference run;
  * with all runs, but filtered on values with regression alerts.
  * Note that alerts are only raised when comparing the most recent run with the previous one and the reference one;
* it sends all these files to a dedicated Slack channel.

## Directory structure

On the reference machine, the benchmarks directory looks like this:
```
/data/tezos-benchmarks/
  - cronjob.sh
  - run_all_benchmarks_on_latest_master.sh
  - rustup-init.sh
  - slack_token
  - snoop_results/
      - _snoop_<DATE>_<COMMIT>/
          - benchmark_results/
          - inference_results/
          - cron_res
          - cron_res_errors
          - STARTED
          - SUCCESS
  - tezos/
      - _snoop/
          - michelson_data/
          - sapling_data/
          - benchmark_results/
          - inference_results/
      - everything else we have in the Octez git repo
  - cron_res
  - cron_res_errors
  - current_run_dir
  - last_run_dir
  - anomalies
```

- `cronjob.sh` is the main script run by Cron. Its sources are in this repository and it needs to be copied to the reference machine whenever it is updated.
- `run_all_benchmarks_on_latest_master.sh` is the script actually launching the benchmarks and called by `cronjob.sh`. Its sources are in this repository and it needs to be copied to the reference machine whenever it is updated.
- `rustup-init.sh` handles some external dependencies. How to maintain this file is under discussion.
- `slack_token` contains an authorization token allowing to send messages to Slack through a specific Slack application (`gas-benchmarks-reports`). Anyone possessing the token can send messages with the application, so the file has restricted access rights. For now, it is used to report the status of the benchmarks run.
- `snoop_results` contains the result of the benchmarks, with one sub-directory per benchmark run.
  - `STARTED` and `SUCCESS` are marker files that can be used by human beings to have a quick look at the benchmarks process status (they contain the PID).
  - Other files are created by being moved from other locations, as described below.
- `tezos` is a clone of https://gitlab.com/tezos/tezos. It is updated automatically in `run_all_benchmarks_on_latest_master.sh`, but it needs to be created prior to the very first Cron job.
  - `michelson_data` and `sapling_data` contain data that are very long to generate but rarely change between two benchmark runs. For now, we decided not to regenerate them, and keep the same data from one run to the other. When to update them is under discussion.
  - Also note that in this directory, `benchmark_results` and `inference_results` only exist during a run of the benchmarks. They are moved to `snoop_results` at the end.
- `cron_res` and `cron_res_errors` are the log files of a benchmark run. They are only present during a run, and moved to `snoop_results` at the end.
- `current_run_dir` and `last_run_dir` are marker files each containing the name of a benchmarks results directory:
  - `current_run_dir` is present as long as benchmarks are running, or when they failed for some reason;
  - `last_run_dir` records the last benchmark process that was completed successfully;
  - their intent is close to that of the status files (`STARTED` and `SUCCESS`), but they are used for scripting and the status files are more convenient for human beings.
- `anomalies` logs some events that prevent the benchmarks from being run correctly. For instance if the processes are triggered while the previous haven't completed yet.

`cronjob.sh` and `run_all_benchmarks_on_latest_master.sh` are two different scripts because we want to create log files early, but we only know their final destination after fetching the most recent `master` commit.

## Cleaning up after a failure

If the benchmark processes failed, some cleaning up is needed before the next run.
* Remove `current_run_dir`: the file indicates that benchmark processes are still running. This is not mandatory since the file will be overwritten by the next run, but it can confuse humans checking the benchmarks status if it is kept.
* Move `cron_res` and `cron_res_errors` to their benchmark results directory: they are log files of a specific run, they're supposed to go into the latter's directory. It should be in `snoop_results/_snoop_<DATE>_<COMMIT>`; the directory can be created if it does not exist.  
  In particular, `cron_res` really needs to be moved or deleted, as the next run will check its absence at the very beginning of the benchmark processes.
* Similarly, move results, i.e. `tezos/_snoop/{benchmark,inference}_results`, to their benchmark results directory: `snoop_results/_snoop_<DATE>_<COMMIT>`. This needs to be done too, otherwise the next run will simply reuse these results.
* Finally, upload results to `s3` if this was the cause of the failure: it could happen because of a network error for instance. The commands to run can be found in `run_all_benchmarks_on_latest_master.sh`.
