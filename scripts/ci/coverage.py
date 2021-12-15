"""This script is used in the CI. It should run in the pipeline of
master. It fetches the coverage information from the most recently
merged branch with coverage.

"""

import sys
from typing import NamedTuple, Union, Optional, Iterator
import os
import re
import itertools
import tempfile
import subprocess

import gitlab
from gitlab import Gitlab
from gitlab.v4.objects import Project, ProjectCommit, ProjectPipelineJob

import pytest


class Config(NamedTuple):
    # Gitlab project id.
    project_id: Union[str, int]
    # Ref starting from which we will fetch coverage.
    start_commit: str
    # The name of the job from coverage will be fetched.
    coverage_job_name: str


DEFAULT_CONFIG = Config(
    start_commit="master",
    project_id="tezos/tezos",
    coverage_job_name="unified_coverage",
)


def eprint(*values: object) -> None:
    print(*values, file=sys.stderr)


def log(commit: ProjectCommit, message: str) -> None:
    fmt = "{:10} ({:40}): {}"
    commit_title = (
        commit.title if len(commit.title) < 40 else commit.title[:37] + "..."
    )
    eprint(fmt.format(commit.id[:10], commit_title, message))


def get_gitlab() -> Gitlab:
    # Connect read-only
    url = "https://gitlab.com"
    glab = gitlab.Gitlab(url)
    if glab is None:
        eprint(f"Could not instantiate Gitlab API bindings for {url}")
        sys.exit(1)
    return glab


def get_project(config: Config, glab: Gitlab) -> Project:
    project = glab.projects.get(config.project_id)
    if project is None:
        eprint(f"GitLab project {config.project_id} was not found")
        sys.exit(1)
    return project


def download_artifacts_from_job(
    project: Project, job_id: int, outdir: Optional[str] = None
) -> None:
    (zip_file_fd, zip_file_name) = tempfile.mkstemp(suffix=".zip")
    # Hack as per https://forum.gitlab.com/t/25436/3
    job = project.jobs.get(job_id, lazy=True)
    with open(zip_file_fd, "wb") as zip_file:
        job.artifacts(streamed=True, action=zip_file.write)

    cmd = ["unzip", "-b", zip_file_name]
    if outdir:
        cmd += ["-d", outdir]
    subprocess.run(cmd, check=True)
    os.unlink(zip_file_name)


def coverage_job_of_commit(
    config: Config, project: Project, commit: ProjectCommit
) -> Optional[ProjectPipelineJob]:
    """
    Inspects the latest pipeline of `commit` in the project `project`.
    It finds the job `config.coverage_job` in this project, and
    returns its coverage if set. Otherwise, `None` is returned.

    """

    commit_id = commit.id

    # Find last pipeline for the commit
    pipelines = project.pipelines.list(
        sha=commit_id, order_by="id", sort="desc"
    )
    if not pipelines:
        log(commit, f"Pipeline not found for commit {commit.web_url}")
        return None
    last_pipeline = pipelines[0]

    # List of jobs
    jobs = last_pipeline.jobs.list(all=True)
    if not jobs:
        log(commit, f"Jobs not found in pipeline {last_pipeline.web_url}")
        return None

    coverage_job = None

    for job in jobs:
        if job.name == config.coverage_job_name:
            coverage_job = job

    if coverage_job is None:
        log(
            commit,
            f"Could not find coverage job `{config.coverage_job_name}` "
            + f"in pipeline {last_pipeline.web_url}. "
            + "Found jobs: "
            + ",".join([job.name for job in jobs]),
        )
        return None

    if coverage_job.status != "success":
        log(
            commit,
            f"Coverage job `{config.coverage_job_name}` "
            + f"({coverage_job.web_url}) was not successful "
            + f" (status: {coverage_job.status}). Ignoring.",
        )
        return None

    if coverage_job.coverage is None:
        log(
            commit,
            "Coverage not found in coverage job "
            + f" `{config.coverage_job_name}` "
            + f"({coverage_job.web_url})",
        )
        return None

    log(
        commit,
        f"Coverage {coverage_job.coverage}% found in coverage job "
        + f"`{coverage_job.name}` "
        + f"({coverage_job.web_url})",
    )

    return coverage_job


def is_merge_commit(commit: ProjectCommit) -> bool:
    return re.match(r"Merge branch '.*' into '.*'", commit.title) is not None


def get_ref_coverage_job(
    config: Config, project: Project, ref_name: str, commit_limit: int = 200
) -> Optional[ProjectPipelineJob]:
    """This function retrieves the coverage of the reference `ref_name` in
    the project `project` by inspecting pipelines of recently merged
    branches.

    It retrieves at most `commit_limit` commits starting at
    `ref_name`.  The first commit is skipped, as it will typically be
    a merge commit with no coverage pipelines. We then search the
    history to the next merge commit. If last commit of the
    corresponding merged branch has coverage, we return
    that. Otherwise, we continue to the next merge commit.

    We assume a semi-linear history and that the merged commits do
    not themselves contain merge commits.
    """

    eprint(f"Checking first {commit_limit} commits")
    commits = itertools.islice(
        project.commits.list(ref_name=ref_name, as_list=False),
        commit_limit,
    )

    commit_count = 0

    # The functions nextc and next_merge_commit are inlined to
    # access `commit_count`
    def nextc(iterator: Iterator[ProjectCommit]) -> ProjectCommit:
        nonlocal commit_count
        commit = next(iterator)
        commit_count += 1
        return commit

    def next_merge_commit(commits: Iterator[ProjectCommit]) -> ProjectCommit:
        merge_commit = nextc(commits)
        while not is_merge_commit(merge_commit):
            log(merge_commit, "Skipping non-merge commit")
            merge_commit = nextc(commits)
        return merge_commit

    try:
        # First, attempt to retrieve coverage from the pipelines of the
        # pred of the first merge commit.
        merge_commit = next_merge_commit(commits)
        log(merge_commit, "Skip first merge commit")
        pred = nextc(commits)
        assert not is_merge_commit(pred)
        log(pred, "Check for coverage in merge commit parent")
        coverage_job = coverage_job_of_commit(config, project, pred)

        if coverage_job is not None:
            return coverage_job

        # If the most recent merged MR did not contain coverage
        # information, start looking in earlier the pipelines for merge
        # commits:
        while True:
            merge_commit = next_merge_commit(commits)

            log(merge_commit, "Check for coverage in merge commit")
            coverage_job = coverage_job_of_commit(config, project, merge_commit)

            if coverage_job is not None:
                return coverage_job

    # Is thrown when the iterator is exhausted. Meaning that either
    # we've reached `commit_limit` or the beginning of history.
    except StopIteration:
        eprint(
            f"No coverage information found in the {commit_count} "
            + f"most recent commits on {ref_name}"
        )
        return None


def main() -> None:
    config = Config(
        start_commit=os.getenv(
            "COVERAGE_START_COMMIT", DEFAULT_CONFIG.start_commit
        ),
        project_id=os.getenv("CI_PROJECT_ID", DEFAULT_CONFIG.project_id),
        coverage_job_name=os.getenv(
            "COVERAGE_JOB_NAME", DEFAULT_CONFIG.coverage_job_name
        ),
    )
    glab = get_gitlab()
    project = get_project(config, glab)
    coverage_job = get_ref_coverage_job(config, project, config.start_commit)

    if coverage_job is not None:
        print(f"Coverage: {coverage_job.coverage}%")
        download_artifacts_from_job(project, coverage_job.id)
        print("Succesfully retrieved and extracted artifacts")

    else:
        print("Coverage: None")


if __name__ == "__main__":
    main()

### Follows a series of integration tests that can be launched by
### running `poetry run pytest coverage.py`. Warning: these call the
### Gitlab API directly.


@pytest.fixture
def config() -> Config:
    return DEFAULT_CONFIG


@pytest.fixture
def glab() -> Gitlab:
    return get_gitlab()


@pytest.fixture
def project(config: Config, glab: Gitlab) -> Project:
    return get_project(config, glab)


class Test:
    def test_coverage_of_commit(self, config: Config, project: Project) -> None:
        # https://gitlab.com/tezos/tezos/-/commit/a073c38f
        commit = project.commits.get("a073c38f")
        coverage_job = coverage_job_of_commit(config, project, commit)
        assert coverage_job is not None
        assert coverage_job.coverage == 66.35

        # https://gitlab.com/tezos/tezos/-/commit/6acb775b
        commit = project.commits.get("6acb775b")
        coverage_job = coverage_job_of_commit(config, project, commit)
        assert coverage_job is not None
        assert coverage_job.coverage == 66.34

    def test_coverage_of_commit_errors(
        self, config: Config, project: Project
    ) -> None:
        # A release branch. It does not contain the `unified_coverage`
        # job https://gitlab.com/tezos/tezos/-/commit/0e7a0e9a
        commit = project.commits.get("0e7a0e9a")
        assert coverage_job_of_commit(config, project, commit) is None

        # A merge request from before 3810. It does not contain the
        # `unified_coverage` job
        # https://gitlab.com/tezos/tezos/-/commit/3dde6969
        commit = project.commits.get("3dde6969")
        assert coverage_job_of_commit(config, project, commit) is None

    def test_coverage_of_commit_errors_failed_job(
        self, config: Config, project: Project
    ) -> None:
        # A commit in !3810: it's pipeline contains the
        # `unified_coverage` job, but it has failed.
        commit = project.commits.get("da80046a")
        assert coverage_job_of_commit(config, project, commit) is None

    def test_get_branch_coverage_success(
        self, config: Config, project: Project
    ) -> None:
        # A commit for which the predecessor has a pipeline with
        # unified_coverage
        # https://gitlab.com/tezos/tezos/-/commit/cd20c132
        coverage_job = get_ref_coverage_job(config, project, "cd20c132")
        assert coverage_job is not None
        assert coverage_job.coverage == 66.29

    def test_get_branch_coverage_no_coverage_job(
        self, config: Config, project: Project
    ) -> None:
        # Before the merge of unified coverage !3810
        # https://gitlab.com/tezos/tezos/-/commit/d99eff5a
        coverage_job = get_ref_coverage_job(config, project, "d99eff5a")
        assert coverage_job is None

    def test_get_branch_coverage_very_early(
        self, config: Config, project: Project
    ) -> None:
        # A very early commit
        # https://gitlab.com/tezos/tezos/-/commit/2064af36
        coverage_job = get_ref_coverage_job(config, project, "2064af36")
        assert coverage_job is None

    def test_get_artifacts_from_job(self, project: Project) -> None:
        commit = project.commits.get(
            "63d6e510923d4f4c27b5bdac2a46050c0e001bbc", lazy=True
        )
        commit.title = "foo"

        outdir = tempfile.mkdtemp("artifacts")
        download_artifacts_from_job(project, 1830153953, outdir=outdir)
        assert os.path.exists(
            os.path.join(outdir, "_coverage_report", "index.html")
        ), "Check coverage file"
