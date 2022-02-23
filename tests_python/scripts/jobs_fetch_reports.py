"""
This script fetches and merges the pytest integration
JUnit reports of a Gitlab CI pipeline. The resulting merged
JUnit report can be used as input to the pytest job selection
plugin, used for job balancing.
"""
import re
import argparse
import urllib.parse
import urllib.request
import urllib.error
import json
import io
from typing import List, TypedDict, Tuple, Optional
import xml.etree.ElementTree as ET
import sys


ProjectPipelineJob = TypedDict('ProjectPipelineJob', {'id': int, 'name': str})


def gitlab_api_project_pipeline_jobs(
    project_id: str,
    pipeline_id: int,
) -> List[ProjectPipelineJob]:
    """
    Fetch ProjectPipelineJobs of pipeline `pipeline_id` in project `project_id`
    """
    project_id_quoted = urllib.parse.quote_plus(str(project_id))

    per_page = 200
    endpoint = 'https://gitlab.com/api/v4/'
    path = f'projects/{project_id_quoted}/pipelines/{pipeline_id}/jobs'
    query = f'?per_page={per_page}'
    next_url: Optional[str] = endpoint + path + query

    # Use link header to detect pagination as per
    # https://docs.gitlab.com/ee/api/#offset-based-pagination
    next_link_re = re.compile(r'<([^>]*)>; rel="next"')
    jobs: List[ProjectPipelineJob] = []
    try:
        while next_url:
            with urllib.request.urlopen(next_url) as request:
                data = json.loads(request.read().decode())
                jobs = jobs + [
                    {'id': job['id'], 'name': job['name']} for job in data
                ]

                link = request.headers.get('link', '')
                next_link_match = next_link_re.search(link)
                if next_link_match:
                    next_url = next_link_match.group(1)
                else:
                    print("No next link, terminating")
                    next_url = None

        return jobs

    except urllib.error.HTTPError as exc:
        print(
            "Could not fetch jobs of pipeline "
            + f"{pipeline_id} in {project_id}: {exc}"
        )
        sys.exit(1)


def gitlab_api_project_pipeline_job_artifact(
    project_id: str, job_id: int, artifact_path: str
) -> bytes:
    """
    Fetch artifact at `artifact_path` of job `job_id` in `project_id`
    """
    project_id_quoted = urllib.parse.quote_plus(str(project_id))
    artifact_path_quoted = "/".join(
        map(urllib.parse.quote_plus, artifact_path.split("/"))
    )

    endpoint = 'https://gitlab.com/api/v4/'
    path = (
        f'projects/{project_id_quoted}/jobs/{job_id}'
        + f'/artifacts/{artifact_path_quoted}'
    )
    url = endpoint + path

    try:
        with urllib.request.urlopen(url) as request:
            return request.read()
    except urllib.error.HTTPError as exc:
        print(
            f"Could not fetch artifact {artifact_path} "
            + f"in job {job_id} in {project_id}: {exc}"
        )
        sys.exit(1)


def merge_junit_reports(
    junit_reports: List[ET.ElementTree],
) -> Tuple[int, ET.ElementTree]:
    """
    Merges the testsuites of a list of JUnit XML reports
    """

    testcases: List[ET.Element] = []
    for junit_report in junit_reports:
        testsuite = junit_report.getroot().find('testsuite')
        assert isinstance(testsuite, ET.Element)
        testcases += testsuite.findall('testcase')

    nb_testcases = len(testcases)
    testsuites = ET.Element('testsuites')
    testsuite = ET.SubElement(testsuites, 'testsuite')
    testsuite.set('tests', str(nb_testcases))
    testsuite.extend(testcases)
    return (nb_testcases, ET.ElementTree(testsuites))


def fetch_and_merge_reports(
    project_id: str,
    pipeline_id: int,
    merged_report_output_file: io.TextIOWrapper,
) -> None:
    """
    Fetches and merges the JUnit XML reports of each pytest
    integration job in pipeline `pipeline_id` of project `project_id`,
    and writes the result to `merged_report_output_file`.
    """

    jobs = gitlab_api_project_pipeline_jobs(project_id, pipeline_id)
    junit_reports = []
    integration_job_re = re.compile(r'integration:pytest (\d+)/(\d+)')
    for job in jobs:
        match = integration_job_re.match(job['name'])
        if match is None:
            continue

        (ci_node_index, ci_node_total) = match.groups()

        artifact_path = (
            "tests_python/reports/"
            + f"report_{ci_node_index}_{ci_node_total}.xml"
        )
        junit_report_str = gitlab_api_project_pipeline_job_artifact(
            project_id, job['id'], artifact_path
        ).decode(encoding="utf-8")

        print(f"Job {job['id']} ({job['name']}): " + f"Fetched {artifact_path}")
        junit_reports.append(ET.ElementTree(ET.fromstring(junit_report_str)))

    if len(junit_reports) == 0:
        print(
            f"Found no jobs in pipeline {pipeline_id} of project "
            + f"{project_id} matching /{integration_job_re.pattern}/"
        )
        sys.exit(1)

    (nb_testcases, merged_junit_report) = merge_junit_reports(junit_reports)
    merged_junit_report.write(merged_report_output_file, encoding='unicode')
    print(
        f"Wrote merged report with {nb_testcases} testcases "
        + f"to {merged_report_output_file.name}"
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="jobs fetch reports")

    parser.add_argument(
        "project_id",
        type=str,
        help="""
        Provide gitlab project id as first argument, e.g.
        `tezos/tezos` or numerical id
        """,
    )

    parser.add_argument(
        "pipeline_id",
        type=int,
        help="""
        Provide pipeline id as second argument, e.g. 391452179
        """,
    )

    parser.add_argument(
        "merged_report_output_file",
        type=argparse.FileType('w'),
        help="""
        where to write merged report
        """,
    )

    args = parser.parse_args()

    fetch_and_merge_reports(
        args.project_id, args.pipeline_id, args.merged_report_output_file
    )


if __name__ == "__main__":
    main()
