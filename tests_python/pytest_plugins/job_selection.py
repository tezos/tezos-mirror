"""Job selection

Support for load balanced batching of tests.
"""

import os
import re
import argparse
from typing import Dict, List, Tuple, Any, Callable, Sequence
import xml.etree.ElementTree as ET
from operator import itemgetter
from datetime import timedelta

from typing_extensions import TypedDict

import pytest
import _pytest

# Used for test classes that lack a previous timing
DEFAULT_TEST_TIME = 60.0

# The solution of a knapsack is a list of Bag, where each Bag contains
# the `total_weight` of the `items` in that bag.
Bag = TypedDict(
    'Bag', {'total_weight': float, 'items': List[Tuple[Any, float]]}
)


# Get the nodeid mangler from the junitxml plugin
def classname_of_nodeid(config: _pytest.config.Config, nodeid: str) -> str:
    """Converts a pytest nodeid to a JUnit XML class name

    Examples:
     foo/bar.py::TestClass::test_method -> foo.bar.TestClass
     bar.py::TestClass::test_method[param1] -> bar.TestClass
    """
    plugin_junitxml = config.pluginmanager.get_plugin("junitxml")
    names = plugin_junitxml.mangle_test_address(nodeid)
    classnames = names[:-1]
    return ".".join(classnames)


def group_prev_timings(junit_report: ET.Element) -> Dict[str, float]:
    """Groups the timings in a JUnit XML report by classname and
    summing the times of cases per class.

    Returns a mapping from classnames to the sum of testcases in that
    class.
    """
    timings = {}

    suite = junit_report.find('testsuite')
    assert isinstance(suite, ET.Element)

    for testcase in suite.findall('testcase'):
        classname = testcase.get('classname')
        if classname is None:
            print(f"Skipping JUnit XML testcase lacking classname: {testcase}")
            continue

        time_attr = testcase.get('time')
        if time_attr is None:
            time = DEFAULT_TEST_TIME
        else:
            time = float(time_attr)

        if classname not in timings:
            timings[classname] = 0.0
        timings[classname] += time

    return timings


def read_prev_timings(junit_report_path: str) -> Dict[str, float]:
    """Read the JUnit XML report in `junit_report_path` and
    returns its timings grouped by class name.
    """
    tree = ET.parse(junit_report_path)
    if tree is None:
        pytest.exit(f"Could not find timings in JUnit XML {junit_report_path}")
    assert isinstance(tree, ET.ElementTree)

    return group_prev_timings(tree.getroot())


def knapsack(items: List[Tuple[Any, float]], bag_count: int) -> List[Bag]:
    """A greedy solution to the knapsack problem.

    The argument `items` is a list of item - weight pairs.  The
    result is a partition of length `bag_count`, where each item is a `Bag`
    containing a subset of `items` such that the `total_weight` of
    each `Bag` is approximately close to each other.

    Example: With an input like:
      items=[("foo", 1.0), ("bar", 2.0), ("baz", 1.0)],
      bag_count=2
    it will return:
      [
        {'total_weight': 2.0, 'items': ["foo", "baz"]},
        {'total_weight': 2.0, 'items': ["bar"]},
      ]

    """
    knapsack: List[Bag] = []
    for _ in range(0, bag_count):
        knapsack.append({'total_weight': 0.0, 'items': []})

    for (item, weight) in items:
        min_index = 0
        min_total_weight = knapsack[0]['total_weight']
        for (index, bag) in enumerate(knapsack[1:]):
            if bag['total_weight'] < min_total_weight:
                min_total_weight = bag['total_weight']
                min_index = index + 1
        knapsack[min_index]['total_weight'] += weight
        knapsack[min_index]['items'].append((item, weight))

    return knapsack


# Hooks
def regex_option_type(
    pat: re.Pattern, error_message: str
) -> Callable[[str], re.Match]:
    def matcher(arg_value) -> re.Match:
        match = pat.match(arg_value)
        if not match:
            raise argparse.ArgumentTypeError(error_message % arg_value)
        return match

    return matcher


def pytest_addoption(parser: _pytest.config.argparsing.Parser) -> None:
    group = parser.getgroup("job selection", "Run tests in balanced batches")
    group.addoption(
        "--job",
        action="store",
        help="specify job (JOB/JOBS_TOTAL where 1 <= JOB <= JOBS_TOTAL)",
        type=regex_option_type(
            re.compile(r'(\d+)/(\d+)'),
            'The `--jobs` argument (value `%s`) should '
            'be on the form JOB/JOBS_TOTAL where 1 <= JOB <= '
            'JOBS_TOTAL',
        ),
    )
    group.addoption(
        "--prev-junit-xml",
        action="store",
        help="previous timings in JUnit XML report used for balancing",
    )
    group.addoption(
        "--jobs-dry-run",
        action='store_true',
        help="run no tests but debug balancing",
    )


def tabulate(
    headers: List[Any], rows: List[List[Any]], padding: int = 3
) -> None:
    """Tabulate a list of items with `headers` and `rows`"""
    headers_s = [str(h) for h in headers]
    rows_s = [[str(c) for c in row] for row in rows]
    cell_width = [
        max([len(row[col_idx]) for row in [headers_s] + rows_s]) + padding
        for col_idx in range(0, len(headers_s))
    ]
    for row in [headers_s] + rows_s:
        width = cell_width[0]
        print(f'{{0: <{width}}}'.format(str(row[0])), end="")
        for (col_idx, col) in enumerate(row[1:]):
            width = cell_width[col_idx + 1]
            print(f'{{0: >{width}}}'.format(str(col)), end="")
        print()


def pp_time(seconds: float) -> str:
    """Pretty print a duration in seconds"""
    return str(timedelta(seconds=int(seconds)))


def job_selection_dry_run(
    jobs_total: int,
    job_current: int,
    jobs_bags: List[Bag],
    timings_selected_items: List[Tuple[str, float]],
    prev_timings: Dict[str, float],
) -> None:
    """
    Runs no tests but prints debugging information
    """

    items_collapsed = [
        (job_idx + 1, bag['total_weight'], len(bag['items']))
        for job_idx, bag in enumerate(jobs_bags)
    ]

    print("Jobs: weight and contents")
    tabulate(
        ["job", "weight", "#classes"],
        [
            [job_idx, pp_time(total_weight), length]
            for (job_idx, total_weight, length) in items_collapsed
        ],
    )

    print()
    print("Jobs: weight and full contents")
    items_full = [
        [job_idx + 1, classname, pp_time(weight)]
        for job_idx, bag in enumerate(jobs_bags)
        for (classname, weight) in bag['items']
    ]
    tabulate(["job", "class", "weight"], items_full)

    print()
    print("Jobs: statistics")

    def avg(vals: Sequence[float]):
        return sum(vals) / len(vals)

    weights = [item[1] for item in items_collapsed]
    lengths = [item[2] for item in items_collapsed]
    headers = [
        "jobs_total",
        "weight: avg",
        "min",
        "max",
        "#classes: avg",
        "min",
        "max",
    ]
    row = [
        f"jobs_total={jobs_total}",
        pp_time(avg(weights)),
        pp_time(min(weights)),
        pp_time(max(weights)),
        str(avg(lengths)),
        str(min(lengths)),
        str(max(lengths)),
    ]
    tabulate(headers, [row])

    space_left = max(weights) * jobs_total - sum(weights)
    print(
        f"Can add {timedelta(seconds=space_left)} "
        + "without increasing wall-time."
    )

    print()
    print("Slowest classes (top 10):")
    tabulate(
        ['weight', 'class'],
        [
            [pp_time(item[1]), str(item[0])]
            for item in timings_selected_items[0:10]
        ],
    )

    print()
    print(f"Would run test classes in job {job_current + 1}/{jobs_total}:")
    current_job_classes = [
        class_name for (class_name, weight) in jobs_bags[job_current]['items']
    ]
    tabulate(
        ['class', 'weight'],
        [
            [item[0], pp_time(item[1])]
            for item in timings_selected_items
            if item[0] in current_job_classes
        ],
    )

    if prev_timings:
        # all class names that have been selected (included by the
        # user on the command line, but not necessarily in the current
        # job)
        selected_classes = [
            class_name for (class_name, _) in timings_selected_items
        ]

        # all classes that have a timing from the junit files but
        # which have not been selected
        unselected_timings = [
            class_name
            for (class_name, _) in prev_timings.items()
            if class_name not in selected_classes
        ]
        print()
        print("Orphaned classes from previous timings file:")
        for class_name in unselected_timings:
            print(class_name)


def job_selection(
    config: _pytest.config.Config,
    items: List[pytest.Item],
    prev_timings: Dict[str, float],
    jobs_total: int,
    job_current: int,
    dry_run: bool,
) -> None:
    # Only select timings for jobs that were previously collected,
    # and give dummy values for collected tests lacking timings
    timings_selected: Dict[str, float] = {}
    for item in items:
        junit_classname = classname_of_nodeid(config, item.nodeid)
        timings_selected[junit_classname] = prev_timings.get(
            junit_classname, DEFAULT_TEST_TIME
        )

    # Sort timings by descending time
    timings_selected_items = sorted(
        timings_selected.items(), key=itemgetter(1), reverse=True
    )

    # Batch test classes
    jobs_bags = knapsack(timings_selected_items, jobs_total)

    # Map classes to bags
    jobs_bags_rev = {
        class_name: index
        for (index, bag) in enumerate(jobs_bags)
        for (class_name, weight) in bag['items']
    }

    def select(item: pytest.Item) -> bool:
        junit_classname = classname_of_nodeid(config, item.nodeid)
        assert (
            junit_classname in jobs_bags_rev
        ), "Expected to find {junit_classname} in the timings"

        job_item = jobs_bags_rev[junit_classname]
        return job_current == job_item

    # Filter test items in place
    if dry_run:
        print("dry run")
        job_selection_dry_run(
            jobs_total,
            job_current,
            jobs_bags,
            timings_selected_items,
            prev_timings,
        )
        items[:] = []
    else:
        items[:] = [item for item in items if select(item)]
        if not items:
            print(
                "Warning: the current job is empty."
                + " Consider rebalancing or reducing the number of jobs"
            )


def pytest_collection_modifyitems(
    config: _pytest.config.Config, items: List[pytest.Item]
) -> None:
    job_config = config.getoption('--job')
    prev_junit_xml = config.getoption('--prev-junit-xml')
    dry_run = config.getoption('--jobs-dry-run')

    if prev_junit_xml is not None and job_config is None:
        pytest.exit('Cannot give the `--prev-junit-xml` flag without `--job`')

    if dry_run and job_config is None:
        pytest.exit('Cannot give the `--jobs-dry-run` flag without `--jobs`')

    if job_config is None:
        return None

    job_current = int(job_config.group(1)) - 1
    jobs_total = int(job_config.group(2))

    if jobs_total <= 0:
        pytest.exit(
            'Cannot run 0 jobs ' + f'(--job-config {job_config.group(0)})'
        )
    if job_current < 0 or job_current >= jobs_total:
        pytest.exit(
            'Job index out of bounds ' + f'(--job-config {job_config.group(0)})'
        )

    if prev_junit_xml is not None:
        if not os.path.isfile(prev_junit_xml):
            pytest.exit(
                f'The file {prev_junit_xml} given to '
                + '--prev-junit-xml does not exist'
            )
        else:
            prev_timings = read_prev_timings(prev_junit_xml)
    else:
        prev_timings = {}

    print(
        f"(job selection: {job_current+1}/{jobs_total} with"
        + f" {len(prev_timings)} timings from {prev_junit_xml})"
    )

    job_selection(config, items, prev_timings, jobs_total, job_current, dry_run)

    return None
