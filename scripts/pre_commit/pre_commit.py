#!/usr/bin/env python3
"""
Script to be executed before committing, to do things fast.
The point is to avoid pushing commits that will trigger silly
CI failures, to save CI runs and have a faster feedback loop (by having
local feedback as opposed to gitlab CI feedback).
To be fast this script is incomplete: it doesn't check everything
that gitlab CI will check; but it is correct: what it checks is required
for gitlab CI to pass.

For the moment this script:

* executes python tests of staged tests_python/*.py test files
  The point is to execute a subset of "pytest" that is required
  for a commit to pass CI. Make sure you've installed everything required
  first: https://tezos.gitlab.io/developer/python_testing_framework.html

  This is most useful when working on the python tests themselves.
* executes tezt tests of staged *.ml files in "tezt/tests" that contain
  the string "let run () =". Like the python tests, the point is to
  execute a required subset of the CI and
  this is most useful when working on the tezt tests themselves.
* lints and typechecks staged tests_python/*.py files.
* checks formatting of staged tests_python/*.py, *.ml, *.mli in
  a fast manner (because it only checks exactly those files not the entire
  worktree).

The first three points are blocking, if the corresponding check fails
the commit is aborted.

Installation: `ln -sr scripts/pre_commit/pre_commit.py .git/hooks/pre-commit`

You can pass "--lint-only" to avoid calling `pytest` and executing tezt tests,
so that the hook is always fast. In this case, install it as follows:

```
cd .git/hooks
echo '#!/usr/bin/env bash' > pre-commit
echo './scripts/pre_commit/pre_commit.py --lint-only "$@"' >> pre-commit
chmod +x pre-commit
```

You can call the hook manually on modified but not yet staged files
to make sure an upcoming call to `git commit` will succeed:
`./scripts/pre_commit/pre_commit.py --unstaged [--lint-only]?`

You can pass "--test-itself" for the precommit to test itself. This is
used in the CI.
"""

import os
import re
import subprocess
import sys
from typing import List, Optional, Tuple
_LINT_ONLY = "--lint-only"
_TEST_ITSELF = "--test-itself"
_UNSTAGED = "--unstaged"


def _find_python_data(tests_python_path: str)\
        -> Optional[Tuple[List[List[str]], List[str]]]:
    """
    Args:
        tests_python_path: the path of the directory `tezos/tests_python`
    Returns:
        A pair of:

        1/ The commands to call on python files
        2/ The directories to consider in `tezos/tests_python`
           (relative to tests_python/)

        or None if an error occured
    """
    makefile_path = os.path.join(tests_python_path, "Makefile")
    if not os.path.isfile(makefile_path):
        print(f"{makefile_path} not found", file=sys.stderr)
        return None
    # Alternatively we could use make itself to retrieve the variables' values
    # (with a rule printing the value of a variable)
    # This list shrinks in loop below. The last element matters (it's used
    # to return the second element of the pair).
    tags = ["TYPECHECK=", "LINT=", "LINT2=", "PACKAGES="]
    result = []
    with open(makefile_path, 'r') as handle:
        for line in handle.readlines():
            for (i, tag) in enumerate(tags):
                if line.startswith(tag):
                    result.append(line[len(tag):].strip())
                    tags.pop(i)
                    if not tags:
                        # Success
                        # Tools are all elements except the last one:
                        tools = result[:-1]
                        packages = result[-1]  # Take last
                        # Here we rely on split() taking care of ignoring
                        # successive whitespaces, as well as trailing ones.
                        return ([t.split() for t in tools], packages.split())
    print(
        "Variables identified by prefixes"
        f" that follow not found in {makefile_path}:",
        file=sys.stderr)
    for tag in tags:
        print(f"  {tag}", file=sys.stderr)
    return None


def _git_diff(staged_or_modified: bool, extension: str) -> List[str]:
    """
    Args:
        extension: the extension of files considered, such as "py" or "ml"
        staged_or_modified (bool) Whether to consider staged files (True)
                                  or modified ones (False)
    Returns: A list of relevant versioned files that are staged or modified
    """
    git_cmd = ["git", "diff"]
    if staged_or_modified:
        git_cmd += ["--cached"]
    git_cmd += ["--name-only", "--diff-filter=ACMR", "*." + extension]
    git_diff_result = subprocess.run(git_cmd,
                                     stdout=subprocess.PIPE,
                                     universal_newlines=True,
                                     check=True)
    # The comprehension filters empty lines
    return [x for x in git_diff_result.stdout.split("\n") if x]


def _git_diff_many(staged_or_modified: bool,
                   extensions: List[str]) -> List[str]:
    """
    Args:
        extensions: the extensions to consider such as ["ml", "mli"]
        staged_or_modified (bool) Whether to consider staged files (True)
                                  or modified ones (False)
    Returns: A list of relevant versioned files that are staged or modified
    """
    result = []
    for extension in extensions:
        result.extend(_git_diff(staged_or_modified, extension))
    return result


def _get_tests_python_path() -> Optional[str]:
    """
    Returns: the path of the tests_python directory or None
             in case of error
    """
    git_root = os.getcwd()  # Correct, because this script should be executed
    # from the root of the repository
    result = os.path.join(git_root, "tests_python")
    if not os.path.isdir(result):
        print("{result} is not a directory, this is unexpected",
              file=sys.stderr)
        return None
    return result


def _get_py_files(tests_python_path: str, staged_or_modified: bool,
                  pytest: bool) -> List[str]:
    """
    Args:
        tests_python_path: the path of the directory `tezos/tests_python`
        staged_or_modified (bool) Whether to consider staged files (True)
                                  or modified ones (False)
        pytest(bool): Whether the files are to be sent to pytest (True)
                      or not (False)
    Returns: A list of relevant .py files
    """
    tests_python_basename = os.path.basename(tests_python_path)

    def _is_file_pytest_relevant(path: str):
        """
        Returns: whether the file is relevant to being an argument to pytest
        """
        forbidden_suffixes = ["__init__.py", "conftest.py"]
        if any([path.endswith(x) for x in forbidden_suffixes]):
            return False
        if not path.startswith(f"{tests_python_basename}/tests"):
            return False
        return True

    git_diff_result = _git_diff(staged_or_modified, "py")
    if pytest:
        return [x for x in git_diff_result if _is_file_pytest_relevant(x)]
    return git_diff_result


def _ocamlformat_check(files: List[str]) -> bool:
    """
    Args:
        files (list(str)): The files on which to call ocamlformat
        staged_or_modified (bool) Whether staged files are considered (True)
                                  or modified ones (False)
    Returns: Whether all files are correctly formatted (True) or not (False).
    """
    result = True

    if not files:
        # Nothing to do
        return result

    formatting_fails: List[str] = []
    unknown_fails: List[str] = []

    for file_ in files:
        cmd = ["ocamlformat", "--check", file_]
        print(" ".join(cmd))
        res = subprocess.run(cmd, check=False, text=True,
                             stderr=subprocess.PIPE)
        if res.returncode > 0:
            # If the file is solely badly formatted (as opposed to not
            # being parseable), ocamlformat will write nothing to stderr.
            # We use that to distinguish the two cases.
            (unknown_fails if res.stderr else formatting_fails).append(file_)
            result = False

    if unknown_fails:
        plural = "" if len(unknown_fails) == 1 else "s"
        print(f"Formatting of the following file{plural}"
              " could not be verified:", file=sys.stderr)
        for file_ in sorted(unknown_fails):
            print(f"  {file_}", file=sys.stderr)
        print("This likely means the concerned files"
              " are syntactically invalid. Please fix them.", file=sys.stderr)

    if formatting_fails:
        plural = "" if len(formatting_fails) == 1 else "s"
        print(f"Badly formatted file{plural}:", file=sys.stderr)
        for file_ in sorted(formatting_fails):
            print(f"  {file_}", file=sys.stderr)
        formatting_fails = [f'"{f}"' if " " in f else f
                            for f in formatting_fails]
        fix = "ocamlformat --inplace " + " ".join(formatting_fails)
        print(f"To fix that, run from the repo's root: {fix}", file=sys.stderr)

    return result


def _call_pytest(tests_python_path: str, files: List[str]) -> int:
    """
    Args:
        tests_python_path: the path of the directory `tezos/tests_python`
        files (list(str)): The files on which to call pytest
    Returns:
        The maximum of return codes of calls to pytest on `files`
    """
    tests_python_basename = os.path.basename(tests_python_path)
    result = 0
    for file_ in files:
        # trim "tests_python/" from start of path
        # because we execute within tests_python
        file_ = file_[len(tests_python_basename) + len(os.sep):]
        cmd = ["poetry", "run", "pytest", file_]
        print(f"{tests_python_basename}> " + " ".join(cmd))
        py_test_result = subprocess.run(cmd,
                                        cwd=tests_python_basename,
                                        check=False)
        result = max(result, py_test_result.returncode)
    return result


def _call_py_linters(tests_python_path: str, files) -> int:
    """
    Args:
        tests_python_path: the path of the directory `tezos/tests_python`
        files (list(str)): The files to lint
    Returns:
        The maximum of return codes of calls to linters
    """
    py_data = _find_python_data(tests_python_path)
    if not py_data:
        return 1
    tools, packages = py_data
    tests_python_basename = os.path.basename(tests_python_path)

    # trim "tests_python/" from start of path
    # because we execute within tests_python (this is required since
    # the move to poetry).
    files = [x[len(tests_python_basename) + len(os.sep):] for x in files]
    # remove files that are not in 'packages' i.e. not in directories
    # listed by tests_python/Makefile's PACKAGES variables
    files = [x for x in files if any([x.startswith(p) for p in packages])]
    if not files:  # Nothing to do
        return 0

    return_code = 0
    for tool in tools:
        cmd = tool + files
        print(f"{tests_python_basename}> " + " ".join(cmd))
        cmd_result = subprocess.run(cmd,
                                    cwd=tests_python_basename,
                                    check=False)
        return_code = max(return_code, cmd_result.returncode)
    return return_code


def _call_tezt(files: List[str], staged_or_modified: bool) -> int:
    """
    Args:
        files (list(str)): All {ml,mli} files to consider. Filtering
                           for tezt has NOT been done yet.
        staged_or_modified (bool) Whether staged files are considered (True)
                                  or modified ones (False)
    Returns:
        The maximum of return codes
    """
    tezt_test_dir = "tezt/tests"
    if not os.path.isdir(tezt_test_dir):
        print(f"Unexpectedly, {tezt_test_dir} directory cannot be found",
              file=sys.stderr)
        return 1

    tezt_files = []
    for file_ in files:
        if not file_.startswith(tezt_test_dir):
            continue
        if not file_.endswith(".ml"):
            continue
        with open(file_, 'r') as handle:
            pattern = re.escape("let run () =")
            match = re.search(pattern, handle.read())
            if match is None:
                continue
        to_add = file_[len(tezt_test_dir) + len(os.sep):]  # remove tezt/tests/
        tezt_files.append(to_add)

    adjective = "staged" if staged_or_modified else "modified"

    if not tezt_files:
        print(f"No {adjective} file relevant to tezt found")
        return 0

    return_code = 0

    for tezt_file in tezt_files:
        cmd = ["dune", "exec", "tezt/tests/main.exe", "--", "--file",
               tezt_file]
        print("> " + " ".join(cmd))
        cmd_result = subprocess.run(cmd, check=False)
        return_code = max(return_code, cmd_result.returncode)

    return return_code


def _main_py_files(staged_or_modified: bool, adjective: str,
                   pytest: bool) -> int:
    """
    Args:
        staged_or_modified (bool): Whether staged files are considered (True)
                                   or modified ones (False)
        adjective (str)
        pytest (bool): whether `pytest` should be called
    Returns: A return code
    """
    return_code = 0

    tests_python_path = _get_tests_python_path()
    if not tests_python_path:
        return 1

    if pytest:
        relevant_pytest_files = _get_py_files(tests_python_path,
                                              staged_or_modified, True)
        if relevant_pytest_files:
            return_code = max(return_code,
                              _call_pytest(tests_python_path,
                                           relevant_pytest_files))
        else:
            print("No %s *.py file relevant to pytest found" % adjective)
    else:
        print(f"{_LINT_ONLY} passed: not calling pytest")

    relevant_pylint_files = _get_py_files(tests_python_path,
                                          staged_or_modified, False)
    if relevant_pylint_files:
        return_code = max(
            return_code,
            _call_py_linters(tests_python_path, relevant_pylint_files))
    else:
        print("No %s *.py file to lint" % adjective)

    return return_code


def _main_test_itself() -> int:
    """
    Test this hook itself, instead of doing its usual operations.

    Returns: A return code
    """
    tests_python_path = _get_tests_python_path()
    if not tests_python_path:
        return 1
    py_data = _find_python_data(tests_python_path)
    py_tools = None if py_data is None else py_data[0]
    if py_tools is None:
        return 1  # Error has been logged in _find_python_data
    py_packages = py_data[1]  # type: ignore
    if py_packages is None:
        return 1  # Error has been logged in _find_python_data
    # Success case
    print(f"All {len(py_tools)} python tools found")
    print(f"{len(py_packages)} python packages to lint found")
    return 0


# I don't use argsparse to avoid adding a non-system dependency
def _parse_arguments() -> Tuple[bool, bool, bool]:
    """
    Returns: A tuple with three Booleans:
        1/ Whether staged (True) or modified (False) files should be considered
        2/ Whether --lint-only was passed
        3/ Whether the hook should test itself instead of doing its normal
           operations
    """
    staged = _UNSTAGED not in sys.argv
    lint_only = _LINT_ONLY in sys.argv
    test_itself = _TEST_ITSELF in sys.argv
    return (staged, lint_only, test_itself)


def _print_help():
    """ Prints the help and exits if "--help" or "-h" was given """
    if "-h" not in sys.argv and "--help" not in sys.argv:
        return
    print("Usage: ./scripts/pre_commit/pre_commit.py [-h|--help]"
          f" [{_LINT_ONLY}]"
          f" [{_TEST_ITSELF}]"
          f" [{_UNSTAGED}]")
    print(f"""This hooks does the following:
1/ Executes python tests of staged *.py files (disable by passing {_LINT_ONLY})
2/ Lints staged *.py files
3/ Formats staged *{{ml,mli}} files
   (and update the commit if possible with formatting changes)
Pass {_TEST_ITSELF} for the hook to test itself (used by CI)
Pass {_UNSTAGED} to do all this on unstaged files""")
    sys.exit(0)


def main() -> int:
    """ The main """
    _print_help()

    staged, lint_only, test_itself = _parse_arguments()
    if test_itself:
        print(f"Recognized {_TEST_ITSELF}")
        return _main_test_itself()
    adjective = "staged" if staged else "modified"

    return_code = _main_py_files(staged, adjective, not lint_only)

    ml_extensions = ["ml", "mli"]
    relevant_ocaml_files = _git_diff_many(staged, ml_extensions)
    if relevant_ocaml_files:
        ocamlformat_res = _ocamlformat_check(relevant_ocaml_files)
        return_code = max(return_code, 0 if ocamlformat_res else 1)
        if lint_only:
            print(f"{_LINT_ONLY} passed: not calling tezt")
        else:
            tezt_rc = _call_tezt(relevant_ocaml_files, staged)
            return_code = max(return_code, tezt_rc)
    else:
        extensions = "{" + ",".join(ml_extensions) + "}"
        print(f"No {adjective} *.{extensions} relevant file found")

    return return_code


if __name__ == "__main__":
    sys.exit(main())
