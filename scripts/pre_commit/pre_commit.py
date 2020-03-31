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

* executes python tests of staged *.py test files
  The point is to execute a subset of "pytest" that is required
  for a commit to pass CI. Be sure you ran "poetry install" first.

  This is most useful when working on the python tests themselves.
* lints and typechecks staged *.py files.
* calls ocamlformat on staged *.ml, *.mli files in a way
  faster than `make fmt`.
  If staged files are being inspected (i.e. if --unstaged is not passed,
  see below), formatted files are added before creating the commit;
  hereby autoformatting the files upon committing.

The first two points are blocking, if the corresponding check fails
the commit is aborted.

Installation: `ln -sr scripts/pre_commit/pre_commit.py .git/hooks/pre-commit`

You can call the hook manually on modified but not yet staged files
to make sure an upcoming call to `git commit` will succeed:
`./scripts/pre_commit/pre_commit.py --unstaged`

You can pass "--lint-only" to avoid calling `pytest`, so that the hook
is always fast. In this case, install it as follows:

```
cd .git/hooks
echo '#!/usr/bin/env bash' > pre-commit
echo './scripts/pre_commit/pre_commit.py --lint-only "$@"' >> pre-commit
chmod +x pre-commit
```

You can pass "--test-itself" for the precommit to test itself. This is
used in the CI.
"""

import os
import subprocess
import sys
from typing import List, Optional, Tuple
_LINT_ONLY = "--lint-only"
_TEST_ITSELF = "--test-itself"
_UNSTAGED = "--unstaged"


def _find_python_data(tests_python_path: str)\
        -> Optional[Tuple[List[str], List[str]]]:
    """
    Args:
        tests_python_path: the path of the directory `tezos/tests_python`
    Returns:
        A pair of:

        1/ The executables to call on python files
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
                        # Tools are all elements except the first one:
                        tools = result[:len(result) - 1]
                        packages = result[len(result) - 1].split(" ")
                        # Be resilient to multiple spaces in a row
                        # Remove leading/trailing WS
                        packages = [p.strip() for p in packages]
                        # Remove empty entries if any
                        packages = [p for p in packages if p]
                        return (tools, packages)
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


def _call_ocamlformat(files: List[str], staged_or_modified: bool):
    """
    Args:
        files (list(str)): The files on which to call ocamlformat
        staged_or_modified (bool) Whether staged files are considered (True)
                                  or modified ones (False)
    """
    if staged_or_modified:
        # If we're dealing with staged files, we don't want to call
        # ocamlformat if the file has unstaged modifications; because
        # adding (in git) the file after having formatted would stage
        # those modifications
        cmd = ["git", "diff", "--name-only"]
        modified_files_result = subprocess.run(cmd,
                                               stdout=subprocess.PIPE,
                                               universal_newlines=True,
                                               check=True)
        trimmed_files = [
            x for x in files
            if x not in modified_files_result.stdout.split("\n")
        ]
        excluded = [x for x in files if x not in trimmed_files]
        if excluded:
            print("Not formatting some files because"
                  " they have unstaged modifications"
                  "\n(formatting them and readding them"
                  " would stage unwanted modifications)."
                  "\nConcerned files:")
            for exclude in excluded:
                print("  " + exclude)
        files = trimmed_files
    for file_ in files:
        cmd = ["ocamlformat", "--inplace", file_]
        print(" ".join(cmd))
        # check=False: we don't want to fail if ocamlformat fails because
        # a file cannot be parsed. And we neither want to track this error.
        subprocess.run(cmd, check=False)
        if staged_or_modified:
            # git add file, so that formatting makes it to the commit
            # This is safe, because of the previous check having
            # no unstaged modification. Hence adding it only stages
            # formatting changes
            #
            # On another topic, we have no way to know if ocamlformat did
            # a modification. Hence we're always readding. If we had
            # this information, we would be able to avoid these calls.
            cmd = ["git", "add", file_]
            print(" ".join(cmd))
            subprocess.run(cmd, check=True)


def _call_pytest(files: List[str]) -> int:
    """
    Args:
        files (list(str)): The files on which to call pytest
    Returns:
        The maximum of return codes of calls to pytest on `files`
    """
    result = 0
    for file_ in files:
        cmd = ["poetry", "run", "pytest", file_]
        print(" ".join(cmd))
        py_test_result = subprocess.run(cmd, check=False)
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
    return_code = 0
    for tool in tools:
        # trim "tests_python/" from start of path
        # because we execute within tests_python
        files_ = [x[len(tests_python_basename) + len(os.sep):] for x in files]
        # remove files that are not in 'packages' i.e. not in directories
        # listed by tests_python/Makefile's PACKAGES variables
        files_ = [x for x in files_ if
                  any([x.startswith(p) for p in packages])]
        cmd = [tool] + files_
        print(f"{tests_python_basename}> " + " ".join(cmd))
        cmd_result = subprocess.run(cmd,
                                    cwd=tests_python_basename,
                                    check=False)
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
            return_code = max(return_code, _call_pytest(relevant_pytest_files))
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
    py_packages = py_data[1]
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
        2/ Whether pytest should be called
        3/ Whether the hook should test itself instead of doing its normal
           operations
    """
    staged = _UNSTAGED not in sys.argv
    pytest = _LINT_ONLY not in sys.argv
    test_itself = _TEST_ITSELF in sys.argv
    return (staged, pytest, test_itself)


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

    staged, pytest, test_itself = _parse_arguments()
    if test_itself:
        print(f"Recognized {_TEST_ITSELF}")
        return _main_test_itself()
    adjective = "staged" if staged else "modified"

    return_code = _main_py_files(staged, adjective, pytest)

    ml_extensions = ["ml", "mli"]
    relevant_ocaml_files = _git_diff_many(staged, ml_extensions)
    if relevant_ocaml_files:
        _call_ocamlformat(relevant_ocaml_files, staged)
    else:
        extensions = "{" + ",".join(ml_extensions) + "}"
        print(f"No {adjective} *.{extensions} relevant file found")

    return return_code


if __name__ == "__main__":
    sys.exit(main())
