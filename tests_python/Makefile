TYPECHECK=poetry run mypy

# pylint does not support pyproject.toml configuration for now
# It is configured by ./.pylint.rc
# In pylint, we ignore the following additional errors:
# - C0330: pylint is wrong (https://github.com/psf/black/issues/48)
# The warnings below are disabled due to the large number of files that would be affected.
# - unspecified-encoding (W1514)
# - consider-using-with (R1732)
# - consider-using-dict-items (C0206)
# - consider-using-f-string (C0209)
PYLINTRC?=../.pylintrc
PYLINT=PYLINTRC=${PYLINTRC} poetry run pylint \
			 --disable C0330 \
			 --disable W1514 \
			 --disable R1732 \
			 --disable C0206 \
			 --disable C0209

# pycodestyle does not support pyproject.toml configuration for now
# https://github.com/PyCQA/pydocstyle/issues/447
#
# In pycodestyle we ignore the following additional errors w.r.t to defaults
# (i.e. E121,E123,E126,E226,E24,E704,W503)
# - E203 :: pycodestyle is wrong (https://github.com/psf/black/issues/315)
#           and black takes care of it
# - E231 :: same as E203
#
# Other error codes are detailed at https://www.flake8rules.com/
PYCODESTYLE=poetry run pycodestyle
PYCODESTYLE_OPTIONS=\
	--ignore=E121,E123,E126,E226,E24,E704,W503,E203,E231 \
	--max-line-length=80

# black is configured in pyproject.toml
BLACK=poetry run black

SRCS?=pytest_plugins process daemons launchers client tools scripts tests_*  examples codec conftest.py

LOG_DIR=tmp

install-dependencies:
	@poetry install

fast:
	poetry run pytest -m "not slow"

all:
	mkdir -p $(LOG_DIR)
	poetry run pytest --log-dir=tmp --tb=no

alpha:
	mkdir -p $(LOG_DIR)
	poetry run pytest --log-dir=tmp --tb=no tests_alpha

tenderbake:
	mkdir -p $(LOG_DIR)
	poetry run pytest --log-dir=tmp --tb=no -m tenderbake

lint: pylint pycodestyle lint_black

# Analyses that we want to run as part of pre-commit hook.
pre_commit_targets: lint typecheck lint_black

fmt: black

typecheck:
	@echo "Typechecking with mypy version `poetry run mypy --version`"
	$(TYPECHECK) $(SRCS)

# black options are set in pyproject.toml
black:
	@echo "Running black formatter"
	$(BLACK) $(SRCS)

lint_black:
	@echo "Running black formatter as style checker"
	$(BLACK) --check $(SRCS)

pylint:
	@echo "Linting with pylint, version:"
	@poetry run pylint --version | sed 's/^/  /'
	$(PYLINT) $(SRCS)

pycodestyle:
	@echo "Linting with pycodestyle version `poetry run pycodestyle --version` (`poetry run which pycodestyle`)"
	$(PYCODESTYLE) $(PYCODESTYLE_OPTIONS) $(SRCS)

clean:
	rm -rf tmp/*  __pycache__ *.pyc */__pycache__ */*.pyc .mypy_cache .pytest_cache .pytype
