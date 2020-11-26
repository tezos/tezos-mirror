TYPECHECK=poetry run mypy
LINT=poetry run pylint
LINT2=poetry run pycodestyle
PACKAGES=daemons launchers client tools scripts tests_alpha examples codec
LOG_DIR=tmp

install-dependencies:
	@poetry install

fast:
	poetry run pytest -m "not slow"

all:
	mkdir -p $(LOG_DIR)
	poetry run pytest --log-dir=tmp --tb=no

lint_all: lint lint2

typecheck:
	@echo "Typechecking with mypy version `poetry run mypy --version`"
	$(TYPECHECK) $(PACKAGES)

lint:
	@echo "Linting with pylint, version:"
	@poetry run pylint --version | sed 's/^/  /'
	$(LINT) $(PACKAGES)

lint2:
	@echo "Linting with pycodestyle version `poetry run pycodestyle --version` (`poetry run which pycodestyle`)"
	$(LINT2) $(PACKAGES)

clean:
	rm -rf tmp/*  __pycache__ *.pyc */__pycache__ */*.pyc .mypy_cache .pytest_cache .pytype
