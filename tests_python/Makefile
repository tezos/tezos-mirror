TYPECHECK=mypy
LINT=pylint
LINT2=pycodestyle
PACKAGES=daemons launchers client tools scripts tests examples tests/multibranch
LOG_DIR=tmp

fast:
	pytest -m "not slow"

all:
	mkdir -p $(LOG_DIR)
	pytest --log-dir=tmp --tb=no

lint_all: lint lint2

typecheck:
	@echo "Typechecking with mypy version `mypy --version`"
	$(TYPECHECK) $(PACKAGES)

lint:
	@echo "Linting with pylint, version:"
	@pylint --version | sed 's/^/  /'
	$(LINT) $(PACKAGES)

lint2:
	@echo "Linting with pycodestyle version `pycodestyle --version`"
	$(LINT2) $(PACKAGES)

clean:
	rm -rf tmp/*  __pycache__ *.pyc */__pycache__ */*.pyc .mypy_cache .pytest_cache .pytype
