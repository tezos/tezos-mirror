TYPECHECK=mypy
# pytest causes redefined-outer-name and no-self-use
LINT=pylint -d redefined-outer-name,no-self-use,missing-docstring,too-many-arguments,too-few-public-methods,fixme,too-many-locals,too-many-public-methods,too-many-instance-attributes,R0801,c-extension-no-member
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
	$(TYPECHECK) $(PACKAGES)

lint:
	$(LINT) $(PACKAGES)

lint2:
	$(LINT2) $(PACKAGES)

clean:
	rm -rf tmp/*  __pycache__ *.pyc */__pycache__ */*.pyc .mypy_cache .pytest_cache .pytype
