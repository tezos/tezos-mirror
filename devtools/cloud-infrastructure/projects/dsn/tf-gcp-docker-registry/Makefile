SHELL := /bin/bash

.PHONY: help
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

.PHONY: lint
lint:
	terraform validate
	tflint

.PHONY: format
format:
	terraform fmt -recursive .

.PHONY: init
init:
	terraform init --upgrade
	tflint --init

.PHONY: plan
plan:
	terraform plan -compact-warnings

.PHONY: apply
apply:
	terraform apply -compact-warnings

.PHONY: apply-auto-approve
apply-auto-approve:
	terraform apply -auto-approve -compact-warnings

.PHONY: state
state:
	terraform state list
