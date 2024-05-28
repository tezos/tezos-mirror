SHELL := /bin/bash

.PHONY: help
## help: Makefile: Prints this help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

.PHONY: lint
## lint: Terraform: Validate configuration files and run linter
lint:
	terraform validate
	tflint

.PHONY: format
## format: Terraform: Rewrite configuration files to a canonical format
format:
	terraform fmt -recursive .

.PHONY: init
## init: Terraform: Initialize
init:
	terraform init --upgrade
	tflint --init

.PHONY: plan
## plan: Terraform: Plan
plan:
	terraform plan

.PHONY: apply
## apply: Terraform: Apply
apply:
	terraform apply

.PHONY: state
## state: Terraform: List resources in the state
state:
	terraform state list
