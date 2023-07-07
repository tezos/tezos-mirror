# Octogram

Octogram is an abstraction layer on top of Tezt’s amazing orchestration
features whose purpose is to provide a scriptable interface to run complex and
large scale scenarios on remote runners.

Scenarios are written in a Yaml file whose structure takes heavy inspiration
from Ansible. As of now, Octogram supports the following features:

  - Define stages expected to run on a subset of the known agents defined by
    regular expressions. These regular expressions can capture part of the
    agent name to personalize each jobs. By default, agents run a stage
    concurrently, but it is also possible to run a stage sequentially on each
    agent.

  - Stages are defined as a collection of jobs which can either be run
    concurrently or sequentially. These jobs consists in executing a specific
    command. As of now, three commands are executed: (1) copy a file from the
    orchestrator to the agent, start a new Octez node on the agent, and (2)
    originate a new smart rollup from within the agent.

    Jobs have access to global variables. They get a copy of them when they are
    started, and they update the latest version of the global variables once
    they have finished to execute.

  - One job can be run several time using the `with_items' field. Each item in
    the list is derived into a unique job, when the `item' variable is
    accordingly set. By default, jobs derived fro a `with_items' field are run
    sequentially, but they can also be run concurrently if necessary. For
    instance, it is possible to concurrently deploy a list of files to an agent
    by listing them in a `with_items' field.

  - Jobs can update the global variables once they have completed. This is
    useful to transfer information over one job to the subsequent ones.

  - Allow cross-agent requests thanks to a global scheme of URI. As of now, it
    is possible to prefix the name of an Octez node chosen by Tezt by the name
    of the agent which manages them. So for instance, if the agent `node' has
    spawn an Octez node whose assigned name is `node1', then `node://node1' can
    be used whenever a node’s endpoint is expected.

## Syntax

Octogram reads a configuration file written in either YAML (`.yml`) or JSON
(`.json`).

This configuration consists in five sections:

- `octogram_binary` allows to specify the location of the Octogram binary used
  by the agents.
- `agents` lists the agents to spawn on remote machines.
- `prometheus_agent` (optional) allows to specify one particular agent
  responsible for managing a Prometheus server aggregating the metrics servers
  spawned during an experiment.
- `vars` allows to define global variables which can be used to define jobs,
  and updated at jobs completion.
- `stages` describes the scenario to run, as a sequence of jobs consisting
  in sequences of jobs to run on certain agents.
