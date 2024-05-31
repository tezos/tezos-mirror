# RPC_toy

The `RPC_toy` tool is used to isolate a resource leak which can appear if there are unclosed connections when running streamed RPCs.

## Running scenario

To test if the leak exists, you need to run the `leak.sh` automated script. There are two potential outputs:

### Leak is present

```shell
$ cd ~/tezos/contrib/RPC_toy

$ dune build

$ ./leak.sh

Entering directory <tezos_directory>
Starting node server                     
Node PID: <node_pid>
[Server response] : Entering directory <tezos_directory>
[Server response] : Leaving directory <tezos_directory>
[Server response] : Server running on port 8080
Make self-loop request to server
[Server response] : Cohttp connection on 1
[Server response] : %!
[Server response] : I slept
[Server response] : I slept
[Server response] : I slept
[Server response] : I slept
Attempting to cancel the self-loop request
Cancelled the self-loop request
[IMPORTANT] Cohttp connection did NOT CLOSE
Attempting to stop the node server
Stopped the node server
```

### Leak is NOT present

```shell
$ cd ~/tezos/contrib/RPC_toy

$ dune build

$ ./leak.sh

Entering directory <tezos_directory>
Starting node server                     
Node PID: <node_pid>
[Server response] : Entering directory <tezos_directory>
[Server response] : Leaving directory <tezos_directory>
[Server response] : Server running on port 8080
Make self-loop request to server
[Server response] : Cohttp connection on 1
[Server response] : %!
[Server response] : I slept
[Server response] : I slept
[Server response] : I slept
[Server response] : I slept
Attempting to cancel the self-loop request
Cancelled the self-loop request
[Server response] : Cohttp connection closed
[IMPORTANT] Cohttp connection CLOSED
Attempting to stop the node server
Stopped the node server
```
