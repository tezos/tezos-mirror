# `teztale`

`teztale` is a tool that gathers information on arrival times of
tezos blocks and consensus operations.

It is splitted in 2 parts:

- `octez-teztale-archiver` that monitors an octez node and feed the server
- `octez-teztale-server` which aggregates and delivers data gathered by
  archiver(s)

## Running

### Server

First, you need to write a configuration file. It is a json file
folowing the schema:

```json
{
"db": "path_to_the_database",
"interfaces": [ { "address":"::1","port":8080 },
                { "address":"0.0.0.0","port":8443,
                  "tls":{ "certfile":"ssl/cert.pem",
                          "keyfile":"ssl/key.pem" }
                }
                <list_of_network_point_to_bind_to>
              ],
"admins": [ {"login":"admin","password":"zzz"},
           <admins_allowed_to_add_remove_users>
          ],
"users": [ {"login":"alice","password":"secret"},
           {"login":"bob","password":"xxx"}
           <archivers_allowed_to_post_data>
         ],
"max_batch_size": <number_of_levels_queryable_at_once>,
"public_directory": "optionally_a_path_to_a_build_of_the_frontend"
}
```

Note: allowed archivers are actually stored in the database. The list `users`
from the config file will automatically be added to the database at launch.
Admins are not stored in the database, so you need to update your configuration
and restart your server if you want to modify them.

You can know the list of users allowed (but not the password) with

```
curl http://localhost:8080/users
```

Adding a user can be achieve with

```
curl -X PUT 'http://admin:zzz@localhost:8080/user' -d '{ "login" : "michmuch", "password" : "foobar" }'
```

Removing a user can be achieve with

```
curl -X DELETE 'http://admin:zzz@localhost:8080/user' -d '{ "login" : "michmuch" }'
```

You can launch the server by giving your config file as argument like

```
octez-teztale-server ./config.json
```

### Archiver

Run an octez node, with a RPC server open on localhost:8732 and do

```
octez-teztale-archiver [--endpoint OCTEZ_NODE_URL:OCTEZ_NODE_RPC_PORT] feed ARCHIVER_LOGIN:ARCHIVER_PASSWORD@SERVER_URL:SERVER_PORT

```

`octez-teztale-archiver` is an octez client, it thus accepts the same global
options and default values as the client (especially the `--endpoint` option).
