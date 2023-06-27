# `teztale`

`teztale` is a tool that gathers information on arrival times of
blocks and consensus operations.

It is splitted in 2 parts:

- `teztale-archiver` that monitors an octez node
- `teztale-server` which aggregates and delivers data gathered by
  archivers

## Building

- Install dependencies by doing `opam install octez caqti-lwt
  caqti-driver-sqlite3 caqti-driver-postgresql safepass`
- clone this repo, `cd teztale` and do `make`

(As an alternative to install dependencies, clone this repo at the
root of an octez worktree)

## Running

### Aggregator

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
"users": [ {"login":"alice","password":"secret"},
           {"login":"bob","password":"xxx"}
           <archivers_allowed_to_post_data>
         ],
"admins": [ {"login":"admin","password":"zzz"},
           <admins_allowed_to_add_remove_users>
          ]
}
```

Note: allowed archivers are actually stored in the database. The list `users`
from the config file will automatically be added to the database at launch.
Admins are not stored in the database, so you need to update your configuration
and restart your server if you want to modify them.

You can know the list of users allowed (but not the password) with

```
curl http://0.0.0.0:8443/users
```

Adding a user can be achieve with

```
curl -X PUT 'http://admin:zzz@0.0.0.0:8880/user' -d '{ "login" : "michmuch", "password" : "foobar" }'
```

Removing a user can be achieve with

```
curl -X DELETE 'http://admin:zzz@0.0.0.0:8880/user' -d '{ "login" : "michmuch" }'
```

You can launch the server by giving your config file as argument like

```
server/main.exe ./config.json
```

For testing purpose **only**, you can use `server/main.exe
examples/config.json`

### Archiver

Run an octez node, with a RPC server open on localhost:8732 and do

```
archiver/main.exe feed <teztale-server-endpoint-including-login:password>
```

`teztale-archiver` is an octez client, it thus accepts the same global
options as the client (especially the `--endpoint` option to specify
where to reach octez node RPC endpoint).

## Docker images

Docker images containing the binaries are available in the docker
image registry of the project on gitlab.

You can take inspiration from the <./docker-compose.yml> file provided
to set them up. You must at least edit the config.json file mounted in
the teztale-server container to move away of the super unsecure
<examples/config.json>! (and the login/password used by the
teztale-archiver container accordingly).
