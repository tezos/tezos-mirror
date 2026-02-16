Transactional storage for Tezos Smart Rollup kernels.

This crate supports dealing with objects for updating storage.
All objects are stored in durable storage.

To use this crate, provide a definition of an object. The object structure
should follow these guidelines:

- it can be created from an [OwnedPath], ie, it implements `From<OwnedPath>`
- it has getters and setters that operate directly on durable storage, each
  getter and setter should take a [StorageV1] as argument to do so (`mut` in
  case of setters).

**NB** an object must only look in durable storage prefixed by its
`OwnedPath`.

To use this crate, create the wanted value struct and storage object like so:

```
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::path::{concat, RefPath, OwnedPath};
use tezos_smart_rollup_storage::storage::Storage;
use tezos_smart_rollup_mock::MockHost;

struct MyValue {
  path: OwnedPath,
}

const VALUE_PATH: RefPath = RefPath::assert_from(b"/value");

impl MyValue {
  pub fn setter(&mut self, host: &mut impl StorageV1, v: &str) {
    let value_path = concat(&self.path, &VALUE_PATH)
        .expect("Could not get path for value");
    host.store_write(&value_path, v.as_bytes(), 0)
        .expect("Could not set value");
  }

  pub fn getter(
      &mut self,
      host: &impl StorageV1,
  ) -> Vec<u8> {
    let value_path = concat(&self.path, &VALUE_PATH)
        .expect("Could not get path for value");
    host.store_read(&value_path, 0, 1024)
        .expect("Could not read value")
  }
}

impl From<OwnedPath> for MyValue {
  fn from(path: OwnedPath) -> Self {
    Self { path }
  }
}

const VALUES_PATH: RefPath = RefPath::assert_from(b"/values");

let mut host = MockHost::default();

let mut storage = Storage::<MyValue>::init(&VALUES_PATH)
    .expect("Could not create storage interface");

storage.begin_transaction(&mut host)
    .expect("Could not begin transaction");

let value_id = RefPath::assert_from(b"/my.value.id");

let mut value = storage.create_new(&mut host, &value_id)
    .expect("Could not create new value")
    .expect("Value already exists");

value.setter(&mut host, "some value");

storage.commit_transaction(&mut host)
    .expect("Could not commit transaction");
```

[OwnedPath]: host::path::OwnedPath
[StorageV1]: host::storage::StorageV1
