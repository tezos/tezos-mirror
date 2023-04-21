Transactional account-storage for Tezos Smart Rollup kernels.

This crate supports dealing with accounts and transactions for updating
said accounts' storage. All accounts are stored in durable storage.

To use this crate, provide a definition of an account. The account structure
should follow these guidelines:

- it can be created from an [OwnedPath], ie, it implements `From<OwnedPath>`
- it has getters and setters that operate directly on durable storage, each
  getter and setter should take a [Runtime] as argument to do so (`mut` in
  case of setters).

**NB** an account must only look in durable storage prefixed by its
`OwnedPath`.

To use this crate, create account struct and storage object like so:

```
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_host::path::{concat, RefPath, OwnedPath};
use tezos_smart_rollup_storage::storage::Storage;
use tezos_smart_rollup_mock::MockHost;

struct MyAccount {
  path: OwnedPath,
}

const VALUE_PATH: RefPath = RefPath::assert_from(b"/value");

impl MyAccount {
  pub fn setter(&mut self, host: &mut impl Runtime, v: &str) {
    let value_path = concat(&self.path, &VALUE_PATH)
        .expect("Could not get path for account value");
    host.store_write(&value_path, v.as_bytes(), 0)
        .expect("Could not set value for account");
  }

  pub fn getter(
      &mut self,
      host: &impl Runtime,
  ) -> Vec<u8> {
    let value_path = concat(&self.path, &VALUE_PATH)
        .expect("Could not get path for account value");
    host.store_read(&value_path, 0, 1024)
        .expect("Could not read account value")
  }
}

impl From<OwnedPath> for MyAccount {
  fn from(path: OwnedPath) -> Self {
    Self { path }
  }
}

const ACCOUNT_PATH: RefPath = RefPath::assert_from(b"/accounts");

let mut host = MockHost::default();

let mut storage = Storage::<MyAccount>::init(&ACCOUNT_PATH)
    .expect("Could not create storage interface");

storage.begin_transaction(&mut host)
    .expect("Could not begin new transaction");

let account_id = RefPath::assert_from(b"/my.account.id");

let mut account = storage.new_account(&mut host, &account_id)
    .expect("Could not create new account")
    .expect("Account already exists");

account.setter(&mut host, "some value");

storage.commit(&mut host)
    .expect("Could not commit transaction");
```

[OwnedPath]: host::path::OwnedPath
[Runtime]: host::runtime::Runtime
