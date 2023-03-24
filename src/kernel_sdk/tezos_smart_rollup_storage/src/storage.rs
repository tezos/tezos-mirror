// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Storage API for transactional account updates.

use crate::layer::Layer;
use crate::StorageError;
use core::marker::PhantomData;
use tezos_smart_rollup_host::path::{OwnedPath, Path};
use tezos_smart_rollup_host::runtime::Runtime;

extern crate alloc;

/// Account storage interface
pub struct Storage<T: From<OwnedPath>> {
    prefix: String,
    layers: Vec<Layer<T>>,
    phantom: PhantomData<T>,
}

impl<T: From<OwnedPath>> Storage<T> {
    /// Create the initial storage
    pub fn init(name: &impl Path) -> Result<Self, StorageError> {
        let name_bytes = name.as_bytes().to_vec();

        Ok(Self {
            prefix: String::from_utf8(name_bytes)
                .map_err(|_| StorageError::InvalidAccountsPath)?,
            layers: vec![Layer::<T>::with_path(name)],
            phantom: PhantomData,
        })
    }

    /// Get account in state given by current transaction
    pub fn get_account(
        &self,
        host: &impl Runtime,
        id: &impl Path,
    ) -> Result<Option<T>, StorageError> {
        if let Some(top_layer) = self.layers.last() {
            Ok(top_layer.get_account(host, id)?)
        } else {
            Err(StorageError::NoStorage)
        }
    }

    /// Get immutable account in state it had before any transaction began
    pub fn get_original_account(
        &self,
        host: &impl Runtime,
        id: &impl Path,
    ) -> Result<Option<T>, StorageError> {
        if let Some(bottom_layer) = self.layers.first() {
            Ok(bottom_layer.get_account(host, id)?)
        } else {
            Err(StorageError::NoStorage)
        }
    }

    /// Create a new account as part of current transaction
    pub fn new_account(
        &mut self,
        host: &mut impl Runtime,
        id: &impl Path,
    ) -> Result<Option<T>, StorageError> {
        if let Some(top_layer) = self.layers.last_mut() {
            Ok(top_layer.new_account(host, id)?)
        } else {
            Err(StorageError::NoStorage)
        }
    }

    pub fn get_or_create_account(
        &self,
        host: &impl Runtime,
        id: &impl Path,
    ) -> Result<T, StorageError> {
        if let Some(top_layer) = self.layers.last() {
            Ok(top_layer.get_or_create_account(host, id)?)
        } else {
            Err(StorageError::NoStorage)
        }
    }

    /// Delete an account as part of current transaction
    pub fn delete_account(
        &mut self,
        host: &mut impl Runtime,
        id: &impl Path,
    ) -> Result<(), StorageError> {
        if let Some(top_layer) = self.layers.last_mut() {
            top_layer.delete_account(host, id)
        } else {
            Err(StorageError::NoStorage)
        }
    }

    /// Begin a new transaction
    pub fn begin_transaction(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<(), StorageError> {
        let new_layer_index = self.layers.len() + 1;
        if let Some(top) = self.layers.last() {
            let new_layer_name = alloc::format!("{}.{}", self.prefix, new_layer_index);
            let new_layer_path = OwnedPath::try_from(new_layer_name.as_bytes().to_vec())?;
            let new_top = top.make_copy(host, &new_layer_path)?;
            self.layers.push(new_top);
            Ok(())
        } else {
            Err(StorageError::NoStorage)
        }
    }

    /// Commit current transaction
    pub fn commit(&mut self, host: &mut impl Runtime) -> Result<(), StorageError> {
        if self.layers.len() > 1 {
            if let (Some(top), Some(last)) = (self.layers.pop(), self.layers.last_mut()) {
                last.consume(host, top)
            } else {
                panic!("Could not commit transaction")
            }
        } else {
            Err(StorageError::NoCurrentTransaction)
        }
    }

    /// Abort current transaction
    pub fn rollback(&mut self, host: &mut impl Runtime) -> Result<(), StorageError> {
        if self.layers.len() > 1 {
            if let Some(top) = self.layers.pop() {
                top.discard(host)
            } else {
                panic!("Could not rollback transaction")
            }
        } else {
            Err(StorageError::NoCurrentTransaction)
        }
    }

    /// Get the number of active transactions, ie, the depth of the transaction stack.
    /// Note that the bottome layer of the transaction stack is _not_ a transaction
    /// itself, but rather the original storage before any transaction currently in progress.
    pub fn transaction_depth(&self) -> usize {
        self.layers.len() - 1
    }
}

#[cfg(test)]
mod test {
    use crate::storage::Storage;
    use host::path::{concat, OwnedPath, RefPath};
    use host::runtime::Runtime;
    use tezos_smart_rollup_mock::MockHost;

    #[derive(PartialEq, Debug)]
    struct TestAccount {
        path: OwnedPath,
    }

    const VALUE_A_PATH: RefPath = RefPath::assert_from(b"/a");
    const VALUE_B_PATH: RefPath = RefPath::assert_from(b"/b");

    impl TestAccount {
        pub fn set_a(&mut self, host: &mut impl Runtime, v: &str) {
            let value_path = concat(&self.path, &VALUE_A_PATH)
                .expect("The account should have a path for a");
            host.store_write(&value_path, v.as_bytes(), 0)
                .expect("Cannot set value for b")
        }

        pub fn set_b(&mut self, host: &mut impl Runtime, v: &str) {
            let value_path = concat(&self.path, &VALUE_B_PATH)
                .expect("The account should have a path for b");
            host.store_write(&value_path, v.as_bytes(), 0)
                .expect("Cannot set value for b")
        }

        pub fn get_a(&self, host: &impl Runtime) -> Vec<u8> {
            let value_path = concat(&self.path, &VALUE_A_PATH)
                .expect("The account should have a path for a");
            host.store_read(&value_path, 0, 1024)
                .expect("No value for a")
        }

        pub fn get_b(&self, host: &impl Runtime) -> Vec<u8> {
            let value_path = concat(&self.path, &VALUE_B_PATH)
                .expect("The account should have a path for b");
            host.store_read(&value_path, 0, 1024)
                .expect("No value for b")
        }
    }

    impl From<OwnedPath> for TestAccount {
        fn from(path: OwnedPath) -> Self {
            Self { path }
        }
    }

    const ACCOUNTS_PATH: RefPath = RefPath::assert_from(b"/accounts");

    #[test]
    fn test_commit() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        let a1_name = RefPath::assert_from(b"/alpha");
        let a2_name = RefPath::assert_from(b"/beta");

        let mut a1 = storage
            .new_account(&mut host, &a1_name)
            .expect("Unable to get first account")
            .expect("No account in storage");
        let mut a2 = storage
            .new_account(&mut host, &a2_name)
            .expect("Unable to get second account")
            .expect("No account in storage");

        a1.set_a(&mut host, "a1");
        a1.set_b(&mut host, "b1");
        a2.set_a(&mut host, "a2");
        a2.set_b(&mut host, "b2");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        let mut a = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage during transaction");

        a.set_a(&mut host, "a11");
        a.set_b(&mut host, "b11");

        storage
            .commit(&mut host)
            .expect("Cannot commit transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage after commit");
        assert_eq!(a11.get_a(&host), b"a11");
        assert_eq!(a11.get_b(&host), b"b11");

        let a21 = storage
            .get_account(&host, &a2_name)
            .expect("Cannot get account a1")
            .expect("No account a2 in storage after commit");
        assert_eq!(a21.get_a(&host), b"a2");
        assert_eq!(a21.get_b(&host), b"b2");
    }

    #[test]
    fn test_rollback() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        let a1_name = RefPath::assert_from(b"/alpha");
        let a2_name = RefPath::assert_from(b"/beta");

        let mut a1 = storage
            .new_account(&mut host, &a1_name)
            .expect("Unable to get first account")
            .expect("No account in storage");
        let mut a2 = storage
            .new_account(&mut host, &a2_name)
            .expect("Unable to get second account")
            .expect("No account in storage");

        a1.set_a(&mut host, "a1");
        a1.set_b(&mut host, "b1");
        a2.set_a(&mut host, "a2");
        a2.set_b(&mut host, "b2");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        let mut a = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage during transaction");

        a.set_a(&mut host, "a11");
        a.set_b(&mut host, "b11");

        storage
            .rollback(&mut host)
            .expect("Cannot rollback transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage after commit");
        assert_eq!(a11.get_a(&host), b"a1");
        assert_eq!(a11.get_b(&host), b"b1");

        let a21 = storage
            .get_account(&host, &a2_name)
            .expect("Cannot get account a1")
            .expect("No account a2 in storage after commit");
        assert_eq!(a21.get_a(&host), b"a2");
        assert_eq!(a21.get_b(&host), b"b2");
    }

    #[test]
    fn test_original_account() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        let a1_name = RefPath::assert_from(b"/alpha");
        let a2_name = RefPath::assert_from(b"/beta");

        let mut a1 = storage
            .new_account(&mut host, &a1_name)
            .expect("Unable to get first account")
            .expect("No account in storage");
        let mut a2 = storage
            .new_account(&mut host, &a2_name)
            .expect("Unable to get second account")
            .expect("No account in storage");

        a1.set_a(&mut host, "a1");
        a1.set_b(&mut host, "b1");
        a2.set_a(&mut host, "a2");
        a2.set_b(&mut host, "b2");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        let mut a = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage during transaction");
        a.set_a(&mut host, "a11");
        a.set_b(&mut host, "b11");

        // Test original account content while inside transaction
        let oa = storage
            .get_original_account(&host, &a1_name)
            .expect("Cannot get original account a1")
            .expect("No original account a1 in storage during transaction");
        assert_eq!(oa.get_a(&host), b"a1");
        assert_eq!(oa.get_b(&host), b"b1");

        // Commit
        storage
            .commit(&mut host)
            .expect("Cannot commit transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage after commit");
        assert_eq!(a11.get_a(&host), b"a11");
        assert_eq!(a11.get_b(&host), b"b11");

        let a21 = storage
            .get_account(&host, &a2_name)
            .expect("Cannot get account a1")
            .expect("No account a2 in storage after commit");
        assert_eq!(a21.get_a(&host), b"a2");
        assert_eq!(a21.get_b(&host), b"b2");
    }

    #[test]
    fn create_new_account_in_transaction() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        let a1_name = RefPath::assert_from(b"/alpha");

        // Arrange
        // - start with no accounts in storage

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        let mut a = storage
            .new_account(&mut host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage during transaction");
        a.set_a(&mut host, "a1");
        a.set_b(&mut host, "b1");

        let oa = storage
            .get_original_account(&host, &a1_name)
            .expect("Cannot get original account a1");

        assert_eq!(oa, None);

        // Commit
        storage
            .commit(&mut host)
            .expect("Cannot commit transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage after commit");
        assert_eq!(a11.get_a(&host), b"a1");
        assert_eq!(a11.get_b(&host), b"b1");
    }

    #[test]
    fn do_nothing() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        // - start with no accounts in storage

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        // Commit
        storage
            .commit(&mut host)
            .expect("Cannot commit transaction");

        // Assert
        // - nothing to assert as store is all empty
    }

    #[test]
    fn delete_account_in_transaction() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        let a1_name = RefPath::assert_from(b"/alpha");
        let a2_name = RefPath::assert_from(b"/beta");

        let mut a1 = storage
            .new_account(&mut host, &a1_name)
            .expect("Unable to get first account")
            .expect("No account in storage");
        let mut a2 = storage
            .new_account(&mut host, &a2_name)
            .expect("Unable to get second account")
            .expect("No account in storage");

        a1.set_a(&mut host, "a1");
        a1.set_b(&mut host, "b1");
        a2.set_a(&mut host, "a2");
        a2.set_b(&mut host, "b2");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        storage
            .delete_account(&mut host, &a1_name)
            .expect("Cannot delete account");

        storage
            .commit(&mut host)
            .expect("Cannot commit transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1");
        assert_eq!(a11, None);

        let a21 = storage
            .get_account(&host, &a2_name)
            .expect("Cannot get account a1")
            .expect("No account a2 in storage after commit");
        assert_eq!(a21.get_a(&host), b"a2");
        assert_eq!(a21.get_b(&host), b"b2");
    }

    #[test]
    fn delete_all_accounts_in_transaction() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        let a1_name = RefPath::assert_from(b"/alpha");
        let a2_name = RefPath::assert_from(b"/beta");

        let mut a1 = storage
            .new_account(&mut host, &a1_name)
            .expect("Unable to get first account")
            .expect("No account in storage");
        let mut a2 = storage
            .new_account(&mut host, &a2_name)
            .expect("Unable to get second account")
            .expect("No account in storage");

        a1.set_a(&mut host, "a1");
        a1.set_b(&mut host, "b1");
        a2.set_a(&mut host, "a2");
        a2.set_b(&mut host, "b2");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        storage
            .delete_account(&mut host, &a1_name)
            .expect("Cannot delete account a1");
        storage
            .delete_account(&mut host, &a2_name)
            .expect("Cannot delete account a2");

        storage
            .commit(&mut host)
            .expect("Cannot commit transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1");
        assert_eq!(a11, None);

        let a21 = storage
            .get_account(&host, &a2_name)
            .expect("Cannot get account a1");
        assert_eq!(a21, None);
    }

    #[test]
    fn delete_account_but_rollback() {
        let mut host = MockHost::default();

        let mut storage = Storage::<TestAccount>::init(&ACCOUNTS_PATH)
            .expect("Could not create basic storage interface@");

        // Arrange
        let a1_name = RefPath::assert_from(b"/alpha");
        let a2_name = RefPath::assert_from(b"/beta");

        let mut a1 = storage
            .new_account(&mut host, &a1_name)
            .expect("Unable to get first account")
            .expect("No account in storage");
        let mut a2 = storage
            .new_account(&mut host, &a2_name)
            .expect("Unable to get second account")
            .expect("No account in storage");

        a1.set_a(&mut host, "a1");
        a1.set_b(&mut host, "b1");
        a2.set_a(&mut host, "a2");
        a2.set_b(&mut host, "b2");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Cannot begin transaction");

        storage
            .delete_account(&mut host, &a1_name)
            .expect("Cannot delete account a1");
        storage
            .delete_account(&mut host, &a2_name)
            .expect("Cannot delete account a2");

        storage
            .rollback(&mut host)
            .expect("Cannot rollback transaction");

        // Assert
        let a11 = storage
            .get_account(&host, &a1_name)
            .expect("Cannot get account a1")
            .expect("No account a1 in storage after commit");
        assert_eq!(a11.get_a(&host), b"a1");
        assert_eq!(a11.get_b(&host), b"b1");

        let a21 = storage
            .get_account(&host, &a2_name)
            .expect("Cannot get account a1")
            .expect("No account a2 in storage after commit");
        assert_eq!(a21.get_a(&host), b"a2");
        assert_eq!(a21.get_b(&host), b"b2");
    }
}
