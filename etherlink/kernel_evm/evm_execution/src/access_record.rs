use alloc::collections::btree_set::BTreeSet;
use primitive_types::{H160, H256};

#[derive(Eq, Clone, PartialOrd, PartialEq, Ord, Debug)]
struct AddressIndex(H160, H256);

// TODO: Implement caching for accessed storage value
// issue: https://gitlab.com/tezos/tezos/-/issues/6879
#[derive(Clone, Debug)]
pub struct AccessRecord {
    accessed_storage_keys: BTreeSet<AddressIndex>,
    accessed_addresses: BTreeSet<H160>,
}

impl AccessRecord {
    pub fn new() -> Self {
        AccessRecord {
            accessed_storage_keys: BTreeSet::new(),
            accessed_addresses: BTreeSet::new(),
        }
    }

    pub fn insert_storage(&mut self, address: H160, index: H256) {
        self.accessed_storage_keys
            .insert(AddressIndex(address, index));
    }

    pub fn contains_storage(&self, address: H160, index: H256) -> bool {
        self.accessed_storage_keys
            .contains(&AddressIndex(address, index))
    }

    pub fn insert_address(&mut self, address: H160) {
        self.accessed_addresses.insert(address);
    }

    pub fn contains_address(&self, address: H160) -> bool {
        self.accessed_addresses.contains(&address)
    }
}
