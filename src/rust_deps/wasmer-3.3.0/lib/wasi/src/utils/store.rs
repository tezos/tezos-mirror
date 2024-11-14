/// A snapshot that captures the runtime state of an instance.
#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct InstanceSnapshot {
    /// Values of all globals, indexed by the same index used in Webassembly.
    pub globals: Vec<u128>,
}

impl InstanceSnapshot {
    pub fn serialize(&self) -> Result<Vec<u8>, bincode::Error> {
        bincode::serialize(self)
    }

    pub fn deserialize(data: &[u8]) -> Result<Self, bincode::Error> {
        bincode::deserialize(data)
    }
}

pub fn capture_snapshot(store: &mut impl wasmer::AsStoreMut) -> InstanceSnapshot {
    let objs = store.objects_mut();
    let globals = objs.as_u128_globals();
    InstanceSnapshot { globals }
}

pub fn restore_snapshot(store: &mut impl wasmer::AsStoreMut, snapshot: &InstanceSnapshot) {
    let objs = store.objects_mut();

    for (index, value) in snapshot.globals.iter().enumerate() {
        objs.set_global_unchecked(index, *value);
    }
}
