//! Generic Artifact abstraction for Wasmer Engines.

use crate::Features;
use enumset::EnumSet;
use std::any::Any;
use std::sync::Arc;
use wasmer_types::entity::PrimaryMap;
use wasmer_types::SerializeError;
use wasmer_types::{
    CpuFeature, MemoryIndex, MemoryStyle, ModuleInfo, OwnedDataInitializer, TableIndex, TableStyle,
};

/// An `Artifact` is the product that the `Engine`
/// implementation produce and use.
///
/// The `Artifact` contains the compiled data for a given
/// module as well as extra information needed to run the
/// module at runtime, such as [`ModuleInfo`] and [`Features`].
pub trait ArtifactCreate: Send + Sync + Upcastable {
    /// Create a `ModuleInfo` for instantiation
    fn create_module_info(&self) -> Arc<ModuleInfo>;

    /// Sets the `ModuleInfo` name
    fn set_module_info_name(&mut self, name: String) -> bool;

    /// Returns the `ModuleInfo` for instantiation
    fn module_info(&self) -> &ModuleInfo;

    /// Returns the features for this Artifact
    fn features(&self) -> &Features;

    /// Returns the CPU features for this Artifact
    fn cpu_features(&self) -> EnumSet<CpuFeature>;

    /// Returns the memory styles associated with this `Artifact`.
    fn memory_styles(&self) -> &PrimaryMap<MemoryIndex, MemoryStyle>;

    /// Returns the table plans associated with this `Artifact`.
    fn table_styles(&self) -> &PrimaryMap<TableIndex, TableStyle>;

    /// Returns data initializers to pass to `VMInstance::initialize`
    fn data_initializers(&self) -> &[OwnedDataInitializer];

    /// Serializes an artifact into bytes
    fn serialize(&self) -> Result<Vec<u8>, SerializeError>;
}

// Implementation of `Upcastable` taken from https://users.rust-lang.org/t/why-does-downcasting-not-work-for-subtraits/33286/7 .
/// Trait needed to get downcasting of `Engine`s to work.
pub trait Upcastable {
    /// upcast ref
    fn upcast_any_ref(&'_ self) -> &'_ dyn Any;
    /// upcast mut ref
    fn upcast_any_mut(&'_ mut self) -> &'_ mut dyn Any;
    /// upcast boxed dyn
    fn upcast_any_box(self: Box<Self>) -> Box<dyn Any>;
}

impl<T: Any + Send + Sync + 'static> Upcastable for T {
    #[inline]
    fn upcast_any_ref(&'_ self) -> &'_ dyn Any {
        self
    }
    #[inline]
    fn upcast_any_mut(&'_ mut self) -> &'_ mut dyn Any {
        self
    }
    #[inline]
    fn upcast_any_box(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl dyn ArtifactCreate + 'static {
    /// Try to downcast the artifact into a given type.
    #[inline]
    pub fn downcast_ref<T: 'static>(&'_ self) -> Option<&'_ T> {
        self.upcast_any_ref().downcast_ref::<T>()
    }

    /// Try to downcast the artifact into a given type mutably.
    #[inline]
    pub fn downcast_mut<T: 'static>(&'_ mut self) -> Option<&'_ mut T> {
        self.upcast_any_mut().downcast_mut::<T>()
    }
}
