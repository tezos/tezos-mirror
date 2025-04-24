// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::CSRRepr;
use crate::bits::Bits64;
use crate::bits::ConstantBits;
use crate::machine_state::csregisters::effects::CSREffect;
use crate::machine_state::csregisters::effects::XieEffect;
use crate::machine_state::csregisters::xstatus::ExtensionValue;
use crate::machine_state::csregisters::xstatus::MPPValue;
use crate::machine_state::csregisters::xstatus::MStatus;
use crate::machine_state::csregisters::xstatus::SPPValue;
use crate::machine_state::csregisters::xstatus::XLenValue;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::EffectCell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Ref;
use crate::struct_layout;

/// RISCV CSRegister.mstatus register state.
/// Accounts for CSR rules like WPRI, WARL, WLRL.
/// Contains only real fields (no shadows) hence it is a public field in [`super::CSRValues`]
pub struct MStatusValue<M: ManagerBase> {
    // Individual fields can be public since they are well typed and respect the WPRI, WARL, WLRL rules.
    // Except for fields which have side-effects. These ones have custom read/write/replace methods
    // to return side-effects to be accounted for
    pub sie: EffectCell<bool, XieEffect, M>,
    pub mie: EffectCell<bool, XieEffect, M>,
    pub spie: Cell<bool, M>,
    pub ube: Cell<bool, M>,
    pub mpie: Cell<bool, M>,
    pub spp: Cell<SPPValue, M>,
    pub mpp: Cell<MPPValue, M>,
    pub fs: Cell<ExtensionValue, M>,
    pub xs: Cell<ExtensionValue, M>,
    // vs is always OFF as we do not support the virtualisation extension
    pub mprv: Cell<bool, M>,
    pub sum: Cell<bool, M>,
    pub mxr: Cell<bool, M>,
    pub tvm: Cell<bool, M>,
    pub tw: Cell<bool, M>,
    pub tsr: Cell<bool, M>,
    pub uxl: Cell<XLenValue, M>,
    pub sxl: Cell<XLenValue, M>,
    pub sbe: Cell<bool, M>,
    pub mbe: Cell<bool, M>,
}

impl<M: ManagerBase> MStatusValue<M> {
    pub fn bind(space: AllocatedOf<MStatusLayout, M>) -> Self {
        Self {
            sie: EffectCell::bind(space.sie),
            mie: EffectCell::bind(space.mie),
            spie: space.spie,
            ube: space.ube,
            mpie: space.mpie,
            spp: space.spp,
            mpp: space.mpp,
            fs: space.fs,
            xs: space.xs,
            mprv: space.mprv,
            sum: space.sum,
            mxr: space.mxr,
            tvm: space.tvm,
            tw: space.tw,
            tsr: space.tsr,
            uxl: space.uxl,
            sxl: space.sxl,
            sbe: space.sbe,
            mbe: space.mbe,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<MStatusLayout, F::Output> {
        MStatusLayoutF {
            sie: self.sie.struct_ref::<F>(),
            mie: self.mie.struct_ref::<F>(),
            spie: self.spie.struct_ref::<F>(),
            ube: self.ube.struct_ref::<F>(),
            mpie: self.mpie.struct_ref::<F>(),
            spp: self.spp.struct_ref::<F>(),
            mpp: self.mpp.struct_ref::<F>(),
            fs: self.fs.struct_ref::<F>(),
            xs: self.xs.struct_ref::<F>(),
            mprv: self.mprv.struct_ref::<F>(),
            sum: self.sum.struct_ref::<F>(),
            mxr: self.mxr.struct_ref::<F>(),
            tvm: self.tvm.struct_ref::<F>(),
            tw: self.tw.struct_ref::<F>(),
            tsr: self.tsr.struct_ref::<F>(),
            uxl: self.uxl.struct_ref::<F>(),
            sxl: self.sxl.struct_ref::<F>(),
            sbe: self.sbe.struct_ref::<F>(),
            mbe: self.mbe.struct_ref::<F>(),
        }
    }
}

impl<M: ManagerBase> NewState<M> for MStatusValue<M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        MStatusValue {
            sie: EffectCell::new(manager),
            mie: EffectCell::new(manager),
            spie: Cell::new(manager),
            ube: Cell::new(manager),
            mpie: Cell::new(manager),
            spp: Cell::new(manager),
            mpp: Cell::new(manager),
            fs: Cell::new(manager),
            xs: Cell::new(manager),
            mprv: Cell::new(manager),
            sum: Cell::new(manager),
            mxr: Cell::new(manager),
            tvm: Cell::new(manager),
            tw: Cell::new(manager),
            tsr: Cell::new(manager),
            uxl: Cell::new(manager),
            sxl: Cell::new(manager),
            sbe: Cell::new(manager),
            mbe: Cell::new(manager),
        }
    }
}

impl<M: ManagerClone> Clone for MStatusValue<M> {
    fn clone(&self) -> Self {
        Self {
            sie: self.sie.clone(),
            mie: self.mie.clone(),
            spie: self.spie.clone(),
            ube: self.ube.clone(),
            mpie: self.mpie.clone(),
            spp: self.spp.clone(),
            mpp: self.mpp.clone(),
            fs: self.fs.clone(),
            xs: self.xs.clone(),
            mprv: self.mprv.clone(),
            sum: self.sum.clone(),
            mxr: self.mxr.clone(),
            tvm: self.tvm.clone(),
            tw: self.tw.clone(),
            tsr: self.tsr.clone(),
            uxl: self.uxl.clone(),
            sxl: self.sxl.clone(),
            sbe: self.sbe.clone(),
            mbe: self.mbe.clone(),
        }
    }
}

struct_layout! {
    pub struct MStatusLayout {
        pub sie: Atom<bool>,
        pub mie: Atom<bool>,
        pub spie: Atom<bool>,
        pub ube: Atom<bool>,
        pub mpie: Atom<bool>,
        pub spp: Atom<SPPValue>,
        pub mpp: Atom<MPPValue>,
        pub fs: Atom<ExtensionValue>,
        pub xs: Atom<ExtensionValue>,
        pub mprv: Atom<bool>,
        pub sum: Atom<bool>,
        pub mxr: Atom<bool>,
        pub tvm: Atom<bool>,
        pub tw: Atom<bool>,
        pub tsr: Atom<bool>,
        pub uxl: Atom<XLenValue>,
        pub sxl: Atom<XLenValue>,
        pub sbe: Atom<bool>,
        pub mbe: Atom<bool>,
    }
}

#[inline(always)]
fn compute_sd(fs: ExtensionValue, xs: ExtensionValue) -> bool {
    fs == ExtensionValue::Dirty || xs == ExtensionValue::Dirty
}

// Impl block for fields which are derived from other values or do not need to be stored in the backend.
impl<M: ManagerBase> MStatusValue<M> {
    /// Read mstatus.fs field
    #[inline(always)]
    pub fn read_sd(&self) -> bool
    where
        M: ManagerRead,
    {
        compute_sd(self.fs.read(), self.xs.read())
    }

    /// Read `mstatus.vs` field. For our implementation, this is a constant.
    #[inline(always)]
    pub const fn read_vs(&self) -> ExtensionValue {
        ExtensionValue::Off
    }
}

// This impl block is here for compatibility with the bits api.
impl<M: ManagerBase> MStatusValue<M> {
    /// Read mstatus as in its 64 bit representation
    #[inline]
    pub fn read(&self) -> CSRRepr
    where
        M: ManagerRead,
    {
        let mstatus = &self;
        let fs = mstatus.fs.read();
        let xs = mstatus.xs.read();
        MStatus::new(
            ConstantBits,
            mstatus.sie.read(),
            ConstantBits,
            mstatus.mie.read(),
            ConstantBits,
            mstatus.spie.read(),
            mstatus.ube.read(),
            mstatus.mpie.read(),
            mstatus.spp.read(),
            ConstantBits,
            mstatus.mpp.read(),
            fs,
            xs,
            mstatus.mprv.read(),
            mstatus.sum.read(),
            mstatus.mxr.read(),
            mstatus.tvm.read(),
            mstatus.tw.read(),
            mstatus.tsr.read(),
            ConstantBits,
            mstatus.uxl.read(),
            mstatus.sxl.read(),
            mstatus.sbe.read(),
            mstatus.mbe.read(),
            ConstantBits,
            compute_sd(xs, fs),
        )
        .to_bits()
    }

    /// Write to mstatus the `value` given in 64 bit representation
    #[inline]
    pub fn write(&mut self, value: CSRRepr) -> Option<CSREffect>
    where
        M: ManagerWrite,
    {
        let value = MStatus::from_bits(value);
        let mstatus = self;

        let effect_sie = mstatus.sie.write(value.sie());
        let effect_mie = mstatus.mie.write(value.mie());
        debug_assert_eq!(effect_sie, Some(CSREffect::Xie));
        debug_assert_eq!(effect_mie, Some(CSREffect::Xie));

        mstatus.spie.write(value.spie());
        mstatus.ube.write(value.ube());
        mstatus.mpie.write(value.mpie());
        mstatus.spp.write(value.spp());
        mstatus.mpp.write(value.mpp());
        mstatus.fs.write(value.fs());
        mstatus.xs.write(value.xs());
        mstatus.mprv.write(value.mprv());
        mstatus.sum.write(value.sum());
        mstatus.mxr.write(value.mxr());
        mstatus.tvm.write(value.tvm());
        mstatus.tw.write(value.tw());
        mstatus.tsr.write(value.tsr());
        mstatus.uxl.write(value.uxl());
        mstatus.sxl.write(value.sxl());
        mstatus.sbe.write(value.sbe());
        mstatus.mbe.write(value.mbe());

        Some(CSREffect::Xie)
    }

    /// Replace mstatus with `value` given in 64 bit representation
    #[inline]
    pub fn replace(&mut self, value: CSRRepr) -> (CSRRepr, Option<CSREffect>)
    where
        M: ManagerReadWrite,
    {
        let value = MStatus::from_bits(value);
        let mstatus = self;

        let (sie, effect_sie) = mstatus.sie.replace(value.sie());
        let (mie, effect_mie) = mstatus.mie.replace(value.mie());
        debug_assert_eq!(effect_sie, Some(CSREffect::Xie));
        debug_assert_eq!(effect_mie, Some(CSREffect::Xie));

        let spie = mstatus.spie.replace(value.spie());
        let ube = mstatus.ube.replace(value.ube());
        let mpie = mstatus.mpie.replace(value.mpie());
        let spp = mstatus.spp.replace(value.spp());
        let mpp = mstatus.mpp.replace(value.mpp());
        let fs = mstatus.fs.replace(value.fs());
        let xs = mstatus.xs.replace(value.xs());
        let mprv = mstatus.mprv.replace(value.mprv());
        let sum = mstatus.sum.replace(value.sum());
        let mxr = mstatus.mxr.replace(value.mxr());
        let tvm = mstatus.tvm.replace(value.tvm());
        let tw = mstatus.tw.replace(value.tw());
        let tsr = mstatus.tsr.replace(value.tsr());
        let uxl = mstatus.uxl.replace(value.uxl());
        let sxl = mstatus.sxl.replace(value.sxl());
        let sbe = mstatus.sbe.replace(value.sbe());
        let mbe = mstatus.mbe.replace(value.mbe());
        let sd = compute_sd(fs, xs);

        let old_value = MStatus::new(
            ConstantBits::from_bits(0),
            sie,
            ConstantBits::from_bits(0),
            mie,
            ConstantBits::from_bits(0),
            spie,
            ube,
            mpie,
            spp,
            ConstantBits::from_bits(0),
            mpp,
            fs,
            xs,
            mprv,
            sum,
            mxr,
            tvm,
            tw,
            tsr,
            ConstantBits::from_bits(0),
            uxl,
            sxl,
            sbe,
            mbe,
            ConstantBits::from_bits(0),
            sd,
        )
        .to_bits();

        (old_value, Some(CSREffect::Xie))
    }
}
