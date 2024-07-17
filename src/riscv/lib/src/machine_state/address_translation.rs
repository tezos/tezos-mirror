// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    bus::{main_memory, Address, Addressable, Bus, OutOfBounds},
    csregisters::{
        satp::{Satp, SvLength, TranslationAlgorithm},
        xstatus::MStatus,
        CSRRepr, CSRegister,
    },
    mode::Mode,
    MachineState,
};
use crate::{
    bits::Bits64, machine_state::address_translation::pte::PageTableEntry,
    state_backend as backend, traps::Exception,
};

mod physical_address;
pub mod pte;
pub mod translation_cache;
mod virtual_address;

/// Offset of the `page offset` field in virtual and physical addresses.
const PAGE_OFFSET_WIDTH: usize = 12;
pub const PAGE_SIZE: u64 = 1 << PAGE_OFFSET_WIDTH;

/// Access type that is used in the virtual address translation process.
/// Section 5.3.2
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AccessType {
    Instruction,
    Load,
    Store,
}

impl AccessType {
    fn exception(&self, addr: Address) -> Exception {
        match self {
            AccessType::Instruction => Exception::InstructionPageFault(addr),
            AccessType::Load => Exception::LoadPageFault(addr),
            AccessType::Store => Exception::StoreAMOPageFault(addr),
        }
    }
}

pub struct SvConstants {
    pub levels: usize,
    pub pte_size: u64,
}

impl SvLength {
    /// LEVELS and PTESIZE constants for each SvZZ algorithm variant.
    pub const fn algorithm_constants(&self) -> SvConstants {
        match self {
            SvLength::Sv39 => SvConstants {
                levels: 3,
                pte_size: 8,
            },
            SvLength::Sv48 => SvConstants {
                levels: 4,
                pte_size: 8,
            },
            SvLength::Sv57 => SvConstants {
                levels: 5,
                pte_size: 8,
            },
        }
    }
}

/// Implementation of the virtual address translation as explained in section 5.3.2.
fn sv_translate_impl<ML, M>(
    bus: &Bus<ML, M>,
    v_addr: Address,
    satp: Satp,
    sv_length: SvLength,
    access_type: AccessType,
) -> Result<Address, Exception>
where
    ML: main_memory::MainMemoryLayout,
    M: backend::Manager,
{
    use physical_address as p_addr;
    use virtual_address as v_addr;

    let SvConstants { levels, pte_size } = sv_length.algorithm_constants();

    // 1. Let a be satp.ppn × PAGESIZE, and let i = LEVELS − 1.
    let mut i = levels.saturating_sub(1);
    let mut a: Address = satp.ppn().to_bits() * PAGE_SIZE;
    // For all translation algorithms the page table entry size is 8 bytes.
    let mut pte: PageTableEntry;
    loop {
        // 2. Let pte be the value of the PTE at address a + va.vpn[i] × PTESIZE.
        // TODO: RV-103: If accessing PTE violates a PMA or PMP check, raise an access-fault
        // exception corresponding to the original access type.
        let vpn_i =
            v_addr::get_vpn_idx(v_addr, &sv_length, i).ok_or(access_type.exception(v_addr))?;
        let addr = a + vpn_i * pte_size;
        pte = PageTableEntry::from_bits(
            bus.read(addr)
                .map_err(|_: OutOfBounds| Exception::LoadAccessFault(addr))?,
        );

        // 3. If pte.v = 0, or if pte.r = 0 and pte.w = 1, stop and raise a page-fault
        //    exception corresponding to the original access type.
        let pte_v = pte.v();
        let pte_r = pte.r();
        let pte_w = pte.w();
        if !pte_v || (!pte_r && pte_w) {
            return Err(access_type.exception(v_addr));
        }

        // 4. Otherwise, the PTE is valid. If pte.r = 1 or pte.x = 1, go to step 5.
        //    Otherwise, this PTE is a pointer to the next level of the page table.
        //    Let i = i − 1. If i < 0, stop and raise a page-fault exception
        //    corresponding to the original access type. Otherwise,
        //    let a = pte.ppn × PAGESIZE and go to step 2.
        let pte_x = pte.x();
        if pte_r || pte_x {
            break;
        }

        if i == 0 {
            return Err(access_type.exception(v_addr));
        }
        i -= 1;
        a = pte.ppn().to_bits() * PAGE_SIZE;
    }

    // TODO: RV-101: MXR aware translation
    // TODO: RV-100: SUM aware translation
    // 5. A leaf PTE has been found. Determine if the requested memory access is
    //    allowed by the pte.r, pte.w, pte.x, and pte.u bits, given the current
    //    privilege mode and the value of the SUM and MXR fields of the mstatus
    //    register. If not, stop and raise a page-fault exception corresponding
    //    to the original access type.

    // 6. If i > 0 and pte.ppn[i−1:0] != 0, this is a misaligned superpage; stop and
    //    raise a page-fault exception corresponding to the original access type.
    let pte_ppn = pte.ppn();
    for idx in (0..i).rev() {
        if pte_ppn.ppn_i(sv_length, idx) != Some(0) {
            return Err(access_type.exception(v_addr));
        }
    }

    // 7. If pte.a = 0, or if the memory access is a store and pte.d = 0, either raise
    //    a page-fault exception corresponding to the original access type, or:
    //    • Set pte.a to 1 and, if the memory access is a store, also set pte.d to 1.
    //    • If this access violates a PMA or PMP check, raise an access exception
    //    corresponding to the original access type.
    //    • Perform atomically:
    //      – Compare pte to the value of the PTE at address a + va.vpn[i] × PTESIZE.
    //      – If the values match, set pte.a to 1 and, if the original memory access is a store, also
    //      set pte.d to 1.
    //      – If the comparison fails, return to step 2
    let pte_a = pte.a();
    let pte_d = pte.d();
    if !pte_a || (access_type == AccessType::Store && !pte_d) {
        // Trying first case for now
        return Err(access_type.exception(v_addr));
    }

    // 8. The translation is successful. The translated physical address is given as
    //    follows:
    //    • pa.pgoff = va.pgoff.
    //    • If i > 0, then this is a superpage translation and pa.ppn[i−1:0] =
    //    va.vpn[i−1:0].
    //    • pa.ppn[LEVELS−1:i] = pte.ppn[LEVELS−1:i].
    let va_page_offset = v_addr::get_page_offset(v_addr);
    let p_addr = (|| {
        let mut pa = p_addr::set_page_offset(0u64, va_page_offset);
        for idx in 0..i {
            let va_vpn_i = v_addr::get_vpn_idx(v_addr, &sv_length, idx)?;
            pa = p_addr::set_ppn_idx(pa, sv_length, idx, va_vpn_i)?;
        }
        for idx in i..levels {
            let pte_ppn_i = pte_ppn.ppn_i(sv_length, idx)?;
            pa = p_addr::set_ppn_idx(pa, sv_length, idx, pte_ppn_i)?;
        }
        Some(pa)
    })();

    p_addr.ok_or(access_type.exception(v_addr))
}

impl<ML: main_memory::MainMemoryLayout, M: backend::Manager> MachineState<ML, M> {
    /// Get the effective hart mode when addressing memory.
    /// Section P:M-ISA-1.6.3
    /// The MPRV (Modify PRiVilege) bit modifies the effective privilege mode, i.e.,
    /// the privilege level at which loads and stores execute.
    /// When MPRV=0, loads and stores behave as normal, using the translation and
    /// protection mechanisms of the current privilege mode. When MPRV=1, load and store memory
    /// addresses are translated and protected, and endianness is applied,
    /// as though the current privilege mode were set to MPP.
    /// Instruction address-translation and protection are unaffected by the setting of MPRV
    #[inline]
    pub fn effective_translation_hart_mode(&self, mode: Mode, access_type: AccessType) -> Mode {
        let mstatus: MStatus = self.hart.csregisters.read(CSRegister::mstatus);
        match access_type {
            AccessType::Store | AccessType::Load if mstatus.mprv() => mstatus.mpp().into(),
            _ => mode,
        }
    }

    /// Get the effective translation mode when addressing memory.
    /// [`None`] represents that either `SATP.mode` is a reserved / not implemented mode or
    /// that the system is in `Machine` mode in which case translation is ignored.
    #[inline]
    pub fn effective_translation_alg(
        &self,
        mode: Mode,
        satp: CSRRepr,
        access_type: AccessType,
    ) -> TranslationAlgorithm {
        // 1. Let a be satp.ppn × PAGESIZE, and let i = LEVELS − 1.
        //    The satp register must be active, i.e.,
        //    the effective privilege mode must be S-mode or U-mode.

        if let Mode::Machine = self.effective_translation_hart_mode(mode, access_type) {
            return TranslationAlgorithm::Bare;
        }

        Satp::from_bits(satp).mode()
    }

    /// Like [`Self::translate`] but allows the caller to prevent extraneous reads from CSRs.
    #[inline]
    pub(crate) fn translate_with_prefetch(
        &self,
        mode: Mode,
        satp: CSRRepr,
        virt_addr: Address,
        access_type: AccessType,
    ) -> Result<Address, Exception> {
        let mode = self.effective_translation_alg(mode, satp, access_type);

        use TranslationAlgorithm::*;
        let sv_length = match mode {
            Bare => return Ok(virt_addr),
            Sv39 => SvLength::Sv39,
            Sv48 => SvLength::Sv48,
            Sv57 => SvLength::Sv57,
        };

        let satp = Satp::from_bits(satp);
        sv_translate_impl(&self.bus, virt_addr, satp, sv_length, access_type)
            .map_err(|_e| access_type.exception(virt_addr))
    }

    /// Translate a virtual address to a physical address as described in section 5.3.2
    #[inline]
    pub fn translate(
        &self,
        virt_addr: Address,
        access_type: AccessType,
    ) -> Result<Address, Exception> {
        let mode = self.hart.mode.read_default();
        let satp = self.hart.csregisters.read(CSRegister::satp);
        self.translate_with_prefetch(mode, satp, virt_addr, access_type)
    }
}
