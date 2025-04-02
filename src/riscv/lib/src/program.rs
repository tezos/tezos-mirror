// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::marker::PhantomData;

use crate::kernel_loader;
use crate::machine_state::memory;
use crate::parser::parse_block;

/// RISC-V program
pub struct Program<'a, MC> {
    _pd: PhantomData<MC>,

    /// Address of the program's entrypoint
    pub entrypoint: memory::Address,

    /// Segments to be written to the main memory
    // Note: `[u8]` owned is `Vec<u8>`. The segment is either re-using an
    // existing slice (e.g. from an open ELF file), or a vector in case the
    // bytes were dynamically created (e.g. chunks for zero-ed memory).
    // Invariant: `segments[index]` corresponds to an array
    // representing bytes at `index..index+length` and
    // all the arrays are non-overlapping
    pub segments: BTreeMap<memory::Address, Cow<'a, [u8]>>,

    /// Raw program headers
    pub program_headers: Option<kernel_loader::ProgramHeaders<'a>>,
}

impl<MC> kernel_loader::Memory for Program<'_, MC> {
    fn write_bytes(
        &mut self,
        mut paddr: u64,
        mut bytes: &[u8],
    ) -> Result<(), kernel_loader::Error> {
        if bytes.is_empty() {
            return Ok(());
        }

        // If there is a chunk before [paddr] then we need to check if that
        // chunk potentially overlaps with the location we want to write.
        if let Some((&prev_paddr, prev_bytes)) = self.segments.range_mut(..paddr).last() {
            if (paddr as usize) < (prev_paddr as usize) + prev_bytes.len() {
                // If there is an overlap, we simply shrink the existing chunk.
                // This eliminates the overlap on the left side given we would
                // have overridden that section anyway.
                prev_bytes.to_mut().resize((paddr - prev_paddr) as usize, 0);
            }
        }

        // If there is a chunk at or after [paddr] then we need to check if that
        // chunk potentially overlaps with the location we want to write.
        let paddr_after = paddr + bytes.len() as u64;
        let mut new_segments = BTreeMap::new();
        for (&next_paddr, _) in self.segments.range(paddr..paddr_after) {
            let nonoverlapping_len = next_paddr - paddr;
            let (nonoverlapping, overlapping) = bytes.split_at(nonoverlapping_len as usize);

            if !nonoverlapping.is_empty() {
                new_segments.insert(paddr, Cow::Owned(nonoverlapping.to_owned()));
            }

            // Continue with the new overlapping chunk that starts exactly at
            // the boundary of the next chunk.
            paddr = next_paddr;
            bytes = overlapping;
        }

        // Commit new entries
        self.segments.append(&mut new_segments);

        if bytes.is_empty() {
            return Ok(());
        }

        // After this point we don't need to dissect `bytes` any further.
        // This means there either is or isn't a chunk to be written to at the
        // exact address `paddr` and no overlaps would occur.

        if let Some(chunk) = self.segments.get_mut(&paddr) {
            if chunk.len() >= bytes.len() {
                // There is a chunk at this exact address, and it has space to
                // be written to.
                chunk.to_mut()[..bytes.len()].copy_from_slice(bytes);
                return Ok(());
            }

            // We don't need an else case here because the code below deals with
            // overriding the entire chunk already.
        }

        self.segments.insert(paddr, Cow::Owned(bytes.to_owned()));

        Ok(())
    }
}

impl<'a, MC: memory::MemoryConfig> Program<'a, MC> {
    /// Parse the given ELF executable and convert it into our program
    /// representation. The main memory configuration `MC` is used to compute the
    /// correct addresses
    pub fn from_elf(elf: &'a [u8]) -> Result<Self, kernel_loader::Error> {
        let start_if_reloc = memory::FIRST_ADDRESS;

        let mut myself = Self {
            _pd: PhantomData,
            entrypoint: start_if_reloc,
            segments: BTreeMap::new(),
            program_headers: None,
        };

        let load_result = kernel_loader::load_elf(&mut myself, start_if_reloc, elf)?;
        myself.entrypoint = load_result.entry;
        myself.program_headers = Some(load_result.program_headers);

        Ok(myself)
    }

    pub fn parsed(&self) -> BTreeMap<u64, String> {
        let mut parsed = BTreeMap::new();
        for segment in &self.segments {
            let mut address = *segment.0;
            let instructions = parse_block(segment.1);
            for instr in instructions {
                parsed.insert(address, instr.to_string());
                address += instr.width() as u64;
            }
        }
        parsed
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::fs;
    use std::io::Cursor;
    use std::marker::PhantomData;

    use crate::kernel_loader;
    use crate::kernel_loader::Memory;
    use crate::machine_state::memory;
    use crate::machine_state::memory::M1M;
    use crate::program::Program;

    #[test]
    fn test_impl_memory_program() {
        let mut program: Program<'_, M1M> = Program {
            _pd: PhantomData,
            entrypoint: 0,
            segments: BTreeMap::new(),
            program_headers: None,
        };
        let mut buffer = Cursor::new(vec![0; 2048]);

        let test = RefCell::new(move |addr, data: Vec<u8>| {
            program.write_bytes(addr, data.as_slice()).unwrap();
            buffer.write_bytes(addr, data.as_slice()).unwrap();

            for (&addr, segment) in &program.segments {
                let addr = addr as usize;
                assert_eq!(
                    &buffer.get_ref()[addr..addr + segment.len()],
                    segment.as_ref()
                );
            }
        });

        proptest::proptest!(|(addr in 0..1024u64, len in 0..1024usize, data: [u8; 1024])| {
            test.borrow_mut()(addr, data[..len].to_vec());
        });
    }

    #[test]
    fn test_program_from_elf() {
        const PATH: &str = "../riscv-dummy.elf";
        let contents =
            fs::read(PATH).expect("Failed read dummy RISC-V kernel (try: make -C src/riscv build)");

        let mut buffer = Cursor::new(Vec::new());
        kernel_loader::load_elf(&mut buffer, memory::FIRST_ADDRESS, &contents).unwrap();
        let buffer = buffer.into_inner();

        let program = Program::<M1M>::from_elf(&contents).unwrap();
        for (addr, segment) in program.segments {
            let addr = addr as usize;
            assert_eq!(&buffer[addr..addr + segment.len()], segment.as_ref());
        }
    }
}
