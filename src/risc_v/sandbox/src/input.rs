use goblin::{
    elf::{Elf, ProgramHeaders},
    elf64::program_header::PT_LOAD,
};
use rvemu::{cpu::BYTE, emulator::Emulator, exception::Exception};
use std::{
    fs::File,
    io::{self, Read},
    iter,
    path::Path,
};

/// Helper function to read the contents of a file.
fn read_file(path: impl AsRef<Path>) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;

    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    Ok(contents)
}

/// Input for the sandbox
pub struct Input {
    entrypoint: u64,
    program_headers: ProgramHeaders,
    contents: Vec<u8>,
}

impl Input {
    /// Load an ELF binary.
    pub fn load_file(path: impl AsRef<Path>) -> goblin::error::Result<Self> {
        let contents = read_file(path)?;
        Self::load(contents)
    }

    /// Load an ELF object.
    pub fn load(contents: Vec<u8>) -> goblin::error::Result<Input> {
        let elf = Elf::parse(contents.as_ref())?;

        Ok(Self {
            entrypoint: elf.entry,
            program_headers: elf.program_headers,
            contents,
        })
    }

    /// Prepare the emulator in a way that it can start executing
    pub fn configure_emulator(&self, emu: &mut Emulator) -> Result<(), Exception> {
        let loadable_segments = self
            .program_headers
            .iter()
            .filter(|header| header.p_type == PT_LOAD);

        for segment in loadable_segments {
            // Copy the region from the file to memory.
            for (file_offset, memory_offset) in iter::zip(segment.file_range(), segment.p_paddr..) {
                emu.cpu
                    .bus
                    .write(memory_offset, self.contents[file_offset] as u64, BYTE)?;
            }

            // If the target memory region is larger than the source region,
            // we must fill the gap with 0s.
            if segment.p_memsz > segment.p_filesz {
                let first_zero = segment.p_paddr + segment.p_filesz;
                let num_zeroes = segment.p_memsz - segment.p_filesz;
                for i in first_zero..(first_zero + num_zeroes) {
                    emu.cpu.bus.write(i, 0, BYTE)?;
                }
            }
        }

        // Setting the program counter (PC) tells the emulator where to start executing.
        emu.initialize_pc(self.entrypoint);

        Ok(())
    }
}
