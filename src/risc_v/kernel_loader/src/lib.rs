use derive_more::{Error, From};
use goblin::{elf::Elf, elf64::program_header::PT_LOAD};

#[derive(Debug, From, Error, derive_more::Display)]
pub enum Error {
    #[display(fmt = "At address {:#x}: {:?}", addr, "msg.clone()")]
    Write {
        msg: Option<String>,
        addr: u64,
    },
    Goblin(goblin::error::Error),
}

/// [LoadResult] is the outcome of loading an ELF file
pub struct LoadResult {
    /// the address at which the entrypoint of the program is located
    pub entry: u64,
    /// index of the last written byte in memory after loading the ELF
    pub last_written: u64,
}

/// [Memory] is an interface to the linear array of bytes of the RISC-V virtual machine
pub trait Memory {
    /// Writes a slice of bytes at the given physical address in the memory of the virtual machine.
    fn write_bytes(&mut self, paddr: u64, bytes: &[u8]) -> Result<(), Error>;

    /// Writes a given number of zeroes at the given physical address.
    fn set_zero(&mut self, paddr: u64, len: u64) -> Result<(), Error> {
        let bytes = vec![0u8; len as usize];
        self.write_bytes(paddr, &bytes)
    }
}

/// Loads an ELF file in the memory of the virtual machine
pub fn load_elf(mem: &mut impl Memory, contents: &[u8]) -> Result<LoadResult, Error> {
    let elf = Elf::parse(contents.as_ref())?;

    assert!(elf.header.e_type == goblin::elf::header::ET_EXEC);

    let loadable_segments = elf
        .program_headers
        .iter()
        .filter(|header| header.p_type == PT_LOAD);

    let mut last_written = 0;

    for segment in loadable_segments {
        // Copy the region from the file to memory.
        mem.write_bytes(segment.p_paddr, &contents[segment.file_range()])?;

        // If the target memory region is larger than the source region,
        // we must fill the gap with 0s.
        if segment.p_memsz > segment.p_filesz {
            let first_zero = segment.p_paddr + segment.p_filesz;
            let num_zeroes = segment.p_memsz - segment.p_filesz;
            mem.set_zero(first_zero, num_zeroes)?;
        }

        last_written = segment.p_paddr + segment.p_memsz;
    }

    Ok(LoadResult {
        entry: elf.entry,
        last_written,
    })
}

impl Memory for rvemu::bus::Bus {
    fn write_bytes(&mut self, paddr: u64, bytes: &[u8]) -> Result<(), Error> {
        for (i, b) in bytes.into_iter().enumerate() {
            let addr = paddr + i as u64;
            self.write(addr, *b as u64, rvemu::cpu::BYTE)
                .map_err(|exn| {
                    let msg = Some(format!("{:?}", exn));
                    Error::Write { msg, addr }
                })?
        }
        Ok(())
    }

    fn set_zero(&mut self, paddr: u64, len: u64) -> Result<(), Error> {
        for addr in paddr..paddr + len {
            self.write(addr, 0, rvemu::cpu::BYTE).map_err(|exn| {
                let msg = Some(format!("{:?}", exn));
                Error::Write { msg, addr }
            })?
        }
        Ok(())
    }
}
