use std::io::Cursor;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;

use derive_more::Error;
use derive_more::From;
use goblin::elf::Elf;
use goblin::elf::Header;
use goblin::elf::header::ET_DYN;
use goblin::elf::header::ET_EXEC;
use goblin::elf::program_header::PT_LOAD;
use goblin::elf::program_header::ProgramHeader;

use crate::machine_state::memory::Permissions;

#[derive(Debug, From, Error, derive_more::Display)]
pub enum Error {
    #[display(fmt = "At address {:#x}: {:?}", addr, "msg.clone()")]
    Write {
        msg: Option<String>,
        addr: u64,
    },
    Goblin(goblin::error::Error),
}

/// Permissions for program regions in memory
pub struct MemoryPermissions {
    /// Starting address of the memory region
    pub start_address: u64,

    /// Length of the memory region
    pub length: u64,

    /// Permissions for the memory region
    pub permissions: Permissions,
}

impl MemoryPermissions {
    /// Extract the memory permissions from a load segment. `reloc` can be used to offset the
    /// memory region.
    pub fn from_load_segment(reloc: Option<u64>, segment: &ProgramHeader) -> Self {
        let start_address = match reloc {
            Some(offset) => offset.wrapping_add(segment.p_vaddr),
            None => segment.p_paddr,
        };

        MemoryPermissions {
            start_address,
            length: segment.p_memsz,

            // This is a simplified version of the permissions. It is aligned with what to
            // expect in an ELF file.
            permissions: if segment.is_executable() {
                Permissions::ReadExec
            } else if segment.is_write() {
                Permissions::ReadWrite
            } else if segment.is_read() {
                Permissions::Read
            } else {
                Permissions::None
            },
        }
    }
}

/// Program headers extracted from the ELF file
pub struct ProgramHeaders<'a> {
    /// Size of each entry in the program headers
    pub entry_size: u64,

    /// Number of program headers entries
    pub num_entries: u64,

    /// Raw contents
    pub contents: &'a [u8],

    /// Memory permissions for each segment
    pub permissions: Vec<MemoryPermissions>,
}

/// [LoadResult] is the outcome of loading an ELF file
pub struct LoadResult<'a> {
    /// Address at which the entrypoint of the program is located
    pub entry: u64,

    /// Index of the last written byte in memory after loading the ELF
    pub last_written: u64,

    /// Raw program headers
    pub program_headers: ProgramHeaders<'a>,
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

/// Loads an executable ELF file in the memory of the virtual machine
pub fn load_elf_nonreloc<'a>(
    mem: &mut impl Memory,
    elf: &Elf<'a>,
    contents: &'a [u8],
) -> Result<LoadResult<'a>, Error> {
    let loadable_segments = elf
        .program_headers
        .iter()
        .filter(|header| header.p_type == PT_LOAD);

    let mut last_written = 0;

    // Capacity of 4 should cover most cases:
    //  .text => rx
    //  .rodata => r
    //  .data => rw
    //  .bss => rw
    let mut permissions = Vec::with_capacity(4);

    for segment in loadable_segments {
        permissions.push(MemoryPermissions::from_load_segment(None, segment));

        // Copy the region from the file to memory.
        mem.write_bytes(segment.p_paddr, &contents[segment.file_range()])?;

        // If the target memory region is larger than the source region,
        // we must fill the gap with 0s.
        if segment.p_memsz > segment.p_filesz {
            let first_zero = segment.p_paddr.wrapping_add(segment.p_filesz);
            let num_zeroes = segment.p_memsz.saturating_sub(segment.p_filesz);
            mem.set_zero(first_zero, num_zeroes)?;
        }

        last_written = segment.p_paddr + segment.p_memsz;
    }

    let program_headers = extract_program_headers(&elf.header, contents, permissions);

    Ok(LoadResult {
        entry: elf.entry,
        last_written,
        program_headers,
    })
}

/// Required memory size for loading.
pub fn mem_size(phs: &[ProgramHeader]) -> usize {
    let phs = phs.iter().filter(|ph| ph.p_type == PT_LOAD);

    let min = phs.clone().map(|ph| ph.p_vaddr).min();
    let max = phs.map(|ph| ph.p_vaddr + ph.p_memsz).max();

    min.and_then(|min| max.map(|max| max - min)).unwrap_or(0) as usize
}

/// Load a relocatable ELF file at the given `start` address. `elf` is the partially parsed
/// ELF file, while `contents` is the raw ELF file.
pub fn load_elf_reloc<'a>(
    mem: &mut impl Memory,
    start: u64,
    elf: &Elf<'a>,
    contents: &'a [u8],
) -> Result<LoadResult<'a>, Error> {
    let loadable_segments = elf
        .program_headers
        .iter()
        .filter(|header| header.p_type == PT_LOAD);

    let mut last_written = start;

    // Capacity of 4 should cover most cases:
    //  .text => rx
    //  .rodata => r
    //  .data => rw
    //  .bss => rw
    let mut permissions = Vec::with_capacity(4);

    for segment in loadable_segments {
        permissions.push(MemoryPermissions::from_load_segment(Some(start), segment));

        // Copy the region from the file to memory.
        let start_addr = start.wrapping_add(segment.p_vaddr);
        mem.write_bytes(start_addr, &contents[segment.file_range()])?;

        // If the target memory region is larger than the source region,
        // we must fill the gap with 0s.
        if segment.p_memsz > segment.p_filesz {
            let first_zero = start_addr.wrapping_add(segment.p_filesz);
            let num_zeroes = segment.p_memsz.saturating_sub(segment.p_filesz);
            mem.set_zero(first_zero, num_zeroes)?;
        }

        last_written = start + segment.p_vaddr + segment.p_memsz;
    }

    for r in elf.dynrelas.iter() {
        let goblin::elf::reloc::Reloc {
            r_offset,
            r_addend,
            r_sym: _,
            r_type,
        } = r;
        match r_type {
            goblin::elf64::reloc::R_RISCV_RELATIVE => {
                let relocated = (start as i64 + r_addend.unwrap_or_default()).to_ne_bytes();
                let buf = &relocated[..];
                mem.write_bytes(start + r_offset, buf)?;
            }
            t => unimplemented!("Unsupported relocation directive {t}"),
        }
    }

    let program_headers = extract_program_headers(&elf.header, contents, permissions);

    Ok(LoadResult {
        entry: elf.header.e_entry + start,
        last_written,
        program_headers,
    })
}

/// Loads an ELF file. If the file is relocatable, loads it at the given `start` address in the memory of the VM.
/// If it is not relocatable, `start` is ignored.
pub fn load_elf<'a>(
    mem: &mut impl Memory,
    start: u64,
    contents: &'a [u8],
) -> Result<LoadResult<'a>, Error> {
    let elf = Elf::parse(contents)?;
    match elf.header.e_type {
        ET_EXEC => load_elf_nonreloc(mem, &elf, contents),
        ET_DYN => load_elf_reloc(mem, start, &elf, contents),
        t => todo!("ELF type {t} is not yet supported"),
    }
}

impl<T> Memory for Cursor<T>
where
    Cursor<T>: Write + Seek,
{
    fn write_bytes(&mut self, paddr: u64, bytes: &[u8]) -> Result<(), Error> {
        // XXX: Use try-block when it becomes stable.
        // https://doc.rust-lang.org/beta/unstable-book/language-features/try-blocks.html
        (|| {
            self.seek(SeekFrom::Start(paddr))?;
            self.write_all(bytes)
        })()
        .map_err(|err| Error::Write {
            msg: Some(err.to_string()),
            addr: paddr,
        })
    }
}

/// Extract the raw program headers from the ELF file.
fn extract_program_headers<'a>(
    header: &Header,
    contents: &'a [u8],
    permissions: Vec<MemoryPermissions>,
) -> ProgramHeaders<'a> {
    let start = header.e_phoff as usize;
    let length = header.e_phentsize as usize * header.e_phnum as usize;
    let contents = &contents[start..][..length];

    ProgramHeaders {
        entry_size: header.e_phentsize as u64,
        num_entries: header.e_phnum as u64,
        contents,
        permissions,
    }
}
