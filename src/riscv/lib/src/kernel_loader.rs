use std::io::Cursor;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;

use elf::ElfBytes;
use elf::abi::DT_RELA;
use elf::abi::DT_RELASZ;
use elf::abi::ET_DYN;
use elf::abi::ET_EXEC;
use elf::abi::PF_R;
use elf::abi::PF_W;
use elf::abi::PF_X;
use elf::abi::PT_LOAD;
use elf::abi::R_RISCV_RELATIVE;
use elf::endian::LittleEndian;
use elf::file::Class::ELF64;
use elf::file::FileHeader;
use elf::relocation::RelaIterator;
use elf::segment::ProgramHeader;

use crate::machine_state::memory::Permissions;

/// Error when parsing and loading the user kernel ELF file
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Failed to write to memory
    #[error("At address {addr:#x}: {msg:?}")]
    Write { msg: Option<String>, addr: u64 },

    /// Failed to parse the ELF file
    #[error("Failed to parse ELF file: {0}")]
    Elf(#[from] elf::ParseError),
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

        let perms_read = segment.p_flags & PF_R != 0;
        let perms_write = segment.p_flags & PF_W != 0;
        let perms_exec = segment.p_flags & PF_X != 0;

        MemoryPermissions {
            start_address,
            length: segment.p_memsz,

            // This is a simplified version of the permissions. It is aligned with what to
            // expect in an ELF file.
            permissions: if perms_exec {
                Permissions::ReadExec
            } else if perms_write {
                Permissions::ReadWrite
            } else if perms_read {
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
    elf: &ElfBytes<'a, LittleEndian>,
    contents: &'a [u8],
) -> Result<LoadResult<'a>, Error> {
    let loadable_segments = elf
        .segments()
        .into_iter()
        .flat_map(|headers| headers.iter())
        .filter(|header| header.p_type == PT_LOAD);

    // Capacity of 4 should cover most cases:
    //  .text => rx
    //  .rodata => r
    //  .data => rw
    //  .bss => rw
    let mut permissions = Vec::with_capacity(4);

    for segment in loadable_segments {
        permissions.push(MemoryPermissions::from_load_segment(None, &segment));

        // Copy the region from the file to memory.
        let segment_contents = &contents[segment.p_offset as usize..][..segment.p_filesz as usize];
        mem.write_bytes(segment.p_paddr, segment_contents)?;

        // If the target memory region is larger than the source region,
        // we must fill the gap with 0s.
        if segment.p_memsz > segment.p_filesz {
            let first_zero = segment.p_paddr.wrapping_add(segment.p_filesz);
            let num_zeroes = segment.p_memsz.saturating_sub(segment.p_filesz);
            mem.set_zero(first_zero, num_zeroes)?;
        }
    }

    let program_headers = extract_program_headers(&elf.ehdr, contents, permissions);

    Ok(LoadResult {
        entry: elf.ehdr.e_entry,
        program_headers,
    })
}

/// Load a relocatable ELF file at the given `start` address. `elf` is the partially parsed
/// ELF file, while `contents` is the raw ELF file.
pub fn load_elf_reloc<'a>(
    mem: &mut impl Memory,
    start: u64,
    elf: &ElfBytes<'a, LittleEndian>,
    contents: &'a [u8],
) -> Result<LoadResult<'a>, Error> {
    let loadable_segments = elf
        .segments()
        .into_iter()
        .flat_map(|headers| headers.iter())
        .filter(|header| header.p_type == PT_LOAD);

    // Capacity of 4 should cover most cases:
    //  .text => rx
    //  .rodata => r
    //  .data => rw
    //  .bss => rw
    let mut permissions = Vec::with_capacity(4);

    for segment in loadable_segments {
        permissions.push(MemoryPermissions::from_load_segment(Some(start), &segment));

        // Copy the region from the file to memory.
        let start_addr = start.wrapping_add(segment.p_vaddr);
        let segment_contents = &contents[segment.p_offset as usize..][..segment.p_filesz as usize];
        mem.write_bytes(start_addr, segment_contents)?;

        // If the target memory region is larger than the source region,
        // we must fill the gap with 0s.
        if segment.p_memsz > segment.p_filesz {
            let first_zero = start_addr.wrapping_add(segment.p_filesz);
            let num_zeroes = segment.p_memsz.saturating_sub(segment.p_filesz);
            mem.set_zero(first_zero, num_zeroes)?;
        }
    }

    let mut relas_addr = None;
    let mut relas_size = None;

    let dynamic_infos = elf
        .find_common_data()
        .into_iter()
        .flat_map(|common| common.dynamic.into_iter())
        .flat_map(|dyn_table| dyn_table.iter());

    for dyn_info in dynamic_infos {
        if dyn_info.d_tag == DT_RELA {
            relas_addr = Some(dyn_info.d_ptr());
        } else if dyn_info.d_tag == DT_RELASZ {
            relas_size = Some(dyn_info.d_val());
        }
    }

    if let Some((addr, size)) = relas_addr.zip(relas_size) {
        let relas_data = &contents[addr as usize..][..size as usize];
        let relas = RelaIterator::new(LittleEndian, ELF64, relas_data);

        for rela in relas {
            match rela.r_type {
                R_RISCV_RELATIVE => {
                    let relocated = (start as i64 + rela.r_addend).to_ne_bytes();
                    let buf = &relocated[..];
                    mem.write_bytes(start + rela.r_offset, buf)?;
                }
                unknown_type => unimplemented!("Unsupported relocation directive {unknown_type}"),
            }
        }
    }

    let program_headers = extract_program_headers(&elf.ehdr, contents, permissions);

    Ok(LoadResult {
        entry: elf.ehdr.e_entry + start,
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
    let elf = ElfBytes::<LittleEndian>::minimal_parse(contents)?;

    match elf.ehdr.e_type {
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
    header: &FileHeader<LittleEndian>,
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
