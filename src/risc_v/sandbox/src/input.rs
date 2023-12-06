use derive_more::{Error, From};
use kernel_loader::LoadResult;
use rvemu::emulator::Emulator;

#[derive(Debug, Error, From, derive_more::Display)]
pub enum Error {
    #[cfg(feature = "std")]
    IO(io::error::Error),
    Elf(kernel_loader::Error),
}

/// Prepare the emulator in a way that it can start executing
pub fn configure_emulator(contents: &[u8], emu: &mut Emulator) -> Result<u64, Error> {
    let LoadResult {
        entry,
        last_written,
    } = kernel_loader::load_elf(&mut emu.cpu.bus, rvemu::bus::DRAM_BASE, contents)?;

    // Setting the program counter (PC) tells the emulator where to start executing.
    emu.initialize_pc(entry);

    Ok(last_written)
}
