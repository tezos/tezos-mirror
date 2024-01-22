use rvemu::{cpu::BYTE, emulator::Emulator};
use std::{
    error::Error,
    io::{self, Write},
    process::exit,
};

// System calls
const WRITE: u64 = 64;
const EXIT: u64 = 93;

// Error codes
const EINVAL: u64 = (-22i64) as u64;
const EBADF: u64 = (-9i64) as u64;

// Known file descriptors
const STDIN: u64 = 0;
const STDOUT: u64 = 1;
const STDERR: u64 = 2;

// Named register mapping, named after RISC-V Spec ABI
// E.g. a7 => x17
const A0: u64 = 10;
const A1: u64 = 11;
const A2: u64 = 12;
const A7: u64 = 17;

/// Handle a system call originating from the user program.
pub fn handle(emu: &mut Emulator) -> Result<(), Box<dyn Error>> {
    // System call number is contained in a7.
    let syscall_number = emu.cpu.xregs.read(A7);
    match syscall_number {
        WRITE => {
            // Read the arguments from a0-a2.
            let fd = emu.cpu.xregs.read(A0);
            let buf = emu.cpu.xregs.read(A1);
            let count = emu.cpu.xregs.read(A2);

            let data_range = buf..buf + count;

            // Writes the message to a given FD target.
            let write_data = |target: &mut dyn Write| -> Result<u64, Box<dyn Error>> {
                let message: Vec<u8> = data_range
                    .map(|i| emu.cpu.bus.read(i, BYTE).map(|i| i as u8))
                    .collect::<Result<Vec<u8>, _>>()
                    .map_err(super::exception_to_error)?;

                let written = target.write(message.as_slice())?;
                Ok(written as u64)
            };

            let bytes_written = match fd {
                STDIN => EINVAL,
                STDOUT => write_data(&mut io::stdout().lock())?,
                STDERR => write_data(&mut io::stderr().lock())?,
                _ => EBADF,
            };

            // Write the result back to a0.
            emu.cpu.xregs.write(A0, bytes_written);
        }

        EXIT => {
            let code = emu.cpu.xregs.read(A0);
            println!("Received request to exit with code {}", code);
            exit(code as i32);
        }

        _ => panic!("Unimplemented system call {}", syscall_number),
    }

    Ok(())
}
