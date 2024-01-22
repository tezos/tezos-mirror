// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! More information on SBI specification:
//!   - https://www.scs.stanford.edu/~zyedidia/docs/riscv/riscv-sbi.pdf

use crate::inbox::Inbox;
use crate::rv::{A0, A1, A2, A3, A6, A7};
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use kernel_loader::Memory;
use rvemu::cpu::{AccessType, BYTE};
use rvemu::emulator::Emulator;
use std::{
    error::Error,
    io::{self, Write},
    process::exit,
};
use tezos_smart_rollup_constants::riscv::{
    SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_SHUTDOWN, SBI_TEZOS_BLAKE2B_HASH256,
    SBI_TEZOS_ED25519_SIGN, SBI_TEZOS_ED25519_VERIFY, SBI_TEZOS_INBOX_NEXT, SBI_TEZOS_META_ADDRESS,
    SBI_TEZOS_META_ORIGINATION_LEVEL,
};
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

type SBIResult = Result<(), Box<dyn Error>>;

/// Read a virtual address from a register and translate it into a physical address.
fn read_physical_address(emu: &mut Emulator, register: u64) -> Result<u64, Box<dyn Error>> {
    let addr = emu.cpu.xregs.read(register);
    emu.cpu
        .translate(addr, AccessType::Load)
        .map_err(super::exception_to_error)
}

/// Read a series of bytes from memory.
fn read_memory(emu: &mut Emulator, address: u64, len: u64) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut buffer = vec![0u8; len as usize];

    for i in 0..len {
        buffer[i as usize] = emu
            .cpu
            .bus
            .read(address + i, BYTE)
            .map_err(super::exception_to_error)? as u8;
    }

    Ok(buffer)
}

/// SBI extension ID 0x01
fn sbi_console_putchar(emu: &mut Emulator) -> SBIResult {
    let c = emu.cpu.xregs.read(A0) as u8;
    io::stdout().lock().write_all(&[c])?;
    emu.cpu.xregs.write(A0, 0);
    Ok(())
}

/// SBI extension ID 0x08
fn sbi_shutdown() -> ! {
    eprintln!("Received SBI shutdown request");
    exit(0)
}

/// Move the Inbox to the next message.
fn sbi_tezos_inbox_next(emu: &mut Emulator, inbox: &mut Inbox) -> SBIResult {
    match inbox.next() {
        Some((level, id, data)) => {
            let dest_addr = read_physical_address(emu, A0)?;
            let max_bytes = emu.cpu.xregs.read(A1);

            let length = max_bytes.min(data.len() as u64);
            let source_data = &data[0..length as usize];

            emu.cpu.bus.write_bytes(dest_addr, source_data)?;

            emu.cpu.xregs.write(A0, level as u64);
            emu.cpu.xregs.write(A1, id as u64);
            emu.cpu.xregs.write(A2, length);
        }

        None => {
            emu.cpu.xregs.write(A0, 0);
            emu.cpu.xregs.write(A1, 0);
            emu.cpu.xregs.write(A2, 0);
        }
    }

    Ok(())
}

/// Metadata relating to the rollup
pub struct RollupMetadata {
    /// Tezos level at which the rollup was originated
    pub origination_level: u64,

    /// Adress of the rollup
    pub address: SmartRollupAddress,
}

/// Provide the rollup's origination level.
fn sbi_tezos_meta_origination_level(emu: &mut Emulator, meta: &RollupMetadata) -> SBIResult {
    emu.cpu.xregs.write(A0, meta.origination_level);
    Ok(())
}

/// Provide the rollup's address.
fn sbi_tezos_meta_address(emu: &mut Emulator, meta: &RollupMetadata) -> SBIResult {
    let dest_addr = read_physical_address(emu, A0)?;
    let max_bytes = emu.cpu.xregs.read(A1);

    let addr_bytes = meta.address.hash().as_ref().as_slice();
    let length = max_bytes.min(addr_bytes.len() as u64);
    let source_data = &addr_bytes[0..length as usize];

    emu.cpu.bus.write_bytes(dest_addr, source_data)?;
    emu.cpu.xregs.write(A0, length);

    Ok(())
}

/// Produce a Ed25519 signature.
fn sbi_tezos_ed25519_sign(emu: &mut Emulator) -> SBIResult {
    let sk_addr = read_physical_address(emu, A0)?;
    let sk_bytes = read_memory(emu, sk_addr, 32)?;
    let sk = SigningKey::try_from(sk_bytes.as_slice())?;

    let msg_addr = read_physical_address(emu, A1)?;
    let msg_len = emu.cpu.xregs.read(A2);
    let msg_bytes = read_memory(emu, msg_addr, msg_len)?;

    let sig = sk.sign(msg_bytes.as_slice());
    let sig_bytes: [u8; 64] = sig.to_bytes();

    let sig_addr = read_physical_address(emu, A3)?;
    emu.cpu.bus.write_bytes(sig_addr, &sig_bytes)?;

    Ok(())
}

/// Verify a Ed25519 signature.
fn sbi_tezos_ed25519_verify(emu: &mut Emulator) -> SBIResult {
    let pk_addr = read_physical_address(emu, A0)?;
    let pk_bytes = read_memory(emu, pk_addr, 32)?;

    let sig_addr = read_physical_address(emu, A1)?;
    let sig_bytes = read_memory(emu, sig_addr, 64)?;

    let msg_addr = read_physical_address(emu, A2)?;
    let msg_len = emu.cpu.xregs.read(A3);
    let msg_bytes = read_memory(emu, msg_addr, msg_len)?;

    let pk = VerifyingKey::try_from(pk_bytes.as_slice())?;
    let sig = Signature::from_slice(sig_bytes.as_slice())?;
    let valid = pk.verify_strict(msg_bytes.as_slice(), &sig).is_ok();

    emu.cpu.xregs.write(A0, valid as u64);

    Ok(())
}

/// Compute a BLAKE2B 256-bit digest.
fn sbi_tezos_blake2b_hash256(emu: &mut Emulator) -> SBIResult {
    let msg_addr = read_physical_address(emu, A1)?;
    let msg_len = emu.cpu.xregs.read(A2);
    let msg_bytes = read_memory(emu, msg_addr, msg_len)?;

    let hash = tezos_crypto_rs::blake2b::digest_256(msg_bytes.as_slice())?;

    let out_addr = read_physical_address(emu, A0)?;
    emu.cpu.bus.write_bytes(out_addr, hash.as_slice())?;

    Ok(())
}

/// Handle a system call originating from the user program.
pub fn handle_sbi(emu: &mut Emulator, meta: &RollupMetadata, inbox: &mut Inbox) -> SBIResult {
    // TODO: https://gitlab.com/tezos/tezos/-/issues/6767
    // Feed errors back to caller instead of raising them in the sandbox.
    // This means this function most likely should return unit.

    // SBI extension is contained in a7.
    let sbi_extension = emu.cpu.xregs.read(A7);
    match sbi_extension {
        SBI_CONSOLE_PUTCHAR => sbi_console_putchar(emu),
        SBI_SHUTDOWN => sbi_shutdown(),
        SBI_FIRMWARE_TEZOS => {
            let sbi_function = emu.cpu.xregs.read(A6);
            match sbi_function {
                SBI_TEZOS_INBOX_NEXT => sbi_tezos_inbox_next(emu, inbox),
                SBI_TEZOS_META_ORIGINATION_LEVEL => sbi_tezos_meta_origination_level(emu, meta),
                SBI_TEZOS_META_ADDRESS => sbi_tezos_meta_address(emu, meta),
                SBI_TEZOS_ED25519_SIGN => sbi_tezos_ed25519_sign(emu),
                SBI_TEZOS_ED25519_VERIFY => sbi_tezos_ed25519_verify(emu),
                SBI_TEZOS_BLAKE2B_HASH256 => sbi_tezos_blake2b_hash256(emu),
                _ => Err(format!(
                    "Unimplemented Tezos SBI extension ({sbi_extension}) function {sbi_function}"
                )
                .into()),
            }
        }
        _ => Err(format!("Unimplemented SBI extension {sbi_extension}").into()),
    }
}

// System calls
const EXIT: u64 = 93;

/// Handle a system call originating from the user program in POSIX style.
pub fn handle_posix(emu: &mut Emulator) -> SBIResult {
    // System call number is contained in a7.
    let syscall_number = emu.cpu.xregs.read(A7);
    match syscall_number {
        EXIT => {
            let code = emu.cpu.xregs.read(A0);
            eprintln!("Received request to exit with code {}", code);
            exit(code as i32);
        }

        _ => Err(format!("Unimplemented system call {syscall_number}").into()),
    }
}
