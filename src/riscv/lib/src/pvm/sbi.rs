// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{PvmHooks, PvmStatus};
use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Addressable},
        registers::{a0, a1, a2, a3, a6, a7, XValue},
        AccessType, MachineState,
    },
    parser::instruction::Instr,
    state_backend::{CellRead, CellWrite, Manager},
    traps::EnvironException,
};
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use tezos_smart_rollup_constants::{
    core::MAX_INPUT_MESSAGE_SIZE,
    riscv::{
        SbiError, SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_SHUTDOWN, SBI_TEZOS_BLAKE2B_HASH256,
        SBI_TEZOS_ED25519_SIGN, SBI_TEZOS_ED25519_VERIFY, SBI_TEZOS_INBOX_NEXT,
        SBI_TEZOS_METADATA_REVEAL,
    },
};

/// Write the SBI error code as the return value.
#[inline(always)]
fn sbi_return_error<ML: MainMemoryLayout, M: Manager>(
    machine: &mut MachineState<ML, M>,
    code: SbiError,
) {
    machine.hart.xregisters.write(a0, code as i64 as u64);
}

/// Write an arbitrary value as single return value.
#[inline(always)]
fn sbi_return1<ML: MainMemoryLayout, M: Manager>(machine: &mut MachineState<ML, M>, value: XValue) {
    // The SBI caller interprets the return value as a [i64]. We don't want the value to be
    // interpreted as negative because that indicates an error.
    if (value as i64) < 0 {
        return sbi_return_error(machine, SbiError::Failed);
    }

    machine.hart.xregisters.write(a0, value);
}

/// Run the given closure `inner` and write the corresponding SBI results to `machine`.
#[inline(always)]
fn sbi_wrap<ML, M, F>(machine: &mut MachineState<ML, M>, inner: F)
where
    ML: MainMemoryLayout,
    M: Manager,
    F: FnOnce(&mut MachineState<ML, M>) -> Result<XValue, SbiError>,
{
    match inner(machine) {
        Ok(value) => sbi_return1(machine, value),
        Err(error) => sbi_return_error(machine, error),
    }
}

/// Respond to a request for input with no input. Returns `false` in case the
/// machine wasn't expecting any input, otherwise returns `true`.
pub fn provide_no_input<S, ML, M>(status: &mut S, machine: &mut MachineState<ML, M>) -> bool
where
    S: CellWrite<Value = PvmStatus>,
    ML: MainMemoryLayout,
    M: Manager,
{
    // This method should only do something when we're waiting for input.
    match status.read() {
        PvmStatus::WaitingForInput => {}
        _ => return false,
    }

    // We're evaluating again after this.
    status.write(PvmStatus::Evaluating);

    // Inform the caller that there is no more input by returning "bytes written" (a0) as 0.
    sbi_return1(machine, 0);
    true
}

/// Provide input information to the machine. Returns `false` in case the
/// machine wasn't expecting any input, otherwise returns `true`.
pub fn provide_input<S, ML, M>(
    status: &mut S,
    machine: &mut MachineState<ML, M>,
    level: u32,
    counter: u32,
    payload: &[u8],
) -> bool
where
    S: CellWrite<Value = PvmStatus>,
    ML: MainMemoryLayout,
    M: Manager,
{
    // This method should only do something when we're waiting for input.
    match status.read() {
        PvmStatus::WaitingForInput => {}
        _ => return false,
    }

    // We're evaluating again after this.
    status.write(PvmStatus::Evaluating);

    sbi_wrap(machine, |machine| {
        // These arguments should have been set by the previous SBI call.
        let arg_buffer_addr = machine.hart.xregisters.read(a0);
        let arg_buffer_size = machine.hart.xregisters.read(a1);
        let arg_level_addr = machine.hart.xregisters.read(a2);
        let arg_counter_addr = machine.hart.xregisters.read(a3);

        // The argument addresses are virtual addresses. We need to translate them to
        // physical addresses.
        let phys_buffer_addr = machine.translate(arg_buffer_addr, AccessType::Store)?;
        let phys_level_addr = machine.translate(arg_level_addr, AccessType::Store)?;
        let phys_counter_addr = machine.translate(arg_counter_addr, AccessType::Store)?;

        // The SBI caller expects the payload to be returned at [phys_dest_addr]
        // with at maximum [max_buffer_size] bytes written.
        let max_buffer_size = payload.len().min(arg_buffer_size as usize).min(
            // If we were to allow more data to be passed, we could run into problems with proof
            // sizes for inputs.
            MAX_INPUT_MESSAGE_SIZE,
        );

        machine
            .bus
            .write_all(phys_buffer_addr, &payload[..max_buffer_size])?;
        machine.bus.write(phys_level_addr, level)?;
        machine.bus.write(phys_counter_addr, counter)?;

        // At the moment, this case is unlikely to occur because we cap [max_buffer_size] at
        // [MAX_INPUT_MESSAGE_SIZE].
        Ok(max_buffer_size as u64)
    });

    true
}

/// Provide metadata in response to a metadata request. Returns `false`
/// if the machine is not expecting metadata.
pub fn provide_metadata<S, ML, M>(
    status: &mut S,
    machine: &mut MachineState<ML, M>,
    rollup_address: &[u8; 20],
    origination_level: u32,
) -> bool
where
    S: CellWrite<Value = PvmStatus>,
    ML: MainMemoryLayout,
    M: Manager,
{
    // This method should only do something when we're waiting for metadata.
    match status.read() {
        PvmStatus::WaitingForMetadata => {}
        _ => return false,
    }

    // We're evaluating again after this.
    status.write(PvmStatus::Evaluating);

    sbi_wrap(machine, |machine| {
        // These arguments should have been set by the previous SBI call.
        let arg_buffer_addr = machine.hart.xregisters.read(a0);

        // The argument address is a virtual address. We need to translate it to
        // a physical address.
        let phys_dest_addr = machine.translate(arg_buffer_addr, AccessType::Store)?;

        machine
            .bus
            .write_all(phys_dest_addr, rollup_address.as_slice())?;

        // [origination_level] should not wrap around and become negative.
        Ok(origination_level as u64)
    });

    true
}

/// Handle a [SBI_TEZOS_INBOX_NEXT] call.
#[inline]
fn handle_tezos_inbox_next<S>(status: &mut S)
where
    S: CellWrite<Value = PvmStatus>,
{
    // Prepare the EE state for an input tick.
    status.write(PvmStatus::WaitingForInput);
}

/// Handle a [SBI_TEZOS_META] call.
#[inline]
fn handle_tezos_metadata_reveal<S>(status: &mut S)
where
    S: CellWrite<Value = PvmStatus>,
{
    // Prepare the EE state for a reveal metadata tick.
    status.write(PvmStatus::WaitingForMetadata);
}

/// Produce a Ed25519 signature.
#[inline]
fn handle_tezos_ed25519_sign<ML, M>(machine: &mut MachineState<ML, M>) -> Result<u64, SbiError>
where
    ML: MainMemoryLayout,
    M: Manager,
{
    let arg_sk_addr = machine.hart.xregisters.read(a0);
    let arg_msg_addr = machine.hart.xregisters.read(a1);
    let arg_msg_len = machine.hart.xregisters.read(a2);
    let arg_sig_addr = machine.hart.xregisters.read(a3);

    let sk_addr = machine.translate(arg_sk_addr, AccessType::Load)?;
    let msg_addr = machine.translate(arg_msg_addr, AccessType::Load)?;
    let sig_addr = machine.translate(arg_sig_addr, AccessType::Store)?;

    let mut sk_bytes = [0u8; 32];
    machine.bus.read_all(sk_addr, &mut sk_bytes)?;
    let sk = SigningKey::try_from(sk_bytes.as_slice()).map_err(|_| SbiError::Failed)?;
    sk_bytes.fill(0);

    let mut msg_bytes = vec![0; arg_msg_len as usize];
    machine.bus.read_all(msg_addr, &mut msg_bytes)?;

    let sig = sk.sign(msg_bytes.as_slice());
    let sig_bytes: [u8; 64] = sig.to_bytes();
    machine.bus.write_all(sig_addr, &sig_bytes)?;

    Ok(sig_bytes.len() as u64)
}

/// Verify a Ed25519 signature.
#[inline]
fn handle_tezos_ed25519_verify<ML, M>(machine: &mut MachineState<ML, M>) -> Result<u64, SbiError>
where
    ML: MainMemoryLayout,
    M: Manager,
{
    let arg_pk_addr = machine.hart.xregisters.read(a0);
    let arg_sig_addr = machine.hart.xregisters.read(a1);
    let arg_msg_addr = machine.hart.xregisters.read(a2);
    let arg_msg_len = machine.hart.xregisters.read(a3);

    let pk_addr = machine.translate(arg_pk_addr, AccessType::Load)?;
    let sig_addr = machine.translate(arg_sig_addr, AccessType::Store)?;
    let msg_addr = machine.translate(arg_msg_addr, AccessType::Load)?;

    let mut pk_bytes = [0u8; 32];
    machine.bus.read_all(pk_addr, &mut pk_bytes)?;

    let mut sig_bytes = [0u8; 64];
    machine.bus.read_all(sig_addr, &mut sig_bytes)?;

    let mut msg_bytes = vec![0u8; arg_msg_len as usize];
    machine.bus.read_all(msg_addr, &mut msg_bytes)?;

    let pk = VerifyingKey::try_from(pk_bytes.as_slice()).map_err(|_| SbiError::Failed)?;
    let sig = Signature::from_slice(sig_bytes.as_slice()).map_err(|_| SbiError::Failed)?;
    let valid = pk.verify_strict(msg_bytes.as_slice(), &sig).is_ok();

    Ok(valid as u64)
}

/// Compute a BLAKE2B 256-bit digest.
#[inline]
fn handle_tezos_blake2b_hash256<ML, M>(machine: &mut MachineState<ML, M>) -> Result<u64, SbiError>
where
    ML: MainMemoryLayout,
    M: Manager,
{
    let arg_out_addr = machine.hart.xregisters.read(a0);
    let arg_msg_addr = machine.hart.xregisters.read(a1);
    let arg_msg_len = machine.hart.xregisters.read(a2);

    let out_addr = machine.translate(arg_out_addr, AccessType::Store)?;
    let msg_addr = machine.translate(arg_msg_addr, AccessType::Load)?;

    let mut msg_bytes = vec![0u8; arg_msg_len as usize];
    machine.bus.read_all(msg_addr, &mut msg_bytes)?;

    let hash = tezos_crypto_rs::blake2b::digest_256(msg_bytes.as_slice());
    machine.bus.write_all(out_addr, hash.as_slice())?;

    Ok(hash.len() as u64)
}

/// Handle a [SBI_SHUTDOWN] call.
#[inline(always)]
fn handle_shutdown<ML, M>(machine: &mut MachineState<ML, M>)
where
    ML: MainMemoryLayout,
    M: Manager,
{
    // This call always fails.
    handle_not_supported(machine);
}

/// Handle a [SBI_CONSOLE_PUTCHAR] call.
#[inline(always)]
fn handle_console_putchar<ML, M>(machine: &mut MachineState<ML, M>, hooks: &mut PvmHooks)
where
    ML: MainMemoryLayout,
    M: Manager,
{
    let char = machine.hart.xregisters.read(a0) as u8;
    (hooks.putchar_hook)(char);

    // This call always succeeds.
    sbi_return1(machine, 0);
}

/// Handle unsupported SBI calls.
#[inline(always)]
fn handle_not_supported<ML, M>(machine: &mut MachineState<ML, M>)
where
    ML: MainMemoryLayout,
    M: Manager,
{
    // SBI requires us to indicate that we don't support this function by returning
    // `ERR_NOT_SUPPORTED`.
    sbi_return_error(machine, SbiError::NotSupported);
}

/// Handle a PVM SBI call. Returns `true` if it makes sense to continue evaluation.
#[inline]
pub fn handle_call<S, ML, M>(
    status: &mut S,
    machine: &mut MachineState<ML, M>,
    hooks: &mut PvmHooks,
    env_exception: EnvironException,
) -> bool
where
    S: CellWrite<Value = PvmStatus>,
    ML: MainMemoryLayout,
    M: Manager,
{
    if let EnvironException::EnvCallFromMMode = env_exception {
        sbi_return_error(machine, SbiError::Failed);
        return true;
    }

    // No matter the outcome, we need to bump the
    // program counter because ECALL's don't update it
    // to the following instructions.
    let pc = machine.hart.pc.read() + Instr::Ecall.width();
    machine.hart.pc.write(pc);

    // SBI extension is contained in a7.
    let sbi_extension = machine.hart.xregisters.read(a7);
    match sbi_extension {
        SBI_CONSOLE_PUTCHAR => handle_console_putchar(machine, hooks),
        SBI_SHUTDOWN => handle_shutdown(machine),
        SBI_FIRMWARE_TEZOS => {
            let sbi_function = machine.hart.xregisters.read(a6);
            match sbi_function {
                SBI_TEZOS_INBOX_NEXT => handle_tezos_inbox_next(status),
                SBI_TEZOS_METADATA_REVEAL => handle_tezos_metadata_reveal(status),
                SBI_TEZOS_ED25519_SIGN => sbi_wrap(machine, handle_tezos_ed25519_sign),
                SBI_TEZOS_ED25519_VERIFY => sbi_wrap(machine, handle_tezos_ed25519_verify),
                SBI_TEZOS_BLAKE2B_HASH256 => sbi_wrap(machine, handle_tezos_blake2b_hash256),
                _ => handle_not_supported(machine),
            }
        }
        _ => handle_not_supported(machine),
    }

    status.read() == PvmStatus::Evaluating
}
