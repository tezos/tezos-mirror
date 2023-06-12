// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::enc::{
    field, put_byte, put_bytes, BinError, BinResult, BinWriter,
};
use tezos_smart_rollup_host::path::Path;

use crate::binary::SetInstruction;

use super::{
    instr::{ConfigInstruction, MoveInstruction, RefBytes, RevealInstruction},
    owned::OwnedConfigProgram,
};

fn put_le_size(size: usize, out: &mut Vec<u8>) -> BinResult {
    let size = u32::try_from(size).map_err(|_| {
        BinError::custom(format!(
            "Expected {} but got {}",
            (u32::MAX >> 2) as usize,
            size
        ))
    })?;
    tezos_data_encoding::enc::put_bytes(&size.to_le_bytes(), out);
    Ok(())
}

/// Encode RefPath as a bytes with prepended path size
fn path_dynamic(p: &impl Path, output: &mut Vec<u8>) -> BinResult {
    let data = p.as_bytes();
    // We can cast it to u8 as path length doesn't exceed 250
    let size = data.len() as u8;
    put_byte(&size, output);
    put_bytes(data, output);
    Ok(())
}

fn bytes_dynamic(b: &impl AsRef<[u8]>, output: &mut Vec<u8>) -> BinResult {
    put_le_size(b.as_ref().len(), output)?;
    put_bytes(b.as_ref(), output);
    Ok(())
}

impl<'a> BinWriter for RefBytes<'a> {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bytes_dynamic(self, output)
    }
}

impl<P: Path> BinWriter for MoveInstruction<P> {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field("MoveInstruction::from", path_dynamic)(
                &data.from, out,
            )?;
            tezos_data_encoding::enc::field("MoveInstruction::to", path_dynamic)(
                &data.to, out,
            )?;
            Ok(())
        })(self, out)
    }
}

impl<P: Path, B: AsRef<[u8]>> BinWriter for RevealInstruction<P, B> {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            field("RevealInstruction::hash", bytes_dynamic)(&data.hash, out)?;
            field("RevealInstruction::to", path_dynamic)(&data.to, out)?;
            Ok(())
        })(self, out)
    }
}

impl<P: Path, B: AsRef<[u8]>> BinWriter for SetInstruction<P, B> {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            field("SetInstruction::value", bytes_dynamic)(&data.value, out)?;
            field("SetInstruction::to", path_dynamic)(&data.to, out)?;
            Ok(())
        })(self, out)
    }
}

impl<P: Path, B: AsRef<[u8]>> BinWriter for ConfigInstruction<P, B> {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        use tezos_data_encoding::enc::{u8, variant_with_field};
        match self {
            ConfigInstruction::Reveal(inner) => variant_with_field(
                "ConfigInstruction::Reveal",
                u8,
                <RevealInstruction<P, B> as BinWriter>::bin_write,
            )(&0, inner, out),
            ConfigInstruction::Move(inner) => {
                tezos_data_encoding::enc::variant_with_field(
                    "ConfigInstruction::Move",
                    u8,
                    <MoveInstruction<P> as BinWriter>::bin_write,
                )(&1, inner, out)
            }
            ConfigInstruction::Set(inner) => variant_with_field(
                "ConfigInstruction::Set",
                u8,
                <SetInstruction<P, B> as BinWriter>::bin_write,
            )(&2, inner, out),
        }
    }
}

// Encode all commands with appended number of commands at the end.
// It makes possible for the installer_kernel to
// parse commands at the end of the kernel binary.
impl BinWriter for OwnedConfigProgram {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        let initial_size = output.len();
        for i in 0..self.0.len() {
            let mut current_instr = vec![];
            self.0[i].bin_write(&mut current_instr)?;
            // Put size of the instruction encoding first,
            // in order to make a decoding easier
            put_le_size(current_instr.len(), output)?;
            output.extend_from_slice(&current_instr);
        }
        put_le_size(output.len() - initial_size, output)?;
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use tezos_data_encoding::enc::BinWriter;

    use crate::binary::NomReader;

    // I have to pass `out` here because for some reason
    // borrow checker complaines about this line:
    //    `T::nom_read(out).unwrap()`
    // saying that `out` is dropped but still borrowed in this line,
    // despite the faact `decoded` should be dropped on leaving the function
    fn roundtrip<'a, T: Debug + PartialEq + Eq + BinWriter + NomReader<'a>>(
        orig: &T,
        out: &'a mut Vec<u8>,
    ) {
        orig.bin_write(out).unwrap();

        let decoded = T::nom_read(out).unwrap();
        assert!(decoded.0.is_empty());
        assert_eq!(*orig, decoded.1);
    }

    #[test]
    fn roundtrip_encdec() {
        use tezos_smart_rollup_host::path::RefPath;

        use crate::binary::instr::{
            ConfigInstruction, MoveInstruction, RefBytes, RevealInstruction,
        };
        roundtrip(&RefBytes("hello".as_bytes()), &mut vec![]);

        roundtrip(
            &MoveInstruction {
                from: RefPath::assert_from("/d".as_bytes()),
                to: RefPath::assert_from("/cc".as_bytes()),
            },
            &mut vec![],
        );

        roundtrip(
            &RevealInstruction {
                to: RefPath::assert_from("/fldl/sfjisfkj".as_bytes()),
                hash: RefBytes("some hash should be 33 bytes".as_bytes()),
            },
            &mut vec![],
        );

        roundtrip(
            &ConfigInstruction::Reveal(RevealInstruction {
                to: RefPath::assert_from("/fldl/sfjisfkj".as_bytes()),
                hash: RefBytes("some hash should be 33 bytes".as_bytes()),
            }),
            &mut vec![],
        );
    }
}
