// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub mod instruction;

use crate::instruction::Instr;
use core::ops::Range;

fn parse_uncompressed_instruction(bytes: u32) -> Instr {
    // TODO parse an uncompressed instruction
    Instr::Unknown { instr: bytes }
}

fn parse_compressed_instruction(bytes: u16) -> Instr {
    // TODO parse a compressed instruction
    Instr::UnknownCompressed { instr: bytes }
}

/// Attempt to parse `bytes` into an instruction. If `bytes` encodes a 2-byte
/// compressed instruction, parse it immediately. If it encodes a 4-byte
/// uncompressed instruction, request 2 extra bytes via `more`.
pub fn parse<E>(bytes: u16, more: impl FnOnce() -> Result<u16, E>) -> Result<Instr, E> {
    if bytes & 0b11 != 0b11 {
        Ok(parse_compressed_instruction(bytes))
    } else {
        let upper = more()?;
        let combined = (upper as u32) << 16 | (bytes as u32);
        Ok(parse_uncompressed_instruction(combined))
    }
}

pub fn u16_iter_from_u8_iter(mut iter: impl Iterator<Item = u8>) -> impl Iterator<Item = u16> {
    std::iter::from_fn(move || {
        let lower = iter.next()?;
        iter.next().map(|upper| u16::from_le_bytes([lower, upper]))
    })
}

pub fn instr_iter_from_u16_iter(
    mut iter: impl Iterator<Item = u16>,
) -> impl Iterator<Item = Instr> {
    std::iter::from_fn(move || parse(iter.next()?, || iter.next().ok_or(())).ok())
}

fn parse_block(bytes: &[u8]) -> Vec<Instr> {
    let iter = bytes.iter().copied();
    instr_iter_from_u16_iter(u16_iter_from_u8_iter(iter)).collect()
}

pub fn parse_segment(contents: &[u8], range: Range<usize>) -> Vec<Instr> {
    parse_block(&contents[range])
}

#[cfg(test)]
mod tests {
    use super::*;

    // rv64ui-p-addiw
    // 0000000080000000 <_start>:
    //     80000000:	0500006f          	jal	zero,80000050 <reset_vector>

    // 0000000080000004 <trap_vector>:
    //     80000004:	34202f73          	csrrs	t5,mcause,zero
    //     80000008:	00800f93          	addi	t6,zero,8
    #[test]
    fn test_1() {
        let bytes: [u8; 12] = [
            0x6f, 0x0, 0x0, 0x5, 0x73, 0x2f, 0x20, 0x34, 0x93, 0xf, 0x80, 0x0,
        ];

        let expected = [
            Instr::Unknown {
                instr: u32::from_le_bytes([0x6f, 0x0, 0x0, 0x5]),
            },
            Instr::Unknown {
                instr: u32::from_le_bytes([0x73, 0x2f, 0x20, 0x34]),
            },
            Instr::Unknown {
                instr: u32::from_le_bytes([0x93, 0xf, 0x80, 0x0]),
            },
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // rv64uc-p-rvc
    // 0000000080002190 <test_21>:
    // 80002190:	01500193          	addi	gp,zero,21
    // 80002194:	6405                	c.lui	s0,0x1
    // 80002196:	2344041b          	addiw	s0,s0,564 # 1234 <_start-0x7fffedcc>
    // 8000219a:	0412                	c.slli	s0,0x4
    // 8000219c:	000123b7          	lui	t2,0x12
    // 800021a0:	3403839b          	addiw	t2,t2,832 # 12340 <_start-0x7ffedcc0>
    // 800021a4:	12741063          	bne	s0,t2,800022c4 <fail>
    #[test]
    fn test_2() {
        let bytes: [u8; 24] = [
            0x93, 0x1, 0x50, 0x1, 0x5, 0x64, 0x1b, 0x4, 0x44, 0x23, 0x12, 0x4, 0xb7, 0x23, 0x1,
            0x0, 0x9b, 0x83, 0x3, 0x34, 0x63, 0x10, 0x74, 0x12,
        ];
        let expected = [
            Instr::Unknown {
                instr: u32::from_le_bytes([0x93, 0x1, 0x50, 0x1]),
            },
            Instr::UnknownCompressed {
                instr: u16::from_le_bytes([0x05, 0x64]),
            },
            Instr::Unknown {
                instr: u32::from_le_bytes([0x1b, 0x4, 0x44, 0x23]),
            },
            Instr::UnknownCompressed {
                instr: u16::from_le_bytes([0x12, 0x04]),
            },
            Instr::Unknown {
                instr: u32::from_le_bytes([0xb7, 0x23, 0x1, 0x0]),
            },
            Instr::Unknown {
                instr: u32::from_le_bytes([0x9b, 0x83, 0x3, 0x34]),
            },
            Instr::Unknown {
                instr: u32::from_le_bytes([0x63, 0x10, 0x74, 0x12]),
            },
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Misaligned buffer, last byte ignored
    #[test]
    fn test_3() {
        let bytes: [u8; 5] = [0x1, 0x5, 0x64, 0x1b, 0x4];
        let expected = [
            Instr::UnknownCompressed {
                instr: u16::from_le_bytes([0x1, 0x5]),
            },
            Instr::UnknownCompressed {
                instr: u16::from_le_bytes([0x64, 0x1b]),
            },
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Expected uncompressed instruction, only 2 bytes left
    #[test]
    fn test_4() {
        let bytes: [u8; 6] = [0x6f, 0x0, 0x0, 0x5, 0x73, 0x2f];
        let expected = [Instr::Unknown {
            instr: u32::from_le_bytes([0x6f, 0x0, 0x0, 0x5]),
        }];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }
}
