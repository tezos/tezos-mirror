// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use nom::combinator::{complete, map, map_res};
use nom::multi::length_data;
use nom::number::complete::{u32, u8};
use nom::sequence::tuple;
use nom::{error::ErrorKind, number::Endianness};
use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_host::path::{Path, RefPath, PATH_MAX_SIZE};
use tezos_smart_rollup_host::storage::StorageV1;

use super::{
    ConfigInstruction, MoveInstruction, RefBytes, RevealInstruction, SetInstruction,
};

// Those types and helpers copy paseted from tezos_data_encoding.
// As it's required to parse refs, lifetime 'a added to NomReader
pub type NomInput<'a> = &'a [u8];

pub type NomError<'a> = nom::error::Error<NomInput<'a>>;

pub type NomResult<'a, T> = nom::IResult<NomInput<'a>, T>;

// NomReader is like tezos_data_encoding::enc::NomReader,
// but tweaked with lifetime 'a
pub trait NomReader<'a>: Sized {
    fn nom_read(input: &'a [u8]) -> NomResult<'a, Self>;
}

pub fn size(input: NomInput) -> NomResult<u32> {
    u32(Endianness::Little)(input)
}

pub fn short_size(input: NomInput) -> NomResult<u8> {
    u8(input)
}

// Copy-pasted from tezos_data_encoding and returning error tweaked
fn bounded_size(max: usize) -> impl FnMut(NomInput) -> NomResult<u32> {
    move |input| {
        let (input, size) = size(input)?;
        if size as usize <= max {
            Ok((input, size))
        } else {
            Err(nom::Err::Error(nom::error::Error {
                input,
                code: ErrorKind::TooLarge,
            }))
        }
    }
}

fn bounded_u8_size(max: usize) -> impl FnMut(NomInput) -> NomResult<u8> {
    move |input| {
        let (input, size) = short_size(input)?;
        if size as usize <= max {
            Ok((input, size))
        } else {
            Err(nom::Err::Error(nom::error::Error {
                input,
                code: ErrorKind::TooLarge,
            }))
        }
    }
}

fn nom_read_ref_path(input: &[u8]) -> NomResult<RefPath> {
    map_res(
        complete(nom::multi::length_data(bounded_u8_size(PATH_MAX_SIZE))),
        |bytes| Ok::<RefPath<'_>, NomError<'_>>(RefPath::assert_from(bytes)),
    )(input)
}

pub fn read_size(
    host: &impl StorageV1,
    path: &impl Path,
    offset: &mut usize,
) -> Result<u32, &'static str> {
    let mut size_buffer = [0; 4];
    host.store_read_slice(path, *offset, &mut size_buffer)
        .map_err(|_| "Couldn't read from kernel boot path")?;
    *offset += 4;

    Ok(u32::from_le_bytes(size_buffer))
}

pub fn completed<T>(x: (&[u8], T)) -> Result<T, &'static str> {
    if !x.0.is_empty() {
        Err("Incompleted parsing")
    } else {
        Ok(x.1)
    }
}

impl<'a> NomReader<'a> for RefBytes<'a> {
    fn nom_read(input: &'a [u8]) -> NomResult<'a, Self> {
        map_res(
            complete(length_data(bounded_size(MAX_FILE_CHUNK_SIZE))),
            |bytes| Ok::<RefBytes<'_>, NomError<'_>>(RefBytes(bytes)),
        )(input)
    }
}

impl<'a> NomReader<'a> for RevealInstruction<RefPath<'a>, RefBytes<'a>> {
    fn nom_read(bytes: &'a [u8]) -> NomResult<'a, Self> {
        map(
            nom::sequence::tuple((
                <RefBytes<'a> as NomReader>::nom_read,
                nom_read_ref_path,
            )),
            |(hash, to)| RevealInstruction { hash, to },
        )(bytes)
    }
}

impl<'a> NomReader<'a> for MoveInstruction<RefPath<'a>> {
    fn nom_read(bytes: &'a [u8]) -> NomResult<'a, Self> {
        map(
            tuple((nom_read_ref_path, nom_read_ref_path)),
            |(from, to)| MoveInstruction { from, to },
        )(bytes)
    }
}

impl<'a> NomReader<'a> for SetInstruction<RefPath<'a>, RefBytes<'a>> {
    fn nom_read(bytes: &'a [u8]) -> NomResult<'a, Self> {
        map(
            nom::sequence::tuple((
                <RefBytes<'a> as NomReader>::nom_read,
                nom_read_ref_path,
            )),
            |(value, to)| SetInstruction { value, to },
        )(bytes)
    }
}

impl<'a> NomReader<'a> for ConfigInstruction<RefPath<'a>, RefBytes<'a>> {
    fn nom_read(bytes: &'a [u8]) -> NomResult<'a, Self> {
        let (input, tag) = nom::number::complete::u8(bytes)?;
        let (input, variant) = match tag {
            0 => (map(
                <RevealInstruction<RefPath<'a>, RefBytes<'a>> as NomReader>::nom_read,
                ConfigInstruction::Reveal,
            ))(input)?,
            1 => (map(
                <MoveInstruction<RefPath<'a>> as NomReader>::nom_read,
                ConfigInstruction::Move,
            ))(input)?,
            2 => (map(
                <SetInstruction<RefPath<'a>, RefBytes<'a>> as NomReader>::nom_read,
                ConfigInstruction::Set,
            ))(input)?,
            _ => {
                return Err(nom::Err::Error(nom::error::Error {
                    input,
                    code: ErrorKind::Tag,
                }))
            }
        };
        Ok((input, variant))
    }
}
