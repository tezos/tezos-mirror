/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Micheline serialization.

use std::mem::size_of;

use crate::{
    ast::{annotations::Annotations, Micheline},
    lexer::{Annotation, Prim},
};

/// Prefix denoting an encoded string.
const STRING_TAG: u8 = 0x01;
/// Prefix denoting an encoded sequence.
const SEQ_TAG: u8 = 0x02;
/// Prefix denoting an encoded bytes sequence.
const BYTES_TAG: u8 = 0x0a;

trait AppEncoder<'a>: IntoIterator<Item = &'a Micheline<'a>> + Sized {
    const NO_ANNOTS_TAG: u8;
    const WITH_ANNOTS_TAG: u8;
    fn encode(prim: &Prim, args: Self, annots: &Annotations, out: &mut Vec<u8>) {
        if annots.is_empty() {
            out.push(Self::NO_ANNOTS_TAG);
        } else {
            out.push(Self::WITH_ANNOTS_TAG);
        }
        prim.encode(out);
        for arg in args {
            encode_micheline(arg, out)
        }
        if !annots.is_empty() {
            annots.encode_bytes(out)
        }
    }
}

impl<'a> AppEncoder<'a> for [&'a Micheline<'a>; 0] {
    const NO_ANNOTS_TAG: u8 = 0x03;
    const WITH_ANNOTS_TAG: u8 = 0x04;
}

impl<'a> AppEncoder<'a> for [&'a Micheline<'a>; 1] {
    const NO_ANNOTS_TAG: u8 = 0x05;
    const WITH_ANNOTS_TAG: u8 = 0x06;
}

impl<'a> AppEncoder<'a> for [&'a Micheline<'a>; 2] {
    const NO_ANNOTS_TAG: u8 = 0x07;
    const WITH_ANNOTS_TAG: u8 = 0x08;
}

impl<'a> AppEncoder<'a> for &'a [Micheline<'a>] {
    const NO_ANNOTS_TAG: u8 = 0x09;
    const WITH_ANNOTS_TAG: u8 = 0x09;
    fn encode(prim: &Prim, args: Self, annots: &Annotations, out: &mut Vec<u8>) {
        match args {
            [] => AppEncoder::encode(prim, [], annots, out),
            [arg] => AppEncoder::encode(prim, [arg], annots, out),
            [arg1, arg2] => AppEncoder::encode(prim, [arg1, arg2], annots, out),
            _ => {
                out.push(Self::WITH_ANNOTS_TAG);
                prim.encode(out);
                with_patchback_len(out, |out| {
                    for arg in args {
                        encode_micheline(arg, out)
                    }
                });
                annots.encode_bytes(out)
            }
        }
    }
}

impl Annotation<'_> {
    pub fn encode_bytes(&self, out: &mut Vec<u8>) {
        match self {
            Annotation::Special(s) => out.extend_from_slice(s.as_bytes()),
            Annotation::Field(s) => {
                out.push(b'%');
                out.extend_from_slice(s.as_bytes());
            }
            Annotation::Variable(s) => {
                out.push(b'@');
                out.extend_from_slice(s.as_bytes());
            }
            Annotation::Type(s) => {
                out.push(b':');
                out.extend_from_slice(s.as_bytes());
            }
        }
    }
}

impl Annotations<'_> {
    pub fn encode_bytes(&self, out: &mut Vec<u8>) {
        with_patchback_len(out, |out| {
            // Add them space-separated
            let mut is_first = true;
            for ann in self.iter() {
                if !is_first {
                    out.push(b' ')
                }
                is_first = false;
                ann.encode_bytes(out);
            }
        })
    }
}

/// Length of some container, usually stored as fixed-length number.
type Len = u32;

/// Put length of something.
fn put_len(len: Len, out: &mut Vec<u8>) {
    out.extend_from_slice(&len.to_be_bytes())
}

/// Put bytestring (with its length).
fn put_bytes(bs: &[u8], out: &mut Vec<u8>) {
    out.push(BYTES_TAG);
    put_len(bs.len() as Len, out);
    out.extend_from_slice(bs)
}

/// Put a Michelson string.
fn put_string(s: &str, out: &mut Vec<u8>) {
    out.push(STRING_TAG);
    put_len(s.len() as Len, out);
    out.extend_from_slice(s.as_bytes())
}

fn with_patchback_len(out: &mut Vec<u8>, f: impl FnOnce(&mut Vec<u8>)) {
    put_len(0, out); // don't know the right length in advance
    let i = out.len();
    let len_place = (i - size_of::<Len>())..i; // to fill length later
    f(out);
    let len_of_written = (out.len() - i) as Len;
    out[len_place].copy_from_slice(&len_of_written.to_be_bytes())
}

/// Put a container.
fn put_seq<V>(list: &[V], out: &mut Vec<u8>, encoder: fn(&V, &mut Vec<u8>)) {
    out.push(SEQ_TAG);
    with_patchback_len(out, |out| {
        for val in list {
            encoder(val, out)
        }
    });
}

/// Recursive encoding function for [Value].
fn encode_micheline(mich: &Micheline, out: &mut Vec<u8>) {
    use Micheline::*;
    match mich {
        Int(_) => todo!(), // for a later MR
        String(s) => put_string(s, out),
        Bytes(b) => put_bytes(b, out),
        Seq(s) => put_seq(s, out, encode_micheline),
        App(prim, args, anns) => AppEncoder::encode(prim, *args, anns, out),
    }
}

impl<'a> Micheline<'a> {
    /// Serialize value.
    #[allow(dead_code)] // Until we add PACK
    fn encode(&self) -> Vec<u8> {
        self.encode_starting_with(&[])
    }

    /// Like [Value::encode], but allows specifying a prefix, useful for
    /// `PACK` implementation.
    fn encode_starting_with(&self, start_bytes: &[u8]) -> Vec<u8> {
        let mut out = Vec::from(start_bytes);
        encode_micheline(self, &mut out);
        out
    }
}
