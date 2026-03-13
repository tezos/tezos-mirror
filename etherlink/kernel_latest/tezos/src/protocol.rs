// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum Protocol {
    S023 = 23,
}

pub const TARGET_TEZOS_PROTOCOL: Protocol = Protocol::S023;

/// The first protocol to support the `next_protocol` field in block
/// headers. Used as the default when decoding headers from kernels
/// that predate this field.
pub const INITIAL_PROTOCOL: Protocol = Protocol::S023;

impl rlp::Encodable for Protocol {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        let val = *self as u8;
        s.append_internal(&val);
    }
}

impl rlp::Decodable for Protocol {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let val: u8 = rlp.as_val()?;
        match val {
            23 => Ok(Protocol::S023),
            _ => Err(rlp::DecoderError::Custom("Unknown protocol version")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Protocol;

    #[test]
    pub fn rpl_encode_protocol() {
        let mut stream = rlp::RlpStream::new();
        stream.append(&Protocol::S023);
        let out = stream.out();
        assert_eq!(out, vec![23]);
    }

    #[test]
    pub fn rlp_decode_protocol() {
        let encoded = vec![23];
        let rlp = rlp::Rlp::new(&encoded);
        let decoded: Protocol = rlp.as_val().unwrap();
        assert_eq!(decoded, Protocol::S023);
    }

    #[test]
    pub fn rlp_roundtrip_protocol() {
        let mut stream = rlp::RlpStream::new();
        let protocol = Protocol::S023;
        stream.append(&protocol);
        let encoded = stream.out();
        let rlp = rlp::Rlp::new(&encoded);
        let decoded: Protocol = rlp.as_val().unwrap();
        assert_eq!(decoded, protocol);
    }

    #[test]
    pub fn rlp_decode_unknown_protocol() {
        let encoded = vec![42];
        let rlp = rlp::Rlp::new(&encoded);
        let result: Result<Protocol, _> = rlp.as_val();
        assert!(result.is_err());
    }
}
