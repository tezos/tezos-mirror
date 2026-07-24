// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum Protocol {
    T024 = 24,
    U025 = 25,
}

pub const TARGET_TEZOS_PROTOCOL: Protocol = Protocol::U025;

/// The oldest protocol the Michelson runtime still supports. Used as the
/// default `next_protocol` when decoding headers from kernels that predate
/// this field.
pub const INITIAL_PROTOCOL: Protocol = Protocol::T024;

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
            24 => Ok(Protocol::T024),
            25 => Ok(Protocol::U025),
            _ => Err(rlp::DecoderError::Custom("Unknown protocol version")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Protocol;

    #[test]
    pub fn rpl_encode_protocol_t024() {
        let mut stream = rlp::RlpStream::new();
        stream.append(&Protocol::T024);
        let out = stream.out();
        assert_eq!(out, vec![24]);
    }

    #[test]
    pub fn rlp_decode_protocol_t024() {
        let encoded = vec![24];
        let rlp = rlp::Rlp::new(&encoded);
        let decoded: Protocol = rlp.as_val().unwrap();
        assert_eq!(decoded, Protocol::T024);
    }

    #[test]
    pub fn rlp_roundtrip_protocol_t024() {
        let mut stream = rlp::RlpStream::new();
        let protocol = Protocol::T024;
        stream.append(&protocol);
        let encoded = stream.out();
        let rlp = rlp::Rlp::new(&encoded);
        let decoded: Protocol = rlp.as_val().unwrap();
        assert_eq!(decoded, protocol);
    }

    #[test]
    pub fn rpl_encode_protocol_u025() {
        let mut stream = rlp::RlpStream::new();
        stream.append(&Protocol::U025);
        let out = stream.out();
        assert_eq!(out, vec![25]);
    }

    #[test]
    pub fn rlp_decode_protocol_u025() {
        let encoded = vec![25];
        let rlp = rlp::Rlp::new(&encoded);
        let decoded: Protocol = rlp.as_val().unwrap();
        assert_eq!(decoded, Protocol::U025);
    }

    #[test]
    pub fn rlp_roundtrip_protocol_u025() {
        let mut stream = rlp::RlpStream::new();
        let protocol = Protocol::U025;
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
