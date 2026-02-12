// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum Protocol {
    S023 = 23,
}

pub const TARGET_TEZOS_PROTOCOL: Protocol = Protocol::S023;

impl rlp::Encodable for Protocol {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        let val = *self as u8;
        s.append_internal(&val);
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
}
