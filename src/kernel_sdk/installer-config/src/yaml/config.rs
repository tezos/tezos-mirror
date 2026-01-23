// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::io::Read;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct MoveArgs {
    pub from: String,
    pub to: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct RevealArgs {
    // Hash in hex form
    pub reveal: String,
    // Path
    pub to: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct SetArgs {
    // Value in hex form
    pub value: String,
    // Path
    pub to: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(try_from = "raw_encodings::InstrSerDeser")]
#[serde(into = "raw_encodings::InstrSerDeser")]
pub enum Instr {
    Move(MoveArgs),

    // Uncomment this
    // #[serde(untagged)]
    // and remove InstrSerDeser workaround type
    // when this one is merged https://github.com/serde-rs/serde/pull/2403
    Reveal(RevealArgs),
    Set(SetArgs),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct YamlConfig {
    #[serde(with = "serde_yaml::with::singleton_map_recursive")]
    pub instructions: Vec<Instr>,
}

impl YamlConfig {
    pub fn from_string(s: &str) -> serde_yaml::Result<YamlConfig> {
        serde_yaml::from_str(s)
    }

    pub fn from_reader<R: Read>(rdr: R) -> serde_yaml::Result<YamlConfig> {
        serde_yaml::from_reader(rdr)
    }
}

mod raw_encodings {
    use super::*;

    // The only purpose of this type is
    // to define serialisation and deserialisation easily.
    // Converted back and forth to Instr,
    // this trick is necessary because currently flattening/untagging
    // of enum variants is not supported,
    // see here https://github.com/serde-rs/serde/issues/1402
    // related PR https://github.com/serde-rs/serde/pull/2403
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    pub(super) struct InstrSerDeser {
        #[serde(rename = "move")]
        #[serde(skip_serializing_if = "Option::is_none")]
        move_: Option<MoveArgs>,

        #[serde(flatten)]
        reveal: Option<RevealArgs>,

        #[serde(skip_serializing_if = "Option::is_none")]
        set: Option<SetArgs>,
    }

    impl TryFrom<InstrSerDeser> for Instr {
        type Error = String;

        fn try_from(value: InstrSerDeser) -> Result<Self, Self::Error> {
            let sm = value.move_.is_some() as u32
                + value.reveal.is_some() as u32
                + value.set.is_some() as u32;

            if sm == 0 {
                Err("Neither of instructions deserialized".to_owned())
            } else if sm > 1 {
                Err(format!(
                    "More than one instruction deserialized {:#?}",
                    &value
                ))
            } else if value.move_.is_some() {
                Ok(Instr::Move(value.move_.unwrap()))
            } else if value.reveal.is_some() {
                Ok(Instr::Reveal(value.reveal.unwrap()))
            } else if value.set.is_some() {
                Ok(Instr::Set(value.set.unwrap()))
            } else {
                Err(format!("Unknown instruction {value:#?}"))
            }
        }
    }

    #[allow(clippy::from_over_into)]
    impl Into<InstrSerDeser> for Instr {
        fn into(self) -> InstrSerDeser {
            let default = InstrSerDeser {
                move_: None,
                reveal: None,
                set: None,
            };
            match self {
                Instr::Move(m) => InstrSerDeser {
                    move_: Some(m),
                    ..default
                },
                Instr::Reveal(r) => InstrSerDeser {
                    reveal: Some(r),
                    ..default
                },
                Instr::Set(s) => InstrSerDeser {
                    set: Some(s),
                    ..default
                },
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::{Instr, MoveArgs, RevealArgs, SetArgs, YamlConfig};
    use std::fs::read_to_string;

    #[test]
    fn encode() {
        let instructions = YamlConfig {
            instructions: vec![
                Instr::Move(MoveArgs {
                    from: "/hello/path".to_owned(),
                    to: "/to/path".to_owned(),
                }),
                Instr::Reveal(RevealArgs {
                    reveal: "a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3a1b2c3".to_owned(),
                    to: "/path".to_owned(),
                }),
                Instr::Set(SetArgs {
                    value: "556e20666573746976616c2064652047414454".to_owned(),
                    to: "/path/machin".to_owned(),
                }),
            ],
        };
        let yaml = serde_yaml::to_string(&instructions).unwrap();
        let expected = read_to_string("tests/resources/config_example1.yaml").unwrap();
        assert_eq!(expected.trim(), yaml.trim());
    }

    #[test]
    fn decode_full() {
        let source_yaml =
            read_to_string("tests/resources/config_example2_invalid_hash.yaml").unwrap();
        let instrs = serde_yaml::from_str::<YamlConfig>(&source_yaml).unwrap();
        let expected_instrs = YamlConfig {
            instructions: vec![
                Instr::Move(MoveArgs {
                    from: "/move/path/from".to_owned(),
                    to: "/move/path/to".to_owned(),
                }),
                Instr::Reveal(RevealArgs {
                    reveal: "aea02c32324433".to_owned(),
                    to: "/path/reveal/to".to_owned(),
                }),
            ],
        };
        assert_eq!(expected_instrs, instrs);
    }
}
