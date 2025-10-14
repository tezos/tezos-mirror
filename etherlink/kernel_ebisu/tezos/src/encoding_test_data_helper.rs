// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#[cfg(test)]
pub mod test_helpers {

    use regex::Regex;
    use std::fs;

    pub fn fetch_generated_data(
        protocol: &str,
        encoding: &str,
        sample_name: &str,
    ) -> Vec<u8> {
        let filepath = format!(
            "{}/{}/{}-- {}- {}.out",
            env!("CARGO_MANIFEST_DIR"),
            "../../../tezt/tests/expected/encoding.ml",
            protocol,
            "protocol encoding regression test",
            encoding
        );

        println!("{}", &filepath);
        let file_content =
            fs::read_to_string(filepath).expect("Should have been able to read the file");

        let re =
            Regex::new(&format!("{}: ([0-9a-f]+)", regex::escape(sample_name))).unwrap();
        let captures = re
            .captures(&file_content)
            .unwrap_or_else(|| panic!("Should have found {sample_name}"));
        let match_ = captures.get(1).unwrap().as_str();
        println!("{match_}");
        hex::decode(match_).expect("Should have been able to decode the hex string")
    }
}
