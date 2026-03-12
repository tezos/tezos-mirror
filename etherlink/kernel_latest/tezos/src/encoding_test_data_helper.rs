// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#[cfg(test)]
pub mod test_helpers {

    use regex::Regex;
    use std::fs;

    use crate::protocol::Protocol;

    const ENCODING_REGRESSION_TRACES_DIRECTORY: &str =
        "../../../tezt/tests/expected/encoding.ml";
    const FALLBACK_ENCODING_REGRESSION_TRACES_DIRECTORY: &str = "regressions";

    pub fn regression_trace(
        dir: &str,
        protocol: Protocol,
        encoding: &str,
    ) -> Result<String, std::io::Error> {
        let filepath = format!(
            "{}/{}/{:?}-- {}- {}.out",
            env!("CARGO_MANIFEST_DIR"),
            dir,
            protocol,
            "protocol encoding regression test",
            encoding
        );
        println!("{}", &filepath);
        fs::read_to_string(filepath)
    }

    pub fn fetch_generated_data(
        protocol: Protocol,
        encoding: &str,
        sample_name: &str,
    ) -> Vec<u8> {
        let file_content = match regression_trace(
            ENCODING_REGRESSION_TRACES_DIRECTORY,
            protocol,
            encoding,
        ) {
            Ok(file_content) => file_content,
            Err(_) => {
                // For frozen protocols, the regression traces are
                // moved to the legacy directory because Tezt
                // checks that all regression traces correspond to
                // regression tests which still exist.
                regression_trace(
                    FALLBACK_ENCODING_REGRESSION_TRACES_DIRECTORY,
                    protocol,
                    encoding,
                )
                .expect("Should have been able to read the file")
            }
        };

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
