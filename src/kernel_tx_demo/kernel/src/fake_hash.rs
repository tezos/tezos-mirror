// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Arbitrary hashes for testing.

use crypto::hash::ContractKt1Hash;
use proptest::{prelude::*, sample};

/// Randomly selected Kt1 address.
pub fn arb_kt1() -> BoxedStrategy<ContractKt1Hash> {
    sample::select(KT1)
        .prop_map(|kt1| {
            ContractKt1Hash::from_base58_check(kt1).expect("kt1 address should be valid")
        })
        .boxed()
}

const KT1: &[&str] = &[
    "KT19c8n5mWrqpxMcR3J687yssHxotj88nGhZ",
    "KT19xDbLsvQKnp9xqfDNPWJbKJJmV93dHDUa",
    "KT1A56dh8ivKNvLiLVkjYPyudmnY2Ti5Sba3",
    "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo",
    "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",
    "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc",
    "KT1BvVxWM6cjFuJNet4R9m64VDCN2iMvjuGE",
    "KT1CM1g1o9RKDdtDKgcBWE59X2KgTc2TcYtC",
    "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy",
    "KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ",
    "KT1CjfCztmRpsyUee1nLa9Wcpfr7vgwqRZmk",
    "KT1Cz7TyVFvHxXpxLS57RFePrhTGisUpPhvD",
    "KT1D5NmtDtgCwPxYNb2ZK2But6dhNLs1T1bV",
    "KT1D68BvUm9N1fcq6uaZnyZvmBkBvj9biyPu",
    "KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43",
    "KT1DfDzRjbXnTGogM2Nih8mpXcSVM54igtNs",
    "KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat",
    "KT1DnfT4hfikoMY3uiPE9mQV4y3Xweramb2k",
    "KT1DrJV8vhkdLEj76h1H9Q4irZDqAkMPo1Qf",
    "KT1EWLAQGPMF2uhtVRPaCH2vtFVN36Njdr6z",
    "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x",
    "KT1FL3C6t9Lyfskyb6rQrCRQTnf7M9t587VM",
    "KT1FN5fcNNcgieGjzxbVEPWUpJGwZEpzNGA8",
    "KT1Fcq4inD44aMhmUiTEHR1QMQwJT7p2u641",
    "KT1FuFDZGdw86p6krdBUKoZfEMkcUmezqX5o",
    "KT1Gow8VzXZx3Akn5kvjACqnjnyYBxQpzSKr",
    "KT1HMCxCyeGbZaGBsLMKVyMbMRzFpZBxKoY7",
    "KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn",
    "KT1HvwFnXteMbphi7mfPDhCWkZSDvXEz8iyv",
    "KT1JW6PwhfaEJu6U3ENsxUeja48AdtqSoekd",
    "KT1KRyTaxCAM3YRquifEe29BDbUKNhJ6hdtx",
    "KT1Kfbk3B6NYPCPohPBDU3Hxf5Xeyy9PdkNp",
    "KT1LQ99RfGcmFe98PiBcGXuyjBkWzAcoXXhW",
    "KT1LZFMGrdnPjRLsCZ1aEDUAF5myA5Eo4rQe",
    "KT1Lc9a9E7vqt6XYtkUbrErDGLQ55HztXV5N",
    "KT1LfQjDNgPpdwMHbhzyQcD8GTE2L4rwxxpN",
    "KT1Ln1MPvHDJ1phLL8dNL4jrKF6Q1yQCBG1v",
    "KT1M1ynE3YXkM7qLZoMppq6szMbBvxX9yQVL",
    "KT1MHDHRLugz3A4qP6KqZDpa7FFmZfcJauV4",
    "KT1MXuZJJFg4EVpLQeLeuHvznTRiNefh3yCs",
    "KT1Mjjcb6tmSsLm7Cb3DSQszePjfchPM4Uxm",
    "KT1Msatnmdy24sQt6knzpALs4tvHfSPPduA2",
    "KT1MzfYSbq18fYr4f44aQRoZBQN72BAtiz5j",
    "KT1PAHhgkwe7QrQgvaJhKJvi4aStCPi6BUWD",
    "KT1PDAELuX7CypUHinUgFgGFskKs7ytwh5Vw",
    "KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn",
    "KT1Puc9St8wdNoGtLiD2WXaHbWU7styaxYhD",
    "KT1PyX9b8WmShQjqNgDQsvxqj9UYdmHLr3xg",
    "KT1Q1kfbvzteafLvnGz92DGvkdypXfTGfEA3",
    "KT1QWdbASvaTXW8GWfhfNh3JMjgXvnZAATJW",
    "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG",
    "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton",
    "KT1RUT25eGgo9KKWXfLhj1xYjghAY1iZ2don",
    "KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9",
    "KT1SLWhfqPtQq7f4zLomh8BNgDeprF9B6d2M",
    "KT1THsDNgHtN56ew9VVCAUWnqPC81pqAxCEp",
    "KT1TZCh8fmUbuDqFxetPWC2fsQanAHzLx4W9",
    "KT1TcAHw5gpejyemwRtdNyFKGBLc4qwA5gtw",
    "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq",
    "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5",
    "KT1TzamC1SCj68ia2E4q2GWZeT24yRHvUZay",
    "KT1Um7ieBEytZtumecLqGeL56iY6BuWoBgio",
    "KT1VDVdGMJYNVzm6QhHoETPz1cQDq7f5UGtF",
    "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN",
    "KT1VsSxSXUkgw6zkBGgUuDXXuJs9ToPqkrCg",
    "KT1VvXEpeBpreAVpfp4V8ZujqWu2gVykwXBJ",
    "KT1W148mcjmfvr9J2RvWcGHxsAFApq9mcfgT",
    "KT1WPEis2WhAc2FciM2tZVn8qe6pCBe9HkDp",
    "KT1XTXBsEauzcv3uPvVXW92mVqrx99UGsb9T",
];
