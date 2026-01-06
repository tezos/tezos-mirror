/*************************************************************************/
/*                                                                       */
/* SPDX-License-Identifier: MIT                                          */
/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>          */
/* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com> */
/*                                                                       */
/*************************************************************************/

/*
 * receipt_bloom.c
 *
 * SQLite C static extension implementing:
 *
 *   receipt_contains_bloom_filter(receipt_blob, bloom256_blob) -> INTEGER
 *
 * This function checks whether a given 256-byte bloom filter is contained
 * in the `logs_bloom` field of a receipt. This function does not allocate
 * anything except for the final integer result.
 *
 * -----------------------------------------------------------------------
 *
 * RECEIPT BINARY FORMAT (argument #1)
 *
 * The receipt is a binary blob consisting of a sequence of "chunks":
 *
 *   [ int32_be length ][ raw data ] # cumulative_gas_used (hex string quantity)
 *   [ int32_be length ][ raw data ] # effective_gas_price (hex string quantity)
 *   [ int32_be length ][ raw data ] # gas_used (hex string quantity)
 *   [ int32_be length ][ raw data ] # logs (dynamic size list)
 *   [ int32_be length ][ raw data ] # logs_bloom (hex string repr of byte sequence)
 *   ... # we don't care about the rest
 *
 * WARNING: This format is directly linked to how receipts are encoded in the
 * data base (see etherlink/bin_node/lib_dev/encodings/transaction_info.ml). If
 * these were to change, the parsing function below would have too.
 *
 * - Length prefixes are 4 bytes, big-endian.
 * - Data immediately follows its length.
 *
 * For this function we assume:
 *
 *   - The receipt has AT LEAST 5 chunks.
 *   - The first 4 chunks are skipped entirely.
 *   - The 5th chunk contains the `logs_bloom` field.
 *
 * LOGS_BLOOM FIELD
 *
 * The 5th chunk (logs_bloom) has the following properties:
 *
 *   - It is ASCII-encoded hexadecimal.
 *   - It represents binary bloom filter data.
 *   - It starts with the ASCII prefix "0x".
 *   - Size is 514 bytes: 0x + 512 hex characters for 256 bytes
 *
 * For the bloom inclusion check we only use the FIRST 256 binary bytes,
 * i.e. the first 512 hex characters (after stripping "0x" if present).
 *
 * -----------------------------------------------------------------------
 *
 * BLOOM FILTER ARGUMENT (argument #2)
 *
 * - Must be a BLOB of EXACTLY 256 bytes.
 * - Treated as a binary bloom filter mask.
 *
 * -----------------------------------------------------------------------
 *
 * BLOOM FILTER INCLUSION LOGIC
 *
 * For each byte i in [0, 255]:
 *
 *   Let:
 *     b1 = byte decoded from logs_bloom (ASCII hex → binary)
 *     b2 = bloom256_blob[i]
 *
 *   Condition:
 *     (b1 & b2) == b2
 *
 * Interpretation:
 *   Every bit set in b2 must also be set in b1.
 *
 * If this holds for all 256 bytes → return 1.
 * Otherwise → return 0.
 *
 * OPTIMIZATION:
 *
 *   If b2 == 0, the condition is always true, so decoding b1 is skipped.
 *
 * -----------------------------------------------------------------------
 *
 * FAILURE BEHAVIOR
 *
 * The function raises an exception if:
 *   - Any argument is NULL
 *   - The receipt is malformed
 *   - Lengths are inconsistent
 *   - logs_bloom contains invalid hex
 *   - bloom256_blob length != 256
 *
 * -----------------------------------------------------------------------
 *
 * See https://www.sqlite.org/appfunc.html for SQLite C API for user
 * defined functions.
 */

#include <sqlite3.h>
#include <stdint.h>
#include <stddef.h>   /* for NULL */
#include <assert.h>

/* ------------------------------------------------------------------ */
/* Hex decoding helpers                                               */
/* ------------------------------------------------------------------ */

/* Convert a single ASCII hex character to its numeric value.
 * Returns -1 if the character is not valid hexadecimal. */
static inline int hex_value(unsigned char c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/* Convert two ASCII hex characters into one byte.
 * Returns -1 on invalid input. */
static inline int hex_byte(unsigned char hi, unsigned char lo)
{
    int h = hex_value(hi);
    int l = hex_value(lo);
    if (h < 0 || l < 0) return -1;
    return (h << 4) | l;
}

/* Read a 32-bit unsigned integer in big-endian order. */
static inline uint32_t read_u32_be(const unsigned char *p)
{
    return ((uint32_t)p[0] << 24) |
           ((uint32_t)p[1] << 16) |
           ((uint32_t)p[2] << 8)  |
           ((uint32_t)p[3]);
}

/* ------------------------------------------------------------------ */
/* SQLite function implementation                                     */
/* ------------------------------------------------------------------ */

#define SQLITE_RETURN_TRUE(ctx) \
    do { sqlite3_result_int((ctx), 1); return; } while (0)

#define SQLITE_RETURN_FALSE(ctx) \
    do { sqlite3_result_int((ctx), 0); return; } while (0)

#define SQLITE_RETURN_ERROR(ctx, msg) \
    do { sqlite3_result_error((ctx), (msg), -1); return; } while (0)

#define READ_U32_BE(ctx, p, end)                        \
    ({                                                  \
        uint32_t _v;                                    \
        if (p + 4 > end)                                \
             SQLITE_RETURN_ERROR(ctx,                   \
               "Cannot read length prefix in receipt"); \
        _v = read_u32_be(p);                            \
        p += 4;                                         \
        _v;                                             \
    })

static void receipt_contains_bloom_filter(
    sqlite3_context *ctx,
    int argc,
    sqlite3_value **argv
) {
    // registered with 2 args
    assert(argc == 2);

    // ---- Argument extraction -------------------------------------

    const unsigned char *receipt = sqlite3_value_blob(argv[0]);
    int receipt_len = sqlite3_value_bytes(argv[0]);

    const unsigned char *bloom = sqlite3_value_blob(argv[1]);
    int bloom_len = sqlite3_value_bytes(argv[1]);

    // Preconditions on arguments
    if (!receipt)
        SQLITE_RETURN_ERROR(
            ctx,
            "receipt_contains_bloom_filter: NULL receipt"
        );
    if (!bloom)
        SQLITE_RETURN_ERROR(
            ctx,
            "receipt_contains_bloom_filter: NULL bloom filter"
        );
    if (bloom_len != 256)
        SQLITE_RETURN_ERROR(
            ctx,
            "receipt_contains_bloom_filter: bloom filter should be 256 bytes"
        );

    const unsigned char *p = receipt;
    const unsigned char *end = receipt + receipt_len;

    // ---- Skip the first four chunks -------------------------------
    // - cumulative_gas_used (hex string quantity)
    // - effective_gas_price (hex string quantity)
    // - gas_used (hex string quantity)
    // - logs (dynamic size list)

    for (int i = 0; i < 4; i++) {
        uint32_t len = READ_U32_BE(ctx, p, end);

        if (p + len > end)
            SQLITE_RETURN_ERROR(
                ctx,
                "receipt_contains_bloom_filter: not enough data in receipt"
            );

        p += len;
    }

    // ---- Fifth chunk: logs_bloom ----------------------------------
    uint32_t logs_bloom_len = READ_U32_BE(ctx, p, end);

    if (p + logs_bloom_len > end)
       SQLITE_RETURN_ERROR(
           ctx,
           "receipt_contains_bloom_filter: not enough data in receipt"
       );

    const unsigned char *logs_bloom = p;

    // Need at least 514 hex characters for 0x + 256 bytes
    if (logs_bloom_len < 514)
      SQLITE_RETURN_ERROR(
           ctx,
           "receipt_contains_bloom_filter: logs_bloom in receipt too small"
       );

    // Skip ASCII "0x" prefix
    logs_bloom += 2;

    // ---- Bloom inclusion check ------------------------------------

    for (int i = 0; i < 256; i++) {
        unsigned char mask = bloom[i];

        // Early AND-zero skip:
        // If mask == 0, (b1 & mask) == mask always holds.
        if (mask == 0) continue;

        int b1 = hex_byte(logs_bloom[2*i], logs_bloom[2*i + 1]);
        if (b1 < 0)
            SQLITE_RETURN_ERROR(
                ctx,
                "receipt_contains_bloom_filter: invalid hex characters in receipt logs_bloom"
            );

        if ((b1 & mask) != mask) {
            // At least one bit of bloom is not in logs_bloom.
            SQLITE_RETURN_FALSE(ctx);
        }
    }

    SQLITE_RETURN_TRUE(ctx);
}

/* ------------------------------------------------------------------ */
/* Extension initialization                                           */
/* ------------------------------------------------------------------ */

int sqlite3_receipt_bloom_init(sqlite3 *db) {

    sqlite3_create_function(
        db,
        "receipt_contains_bloom_filter",
        2,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC,
        NULL,
        receipt_contains_bloom_filter,
        NULL,
        NULL
    );

    return SQLITE_OK;
}
