/*************************************************************************/
/*                                                                       */
/* SPDX-License-Identifier: MIT                                          */
/* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>          */
/*                                                                       */
/*************************************************************************/

/*
 * zstd.c
 *
 * SQLite static extension exposing deterministic scalar functions for
 * zstd compression and decompression:
 *
 *   zstd_compress(blob)                -> blob   (1-arg, level from
 *                                                  per-connection state)
 *   zstd_compress(blob, level INTEGER) -> blob   (2-arg, explicit level)
 *   zstd_decompress(blob)              -> blob
 *
 * The 1-arg [zstd_compress] reads its level from the per-connection
 * user-data slot set at registration time, so callers don't have to
 * thread the level through every SQL statement. A level of 0 is
 * interpreted as "no compression, return input unchanged" — that is
 * never the intended meaning of zstd's own level 0, so the SQL form
 * is independent of any zstd library convention.
 *
 * [zstd_decompress] is deliberately a pass-through when its input does
 * not start with the zstd magic number (0x28B52FFD, little-endian),
 * so queries compose correctly over a mix of compressed and
 * uncompressed rows. This is the mechanism that lets the EVM node's
 * SQLite database migrate incrementally from uncompressed to
 * compressed without breaking reads mid-flight.
 *
 * NULL -> NULL (SQLite convention).
 * Empty blob -> empty blob (all functions).
 *
 * [zstd_compress] also passes through inputs of [ZSTD_EXT_MIN_COMPRESS_SIZE]
 * bytes or fewer: the minimum zstd frame envelope (magic + frame header
 * + block header + optional checksum) is around 12-16 bytes, so any
 * payload at or below that threshold is mathematically guaranteed to
 * expand under [ZSTD_compress]. Passing through avoids inflating
 * storage on small values and is round-trip-safe with [zstd_decompress]
 * (which already passes any non-magic input through unchanged).
 */

#include <stdint.h>
#include <string.h>

#include <sqlite3.h>
#include <zstd.h>

/* Decompressed-size safety cap to avoid pathological inputs allocating
 * huge buffers. 512 MiB is well above any realistic EVM-node blob. */
#define ZSTD_EXT_MAX_DECOMPRESSED_SIZE ((sqlite3_uint64)(512ULL * 1024ULL * 1024ULL))

/* Inputs at or below this size are passed through unchanged by
 * [zstd_compress]: a minimum zstd frame is ~12-16 bytes (magic +
 * frame header + block header + optional checksum), so any payload
 * up to 16 bytes is guaranteed to grow when compressed. Pass-through
 * keeps storage tight and round-trips through [zstd_decompress]'s
 * existing non-magic-pass-through. */
#define ZSTD_EXT_MIN_COMPRESS_SIZE 16

static const unsigned char ZSTD_MAGIC_LE[4] = {0x28, 0xB5, 0x2F, 0xFD};

static int is_zstd_frame(const unsigned char *data, int n) {
    return n >= 4 && memcmp(data, ZSTD_MAGIC_LE, 4) == 0;
}

/* Common body of [zstd_compress(blob)] and [zstd_compress(blob, level)].
 * [level <= 0] means "pass through unchanged"; that lets the 1-arg form
 * inherit a No_compression intent from per-connection state without
 * having to register a separate function. */
static void zstd_ext_compress_with_level(
    sqlite3_context *ctx, sqlite3_value *input_value, int level
) {
    if (sqlite3_value_type(input_value) == SQLITE_NULL) {
        sqlite3_result_null(ctx);
        return;
    }

    if (level <= 0) {
        /* Preserves the original storage class (TEXT/BLOB) — same path as
         * [zstd_decompress]'s magic-mismatch passthrough. */
        sqlite3_result_value(ctx, input_value);
        return;
    }

    const void *input = sqlite3_value_blob(input_value);
    int input_size = sqlite3_value_bytes(input_value);

    /* [sqlite3_value_bytes] returns a non-negative int for blobs — guard
     * the narrowing conversion defensively anyway. */
    if (input_size < 0) {
        sqlite3_result_error(ctx, "zstd_compress: negative input size", -1);
        return;
    }

    if (input_size == 0) {
        sqlite3_result_zeroblob(ctx, 0);
        return;
    }

    /* Below the minimum-frame threshold compression always inflates,
     * so return the input unchanged. [zstd_decompress] is already a
     * pass-through for non-magic input, so subsequent reads round-trip. */
    if (input_size <= ZSTD_EXT_MIN_COMPRESS_SIZE) {
        sqlite3_result_value(ctx, input_value);
        return;
    }

    size_t bound = ZSTD_compressBound((size_t)input_size);
    if (ZSTD_isError(bound)) {
        sqlite3_result_error(ctx, ZSTD_getErrorName(bound), -1);
        return;
    }
    void *buf = sqlite3_malloc64((sqlite3_uint64)bound);
    if (buf == NULL) {
        sqlite3_result_error_nomem(ctx);
        return;
    }

    size_t written = ZSTD_compress(buf, bound, input, (size_t)input_size, level);
    if (ZSTD_isError(written)) {
        sqlite3_free(buf);
        sqlite3_result_error(ctx, ZSTD_getErrorName(written), -1);
        return;
    }

    /* SQLite takes ownership of [buf] and frees it via sqlite3_free. */
    sqlite3_result_blob64(ctx, buf, (sqlite3_uint64)written, sqlite3_free);
}

/* 1-arg [zstd_compress(blob)]: reads the level from the per-connection
 * user-data slot established at register time. */
static void zstd_ext_compress_unary(
    sqlite3_context *ctx, int argc, sqlite3_value **argv
) {
    (void)argc;
    int *level_ptr = (int *)sqlite3_user_data(ctx);
    int level = level_ptr != NULL ? *level_ptr : 0;
    zstd_ext_compress_with_level(ctx, argv[0], level);
}

/* 2-arg [zstd_compress(blob, level)]: kept for tests and any caller
 * that wants to pin a specific level inline without going through the
 * per-connection state. */
static void zstd_ext_compress_binary(
    sqlite3_context *ctx, int argc, sqlite3_value **argv
) {
    (void)argc;
    int level = sqlite3_value_int(argv[1]);
    zstd_ext_compress_with_level(ctx, argv[0], level);
}

static void zstd_ext_decompress(
    sqlite3_context *ctx, int argc, sqlite3_value **argv
) {
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL) {
        sqlite3_result_null(ctx);
        return;
    }

    const unsigned char *input = sqlite3_value_blob(argv[0]);
    int input_size = sqlite3_value_bytes(argv[0]);

    if (input_size < 0) {
        sqlite3_result_error(ctx, "zstd_decompress: negative input size", -1);
        return;
    }

    /* Not a zstd frame (empty, too short, or wrong magic): pass through.
     * This is the backward-compatibility property. */
    if (!is_zstd_frame(input, input_size)) {
        sqlite3_result_value(ctx, argv[0]);
        return;
    }

    unsigned long long decompressed_size =
        ZSTD_getFrameContentSize(input, (size_t)input_size);

    if (decompressed_size == ZSTD_CONTENTSIZE_ERROR) {
        sqlite3_result_error(
            ctx, "zstd_decompress: invalid zstd frame", -1
        );
        return;
    }
    if (decompressed_size == ZSTD_CONTENTSIZE_UNKNOWN) {
        sqlite3_result_error(
            ctx, "zstd_decompress: zstd frame with unknown content size", -1
        );
        return;
    }
    if (decompressed_size > ZSTD_EXT_MAX_DECOMPRESSED_SIZE) {
        sqlite3_result_error(
            ctx, "zstd_decompress: decompressed size exceeds safety cap", -1
        );
        return;
    }

    if (decompressed_size == 0) {
        sqlite3_result_zeroblob(ctx, 0);
        return;
    }

    void *buf = sqlite3_malloc64((sqlite3_uint64)decompressed_size);
    if (buf == NULL) {
        sqlite3_result_error_nomem(ctx);
        return;
    }

    size_t written = ZSTD_decompress(
        buf, (size_t)decompressed_size, input, (size_t)input_size
    );

    if (ZSTD_isError(written)) {
        sqlite3_free(buf);
        sqlite3_result_error(ctx, ZSTD_getErrorName(written), -1);
        return;
    }
    if (written != (size_t)decompressed_size) {
        sqlite3_free(buf);
        sqlite3_result_error(
            ctx, "zstd_decompress: decompressed size mismatch", -1
        );
        return;
    }

    sqlite3_result_blob64(ctx, buf, (sqlite3_uint64)written, sqlite3_free);
}

int sqlite3_zstd_init(sqlite3 *db, int level) {
    int rc;

    /* Per-connection storage for the level used by the 1-arg form. The
     * destructor (sqlite3_free) reclaims it when SQLite closes the
     * function (i.e. when the connection is closed or the function is
     * re-registered). */
    int *level_ptr = (int *)sqlite3_malloc(sizeof(int));
    if (level_ptr == NULL) return SQLITE_NOMEM;
    *level_ptr = level;

    rc = sqlite3_create_function_v2(
        db,
        "zstd_compress",
        1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC,
        level_ptr,
        zstd_ext_compress_unary,
        NULL,
        NULL,
        sqlite3_free
    );
    if (rc != SQLITE_OK) {
        /* On failure, [sqlite3_create_function_v2] is documented to
         * still call the destructor on [pApp]; do not free [level_ptr]
         * here. */
        return rc;
    }

    rc = sqlite3_create_function(
        db,
        "zstd_compress",
        2,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC,
        NULL,
        zstd_ext_compress_binary,
        NULL,
        NULL
    );
    if (rc != SQLITE_OK) return rc;

    rc = sqlite3_create_function(
        db,
        "zstd_decompress",
        1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC,
        NULL,
        zstd_ext_decompress,
        NULL,
        NULL
    );
    return rc;
}
