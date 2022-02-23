SELECT
    *,
    dst.address AS tx_dst_addr,
    CASE WHEN dst.address < 't'
        AND op_kind = 8 THEN
        'contract'
    WHEN op_kind = 9 THEN
        'origination'
    ELSE
        'regular'
    END AS transaction_kind
FROM ( SELECT DISTINCT ON (c.operation_alpha.autoid)
        c.operation_alpha.autoid AS op_id,
        c.operation_alpha.operation_kind AS op_kind,
        c.origination.source_id AS orig_src_id,
        c.origination.status AS orig_status,
        c.tx.destination_id AS tx_dst_id,
        c.tx.source_id AS tx_src_id,
        c.tx.status AS tx_status
    FROM
        c.block
        INNER JOIN c.operation_alpha ON c.block.hash_id = c.operation_alpha.block_hash_id
        LEFT JOIN c.tx ON c.tx.operation_id = c.operation_alpha.autoid
        LEFT JOIN c.origination ON c.origination.operation_id = c.operation_alpha.autoid
    WHERE
        c.block.timestamp >= ?
        AND c.block.timestamp <= ?
        AND c.operation_alpha.operation_kind IN (8, 9)
        AND (c.tx.status = 0
            OR c.origination.status = 0)) AS _init
    LEFT JOIN c.addresses AS dst ON dst.address_id = _init.tx_dst_id
