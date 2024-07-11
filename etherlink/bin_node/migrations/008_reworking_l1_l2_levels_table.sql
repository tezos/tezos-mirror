ALTER TABLE l1_latest_level_with_l2_level
RENAME TO l1_l2_levels_relationships;

ALTER TABLE l1_l2_levels_relationships
RENAME COLUMN l2_level TO latest_l2_level;

ALTER TABLE l1_l2_levels_relationships
ADD finalized_l2_level INT DEFAULT 0;
