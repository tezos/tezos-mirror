-- rolling is no longer a valid value for history_mode
UPDATE metadata
SET value = 'rolling:14'
WHERE key = 'history_mode' AND value = 'rolling';
