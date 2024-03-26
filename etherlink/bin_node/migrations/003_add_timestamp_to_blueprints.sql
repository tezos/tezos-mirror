-- We need to drop the content of [Executable_blueprint] in order to add a
-- column that should not be left empty.
DELETE FROM executable_blueprints;

ALTER TABLE executable_blueprints
ADD COLUMN timestamp DATETIME NOT NULL;
