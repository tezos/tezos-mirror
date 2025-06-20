CREATE TABLE sequencer_upgrades (
  injected_before INT NOT NULL,
  sequencer TEXT NOT NULL,
  pool_address TEXT NOT NULL,
  activation_timestamp INT NOT NULL,
  applied_before INT
)
