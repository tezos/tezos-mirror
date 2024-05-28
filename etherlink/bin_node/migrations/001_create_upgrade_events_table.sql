CREATE TABLE kernel_upgrades (
  applied_before INT NOT NULL,
  root_hash TEXT NOT NULL,
  activation_timestamp INT NOT NULL,
  applied INT
)
