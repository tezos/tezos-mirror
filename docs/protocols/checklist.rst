Protocol CI Checklist
=====================

The release of a new protocol requires some manual configuration in the CI.  Namely, the new n+1 protocol must be added to those being built and tested, and the old n-1 protocol removed.  This is done by adding are removing the respective protocols to `active_protocol_versions`, and changing the `build` job in `.gitlab/ci/build.yml` to `rm` the appropriate older protocol.

Additionally, the `unit:NNN_PROTONAME` unit test jobs in `.gitlab/ci/unittest.yml` must be updated to test the new protocols and stop testing the old ones, in the same manner as above.
