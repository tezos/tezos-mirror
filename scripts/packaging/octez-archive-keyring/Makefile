ACTIVE_KEYS = $(wildcard active-keys/*.asc)

all: octez-archive-keyring.gpg

# Build the combined keyring by dearmoring all active public keys into a
# single binary OpenPGP key stream (concatenated certificates).
#
# This is the format APT expects for a `signed-by` keyring and the only one
# that works across all supported releases: it is read by both gpgv
# (bookworm/Ubuntu) and Sequoia's sqv (trixie+, which is APT's default
# verifier there). Do NOT build this with `gpg --keyring --import`: modern
# gpg (2.4) writes a *keybox* database to that file, which gpgv tolerates but
# sqv cannot parse ("Failed to parse keyring ... EOF"), breaking apt-get
# update on trixie.
octez-archive-keyring.gpg: $(ACTIVE_KEYS)
	@test -n "$(ACTIVE_KEYS)" || { echo "No .asc files in active-keys/"; exit 1; }
	rm -f $@
	cat $(ACTIVE_KEYS) | gpg --dearmor > $@

clean:
	rm -f *.gpg

.PHONY: all clean
