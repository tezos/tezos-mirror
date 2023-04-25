# Releasing the SDK

There may be frequent updates to the Kernel SDK. To ensure updates/fixes get to users quickly, the SDK should be re-released
when appropriate (for example once-per-week).

Rust encourages the use of [semver](https://semver.org/). While we have a bit more leeway while the SDK remains pre-1.0, we
should be mindful that the SDK is being used by external developers already.

## Checklist

- [ ] Changes are documented in [./CHANGES.rst]
- [ ] `SDK_VERSION` is updated in [./Makefile]
- [ ] once version update is merged, manually trigger the `publish_kernel_sdk` job on master.
