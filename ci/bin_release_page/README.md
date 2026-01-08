# Octez Release Page

The release assets are distributed either all at once (typically with Octez major releases) or for individual components (typically with Octez minor releases).

To make these assets easily accessible, release pages can be generated using the `ci/bin_release_page/release_page.ml` script.
This script enables the creation of release pages for any given component, specifying a set of asset types and the storage location where the assets are distributed.

## Usage

Details on the usage are available with:

```shell
release_page --help
```

## Example

```shell
release_page --component rollup-node --title "Rollup Node Releases" --bucket release-page-test.nomadic-labs.com --path releases/ changelog binaries packages
```

will create a release page titled `Rollup Node Releases` for the `rollup-node` component.
The page will contain asset sections for each version, read from `release-page-test.nomadic-labs.com/releases/rollup-node/versions.json`.
The sections will contain:
  - A link to the changelogs.
  - A list of links to download binaries stored in `release-page-test.nomadic-labs.com/releases/rollup-node/rollup-node-vx.y/binaries/`.
  - A link to the packages install instructions.

The script lists the assets by reading the contents of directories in
`s3://release-page-test.nomadic-labs.com/releases/`.

## Storage

Currently, the release page uses S3 buckets for asset storage.

As mentioned in the usage documentation , you should use the `--bucket BUCKET` and `--path PATH` arguments to specify the bucket and path where the release page assets are stored. `BUCKET` is the address of the bucket, and `PATH` is the location within the bucket where the assets are kept.
The `PATH` argument is optional; if not provided, the root of the bucket will be used.

The script makes the following assumptions:

  - Each component `COMPONENT` has a base path of `BUCKET/PATH/COMPONENT`, which we refer to as `COMPONENT_LOCATION`.
    The only exception is component `octez`, for which the base path is `BUCKET/PATH`.
  - For each component, `COMPONENT_LOCATION/versions.json` contains the list of released versions for this component.
  - For each component, assets for this component are stored in `COMPONENT_LOCATION/COMPONENT-VERSION/`.
    For instance, executables are stored in `COMPONENT_LOCATION/COMPONENT-VERSION/binaries/ARCH`.
  -  `COMPONENT_LOCATION/COMPONENT-VERSION/binaries/arch` contains a `sha256sums.txt` file listing the executables checksums.

Thus, the bucket structure should look like this:

```
s3://<BUCKET>/<PATH>/
├── versions.json
├── octez-vX.Y
│   ├── binaries/x86_64/
│   │   ├── sha256sums.txt
│   │   ├── exe1
│   │   ├── ...
│   ├── binaries/arm64/
│   │   ├── sha256sums.txt
│   │   ├── exe1
│   │   ├── ...
│   └── .../
├── ...
├── COMPONENT/
│   ├── versions.json
│   ├── COMPONENT-vX.Y/
│   │   ├── binaries/x86_64/
│   │   │   ├── sha256sums.txt
│   │   │   ├── exe1
│   │   │   ├── ...
│   │   ├── binaries/arm64/
│   │   ├── .../
│   └── .../
```

## Version

As detailed in the previous section, each component are required to have `COMPONENT_LOCATION/versions.json`.

This file is a JSON file with the following schema:

```
{
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "major": { "type": "integer" },
      "minor": { "type": "integer" },
      "rc": { "type": "integer" },
      "latest": { "type": "boolean" },
      "announcement": { "type": "string" }
    },
    "required": ["major", "minor"]
  }
}
```

Each item of the array represents a version `<major>.<minor>[~rc]`.
If `latest` is `true`, the version will be identified as "latest" in the generated page, for instance by appending "(latest)" to its section title. Only one version should be identified as latest, although this is not enforced by the script.

For instance, a valid JSON would look like:

```
[
  {
    "major": 22,
    "minor": 1,
    "announcement": "https://octez.tezos.com/docs/releases/version-22.html"
  },
  {
    "major": 23,
    "minor": 0,
    "rc": 1,
    "announcement": "https://octez.tezos.com/docs/releases/version-23.html"
  },
  {
    "major": 23,
    "minor": 0,
    "latest": true,
    "announcement": "https://octez.tezos.com/docs/releases/version-23.html"
  }
]
```

## Assets

There are 3 types of assets that can be represented in the release page:
  - `changelog`
  - `packages`
  - `binaries`
  - `dashboards`

The `changelog` will simply be represented by a section with a link to the changelog of the version.

The `packages` will be described by a section for each architecture containing a link to the instruction to install or update the packages (Debian, RPM, …).

The `binaries` will be represented as a list of links to download each binary, along with its associated checksum.
The binaries are those listed in `BUCKET/PATH/COMPONENT/COMPONENT-vX.Y/binaries/ARCH/`.

The `dashboards` will be represented as a list of links to download each dashboard JSON file, along with its associated checksum.
The JSON files are those listed in `BUCKET/PATH/COMPONENT/COMPONENT-vX.Y/dashboards/`.
