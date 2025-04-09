# Tobi: documentation

Tobi is a package manager for internal components.
It allows to work on a component without having to recompile its dependencies
after each `git pull`.

## Tutorial

- [Get familiar with Tobi](get_familiar_with_tobi.md)

## Reference

### Help pages

You can obtain these pages with `--help`. The main help page lists Tobi's commands:

- [`tobi --help`](tobi--help.html)

Each command has its own help page:

- [`tobi list --help`](tobi_list--help.html)
- [`tobi install --help`](tobi_install--help.html)
- [`tobi reset --help`](tobi_reset--help.html)
- [`tobi build --help`](tobi_build--help.html)

### Configuration

- [Configuration file (`tobi/config`)](configuration_file.md)
- [What Tobi needs from Opam files](what_Tobi_needs_from_Opam_files.md)

### Cache

- [The `_tobi` directory](the_tobi_directory.md)

## Explanations

- [Why this configuration file format](why_this_configuration_file_format.md)
- [Why only equality constraints for internal dependencies](why_only_equality_constraints_for_internal_dependencies.md)
