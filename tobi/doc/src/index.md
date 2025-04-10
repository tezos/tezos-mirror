# Tobi: documentation

Tobi is a package manager for internal components.
It allows to work on a component without having to recompile its dependencies
after each `git pull`.

## Tutorial

- [Get familiar with Tobi](get_familiar_with_tobi.md)

## How-to guides

- [How to compile and install Tobi](how_to_compile_and_install_Tobi.md)

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

- [Why and when can Tobi save time](why_and_when_can_Tobi_save_time.md)
- [How `tobi list --installed` works](how_tobi_list_installed_works.md)
- [How `tobi install` works](how_tobi_install_works.md)
- [How `tobi reset` works](how_tobi_reset_works.md)
- [How `tobi build` works](how_tobi_build_works.md)
- [Why this configuration file format](why_this_configuration_file_format.md)
- [Why only equality constraints for internal dependencies](why_only_equality_constraints_for_internal_dependencies.md)
