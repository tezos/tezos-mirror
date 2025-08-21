# Tobi: documentation

Tobi is a package manager for internal components.
It allows to work on a component without having to recompile its dependencies
after each `git pull`.

## Tutorial

- [Get familiar with Tobi](get_familiar_with_tobi.md)

## How-to guides

- [How to compile and install Tobi](how_to_compile_and_install_Tobi.md)
- [How to declare a component](how_to_declare_a_component.md)
- [Troubleshooting component installation](troubleshooting_component_installation.md)

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

- [The vision for the monorepo](the_vision_for_the_monorepo.md)
- [Why and when can Tobi save time](why_and_when_can_Tobi_save_time.md)
- [Why not just use `opam pin`](why_not_just_use_opam_pin.md)
- [How `tobi list --installed` works](how_tobi_list_installed_works.md)
- [How `tobi install` works](how_tobi_install_works.md)
- [How `tobi reset` works](how_tobi_reset_works.md)
- [How `tobi build` works](how_tobi_build_works.md)
- [Why this configuration file format](why_this_configuration_file_format.md)
- [Why both Opam files and `tobi/config`](why_both_Opam_files_and_tobi_config.md)
- [Why only equality constraints for internal dependencies](why_only_equality_constraints_for_internal_dependencies.md)
- [Why "Tobi"](why_tobi.md)
