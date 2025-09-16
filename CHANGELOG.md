# Revision history for thale

## 0.1.0.0 -- 2025-09-15

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2025-09-16

* Added a fully-featured CLI:
  * Commands: `run`, `build`, `format`, `check`, `repl`, `version`, `new`.
  * Dynamic version reporting using `Paths_thale.version`.
  * Improved help and usage text.
* Implemented `thale new <project>` to scaffold a new Thale project:
  * Creates project directory and `src/` folder.
  * Generates `Main.th` with a default `main` function.
  * Generates `thale.toml` with project metadata (`name`, `version`, `description`, `authors`, `license`, `homepage`, `repository`).
* Added `Format` command to format Thale source files.
* Refactored CLI code into modules:
  * `Thale.Cli.Cli` — main dispatcher.
  * `Thale.Cli.Parser` — command parser.
  * `Thale.Cli.Options` — command data types.
* Updated `commandParser` to include `new` and `format` commands with proper `progDesc`.
* Ensured safe directory creation for `thale new`, prevents overwriting existing projects.
* Improved user experience with structured help output and readable command descriptions.