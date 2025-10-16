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

## 0.3.0.0 -- 2025-09-18

* Core Language infrastructure:
  * Implemented full **Lexer**, **Parser** and **AST** for Thale.
  * Parser now supports:
    * `val` declarations with type annotations or type inference.
    * Function declarations with typed parameters and return types or type inference.
    * `match ... with` expressions with multiple branches.
    * `Tuple`, `Array`, and `List` literals.
    * Arithmetic and comparison operators (`+`, `-`, `*`, `/`, `<`, `>`, etc.).
    * Lambda expressions.
  * Fixed Issues in the **Parser** involving, blocks with match expressions.
  * AST supports nested expressions, sequences (Seq), and branch-local variable scoping.
* Functional programming features:
  * Recursive functions fully supported.
  * Multi-expression branches now handled correctly.
* Project infrastructure:
  * Updated `.cabal` file:
    * Added a shared `common warnings` stanza for strict compilation flags.
    * Simplified and centralized GHC warning/error options (`-Wall -Wcompat -Werror`).
    * Reduced duplication between library and executable definitions.
  * Improved **README** for clarity and readability.
  * Added more **examples** (`Fibonacci`, `Bubble_sort`, etc.) showcasing recursion, pattern matching, and type inference.
* CLI infrastructure:
  * Implemented `new --interactive`.
  * Initial implementation of `thale format` formatter for Thale source code.

## 0.4.0.0 -- 2025-10-03

* Core Language infrastructure:
  * Complete redesign of Thale's syntax.
  * Removed Built-in **String**, **Int**.
* Project infrastructure:
  * Updated `.cabal` file:
    * Removed shared `common warnings` stanze for compilation flags.
  * Improved **README** for clarity.
* CLI infrastructure:
  * Removed Initial implementation of `thale format` formatter for Thale source code.
  * Initial implementation of `thale verify` to verify Thale.

## 0.5.0.0 -- 2025-10-11

* Core Language infrastructure:
  * Introduced Thale's IR as the foundation for code generation.
  * Implemented an initial IR optimizer for basic performance improvements.
  * Error Handling:
    * Added `Error.hs` for unified error reporting.
    * Full support for parser and lexer errors with precise line/column reporting.
  * Parser & Lexer Enhancements
    * Improved operator and symbol handling for consistent parsing.
    * Enhanced error messages for unexpected tokens and operator misuse.
    * Laid groundwork for future features like optional type inference and advanced parsing.
* Project infrastructure:
  * Cabal & Build System:
    * Updated .cabal file to bump version and align dependencies.
  * Refactored File Structure
    * CLI commands moved into Cli/Subcommand for modularity.
    * Internal utilities centralized in Util/ and project configuration logic in Data/.
* CLI infrastructure:
  * Updated `Entry.hs`:
    * Optimized file reading for faster project loading.
    * Modular CLI command design for easier maintenance and extension.
    * Each subcommand (Build, Check, Doc, Format, New, Repl, Run, Test, Verify) now resides in its own module.