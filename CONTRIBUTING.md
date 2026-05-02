# Contributing

## Principles

**1. `tinyverse` -- no unnecessary dependencies.**
Before adding a package dependency, ask: can this be done in base R? A new `Imports` entry must be justified by substantial functionality that would be unreasonable to replicate. Avoid dependencies that themselves have long dependency chains.

**2. Works out of the box for everyone.**
The package must install cleanly on macOS, Linux, and Windows without requiring the user to install system libraries. Optional system libraries (e.g. OpenMP, OpenBLAS) may be detected and used for performance, but must degrade gracefully when absent. Never assume Homebrew, apt, or any package manager is available.

**3. `tidyverse` style.**
Follow the [tidyverse style guide](https://style.tidyverse.org). Use `air` to format R code before submitting. `snake_case` for functions and variables, using descriptive names.

**4. Commit early, commit often.**
Each commit should represent one logical change. PRs made up of a single monolithic commit will be asked to be rebased. If you fixed a bug, added a feature, and updated docs, that is three commits -- not one.
