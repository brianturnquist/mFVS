# Contributing to mFVS

Thank you for your interest in mFVS. Its goal is a modern Fortran version of the Forest Vegetation Simulator (FVS) that is easier to understand, fix and update, while producing the same results as the official model. Contributions of any size are welcome, from fixing a comment to merging a family of duplicated routines. If you are unsure about anything, open an issue and ask.

Please read the [README](README.md) first. It explains the project's goals, its current status and how to build and test the code.

## Ground rules

**1. Modernization must not change results.** A change that restructures or cleans up code must leave model output unchanged for every variant. Show this by running the regression tests and comparing the output with a reference build (see [Verifying a change](#verifying-a-change)). A difference in output means the change is not yet correct, even if the new code looks right.

**2. Keep model changes separate.** Bug fixes and other changes that alter results are welcome, but each should be its own commit and pull request. It should explain what changes and why, and show the before and after results for the affected tests. Never combine one with restructuring, or the regression comparison can no longer separate the two.

**3. Record bugs instead of fixing them in passing.** If you find a defect while modernizing, add it to [FOUND_BUGS.md](FOUND_BUGS.md) rather than fixing it in the same change. Give its location, the affected variants, its effect, how you confirmed it and a suggested fix. Defects are being collected so they can be fixed deliberately once modernization is complete.

**4. One definition per routine.** Do not add per-variant copies of a routine. When variants differ, express the difference inside one shared routine (see [Variant-specific code](#variant-specific-code)), or put the differing values in data.

## Verifying a change

The `.save` files in `tests/` contain results from upstream FVS releases, and some tests differ from them or stop with a non-zero code even in unmodified code. They therefore cannot show whether *your* change altered results. Instead, compare your build with a reference build of the pre-modernization code:

1. **Build the reference.** Check out upstream commit `c2105365` in a separate directory, initialize its submodule and build it:
   ```sh
   git worktree add ../mFVS-ref c2105365
   cd ../mFVS-ref && git submodule update --init
   ```
   At that commit, BC and ON fail to link because their source lists lack four database files. To build them, add these lines to `bin/FVSbc_sourceList.txt` and `bin/FVSon_sourceList.txt` in the reference tree:
   ```
   ../vdbsqlite/dbsreference.f
   ../dbsqlite/dbs_fiavbc_atrtls.f
   ../dbsqlite/dbs_fiavbc_cutlst.f
   ../dbsqlite/dbs_fiavbc_trls.f
   ```
   BC also needs `../volume/NVEL/wdbkwtdata.inc`.
2. **Run the tests in both trees.** In each tree, build the variants your change affects and run `make` in the matching `tests/FVSxx` folders. When in doubt, build and test all variants: a change to shared code can affect any of them.
3. **Compare every generated file:** listing (`.out`), summary (`.sum`), tree lists, and database dumps. Ignore only lines that differ in run identifiers (UUIDs), dates, times and version strings. Anything else must match exactly.

The BC and ON test decks currently stop while reading their keyword files, in both the reference and current code (bug 5 in [FOUND_BUGS.md](FOUND_BUGS.md)). Matching output for those two variants therefore does not show that a change is safe. Review changes that affect BC or ON with extra care.

## How the build works

Each variant is built from the files listed in `bin/FVSxx_sourceList.txt`. The makefile copies all of them into one flat directory, `bin/FVSxx_buildDir`, before compiling. This has a few consequences:

- **Unique file names.** Every source file's name must be unique across the whole repository, or one file will silently replace another in the build directory.
- **New files.** A new source file must be added to the source list of every variant that uses it. A new include file must also be listed in `bin/fortran_includes.txt`.
- **Include changes.** Every object file depends on every include file, so changing an include file recompiles the whole variant.
- **Modules.** The makefile compiles files whose names end in `_mod.f90` first, so name new module files that way.

## Coding conventions

Write new and changed code in the style of the converted source:

- free-form `.f90` source, with `implicit none` in every program unit;
- declarations using `::`, one statement per line;
- Fortran keywords in lower case; variable and routine names in upper case, kept from the legacy code for now;
- two-space indentation and `!` comments;
- a banner comment at the top of each file naming the routine and saying what it does.

Prefer modules and explicit interfaces for new code, so that the compiler can check calls. Several bugs in [FOUND_BUGS.md](FOUND_BUGS.md) exist only because it could not.

## Variant-specific code

Shared routines select variant-specific behavior with three values:

| Value | Where it is defined | When to use it |
|---|---|---|
| `VARACD` | Common block `CONTRL`, set at run time by `GRINIT` | Most variant-specific branches, e.g. `if (VARACD .eq. 'OC') ...` or `select case (VARACD)` |
| `VARIANTCD` | Parameter in each variant's `PRGPRM.f90` | Code that can run before `GRINIT`, such as the first output header or file opening |
| `LMETRICVAR` | Parameter in each variant's `PRGPRM.f90`; `.true.` only for BC and ON | Choosing between metric and imperial units, instead of keeping a separate copy in `metric/` |

## Commits and pull requests

Keep commits small and focused, with messages that say what changed and why. In a pull request, describe the change, list the variants it affects and report how you verified it. Say which variants you compared against the reference and whether any output differed. For a model-changing fix, link the [FOUND_BUGS.md](FOUND_BUGS.md) entry it resolves, summarize how results change, and update or remove that entry.

## Reporting problems

Report bugs, questions and suggestions as GitHub issues. Include the commit or version, the variant, and a keyword file and input data that reproduce the problem where possible. To report a security problem, follow [Security.md](Security.md) instead of opening a public issue. If the problem also exists in official FVS, consider reporting it to the FVS Staff as described in the [upstream repository](https://github.com/USDAForestService/ForestVegetationSimulator).

## Conduct

Be respectful, patient and constructive in issues, reviews and discussions. Assume good intent, and focus criticism on the code rather than on people.

## Public domain

mFVS is in the public domain within the United States, and copyright and related rights in the work are waived worldwide through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/). All contributions are released under the same dedication. By submitting a pull request, you agree to waive your copyright interest in your contribution on these terms. See [license.txt](license.txt).
