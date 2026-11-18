# mFVS: a modern Fortran Forest Vegetation Simulator

mFVS is a modernization of the [Forest Vegetation Simulator](https://www.fs.usda.gov/fvs/index.shtml) (FVS), the USDA Forest Service's individual-tree forest growth model. Its long-term goal is a version of FVS written in modern Fortran that is easier to understand, fix, update and build on, while producing the same results as the official model.

> **mFVS is an independent project** built from the public-domain FVS source code. It is not affiliated with, maintained by, or endorsed by the USDA Forest Service. For the official FVS software, releases, documentation, training and support, see the [USDA Forest Service FVS repository](https://github.com/USDAForestService/ForestVegetationSimulator) and the [FVS website](https://www.fs.usda.gov/fvs/index.shtml).

## About FVS

The Forest Vegetation Simulator is a family of individual-tree, distance-independent forest growth simulation models. Since the first model was developed for northern Idaho in 1973, FVS has grown into a collection of geographic *variants* covering forests across the United States and parts of Canada. It can simulate a wide range of silvicultural treatments for most major tree species, forest types and stand conditions. FVS is maintained by the FVS Staff of the USDA Forest Service [Forest Management Service Center](https://www.fs.usda.gov/forestmanagement/aboutus/fmsc.shtml) in Fort Collins, Colorado, and reflects five decades of work by many scientists and programmers.

## Why modernize FVS

The FVS source code carries its long history with it. It was written in fixed-form FORTRAN 77, and many routines exist as separate per-variant copies that differ only in a few coefficients or lines. Shared data passes through common blocks in include files, and procedures are called without interfaces the compiler can check. Code like this is hard to read and risky to change. A fix has to be repeated in every copy, and mismatched arguments between a call and its routine go undetected; [FOUND_BUGS.md](FOUND_BUGS.md) records several examples that have survived for years.

mFVS aims to make FVS:

- **easier to understand:** one definition of each routine, with variant differences stated where they occur, in readable modern Fortran;
- **easier to fix and update:** a change made once applies to every variant that uses it, and the compiler checks procedure calls;
- **easier to access and modify:** a standard open-source toolchain (gfortran and GNU make), clear build and test instructions, and documented known defects.

## Same answers, cleaner code

The guiding rule is that modernization must not change model results. Every step is checked against a fixed reference: upstream FVS commit [`c2105365`](https://github.com/USDAForestService/ForestVegetationSimulator/commit/c2105365) (the 2027.1.0 release plus one README edit, 28 October 2026), the last commit before modernization began. The regression tests are run with both versions, and their outputs must match apart from run identifiers, dates and times. R04's explicitly authorized rounding-level power rewrite is applied to both versions; the [parity guidance](tests/parity/README.md) describes this arithmetic exception.

Defects found along the way are deliberately *not* fixed during modernization, because a fix changes results and would mix model changes with code restructuring. Instead they are recorded in [FOUND_BUGS.md](FOUND_BUGS.md), to be fixed later as separate, documented changes.

## Status

*As of November 2026.*

- **Free-form conversion: done.** All Fortran source compiled by the build has been converted from fixed-form `.f` to free-form `.f90`, including the National Volume Estimator Library (NVEL) submodule.
- **Consolidation: in progress.** So far, 122 duplicate source files have been merged into single shared routines, removing about 21,000 lines of code.
- **Verification.** All 24 variants build. For 22 variants, the regression tests produce output identical to the reference. BC and ON have separately verified keyword, volume, expression and summary repairs, so their results deliberately differ from the failing original. BC StandStructure now completes without its expression-related STOP 20. Neither Canadian variant has pristine-original simulation parity (see [FOUND_BUGS.md](FOUND_BUGS.md)).

Planned next steps:

1. Finish consolidating duplicated routines, including the remaining metric copies used by BC and ON.
2. Move shared data and routines from common blocks and include files into modules, so the compiler checks every call.
3. Restore BC and ON regression testing, then fix the recorded bugs one at a time, each as a documented model change.

## Variants

| Code | Area | Code | Area |
|---|---|---|---|
| AK | Alaska | NE | Northeast |
| BC | British Columbia (metric) | OC | ORGANON Southwest Oregon |
| BM | Blue Mountains | ON | Ontario (metric) |
| CA | Inland California and Southern Cascades | OP | ORGANON Northwest Oregon |
| CI | Central Idaho | PN | Pacific Northwest Coast |
| CR | Central Rockies | SN | Southern |
| CS | Central States | SO | South Central Oregon and Northeast California |
| EC | East Cascades | TT | Eastern Idaho and Western Wyoming |
| EM | Eastern Montana | UT | Utah |
| IE | Inland Empire | WC | Westside Cascades |
| KT | Kootenai, Kaniksu and Tally Lake | WS | Western Sierra Nevada |
| LS | Lake States | NC | Klamath Mountains |

## Building

You need the GNU Fortran, C and C++ compilers (`gfortran`, `gcc`, `g++`) and GNU `make` with a POSIX shell, as the original FVS build did; nothing else is required. On Windows, install a MinGW-w64 toolchain that provides them, for example [MSYS2](https://www.msys2.org/)'s MinGW-w64 packages.

```sh
git clone --recurse-submodules https://github.com/brianturnquist/mFVS.git
cd mFVS/bin

make FVSie.exe   # Windows: one variant (FVSie.exe and FVSie.dll)
make FVSie       # Linux/macOS: one variant (FVSie and FVSie.so)
make US          # all U.S. variants
make Canada      # BC and ON
make all         # all 24 variants
make clean       # remove build directories and programs
```

Programs are written to `bin/`. Each variant uses `bin/FVSxx_buildDir`, from the files listed in `bin/FVSxx_sourceList.txt`. Compilation reads the original source paths and stages regular copies of selected includes/headers, preserving the variant's lookup view. Objects, module files, dependency files and local completion receipts stay in the build directory. The version string is taken from git, and a build made from a working tree with uncommitted changes is marked `-dirty`.

GNU Make uses Fortran `-O2 -ffp-contract=off`, keeping debug information and
the existing floating-point traps. For unoptimized debugging, run
`make FVSie FVS_FOPT=-O0` (substitute your variant). Compiler settings are
tracked, so switching back to the default flags rebuilds the affected objects.

Dependency preparation and compilation reuse are done by `bin/fvsbuild.c`.
The build first compiles it into `bin/.build_tool/` with the same C compiler,
and it runs no program except the compilers.

Audited SQLite C, FOFEM C and NVEL Fortran compilations are reused through
`bin/.shared_build/`. Each binary still links its own runtime storage, in the
original source-list order. The cache binds the compiler, target, flags,
preprocessed text, headers and module interfaces; it uses atomic publication
and locks across parallel builds. Unsupported extra-output/profile/plugin or
response-file configurations compile normally. Make uses immutable hard links
where supported, with a copy fallback; CMake uses private object copies.

Use `make clean` before measuring a fully uncached build, then build with
`FVS_SHARED_GROUPS=`. `FVS_SHARED_CACHE=/path/to/cache` selects another cache.
`make clean` removes only the project's default cache, the compiled helper and
the variant build files.
`FVS_NATIVE_SOURCES=0` restores full source staging when diagnosing a build.
For CMake, the corresponding cache settings are `FVS_SHARED_GROUPS` and
`FVS_SHARED_CACHE`; an explicitly supplied compiler launcher is retained.
CMake compiles the helper at configure time with the project's GNU C compiler.
Cross-compiling projects, and projects whose C compiler is not GNU, build
without reuse.

CMake uses the same GNU Fortran defaults in empty/Debug configurations, while
preserving explicit optimization switches and Release/size settings. For an
unoptimized Debug build, configure from `bin/` with
`cmake -G "Unix Makefiles" -DFVS_VARIANTS=FVSie -DCMAKE_BUILD_TYPE=Debug -DFVS_OPTIMIZE_FORTRAN=OFF .`,
then build `FVSie_CmakeDir`. The option is also available when configuring a
generated variant project directly. Other Fortran compilers retain their flags.

On Windows, the makefile relies on the `OS` environment variable. If `make` reports that there is no rule for `FVSie.exe`, add `OS=Windows_NT` to the command. If `gfortran` reports that it cannot create a temporary file, make sure `TMP` points to a writable directory.

Optional release copies can be made with
`make package FVS_PACKAGE_VARIANTS=FVSie`. The linker writes stripped copies of
the program and shared library to `bin/release/`, from the variant's objects in
link order; developer binaries and objects are left intact. `FVS_PACKAGE_DIR`
selects another directory (not `bin/` itself). `FVS_RELEASE_LDFLAGS` replaces
the stripping options, `-Wl,-S -Wl,-x` on macOS and `-Wl,--strip-debug`
elsewhere. Keep the unstripped developer binaries for debugging: separate
symbol files would need tools the original build never used.

## Running the tests

The regression tests need the `sqlite3` command-line program on the `PATH`.

```sh
cd tests
make US              # all U.S. variants; also: make Canada, make all
cd FVSie && make     # or a single variant
```

Each test runs a variant on the keyword files in its `tests/FVSxx` folder. It then uses `diff` to compare the summary output and database dumps with the `.save` files from upstream FVS. Several tests end with a non-zero stop code (such as `STOP 10` or `STOP 20`) in the reference version too; the makefiles ignore these codes. To check a modernization change, compare against output from a reference build as described in [Contributing.md](Contributing.md).

## Repository layout

| Folder | Contents |
|---|---|
| `bin/` | Makefile and the per-variant source lists |
| `base/`, `common/`, `vbase/` | Core model shared by all variants, and the include files (common blocks) |
| `ak/` … `ws/`, `canada/` | Variant-specific code; `canada/` holds BC and ON |
| `metric/` | Metric-unit copies of routines for BC and ON that have not yet been merged |
| `v*/` (e.g. `vestb/`, `vdbsqlite/`) | Versions of shared routines used by a subset of variants |
| `fire/` | Fire and Fuels Extension |
| `estb/`, `strp/` | Regeneration establishment |
| `mistoe/`, `rd/` | Dwarf mistletoe and root disease |
| `covr/`, `clim/`, `econ/` | Cover, Climate-FVS and economic analysis |
| `dbsqlite/` | SQLite database input and output |
| `organon/`, `vorganon/` | ORGANON growth model (OC and OP) |
| `volume/`, `fiavbc/` | Volume, biomass and carbon equations; `volume/NVEL` is a git submodule holding the mFVS fork of the National Volume Estimator Library (see its README) |
| `pg/` | Stop and restart support |
| `tests/` | Regression tests |
| `changeNotes/` | Historical upstream change notes |

The unsupported Acadian sources were retired on 16 November 2026; they
remain available in Git history. Separate Acadian development is available
in [fvs-modern](https://github.com/advanced-forestry-systems/fvs-modern).
See F25 in [CLEANUP_FINDINGS.md](CLEANUP_FINDINGS.md) for the retirement decision.

## Known issues

Pre-existing defects found during modernization are listed in [FOUND_BUGS.md](FOUND_BUGS.md), with their locations, affected variants and suggested fixes.

## Contributing

Contributions are welcome. Please read [Contributing.md](Contributing.md), especially the rule that modernization changes must not change model results. To report a security problem, see [Security.md](Security.md).

## License

FVS is in the public domain in the United States, and mFVS's own changes are dedicated to the public domain worldwide under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). Some bundled third-party components have their own terms. See [license.txt](license.txt).

## Acknowledgements

mFVS exists because of the FVS Staff at the Forest Management Service Center and the many scientists, biometricians and programmers who have built and maintained FVS since 1973. The current FVS team is listed in the [upstream repository](https://github.com/USDAForestService/ForestVegetationSimulator).
