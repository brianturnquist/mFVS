# Comparisons with fixed references

These tools check that a change leaves model results identical to the original
fixed-form code: main revision `c210536536d81fd9b46dccb9a571f0de1e55e944` with
NVEL revision `38548071d5aa652bb90c7f111f86b427f798a1c9`. Both must already be
available locally. Use Python 3.12+, GNU Fortran, a GNU C compiler, GNU make,
the `sqlite3` command and Git.

Builds and results are temporary. Keep them outside the repository and do not
commit generated reports, archives, logs or run metadata.

On Windows, use Rtools 4.5's GNU toolchain: put
`C:\rtools45\x86_64-w64-mingw32.static.posix\bin` (gfortran, gcc, sqlite3)
and `C:\rtools45\usr\bin` (make) on `PATH`. MSYS make does not hand `TMP` and
`TEMP` to the native compilers and FVS programs it starts, which then cannot
create temporary files, and the makefile chooses its Windows settings from
`OS`. `check_suite.py` passes `OS`, `TMP`, `TEMP` and `GFORTRAN_TMPDIR` to make
as command-line variables, which reach every recipe; do the same for a manual
build, for example `make OS=Windows_NT TMP=%TMP% TEMP=%TMP% FVSie.setup -j8`.
Use a cache directory under your temporary folder rather than `/private/tmp`.

## Check a change: the whole regression suite

Run this from the repository root before committing a change:

```sh
python3 tests/parity/check_suite.py --cache-dir /private/tmp/mfvs-suite
```

It does four things:

1. **Checks the source lists** without compiling: every entry exists, no list
   names two files with the same name, every `bin/fortran_includes.txt` entry
   exists, and every new source file is in at least one source list.
2. **Builds two versions** of each variant: original F77 with the R04 square
   rewrites described below, and your working tree (including uncommitted and
   untracked files).
3. **Runs each variant's `tests/FVSxx` makefile** in both versions. That is
   the full existing suite, including the stop/restart, database, climate,
   economics and fire tests.
4. **Compares every generated file.** Everything must match original F77,
   including the binary restart (`.stop`) files.

It prints one line per variant and ends with `PASS` or `FAIL` and the commits
compared. Put that last line in the commit message. The exit status is 0 only
on `PASS`.

Options:

| Option | Use |
|---|---|
| `--baseline REV` | The commit before the change, used only to find new source files for the source-list check. The default `HEAD` suits uncommitted work; use `HEAD~1` to check the last commit. |
| `--variants ie sn` | Limit the run while editing. Run all variants before committing a change to shared code. |
| `--static-only` | Check only the source lists; takes about a second. |
| `--workers N`, `--jobs N` | Variants built at once, and compiler processes per build (default 4 and 4). |
| `--with-new-decks` | Run the working tree's `tests/FVSxx` folders on the references too. Use it when a change adds or edits decks the reference commits do not have; otherwise their outputs appear only in the working tree. |
| `--program TEMPLATE` | Run each variant's decks against another program configuration of the working tree, `{variant}` substituted (default `FVS{variant}`). The first word is built with `make WORD.setup` and the test makefile is pointed at the command. Until SP15/SP16 add a combined target, it must be one of the `FVSxx` programs. |
| `--poison-storage` | Raise the working tree's storage sizes and poison inactive COMMON slots (see below). Real variants need SP05/SP06 first. |

Results are cached in `--cache-dir` by the content of each variant's source
list, selected files, test folder and compiler version. The original is built
once. A variant whose inputs your change does not touch is not rebuilt, so
after the first run a check costs only the variants you changed. Delete the
cache directory at any time to start again; it holds nothing that must be kept.

When a variant fails, the output names the files that differ and the cached
folders that hold each version's outputs, so you can `diff` them directly.

## Corrected reference and immediate pre-change version

Use this for module/procedure/state restructuring after the selected bug fixes:

```sh
python3 -B tests/parity/check_suite.py --reference corrected --cache-dir /private/tmp/mfvs-corrected
```

The fixed corrected source is main
`d31a9ff0d0a51e1f0622d0a9b8d431dd310b54c4`, with NVEL
`2503c0de71cb07b7d04c4d2edba38a3ff27237fc`. Both identities are pinned in
`prepare_builds.py`. The suite also checks HEAD as the immediate pre-change
commit; `--immediate REV` selects another committed immediate version without
changing that fixed reference. It derives each commit's own NVEL gitlink.
No F77 source accommodations are applied to corrected/immediate snapshots.
`--baseline` continues to control only the new-source membership check.

Corrected/immediate text and database comparisons keep the existing exact
rules. Restart bytes are exact after run-identifier normalization: neither
the original TT location allowance nor the original decay-code allowance
applies. Identical selected inputs share the existing content cache. Use a
new cache directory when changing ambient compiler/Make switches; those
settings are not all represented in the suite's run key.

Phase 1 passed 24/24 against both corrected and immediate `d31a9ff0`, and
reproduced the separate original-F77 22/24 result. As a negative control,
`--immediate 3f456123 --variants bc on` kept the corrected comparison at 2/2
but rejected both older immediate runs because they precede the selected
expression/summary repairs. A changed year in actual IE output was also
rejected. These controls used disposable copies of sources and outputs.

The small comparator controls include text, missing files, SQLite one-ULP
changes, restart changes in both formerly allowed fields, and unchanged
run-identifier normalization:

```sh
python3 -B tests/parity/test_corrected_reference.py
```

The [module inventory](../../MODULE_INVENTORY.md) records the supported scope,
known defects, restart/Canadian coverage limits and the first pilot. Run the
original command separately to keep intentional corrections visible.

Original BC/ON stop during keyword input (bug 5). Both variants now have
separately verified keyword, volume, expression and summary repairs, so their
pristine-original comparisons differ. BC StandStructure now completes without
its expression-related STOP 20. Compare restructuring against a reproducible
corrected reference and its immediate baseline; keep those controlled
comparisons separate from original F77 parity. The focused checks below cover
the repaired paths that the original decks cannot reach.

R04 authorizes integer `**2` in place of real-literal square exponents. The
reference preparation applies that same mechanical rewrite to its 174 selected
sites, in addition to the BC/ON link inputs. Their bases are all real and their
exponents are default real, so each result keeps its kind, including double
precision in SF_TAPER. No other reference arithmetic is changed. The comparison
remains exact, with the existing masks and no numerical tolerance; it now tests
against this explicitly adjusted reference rather than pristine arithmetic.
See [R04](../../OPTIMIZATION_TASKS.md) for the separately verified stages and
the output changes caused by the authorized rounding change.

## What is compared

Build-support changes also have compiled behavioral checks:

```sh
python3 -B tests/parity/check_build_support.py
```

These compile the build's launcher, `bin/fvsbuild.c`, and call it as Make
does. They cover cache identity, concurrent publication, immutable object
reuse, separate library state and Fortran interface restoration/invalidation.
Use temporary native builds for dependency/touch, source-list, clean/parallel
and packaging checks. The helper's source participates in the full-suite
cache identity, and the source-list check treats `bin/` as build tooling. These
test tools use Python; the build they check does not.

The comparator checks which files were produced, program stop codes, all text,
and every SQLite schema and typed row. It masks only the version, date and
time fields of recognized header lines and the CaseID identifiers; REAL values
are compared exactly.

Restart files are compared byte for byte after masking run identifiers. Two
exceptions cover values that original F77 reads from uninitialized memory, so
that no two runs need agree. The first is one integer in TT restart files
(bug 11 in [FOUND_BUGS.md](../../FOUND_BUGS.md)). Bug 11 is repaired, so
only the original side may differ there; the working tree must store 1. The
second is a tree-file decay code (bug 12). The original takes it from the
previous record's history code, undefined for the first record, so a live tree
can store 3; the repaired working tree must store 0. A working-tree 3 where the
original stores 0 is not accepted. Both exceptions stay as long as comparisons
are made with the unrepaired original.

`check_comparator.py` checks the comparator itself against deliberately
changed IE outputs.

## Module pilot (MOD07)

```sh
python3 -B tests/parity/check_istfnb_pilot.py
```

ISTFNB moved into ISTFNB_MOD, with `base/istfnb.f90` kept as the external
wrapper, and SUMHED, its only internal caller, imports the module function.
The suite does not compare screen output, so this check builds one driver per
variant twice, with that variant's includes: with the frozen pre-pilot ISTFNB
and SUMHED (`--frozen`, default `0cf347e4`, the commit before MOD07) and with
the working tree. Both must
print the same bytes for SUMHED's header under seven titles (blank, leading
blanks, a full 72 characters, a final character only, tab and NUL) and with
screen output off, and for ISTFNB on all 87,381 strings of length 0–8 over
blank, `a`, tab and NUL, which are also checked against `VERIFY`. The working
tree's module function and wrapper must agree on every string. A call passing
an INTEGER to the module function must be rejected by the compiler, while the
same call to the legacy external still compiles. REVISE is the only stub.

## Poisoned storage (SP03)

```sh
python3 -B tests/parity/check_poison_storage.py
```

`poison_storage.py` prepares a disposable tree for
`check_suite.py --poison-storage`: it raises each variant's `PRGPRM` storage
sizes to the largest in the union (MAXSP 108, MAXTRE 6000, MAXPLT 500),
generates a per-variant POISON_INACTIVE routine from the inventory's COMMON
layouts, and calls it after GRINIT in INITRE. The routine fills every array's
tail beyond the variant's own size with signalling NaNs (reals), `-HUGE`
(integers) or `ACHAR(127)` (characters); logical arrays and derived extents
are listed as not poisoned. It works at run time, because `-finit-*` flags
skip locals of procedures with ENTRY (bug 63) and crash on `vbase/morts.f90`
(bug 58). Module variables, SAVE locals and procedure-local COMMON are not
covered, and restarted runs are not re-poisoned.

`check_poison_storage.py` proves the mechanism on a planted fixture compiled
with the makefile's trap flags: code within the active extent gives the
unpoisoned output, a real read of an inactive species slot stops with a
floating-point exception and an integer read changes the output. Real
variants will not build in this mode until SP05 removes the MAXSP-shaped
DATA statements and SP05/SP06 move loops to active extents.

## Canadian decks and repairs (SP19)

BC and ON gained focused decks in their test makefiles: `BCRestart` and
`ONRestart` (a straight run, then a stop and restart; the summaries are
diffed, and bug 70 records that they currently differ), `ONFocused` (two
stands; fire, fuel, snag, down-wood, carbon, burn and mortality reports;
MCDEFECT; VOLEQNUM), `ONLimit` (the Hardwood stand repeated to 5,828 tree
records, generated by sqlite3, against ON's 6,000-record storage),
`BCVolume` (VOLEQNUM, MCDEFECT, merchantability standards) and one deck per
repair: `BCVoleqnumInvalid` (bug 47), `BCBiocontrol` (bug 28) and
`ONCrnmult` (bug 44). The suite compares all their outputs.

```sh
python3 -B tests/parity/check_canadian_repairs.py
python3 -B tests/parity/check_canadian_repairs.py --component decks --build-tree /path/to/built/tree
```

The default `formats` component needs only Git: each FORMAT repaired for bug
22 must print what the original F77 FORMAT at `c2105365` printed, and no
quote-joined split may remain. `decks` runs the three repair decks with the
FVSbc and FVSon programs of an already built tree and checks the repaired
behavior directly: a printable equation table with BC's `---` defaults
(bug 47), the DMBCI species-code echo and DMBCA's application (bug 28), and
a first-cycle CRNMULT for MH that changes MH crowns only (bug 44). On a
pre-repair build all four deck checks fail.

## Focused pre-module bug repairs

```sh
python3 -B tests/parity/check_focused_bugs.py
```

This builds BC, ON, EM and IE in a temporary snapshot. The direct driver links
the selected real model objects and rebuilds affected routines with bounds
checks and signaling-NaN locals (subject to bug 63's ENTRY limit). It covers
all expression constants, fresh and legacy metric summary schemas, ON live
top-kill and cruise snag volumes, EM's missing-crown transition and consecutive
FIA writers. Native BC/ON decks check summary indexing across cycles. A SQLite
check also verifies that the memory-table helper detects changed row values.
`--build-tree /path/to/built/snapshot` explicitly reuses prepared objects.

These are correctness checks against defined inputs and product contracts,
not claims of pristine-original Canadian parity. Sections 9, 20, 30, 31, 38,
54, 55, 57 and 68 of [FOUND_BUGS.md](../../FOUND_BUGS.md) record the repairs.

## Focused NVEL correctness repairs

```sh
python3 -B tests/parity/check_nvel_repairs.py
```

This uses temporary builds to check the R9HT underflow repair and independent
R8PREPCOEF records. R9HT heights must match pinned original F77 exactly, with
underflow allowed only in the fixture's original oracle and enabled for the
repaired calls. R8 checks cover repeated and alternating records, preserved
geometry, aliases and errors, under default settings and static-local storage.
The VOLINITNVB check (bug 51) fills the stack with blank, `P`, digit, NUL and
255 bytes before each of 120 default-equation calls. The repaired routine
must return identical bits for every fill, and they must equal the frozen
pre-repair routine's results with a blank. A `P` fill must change the
pre-repair results, which shows that the fill reaches the undefined byte.
These checks do not relax the full-suite comparator or establish Canadian
simulation parity. `--component r9ht`, `--component r8prep` or
`--component nvbeqn` selects one repair.

## Focused NVEL parsing checks

```sh
python3 -B tests/parity/check_nvel_parsing.py
```

`digits` compares NVEL's `DIGITREAD` and `DIGITWRITE` helpers, through the
call-site pattern that falls back to the original formatted READ or WRITE,
with that statement alone. DIGITREAD must take exactly the fields of digits
and blanks, following the READ's `BLANK='NULL'` rule. The cases cover every
all-digit field up to five characters, sampled wider fields, every one- and
two-character field, three- and four-character fields mixing digits with
blanks, signs and other characters, and narrower reads of 4-character
records. DIGITWRITE is compared over widths 1–9, record lengths, signs,
powers of ten and overflow. Values, IOSTAT codes, records and declined
calls must match exactly, under default and `-fcheck=all` builds.

`nsvb` and `volinit` build NVEL twice, at a frozen revision and from the
working tree, and link one compiled driver with each. All case outputs,
including mutated arguments, must match byte for byte, and invalid fields
must stop both builds with the same runtime error.
- `nsvb` (`nsvb_cases.f90`, NVEL `d0e634fa`, before O06) covers SPEQCOEF,
  CALC_EQVW and NVBC.
- `volinit` (`volinit_cases.f90`, NVEL `9655442`, before O09) covers
  NVB_EcoProv, GetRegnWF, NVB_DefaultEq, VOLINIT and VOLINITNVB. It uses every
  district, forest, region and regional weight-factor row, the default
  equations of 23 variant/region pairs, FIA, Hann, blank and NSVB equations,
  and cruise types F, I, C and B.

Both builds receive one test-only control: R1KEMP's unassigned NONSAW is set
to 0 (bug 53), so that `VOL(5)` can be compared. Inputs that reach bug 52's
undefined species data or bug 6's unrepaired R9DIB underflow are avoided.

`natcrs` compares O12's NATCRS statements with the originals. It uses every
FIA and alpha species code in the variants' block data and forest numbers
from -150 to 1200. `--component` selects one of `digits`, `nsvb`, `volinit`
or `natcrs`.

## Focused stocking checks

```sh
python3 -B tests/parity/check_stkval.py
```

This runs 60 direct cases with each variant's selected includes and COMMON
dimensions, with bounds checking. Result bits, stocking/size classes and debug
output must match the frozen pre-optimization routine at `d4341283`, with only
the documented zero-coefficient-row repair applied to that reference. Separate
cases check the existing unknown-species fallback. This is a controlled
corrected-baseline comparison, not pristine-original simulation parity.
Use `--variants pn ca sn` for a focused run; the default covers all 24 variants.

## Focused Canadian snag-volume repairs

```sh
python3 -B tests/parity/check_snag_volumes.py
```

This uses native BC/ON COMMON dimensions, the actual CFTOPK/Behre routines
and small providers with the Canadian total/merchantable return contracts.
Both FMSVOL entries, truncation/negative-height argument behavior and merchant
selection are checked exactly. ON's below-minimum positive placeholder must
not enter merchantable taper integration, and neither provider supplies SCF.
Bounds checks, floating-point traps and a second signaling-NaN build catch
undefined result reads. Native snag/debug/restart decks and the full suite
provide separate integration coverage; this fixture does not establish
pristine-original Canadian simulation parity.

## Focused CWD-pair reuse checks

```sh
python3 -B tests/parity/check_cwd_reuse.py
```

This compares the current paired entries with frozen pre-reuse FMCWD in every
variant's native COMMON dimensions. Debris arrays must match bit for bit, with
the expected volume-call counts for positive/zero fall and breakage, a valid
zero volume, hard/soft mixtures, FVS/NVEL debug modes, unaudited equation
families and ON's unsupported NATCRS path. Independent CWD1/CWD2 and CWD3 calls
must always calculate their own volumes. The driver uses a counting volume
provider; the actual providers, RNG state and restart bytes are checked by
the full suite and exercising model decks separately. FVSVOL debug-only I1
remains undefined (bug 56); extended comparisons control it in both temporary
builds, while native uncontrolled FVSVOL debug text remains unverified.

## Focused database transaction checks

```sh
python3 -B tests/parity/check_db_transactions.py
```

The decks only reach DBSREFERENCE with no transaction open and every
statement succeeding. This check builds IE's selected includes with the
repository's SQLite and wrapper under the Makefile's SQLite settings. It then
runs the batched routine and the frozen pre-batching routine through five
cases:

- two stands written to one database;
- inside a transaction opened by the caller, then rolled back;
- after a failed DBSFMCANPR left its transaction open (bug 66);
- a species code whose INSERT fails to prepare part-way through the loop;
- a commit refused because another connection holds a read lock.

For each case, the autocommit state, outstanding statements (bug 65), rows
each connection sees, close codes and final typed rows must match exactly.
Expected facts are also asserted, so each case is known to reach its path:
the caller's rollback removes the stand's rows, rows written before the
failure persist, and a refused commit leaves no transaction open. DBSCASE is a
stub because the cases preset CASEID. The lock case removes the wrapper's
busy handler, so each refusal is immediate rather than after its 20-second
retry limit. The full suite and the IE file/memory API runs cover integrated
output.

## Single decks

For a focused case that the suite does not reach, the older tools run one
deck at a time in three versions:

```sh
python3 tests/parity/prepare_builds.py \
  --work-dir /private/tmp/mfvs-check-build \
  --evidence-dir /private/tmp/mfvs-check-results \
  --current-revision PRE_CHANGE_COMMIT --variants ie --jobs 4

python3 tests/parity/run_cases.py \
  --evidence-dir /private/tmp/mfvs-check-results --cases ie-iet01

python3 tests/parity/compare_outputs.py \
  --runs /private/tmp/mfvs-check-results/runs.json \
  --left original --right candidate --case ie-iet01 \
  --report /private/tmp/mfvs-check-results/comparison.json
```

Discard both temporary directories when finished. Leave model bug fixes for
separate work, and record newly found bugs in
[FOUND_BUGS.md](../../FOUND_BUGS.md).
