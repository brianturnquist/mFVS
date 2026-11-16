# Comparisons with original F77

These tools check that a change leaves model results identical to the original
fixed-form code: main revision `c210536536d81fd9b46dccb9a571f0de1e55e944` with
NVEL revision `38548071d5aa652bb90c7f111f86b427f798a1c9`. Both must already be
available locally. Use Python 3.12+, GNU Fortran, a GNU C compiler, GNU make,
the `sqlite3` command and Git.

Builds and results are temporary. Keep them outside the repository and do not
commit generated reports, archives, logs or run metadata.

## Check a change: the whole regression suite

Run this from the repository root before committing a change:

```sh
python3 tests/parity/check_suite.py --cache-dir /private/tmp/mfvs-suite
```

It does four things:

1. **Checks the source lists** without compiling: every entry exists, no list
   names two files with the same name, every `bin/fortran_includes.txt` entry
   exists, and every new source file is in at least one source list.
2. **Builds two versions** of each variant: original F77 and your working
   tree (including uncommitted and untracked files).
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

Results are cached in `--cache-dir` by the content of each variant's source
list, selected files, test folder and compiler version. The original is built
once. A variant whose inputs your change does not touch is not rebuilt, so
after the first run a check costs only the variants you changed. Delete the
cache directory at any time to start again; it holds nothing that must be kept.

When a variant fails, the output names the files that differ and the cached
folders that hold each version's outputs, so you can `diff` them directly.

Original BC/ON stop during keyword input (bug 5). Both variants now have
repaired buffers and run farther, so their pristine-original comparisons
differ. Their outputs match the immediate pre-extraction baselines, with
only the same ON buffer repair applied to its baseline; BC StandStructure
still fails later. Keep those controlled comparisons separate from
original F77 parity.

## What is compared

The comparator checks which files were produced, program stop codes, all text,
and every SQLite schema and typed row. It masks only the version, date and
time fields of recognized header lines and the CaseID identifiers; REAL values
are compared exactly.

Restart files are compared byte for byte after masking run identifiers. Two
exceptions cover values that original F77 reads from uninitialized memory, so
that no two runs need agree. The first is one integer in TT restart files
(bug 11 in [FOUND_BUGS.md](../../FOUND_BUGS.md)). Bug 11 is repaired, so
only the original side may differ there; the working tree must store 1. The
second is a tree decay code that is 0 in one file and 3 in the other (bug 12).
Remove that exception when bug 12 is fixed. The TT exception stays as long as
comparisons are made with the unrepaired original.

`check_comparator.py` checks the comparator itself against deliberately
changed IE outputs.

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
