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
2. **Builds three versions** of each variant: original F77, the baseline
   commit, and your working tree (including uncommitted and untracked files).
3. **Runs each variant's `tests/FVSxx` makefile** in every version. That is
   the full existing suite, including the stop/restart, database, climate,
   economics and fire tests.
4. **Compares every generated file.** Everything must match original F77,
   except restart (`.stop`) files, which must match the baseline: they already
   differ from the original (bug 10 in [FOUND_BUGS.md](../../FOUND_BUGS.md)).

It prints one line per variant and ends with `PASS` or `FAIL` and the commits
compared. Put that last line in the commit message. The exit status is 0 only
on `PASS`.

Options:

| Option | Use |
|---|---|
| `--baseline REV` | The commit before the change. The default `HEAD` suits uncommitted work; use `HEAD~1` to check the last commit. |
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

BC and ON stop during keyword input in every version (bug 5). Their lines show
only that all versions fail the same way, not that their simulations agree.

## What is compared

The comparator checks which files were produced, program stop codes, all text,
and every SQLite schema and typed row. It masks only the version, date and
time fields of recognized header lines and the CaseID identifiers; REAL values
are compared exactly. In restart files only the run identifiers are masked.

`check_comparator.py` checks the comparator itself against deliberately
changed IE outputs.

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
