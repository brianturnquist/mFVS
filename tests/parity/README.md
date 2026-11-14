# Original-upstream comparison tools

These standard-library Python tools support the small modernization tasks in
[CLEANUP_TRACKER.md](../../CLEANUP_TRACKER.md). They prepare isolated snapshots,
run a bounded set of decks, and compare every generated artifact. They do not
repair model bugs, edit model source, or replace the existing regression tests.

The first retained run is documented in the
[P01–P03 report](evidence/2026-11-13-p01-p03/README.md). Its results cover IE
`iet01` and the Canadian input-failure boundary, not all variants or all model
branches.

## Requirements and scope

Use Python 3.12 or newer, git, GNU Fortran, a GNU C compiler, GNU make, `patch`,
and SQLite. Both pinned main-repository and NVEL objects must already be
available locally. No network access or package installation is performed.
The retained run used macOS arm64 and GCC/GFortran 15.2.0. Other platforms have
not been validated by that receipt.

The original main revision is fixed at
`c210536536d81fd9b46dccb9a571f0de1e55e944`, with original NVEL
`38548071d5aa652bb90c7f111f86b427f798a1c9`. Supply the immediate pre-cleanup
main revision explicitly. The candidate is a separate snapshot of the current
working files, including new untracked source files. Both submodule identity
and dirty status are recorded.

The tools currently prepare IE, BC, and ON only. Extend deck coverage explicitly
when another cleanup requires it. Do not assume this first deck reaches every
changed branch.

## Fresh verification

Run from the repository root. Choose a fresh work directory and evidence
directory for each attempt. Keep receipts from earlier accepted changes.
The example below pins the pre-P01–P03 commit; choose the appropriate immediate
pre-cleanup commit for later tasks.

```sh
python3 tests/parity/prepare_builds.py \
  --work-dir /private/tmp/mfvs-parity-NEXT \
  --evidence-dir /private/tmp/mfvs-parity-NEXT-evidence \
  --current-revision 42d1d2504dbc6162adf8cf4cd8071fa3b0d332f8 \
  --jobs 4

python3 tests/parity/run_cases.py \
  --evidence-dir /private/tmp/mfvs-parity-NEXT-evidence

python3 tests/parity/compare_outputs.py \
  --runs /private/tmp/mfvs-parity-NEXT-evidence/runs.json \
  --left original --right current --case ie-iet01 \
  --report /private/tmp/mfvs-parity-NEXT-evidence/ie-original-current.json
```

Repeat the comparison for original/candidate and current/candidate. For P03,
compare original/current for `bc-StandStructure`, `bc-YSM-SkyRanch`, and
`on-Hardwood`. A comparison exit code of 0 means the recorded artifacts match
under the contract below. It does not establish that a model run succeeded;
inspect the recorded exit status, errors, and evidence of simulation progress.

```sh
python3 tests/parity/check_comparator.py \
  --runs /private/tmp/mfvs-parity-NEXT-evidence/runs.json \
  --evidence-dir /private/tmp/mfvs-parity-NEXT-evidence

python3 tests/parity/record_evidence.py \
  --evidence-dir /private/tmp/mfvs-parity-NEXT-evidence
```

The final recording step is specific to P01–P03: it asserts the known original
Canadian interfaces and expected input-failure boundary. If future bug repairs
are separately authorized, revise those assertions in their own documented
change. Never weaken a failed assertion to pass a consolidation.

Move or copy a completed evidence directory into the repository before checking
off a task. Include its human-readable scope/results report and retain all
receipts and raw artifacts. That later verification task controls which files
are stored; the tools do not commit or publish them.

## Build provenance

`prepare_builds.py` uses git archives for original/current and a working-file
copy for the candidate. Compilation occurs entirely outside the checkout.
The only original source edits are the documented missing BC/ON link inputs in
the source manifests. Their exact patch is stored as `reference-build.patch`.
No original Fortran or C source is modified.

All states use the same explicit flags, retaining the existing numerical/trap
settings. Legacy-language switches let the modern compiler accept the original
source. `-Dunix` selects the Unix C interface; `-fPIC` permits the shared build.
No optimization, precision promotion, bounds repair, or model-changing patch
is introduced. Exact commands, versions, selected-input hashes, compiler logs,
and binary hashes are in `builds.json` and compressed build logs.

Archive snapshots contain no git metadata. Version/revision macros therefore
use their source defaults (`unknown` in this run); the true identities are
retained independently in the receipts. A candidate binary is reused only when
its complete selected source/build inputs and common compiler flags match an
already built state exactly. Reuse is explicit in `builds.json`; it is not
reported as a separate compilation.

## Exact comparison contract

- Compare the complete generated-file inventory, raw capture hashes, process
  exit status, text whitespace, and all nonmetadata values.
- Normalize only the version/revision/date/time fields in recognized GROHED
  headers and the date/time/revision fields in recognized `-999` summary
  headers. Keep the rest of each header, including species/variant titles,
  stand identity, weights, and record counts. Timestamp-like model text outside
  those contexts is significant.
- Map only known CaseID UUIDs using stable FVS_Cases descriptors and repeated
  occurrence order. Keep the mapped identity in every child row, preserving
  case relationships and row multiplicity.
- Compare every SQLite schema object and table, column names, NULLs, value
  types, and complete rows. Only `FVS_Cases.Version`, `RV`, and `RunDateTime`
  values are metadata. Schema and metadata value types remain significant.
- Ignore SQLite physical row order and storage layout. Sort canonical typed
  rows while retaining duplicates; encode REAL values with exact `float.hex()`
  representations. Apply no numerical tolerance, rounding, or row filtering.
- Record normalization counts and per-table row counts/hashes in each
  comparison report. Unsupported or unresolved identities fail rather than
  being silently dropped.

The negative controls mutate real copies of the recorded IE outputs and update
their capture hashes. They require CLI exit status 1 for changed model text,
summary-header counts, whitespace, timestamp-like model text, a one-ULP REAL,
schema, CaseID relationships, row multiplicity, missing outputs, and exit status.
Positive metadata controls cover both IE and Ontario header layouts while
confirming that changed titles/variant codes are still rejected.

## Replay retained artifacts

`artifacts.tar.gz` contains raw inputs and outputs under `runs/STATE/CASE/`.
Verify its SHA256 against `artifacts.json`, then extract into a fresh directory.
The `--artifacts-root` option replaces the original temporary paths without
altering the retained run receipt:

```sh
python3 tests/parity/compare_outputs.py \
  --runs tests/parity/evidence/2026-11-13-p01-p03/runs.json \
  --left original --right current --case ie-iet01 \
  --artifacts-root /private/tmp/mfvs-parity-replay/runs \
  --report /private/tmp/mfvs-parity-replay/ie-comparison.json
```

Raw simulator binaries and build directories remain temporary. Their hashes,
exact source identities, input hashes, build patch, logs, and replayable run
artifacts are retained in the repository.
