# D01–D03 retirement receipt

Completed: 13 November 2026, America/Denver. Raw simulator timestamps use UTC
and are retained as emitted. Model bugs remain unchanged and deferred in
[FOUND_BUGS.md](../../../../FOUND_BUGS.md), whose bytes were not changed here.

Removed only these unselected Fortran sources:

| Task | Removed file | Lines | Bytes | Disposition |
|---|---|---:|---:|---|
| D01 | `sn/sitsetOld.f90` | 728 | 20,858 | Superseded production implementation, apparently forgotten after replacement |
| D02 | `rd/rdrout.f90` | 73 | 1,505 | Explicitly obsolete after General Report Writer migration |
| D03 | `vbase/natcrz.f90` | 314 | 10,101 | Retired cruise-export pathway whose downstream processor was unavailable |
| Total | Three files | 1,115 | 32,464 | No selected implementation removed |

## States and recovery

| State | Main revision | NVEL revision |
|---|---|---|
| Original fixed-form upstream | `c210536536d81fd9b46dccb9a571f0de1e55e944` | `38548071d5aa652bb90c7f111f86b427f798a1c9` |
| Immediate pre-deletion baseline | `3b26bc3621dffb53a64dd9ebd6dcca84a2fbf784` | `a73e697e220ffd0f792aa7ac10d84c911edb7ad7` |
| Candidate | Working snapshot after exactly the three model-source deletions | Same current NVEL |

Each removed file is recoverable byte for byte from the pre-deletion commit:

```sh
git show 3b26bc3621dffb53a64dd9ebd6dcca84a2fbf784:sn/sitsetOld.f90
git show 3b26bc3621dffb53a64dd9ebd6dcca84a2fbf784:rd/rdrout.f90
git show 3b26bc3621dffb53a64dd9ebd6dcca84a2fbf784:vbase/natcrz.f90
```

[retirement.json](retirement.json) records each blob ID, SHA256, byte/line count,
and recovery command. This avoids keeping another dormant source copy in a
different directory. Git history and historical change notes remain intact.

## D01 — Superseded SN SITSET

The old header explicitly says it was replaced for the North Carolina
merchantability update and should be deleted after testing. Local history
records the official NC update in commit
`a13f4a85f0204ba5b87efb99256479e2a3cae0c4` (2 February 2023), while the header
describes the change as Q2 2023. The retained implementation also has later
upstream changes. No separate historical 2023 full-test receipt was located;
that historical condition is not claimed as independently proven.

The files are not scientifically identical. The selected implementation adds
or changes NC merchantability diameters/tops, sawtimber cubic defaults,
Region-9 forest/species handling, site-index warning registration, and
FIA/NVEL volume-equation setup. The full source comparison is retained in
[D01-sitset-differences.patch](D01-sitset-differences.patch), for explanation and
recovery context only; that patch was not applied to the model.

This is superseded operational code, not evidence of an abandoned research
method. Current SN selects `sn/sitset.f90` exclusively. That selected file is
unchanged. Retirement is supported by present build exclusion, unchanged
compiled-input identity, and fresh comparisons with the original upstream SN
version, including both NC district branches. Those comparisons establish
equivalence for the tested cases, not independent scientific validation of
every merchantability rule or a reconstruction of all testing done in 2023.

## D02 — Obsolete root-disease output assembler

The [26 March 2015 change note](../../../../changeNotes/2015.03.26_ldavid.txt)
explicitly says `rdrout.f` is obsolete and its former call was removed because
GENPRT handles root-disease summary and detailed reports. The removed file's
own header repeats that retirement notice.

The selected `base/fvs.f90` calls GENPRT, implemented as an ENTRY in
`base/genrpt.f90`. Root-disease report producers use the General Report Writer's
GETID/GETLUN mechanism. The former RDROUT call is only a comment. No selected
source calls RDROUT. The separate RDROUT ENTRY in selected `base/exrd.f90`
remains unchanged; the whole stub file and its other extension contracts remain.

No root-disease algorithm or report implementation was changed. The old
assembler is retired infrastructure, not an abandoned scientific pathway.

## D03 — Retired US national cruise export

The [28 July 2021 change note](../../../../changeNotes/2021.07.28_ldavid.txt)
says the CRUZFILE postprocessor was no longer available and describes removal
of the keyword and NATCRZ calls from US model flow. The unselected
`vbase/natcrz.f90` was a leftover from that explicit retirement.

Current BC/ON builds select the separate `canada/bc/natcrz.f90` stub. Its bytes
and build membership are unchanged. No selected source calls the removed US
implementation; unselected historical ON code still contains a NATCRZ call.
This removal does not claim that the compiled Canadian stub is dead or remove
its exported entry point.

## Build selection and historical references

All 24 current root `bin/FVS*_sourceList.txt` input dictionaries are identical
before and after deletion, including selected source/include bytes, manifests,
makefile, and include classification. No selected source references any removed
filename. The three deleted files are absent from those input sets. CMake reads
the root manifests; its selection code is unchanged. No CMake rebuild was
attempted, and B01 remains open.

All other 2,145 tracked main-repository Fortran/C/include files checked are
unchanged. Original/current snapshot and NVEL git-blob integrity also pass.
The original archive's only differences remain the documented BC/ON link-input
additions, retained in [reference-build.patch](reference-build.patch). They do
not change original equations or affect the IE/SN source lists.

Two dormant Canadian source lists still refer to the removed files:

| Historical list | Reference | Missing input paths before → after |
|---|---|---:|
| `canada/bin/FVSon_sourceList.txt` | `../vbase/natcrz.f90` | 87 → 88 |
| `canada/bin/FVSbcc_notReady.txt` | `../rd/rdrout.f90` | 114 → 115 |

No supported in-repository build recipe selects either list. They were already
incomplete even when interpreted relative to the root build directory. Their
historical contents are retained unchanged, rather than mixing source-list or
ODBC retirement into D01–D03. This finding does not promise compatibility with
custom external builds that explicitly select those old lists. Their support
disposition belongs with the separate dormant-feature review.

## Fresh output verification

Original and pre-deletion IE/SN builds succeeded with the same compiler
settings used in P01–P03: GNU Fortran/GNU C 15.2.0 on macOS arm64. Exact paths,
versions, commands, flags, selected-input hashes, and binaries are recorded in
[builds.json](builds.json), with complete compressed build logs.

Candidate binaries are explicitly reused from the pre-deletion builds because
their complete selected inputs are identical. Each candidate simulation still
runs in its own new directory/process. There is no claim of a separate
candidate compilation.

| Case | Input | Exit in all states | Summary records in each | Artifacts compared |
|---|---|---:|---:|---:|
| `ie-iet01` | Unchanged existing IE deck/tree file | 20 | 50 | 7 |
| `sn-snt01` | Unchanged existing SN deck/tree file | 10 | 55 | 8 |
| `sn-nc03` | First SN stand, STDINFO location 81103, then STOP | 10 | 11 | 7 |
| `sn-nc05` | First SN stand, STDINFO location 81105, then STOP | 10 | 11 | 7 |

The derived cases exercise the two NC merchantability branches: forest index
11 with district 3, and the other-district case with district 5. Their echoed
STDINFO values and distinct simulated volume results are retained. The recipe,
original source-file hashes, and exact derived-input hashes are in
[runs.json](runs.json). Existing repository test decks were not edited.

All four cases pass original/current, original/candidate, and current/candidate
comparisons: **12 comparisons passed**, covering all generated artifacts,
complete SQLite schemas/typed rows, nonmetadata text, warnings/errors, and
actual exit codes. See [comparisons.json](comparisons.json) and its linked
`compare-*.json` receipts. The comparator and normalization contract are
unchanged from P01–P03; no tolerance or new output exclusion was added.

The existing IE intentional FVS04 error and plot-count warnings remain. SN
retains its plot-count warnings and STOP 10. Inputs were not modified during
execution. No newly discovered model defect required an entry in the bug
tracker. No model bug was repaired or suppressed.

This is bounded numeric verification plus all-variant build-input identity,
not a new full-24-variant simulation certification. BC/ON simulation parity
remains blocked as recorded in P03; those failures were not repaired or rerun
here.

## Reproduction and retained evidence

Preparation/run commands, from the repository root with a fresh work location:

```sh
python3 tests/parity/prepare_builds.py \
  --work-dir /private/tmp/mfvs-retirement-d01-d03-REPLAY \
  --evidence-dir /private/tmp/mfvs-retirement-d01-d03-REPLAY-evidence \
  --current-revision 3b26bc3621dffb53a64dd9ebd6dcca84a2fbf784 \
  --variants ie sn --jobs 4

python3 tests/parity/run_cases.py \
  --evidence-dir /private/tmp/mfvs-retirement-d01-d03-REPLAY-evidence \
  --cases ie-iet01 sn-snt01 sn-nc03 sn-nc05
```

The candidate in a fresh preparation is the working tree at execution time;
use the recorded cleanup state to reconstruct this specific change. Do not
reinterpret results from later model changes as this receipt.

- [verify_retirement.py](verify_retirement.py) recreates the source-retirement
  proof against the recorded snapshots/current working state. Run it with
  `--evidence-dir` pointing to a fresh preparation if the original temporary
  snapshots are unavailable.
- [retirement.json](retirement.json) contains all-variant before/after input
  fingerprints, other-model-file checks, preserved implementations, source
  integrity, stale-list classification, and recovery identities.
- [artifacts.json](artifacts.json) identifies the raw run archive by SHA256.
- [artifacts.tar.gz](artifacts.tar.gz) retains all twelve runs' raw inputs,
  generated outputs, stdout/stderr, and exit status.
- [replay.json](replay.json) records independent re-comparison from the stored
  archive, without using the original run directories.
- [verification.json](verification.json) identifies the Python/SQLite reader
  versions and exact verification tool hashes.
- [checksums.json](checksums.json) covers the retained evidence files.

For output replay after extracting the archive, use the existing
[artifact replay instructions](../../README.md#replay-retained-artifacts) with
this directory's `runs.json`, a case above, and each of the three state pairs.
No temporary executable is needed to replay captured output comparisons.
