# C13–C18 extraction evidence

Date: 14 November 2026.

**All six planned extractions are complete and checked off. All recorded
checks pass.** On 14 November 2026 the user explicitly answered “Accept this
evidence for C13/C14”, accepting exact SQLite schema/row comparisons and
executable-statement evidence for completion while leaving BC/ON integrated
simulation parity blocked. [acceptance.json](acceptance.json) retains the
response, evidence hashes and limited scope. No other task gate is waived.
No model bug was repaired.

## Source identities and scope

| State | Main repository | NVEL |
| --- | --- | --- |
| Original F77 | `c210536536d81fd9b46dccb9a571f0de1e55e944` | `38548071d5aa652bb90c7f111f86b427f798a1c9` |
| Immediate baseline | `fb27261e7dc91100d98509218b14686d64efa085` | `a73e697e220ffd0f792aa7ac10d84c911edb7ad7` |
| Candidate | Immediate baseline plus C13–C18 | Unchanged |

[plan.json](plan.json) was stored before model edits. The six `Cxx.patch` and
`Cxx-extraction.json` pairs retain each migration separately. They replay in
order against the baseline and verify source/manifest hashes at every step.
Public routine names and signatures remain unchanged. No implementation file
was retired: callers retain their distinct model/report behavior.

[extractions.json](extractions.json) verifies that expanding each helper call
reproduces the complete executable caller from both baselines. Quoted literals
remain exact. Original mortality routines require only the already completed
`DO 150` to `DO`/`END DO` structural normalization; the label and GO TO target
are retained. This is executable-statement evidence, not a claim that the new
call boundary generates identical machine code.

All 24 source lists select the FIA helper once. CA, OC, NC, TT, UT, SO and WS
also select the mortality helper once. Every selected input difference is
accounted for. All 2,085 other tracked model source/include/C files, COMMON
declarations, build recipes, include classification, repository decks and
current NVEL are unchanged. The affected sources total **2,349 lines before
and 1,830 after**, including both new helpers: **519 fewer lines**.

Recover any pre-cleanup caller independently, for example:

```sh
git show fb27261e7dc91100d98509218b14686d64efa085:ca/varmrt.f90 > /private/tmp/ca-varmrt-before.f90
```

## Separately verified tasks

| Task | Shared definition and migration | Consumers | Direct comparisons |
| --- | --- | --- | ---: |
| C13 | First FIA group from TRLS (two sites) and ATRTLS into `dbsqlite/dbs_fiavbc_bindtree.f90` | All 24 | 72 |
| C14 | CUTLST migration to the same FIA helper, after C13 passed | All 24 | 72 |
| C15 | CA VARMRT / NC NWCMRT tail into `vbase/mortalloc.f90` | CA, OC, NC | 108 |
| C16 | TTMRT / UTMRT migration after C15 passed | TT, UT | 72 |
| C17 | SCOMRT migration | SO | 36 |
| C18 | WS VARMRT migration | WS | 36 |

Each step passed its direct tests before the next migration. There are **396
successful direct executions and 396 exact pairwise comparisons** across the
six steps. Each fixture is compiled/run independently in original, immediate
baseline and candidate states. See `targeted-C13.json` through
`targeted-C18.json` for source/include hashes, commands, driver hashes,
binary hashes, output identities and comparisons. The previous batch's
candidate snapshot is used only after matching all its selected inputs to the
committed immediate baseline; 1,589 selected Git blobs are verified.

### C13/C14: common numeric bindings

The nine contiguous fields are DBH, Ht, EstHt, TruncHt, PctCr, Cull, WdldStem,
DecayCd and CarbFrac. Column increments, integer/double bind types and the
default-REAL height calculation before REAL*8 assignment remain in their
original order. The caller still computes truncated height and owns IDs,
species, row selection, probability, mortality, SQL construction and the
remaining volume/biomass/carbon bindings.

The actual three writers use real repository `fvsqlite3.c` and `sqlite3.c`.
Only deterministic CaseID/error-reporting context is supplied by the harness.
For every variant, twelve databases cover disabled reports, inactive FIAVBC,
cycle-zero inventory-dead rows, and later-cycle reporting. Populated tables
contain respectively 6/3/3 and 4/3/3 rows for TRLS/ATRTLS/CUTLST. Inputs include
zero probability, zero cut probability, ordinary/ES/CM IDs, positive/zero/
negative normal height, rounding boundaries and an integer above the default
REAL exact-integer range. ATRTLS/CUTLST run at valid positive cycle indices;
TRLS also runs at cycle zero. Fixtures do not invoke the unrelated Canadian
input-reader interfaces.

Every schema object, column, row, value type, literal CaseID, REAL hexadecimal
value, state byte, exit status and diagnostic matches. No metadata or UUID
normalization is used in the direct SQLite comparisons. Five controls reject
a one-ULP REAL change, changed integer, changed tree ID, missing row and schema
addition. Initial harness attempts were corrected for ATRTLS's cycle-index
contract and statement teardown; no model code was changed to make them pass.

### C15–C18: common allocation tail

Variant tolerance tables, efficiency equations, crown-ratio corrections,
background mortality and initialization remain in their callers. The shared
tail preserves every arithmetic expression, operation order, branch, label,
stop condition, array update and debug format. Its PASS1 adjustment is copied
verbatim; no numerical correction is part of this work.

Twelve cases per consumer cover background mortality and targets of 1%, 50%,
90%, 98% and 99.9% of initial TPA, with debug both enabled and disabled.
Actual traces demonstrate saturation and redistribution in all seven
consumers, upward/downward adjustment across the suite, and all three MINSTP
bands. [branch-controls.json](branch-controls.json) records measured coverage
per variant, including which initial pass bands each fixture actually reaches.
Complete WK2 state, TOKILL/SUMKIL, unchanged input arrays and diagnostics match
both baselines exactly, with bounds checking in addition to normal FPE flags.

## Integrated verification and replay

All **72 state/variant build identities** succeeded: **48** explicitly reused
original/baseline binaries after exact identity checks and **24 freshly built
candidates**. All **75 integrated executions and 75 pairwise comparisons**
pass: **66 US simulation comparisons** and **nine Canadian input-failure
boundary comparisons**. The standard deck ran for every US variant, with both
named BC decks and the ON deck retained as failure-boundary evidence. Canadian simulation
coverage remains blocked by existing model bugs; passing direct writer tests
does not establish Canadian integrated simulation parity.

Build preparation reuses a retained binary only after complete selected-input,
compiler/flags/platform and binary-hash checks. Every candidate is rebuilt.
Original/current/candidate executions are independent. No optimization,
precision promotion, trap suppression, numerical tolerance or model repair is
introduced. `reference-build.patch` contains only the established original
BC/ON linkage-list accommodations.

[builds.json](builds.json), [runs.json](runs.json), the `comparisons/` reports,
[source-integrity.json](source-integrity.json) and
[canadian-boundary.json](canadian-boundary.json) retain the full identities
and results. [artifacts.tar.gz](artifacts.tar.gz) retains raw drivers, inputs,
states, diagnostics and databases; its identity is in
[artifacts.json](artifacts.json). [replay.json](replay.json) confirms exact
replay of all 75 integrated comparisons, all 396 direct comparisons and the
three-state lifecycle reproduction.

[protection.json](protection.json) and
[prior-evidence-sha256.json](prior-evidence-sha256.json) verify all **933**
earlier evidence files unchanged, including the previously untracked C07/C10
acceptance. The integrated comparator is unchanged and remains bound to its
ten retained negative controls and 24 header controls. Five additional literal
SQLite mutation controls pass for the direct writer comparator. Full file
checksums are stored with the final receipt.

Replay raw captures after verifying the archive hash:

```sh
python3 tests/parity/replay_extractions.py --evidence-dir tests/parity/evidence/2026-11-14-c13-c18
python3 tests/parity/verify_extractions.py --evidence-dir tests/parity/evidence/2026-11-14-c13-c18 --baseline-evidence tests/parity/evidence/2026-11-14-c07-c12
```

For a fresh build, choose a new temporary work directory and a new evidence
directory, copy `plan.json`, and run `prepare_builds.py` with the pinned
immediate revision and all 24 variants. `--reuse-evidence` is optional; it
retains full identity checks. Run `run_cases.py` with all 25 standard/named
cases and `--include-canadian-candidate`, then compare all three state pairs
for every case. Direct stage tests can be rebuilt with `check_extractions.py`
using `--task C13` through `--task C18`, the retained previous baseline
receipt and a fresh temporary work directory. To reconstruct intermediate
model states, apply the six patches sequentially to the immediate baseline.
The direct tool intentionally selects current working source as its candidate.

## Deferred bug

The normal CUTLST → ATRTLS sequence retains two prepared statements; normal
finalization releases only the tracked one, leaving an orphan that causes
SQLite close to return SQLITE_BUSY (5). This reproduces in all three source
states with actual writers and real SQLite. No scientific-output impact is
demonstrated. It is documented in [FOUND_BUGS.md, item 9](../../../../FOUND_BUGS.md)
for separate investigation/repair. [sqlite-lifecycle.json](sqlite-lifecycle.json)
binds the source hashes and observations; `check_sqlite_lifecycle.py` and the
retained drivers reproduce it. Harness teardown releases its own test
resources after observing the defect. All model defects remain unfixed.
