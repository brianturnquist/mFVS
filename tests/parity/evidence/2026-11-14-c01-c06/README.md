# C01–C06 consolidation evidence

Date: 14 November 2026. Scope: six requested consolidation families.

**Status: C01–C06 are complete and checked off. On 14 November 2026 the user
accepted the exact routine and runtime-object evidence for C01/C06 while
leaving BC/ON simulation parity blocked.**

The families now use six definitions instead of fourteen, reducing model source
from 2,652 to 1,168 lines (1,484 lines removed net). No model bug is fixed.
Each family has one canonical shared home, with its existing variant differences
preserved through dimensions, data tables or narrow compile-time branches.

## Source identities

| State | Main repository | NVEL |
| --- | --- | --- |
| Original F77 | `c210536536d81fd9b46dccb9a571f0de1e55e944` | `38548071d5aa652bb90c7f111f86b427f798a1c9` |
| Immediately before cleanup | `1a8aef8b210ebf532d8c94291fea511f11a6a19a` | `a73e697e220ffd0f792aa7ac10d84c911edb7ad7` |
| Candidate | Pre-cleanup commit plus the six planned consolidations | Unchanged |

[plan.json](plan.json) was stored before model edits. The manifest-derived
consumers include KT for C02/C03, and BC/CA/KT/NC/OC/WS as additional consumers
of the IE FMSFALL implementation. ESTIME affects 23 variants; AK uses its own
implementation and is unchanged.

[consolidation.json](consolidation.json) records exact source hashes, public
signatures, per-variant selection, per-source diffs, Git blobs and recovery
commands. Every source list selects exactly the planned replacement; all other
selected bytes are unchanged. All 2,103 other tracked main-repository model
source/include files and all current NVEL files are unchanged. The compile-time
variant parameters and every COMMON include are untouched.

## Individual implementation records

### C01 — ESTIME

**Canonical home:** [estb/estime.f90](../../../../estb/estime.f90).
**Consumers:** BC, BM, CA, CI, CR, CS, EC, EM, IE, KT, LS, NC, NE, OC, ON, OP, PN, SN, SO, TT, UT, WC, WS.

Removed the unused ESCOM2 include from shared ESTIME and redirected every STRP consumer. ESCOM2 declares species-probability storage; ESTIME neither reads nor writes it and the include has no DATA initialization or EQUIVALENCE effects. The timing statements and arithmetic order are unchanged.

Direct fixtures exercise all 23 consumers, five history patterns (including overlapping histories and the 20-history capacity), every duration 0–19, exact timing outputs, and untouched probability-array sentinels. There are 69 three-state pairwise comparisons. BC and ON also have identical original/current/candidate runtime objects, constants, relocations and external/common symbol sizes.

Per-source before/shared diffs and recovery identities are recorded under this
task ID in [consolidation.json](consolidation.json).

### C02 — ESPSUB

**Canonical home:** [estb/espsub.f90](../../../../estb/espsub.f90).
**Consumers:** CI, IE, KT.

The first ten probability equations remain unchanged in existing estb/espsub.f90. A single PSUB(11:MAXSP)=0.0 clears exactly the old tail: KT 11, CI 11–19, IE 11–23. No new equation or probability correction is introduced.

Original/current/candidate fixtures cover all three consumers with nonzero tail sentinels, all 16 habitat indices, forest/physiographic/site-preparation branches and overstory-threshold sides. Every tail and collateral-array sentinel is checked explicitly; bounds checking is enabled in all three states.

Per-source before/shared diffs and recovery identities are recorded under this
task ID in [consolidation.json](consolidation.json).

### C03 — ESPXCS

**Canonical home:** [estb/espxcs.f90](../../../../estb/espxcs.f90).
**Consumers:** CI, IE, KT.

Applied the same dimension-preserving consolidation independently to ESPXCS. All first-ten excess-probability equations remain in their previous order. PXCS(11:MAXSP)=0.0 preserves each variant's old tail.

Separate fixtures and receipts cover the three consumers, initialized PXCS tails and untouched PSUB/PADV storage. This is independently compared with F77; C02 passing is not used as evidence for C03.

Per-source before/shared diffs and recovery identities are recorded under this
task ID in [consolidation.json](consolidation.json).

### C04 — FMCROW

**Canonical home:** [fire/base/fmcrow.f90](../../../../fire/base/fmcrow.f90).
**Consumers:** KT, NC.

Shared FMCROW retains two explicit tables of lengths 11 and 12. VARIANTCD selects NC's table, with KT using its original table. Every mapping literal is unchanged, including NC species 12 mapping to equation 19. The legacy comment said 3; the discrepancy is recorded in FOUND_BUGS.md without changing the numeric mapping.

Both consumers exercise every species with real RDPSRT, PCTILE, FMCROWW and ALGSLP helpers. Fixtures include crown-growth skip/re-entry, no trees, zero diameter/height and debug output. The fail-fast NATCRS harness symbol is never called: neither mapping includes the aspen equation that invokes it. Each original DATA table is also checked literally against its canonical counterpart.

Per-source before/shared diffs and recovery identities are recorded under this
task ID in [consolidation.json](consolidation.json).

### C05 — FMCFMD

**Canonical home:** [fire/base/fmcfmd.f90](../../../../fire/base/fmcfmd.f90).
**Consumers:** OP, PN, WC.

OP keeps species 24 in the WOCT group in the non-WC branch; PN sends it through the old DFCT default. WC's outer branch is unchanged. OP's sum remains FMTBA(28)+FMTBA(24)+FMTBA(25), and PN/WC retain their original addition order.

Fixtures cover all species in OP, PN and WC, cover types, dry/wet contexts, dynamic/static modes, activity fuels, no-tree fallback and the user-model early return. Real sorting, interpolation, FMDYN and FMCHKFWT helpers are used. Saved habitat context is supplied at PNFGS/PNWET boundaries rather than running unrelated FMCBA initialization. The species-24 fixture observes dominant group 6 for OP and group 2 for PN/WC.

Per-source before/shared diffs and recovery identities are recorded under this
task ID in [consolidation.json](consolidation.json).

### C06 — FMSFALL

**Canonical home:** [fire/base/fmsfall.f90](../../../../fire/base/fmsfall.f90).
**Consumers:** BC, CA, CI, IE, KT, NC, OC, WS.

Shared FMSFALL uses the CI source's comments and equations, with its species-2/8 adjustment guarded by compile-time VARIANTCD=CI. The adjustment stays inside D>=18. All seven consumers of the former IE definition retain the unadjusted behavior, including BC.

All eight consumers match original/current/candidate for 4,320 direct cases each, spanning species 1/2/8/MAXSP, diameter immediately below/at/above 18, the base-rate floor, zero/positive/near-five-percent density, post-fire probability endpoints, both time-window sides, ALLDWN alternatives and both callers. CI/IE fixtures demonstrably differ only where expected; BC runtime object sections, constants, relocations and symbol sizes match across all three states.

Per-source before/shared diffs and recovery identities are recorded under this
task ID in [consolidation.json](consolidation.json).

## Direct comparison contract and evidence

[targeted.json](targeted.json) records 126 executions for 42 routine/variant
combinations and all 126 pairwise comparisons. Each compares raw unformatted
state bytes, stdout, stderr and exit status without rounding or tolerance.
The fixtures compare actual selected F77 routines with immediate pre-cleanup
and candidate routines; they do not recreate equations as an expected-value
oracle. Supplemental bounds checking is applied equally to every state.

Only generated original drivers are formatted as fixed form to include the
unchanged original .F77 headers. Generated drivers and exact harness context
source are retained with raw runs. Model sources/includes are never converted
or patched for these fixtures. Debug switches, saved habitat values and a
diagnostic flag sink are harness context; all numerical helpers listed above
are selected from each corresponding model snapshot.

[branch-controls.json](branch-controls.json) binds nonzero-tail/collateral
checks, complete mapping tables, OP versus PN/WC group discrimination, and CI
versus IE adjustment boundaries. [object-equivalence.json](object-equivalence.json)
provides additional BC/ON evidence using objects compiled with the exact
integrated-build flags, without supplemental bounds checking. It compares
every runtime __TEXT/__DATA/__DATA_CONST section, symbolic runtime relocations
and external/common symbol identities and sizes, omitting DWARF debug data.
It is evidence for these routines on this compiler/platform; integrated
Canadian simulation is not claimed.

The compiler's normalized routine trees are retained in `routine-trees/`.
Trees can differ structurally after consolidation (for example an explicit
constant-false branch) while runtime code and outputs match. Tree equality is
reported separately and is not silently required or treated as output parity.

## Integrated matrix and metadata

The matrix builds original, immediate pre-cleanup and candidate versions of
all 23 consumers with the same recorded flags, GCC/GFortran 15.2.0 on macOS
arm64. There is no scientific reference patch: only the documented original
BC/ON missing-link manifest additions are applied. [reference-build.patch](reference-build.patch)
retains that exact accommodation.

Standard decks run in separate state/case directories. Every generated file,
text byte outside identified metadata, SQLite schema/table/typed row, and
baseline exit status is compared. Nonzero baseline statuses are preserved.

[header-layout-investigation.json](header-layout-investigation.json) records
initial CA/CR/TT comparator rejections. Original GROHED FORMAT statements tab back
to RV and date columns, sometimes leaving one/two title characters between
them. TT also retains a title character in the two skipped columns before the
time. The metadata recognizer accepts both layouts while retaining every such
character verbatim. It still masks only version/revision/date/time and known
CaseID aliases. No model/header formatter or scientific value was changed.
Metadata controls require changed titles and those residual characters to fail;
the existing one-ULP, schema, CaseID-relationship and other mismatch controls
are also rerun.

## Canadian acceptance boundary

The existing [P03 tracker gate](../../../../CLEANUP_TRACKER.md) says: “Do not
check off an affected consolidation as verified while its required BC/ON
simulation coverage remains blocked.” C01 affects both Canadian variants;
C06 affects BC. Their keyword-input bugs are intentionally unfixed.

Their full simulations therefore remain unavailable. Direct routine comparison
and runtime-object equivalence provide additional evidence for the changed
routines, and the candidate failure boundary is also executed. These are
reported separately from simulation parity. On 14 November 2026 the user
explicitly answered “Accept this evidence for C01/C06”. This authorizes their
completion using the direct and runtime-object evidence, as a limited exception
for these two tasks. [acceptance.json](acceptance.json) retains the response and
scope. The simulation gate for other tasks and the P03 boundary remain unchanged;
no Canadian integrated simulation result or model bug fix is claimed.

## Historical auxiliary lists and protected material

[legacy-lists.json](legacy-lists.json) records unconsumed Canadian historical
lists. Under the root-bin interpretation, ON's already-missing count increases
88→89 for the old ESTIME path, and bcc_notReady increases 115→117 for old ESTIME
and IE FMSFALL paths. No current root recipe selects those lists. They remain
unchanged; the root CMake globs use the distinct root manifests.

Every older P01–P03 and D01–D13 evidence file remains byte-identical. No D14
or later C/M/R/U/B/L task is implemented. The NC comment discrepancy is added
to [FOUND_BUGS.md](../../../../FOUND_BUGS.md#7-historical-nc-crown-mapping-comment-disagrees-with-its-table);
all five confirmed defects and the R9 guard concern remain unfixed.

## Reproduction and artifacts

Use [prepare_builds.py](../../prepare_builds.py) with the pre-cleanup commit,
the 23 variants in plan.json, `--jobs 2 --build-workers 3`, and a fresh work
directory. The build tool now accepts every root-manifest variant and writes
completed receipts from one thread. Numeric flags are unchanged.

Use [run_cases.py](../../run_cases.py) with the recorded cases and
`--include-canadian-candidate`. Its optional `--resume` validates captured
binary/output hashes before retaining a completed run.

Run [check_consolidation.py](../../check_consolidation.py) and
[verify_consolidation.py](../../verify_consolidation.py) against the new evidence
directory and snapshots. Generated direct-run data lives under
`runs/targeted/STATE/CNN-variant/`; integrated runs use `runs/STATE/case/`.

The retained raw archive, build/run receipts, exact source identities,
comparison reports, tool hashes, replay results and checksums are described
in the completion section below.

## Completion results

- All **69 fresh builds** succeeded: 23 consumers × original/current/candidate.
  No candidate binary was reused.
- All **72 integrated comparisons** passed across 24 decks: 63 comparisons of
  21 US consumers' simulation outputs, and nine comparisons of the three
  Canadian input-failure boundaries. Canadian candidate executions are explicit.
- All **126 direct comparisons** passed across 42 routine/variant fixtures,
  with three independently compiled states each.
- All **10 deliberate output-change controls** were rejected, including a
  one-ULP REAL change and a wrong CaseID relationship.
- Every recorded header-layout control passed, including rejection of changed
  title characters in both metadata gaps. The exact control count and layout
  cases are retained in metadata-controls.json.
- All 72 integrated and 126 targeted comparisons replayed from a fresh
  extraction of the retained raw archive. Input, generated driver, harness
  context and captured output hashes were verified.

[builds.json](builds.json), [runs.json](runs.json), [targeted.json](targeted.json),
[comparator-controls.json](comparator-controls.json), [metadata-controls.json](metadata-controls.json)
and [replay.json](replay.json) retain those results. [artifacts.json](artifacts.json)
binds the raw [artifact archive](artifacts.tar.gz). [change-identity.json](change-identity.json)
binds the candidate model inputs and verification tools;
[checksums.sha256](checksums.sha256) binds the retained evidence files.

The integrated decks retain their original error/STOP behavior: IE exits 20,
the other US standard decks exit 10, and BC/ON exit 20 before simulation. No
baseline error, output row or nonmetadata byte is dropped from comparison.
The Canadian runs produce neither simulation summaries nor output databases.
Their failures remain covered by FOUND_BUGS.md; matching them is not simulation
parity. The separate direct and object evidence does not change that claim.

There are 24 retained header-layout controls across eight variant layouts.
The tracker has 41 completed and 79 pending items following the user's C01/C06
acceptance. Source changes are uncommitted.
