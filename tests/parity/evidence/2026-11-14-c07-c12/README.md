# C07–C12 consolidation evidence

Date: 14 November 2026.

**C07–C12 are complete and checked off. On 14 November 2026 the user accepted
the exact routine/file-output and executable-statement evidence for C07/C10
while leaving BC/ON simulation parity blocked.**

Thirteen selected definitions now use six shared definitions, reducing model
source from 4,587 to 2,358 lines (2,229 fewer). Eleven former copy files are
removed. Distinct equations, variant defaults, tables, diagnostics and known
model bugs are preserved. No later cleanup task is implemented.

## Source identities and exact scope

| State | Main repository | NVEL |
| --- | --- | --- |
| Original F77 | `c210536536d81fd9b46dccb9a571f0de1e55e944` | `38548071d5aa652bb90c7f111f86b427f798a1c9` |
| Before cleanup | `92b1deeb7554b50af60e271dddcd6c7413cbb70e` | `a73e697e220ffd0f792aa7ac10d84c911edb7ad7` |
| Candidate | Before-cleanup revision plus the six planned consolidations | Unchanged |

[plan.json](plan.json) was stored before edits. [consolidation.json](consolidation.json)
records all consumers, exact canonical hashes, public signatures, source-list
rewrites, per-source replacement diffs and old Git blob/recovery identities.
All 24 variants select exactly the planned definitions. Every other selected
file is unchanged, as are all 2,096 other tracked main-repository model
source/include files and all current NVEL files. Every COMMON declaration,
build recipe, include classification and repository input deck is unchanged.

Recover any retired implementation independently, for example:

```sh
git show 92b1deeb7554b50af60e271dddcd6c7413cbb70e:op/orgspc.f90 > /private/tmp/op-orgspc.f90
```

This preserves the original file for inspection without introducing another
compiled copy. The shared replacements retain the required science.

## Individual task records

### C07 — FMSFALL

**Shared home:** [fire/base/fmsfall.f90](../../../../fire/base/fmsfall.f90).
**Consumers:** BC, CA, CI, CS, IE, KT, NC, OC, SN, WS.

The shared post-fire initialization remains common. CS/SN use their former capped MODRATE and 12-inch algorithm, then return before the existing CI/IE algorithm. CS keeps KSP>2; SN keeps KSP/=2. The prior eight shared consumers retain their executable body after compile-time selection.

Direct tests cover all ten consumers, species 1/2/3/8/MAXSP, both sides of 12 and 18 inches, the base floor, the rate cap, zero/positive/near-five-percent density, post-fire probability endpoints, both time windows and both callers. The CS/SN species-1 difference is observable at the five-percent density boundary.

[Focused evidence](shared-snag-statement-proof.json). Per-source diffs, hashes and recovery commands are
under this ID in [consolidation.json](consolidation.json).

### C08 — GRINIT

**Shared home:** [vbase/grinit.f90](../../../../vbase/grinit.f90).
**Consumers:** PN, WC.

GRINIT now lives in vbase/. VARIANTCD sets PN/WC identity, and narrow branches preserve ELEV 7/35, IFOR 2/6 and TLONG 123/122 at their original initialization positions. No other initialization statement or callback order changes.

Each three-state fixture calls actual GRINIT, LNKINT and DBINIT before growth, captures every assigned variable (210 complete scalars/arrays/strings), then repeats after perturbing defaults. All raw state bytes match original F77 and the immediate baseline.

[Focused evidence](targeted.json). Per-source diffs, hashes and recovery commands are
under this ID in [consolidation.json](consolidation.json).

### C09 — CRATET

**Shared home:** [vbase/cratet.f90](../../../../vbase/cratet.f90).
**Consumers:** CS, NE.

CS and NE share calibration code in vbase/. NE retains intercept-plus-slope diagnostics; CS retains slope-only output. The internal format label differs in the shared source, but the printed text still ends with the original (9005 CRATET) tag.

Full-engine cs-cratet-debug and ne-cratet-debug decks prepend only the DEBUG request and routine-name record to isolated copies of existing inputs. They exercise real calibration dependencies. Every state prints the intended layout, and all six pairwise debug-deck comparisons pass.

[Focused evidence](cratet-debug.json). Per-source diffs, hashes and recovery commands are
under this ID in [consolidation.json](consolidation.json).

### C10 — ESADDT

**Shared home:** [estb/esaddt.f90](../../../../estb/esaddt.f90).
**Consumers:** AK, BC, BM, CA, CI, CR, CS, EC, EM, IE, KT, LS, NC, NE, OC, ON, OP, PN, SN, SO, TT, UT, WC, WS.

All 24 consumers use estb/esaddt.f90. Compile-time IDF retains 5 for AK/CI/EM/IE/KT and 3 for former STRP consumers. Every other hard-coded species bin remains unchanged. Known bug 3 is intentionally preserved.

Three-state fixtures call actual ESADDT and MYOPEN, exporting every species at DBH=1 and a tree just above 1. They exercise no requests, successful export/response, a species-3 threshold change and unsupported method handling. A controlled local Python response supplies .es2; the option table/import notification context is harness code. All actual .es1/.es2 files, state and diagnostics match exactly. The complete executable bodies also match F77/baseline after resolving CASE(IDF), including unchanged error-path statements.

[Focused evidence](esaddt-statement-proof.json). Per-source diffs, hashes and recovery commands are
under this ID in [consolidation.json](consolidation.json).

### C11 — FMVINIT

**Shared home:** [fire/base/fmvinit.f90](../../../../fire/base/fmvinit.f90).
**Consumers:** PN, WC.

FMVINIT now lives in fire/base/. PN species 6 keeps initial V2T=20.6 lb/ft3; WC keeps its former Douglas-fir-group value 28.1. All other defaults match their former sources. The existing final division by 2000 remains in its original place, returning approximately 0.0103/0.01405 tons/ft3.

Fixtures capture every assigned variable (37 complete scalars/arrays), every species, and a repeated initialization after sentinel perturbations. Species-6 checks inspect final converted values, and every raw state byte matches each variant's original and baseline.

[Focused evidence](branch-controls.json). Per-source diffs, hashes and recovery commands are
under this ID in [consolidation.json](consolidation.json).

### C12 — ORGSPC

**Shared home:** [organon/orgspc.f90](../../../../organon/orgspc.f90).
**Consumers:** OC, OP.

ORGSPC now lives in organon/. Separate 50-entry OC and 39-entry OP tables retain their exact species order, values and lengths. VARIANTCD selects the table. Original species-order/mapping commentary is kept with each table; the OP heading identifies its own table.

Every valid input index is called in original/current/candidate. All returned FIA integers match. Independently, every DATA-table literal and length is checked against the immediate pre-cleanup source. No new invalid-index policy or scientific remapping is introduced.

[Focused evidence](branch-controls.json). Per-source diffs, hashes and recovery commands are
under this ID in [consolidation.json](consolidation.json).

## Builds and reuse provenance

All **72 state/variant build identities** succeeded: original, baseline and
candidate for all 24 variants. There were **26 fresh compilations**: original
and baseline AK plus all 24 candidates. The other **46 baseline/reference
binaries** were reused from retained C01–C06 evidence after exact matching of:

- the retained builds.json checksum;
- every selected source/include, manifest and make input hash;
- tool versions, numeric/C flags, platform and machine;
- the retained binary's actual SHA256.

[builds.json](builds.json) explicitly identifies each reused state, prior evidence
receipt and binary hash. Reuse is not counted as a new compilation. Every
state still runs in an independent process/directory. The prior cached
candidate becomes the new baseline only because the complete selected input
bytes match the committed pre-cleanup version exactly. Missing cache binaries
are rebuilt; changed binaries or settings are rejected.

The platform/compiler settings remain macOS arm64 and GNU Fortran/GCC 15.2.0.
The three concurrent variant builds each use two compiler processes. No
numeric flags, optimization, precision or model source accommodations changed.
The original reference receives only its documented missing Canadian link
inputs; [reference-build.patch](reference-build.patch) retains the exact change.
[source-integrity.json](source-integrity.json) checks all original/baseline
archive blobs, their NVEL snapshots, and reference patch replay.

## Output and branch comparisons

All **81 integrated pairwise comparisons** passed across 27 decks:

| Coverage | Decks | Pairwise comparisons |
| --- | --- | --- |
| Standard US simulation outputs, all 22 consumers | 22 | 66 |
| CS/NE CRATET debug decks | 2 | 6 |
| Existing BC/ON invalid-keyword failure boundary | 3 | 9 |

The US standard/developer debug runs retain their baseline exit codes (IE 20,
others 10); all Canadian runs retain 20 and stop before simulation. Every output
file, nonmetadata text byte, SQLite schema/table/typed row and return status
is compared. No error or model value is suppressed.

All **120 direct comparisons** passed across 40 routine/variant fixtures, each
compiled and run in three states. C09 instead uses the full-engine debug decks
so calibration dependencies are real. Supplemental bounds checking is applied
equally to all three direct-test states. Generated original drivers use fixed
form to include unchanged .F77 headers; original model sources are never
reformatted or patched by the fixtures.

[branch-controls.json](branch-controls.json) verifies the observed CS/SN split,
initialization defaults, final species-6 unit conversion, seedling-bin behavior
and complete ORGANON tables. [cratet-debug.json](cratet-debug.json) records
actual intercept presence/absence and sample lines in all debug runs.

[esaddt-statement-proof.json](esaddt-statement-proof.json) compares the entire
ESADDT executable body from IDANUW through END, after joining actual fixed/free
continuations and resolving CASE(IDF) to the former selected constant. Labels,
string literals and all executable statements are retained; only outside-literal
whitespace/case and continuation syntax are normalized. Original, baseline and
resolved candidate match for all 24 variants. Former declarations/DATA and
COMMON includes remain unchanged except the new constant parameter.

[shared-snag-statement-proof.json](shared-snag-statement-proof.json) establishes
that the newly added CS/SN branch is compile-time false for all eight earlier
shared consumers; removing that unreachable block leaves the whole previous
executable body identical. Direct original comparisons independently confirm
the numeric outputs, including BC. [object-inspection.json](object-inspection.json)
retains an additional BC disassembly inspection: adding the local MODRATE
changes local stack offsets, so runtime-object identity is **not** claimed or
used for acceptance. No code was altered merely to force identical objects.

The [comparison contract](../../README.md#exact-comparison-contract) and
comparator code are unchanged. [comparison-contract.json](comparison-contract.json)
binds their hashes to the retained 10 deliberate mismatch-rejection controls
and 24 header-layout controls from C01–C06. No tolerance, rounding, new field
mask or metadata-policy expansion was added in this batch.

## Canadian acceptance boundary

P03 still requires Canadian integrated simulation coverage before checking off
affected consolidation tasks. Its earlier user exception explicitly applied
to C01/C06 only. C07 affects BC through the existing shared implementation;
C10 affects BC and ON. Their input bugs are intentionally unfixed.

All relevant direct routine/file-output comparisons and executable-statement
checks passed. Candidate Canadian failure boundaries also match. Those facts
are recorded separately from integrated simulation parity, which remains
blocked. On 14 November 2026 the user explicitly answered “Accept this evidence
for C07/C10”, authorizing completion of these two tasks using the alternative
evidence. [acceptance.json](acceptance.json) retains the response and evidence
hashes. The exception applies only to C07/C10; no Canadian simulation
verification, runtime-object equality or model bug fix is claimed.

## Stale code, bugs and historical lists

The retired files were active redundant definitions, not abandoned scientific
pathways. Their numeric differences and useful comments survive in shared
code/data and exact Git recovery references. The old CS commented sapling-height
alternatives remain in the shared CRATET source; they are not activated.

Bug 3's former ESTB seedling profile is preserved, including its incorrect
indices. Former STRP consumers keep their original index-3 profile and every
other bin; their scientific correctness is not newly asserted. C10 also found
conditional IPYR/IKEEP initialization and close/error-path concerns, recorded
as deferred, reachability/impact-unconfirmed investigation in
[FOUND_BUGS.md](../../../../FOUND_BUGS.md#8-deferred-esaddt-investigation-locals-on-short-optionerror-paths).
No default, error recovery or bin correction was applied. The fixtures cover
configured successful service responses, not every external-model import or
failure path.

[legacy-lists.json](legacy-lists.json) retains the unsupported Canadian auxiliary
list review. Only historical ON gains a removed reference (STRP ESADDT) under
the root-bin interpretation, changing its already-missing count from 89 to 90
of 521. No current qualified recipe consumes it; root CMake globs use root
manifests. Historical lists are left unchanged as separate cleanup work.

All prior evidence files are unchanged. No C13+ or D14 task is implemented.

## Replay and provenance

[runs.json](runs.json) and [targeted.json](targeted.json) retain inputs, generated
drivers/context, commands, source hashes, binary hashes, output inventories and
exit codes. The raw [archive](artifacts.tar.gz) includes all integrated/direct
runs, actual .es1/.es2 files and the local regeneration response script.
[artifacts.json](artifacts.json) binds its SHA256 and size.

All **81 integrated and 120 direct comparisons** were replayed from a fresh
archive extraction. [replay.json](replay.json) records identical reports,
verified raw input/driver/context/script hashes and exact exported-file bytes.

Rebuild with prepare_builds.py against the pre-cleanup commit and all 24
variants; optionally add --reuse-evidence for the named retained evidence.
Run the 27 recorded cases with --include-canadian-candidate, including the two
CRATET debug cases. Run check_consolidation.py and verify_consolidation.py
against the new evidence/snapshots. A fresh direct-run directory is required.
The capture inventories and hashes identify the exact candidate; temporary
binaries and snapshots can be reconstructed from retained source identities.

[change-identity.json](change-identity.json) binds the model and verification
inputs. [checksums.sha256](checksums.sha256) binds this evidence set. Source
changes are uncommitted.

The tracker has 47 completed and 73 pending items following the user's C07/C10
acceptance. The gate for other tasks remains unchanged.
