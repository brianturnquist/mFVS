# D04–D13 retirement evidence

Completed: 13 November 2026. Scope: 28 separately reviewed files, including all
five D12 and fifteen D13 entries. D14 remains pending.

Removed 6,145 lines / 189,493 bytes of unselected Fortran. No selected model
source, include, source list, build recipe, test deck, reference output, or NVEL
file changed. The complete build input sets for all 24 variants match the
pre-deletion commit. The other 2,117 main-repository model source/include files
and all 285 tracked current NVEL files are unchanged.

These definitions already have selected homes in shared code or NVEL; this
batch needed no new model wrappers or centralized copies. An alternative can
contain useful different science even when it is unselected. Each task below
therefore retains its own purpose, differences, coefficient literals, full
replacement diff, exact blob identity and recovery command.

## Source identities and recovery

| State | Main repository | NVEL |
| --- | --- | --- |
| Original F77 | `c210536536d81fd9b46dccb9a571f0de1e55e944` | `38548071d5aa652bb90c7f111f86b427f798a1c9` |
| Immediately before deletion | `9bc35a16d6dec5fc79f513ae524297077e5298e3` | `a73e697e220ffd0f792aa7ac10d84c911edb7ad7` |
| Candidate | Before-deletion revision plus exactly these 28 model-file removals | Unchanged |

[plan.json](plan.json) was stored before deletion. [retirement.json](retirement.json)
records the source hashes, Git blobs, full signatures, DATA statements, selected
replacement hashes/membership, per-variant build-input digests, integrity checks,
and recovery commands. [original-selection.json](original-selection.json)
independently confirms all 28 alternatives were also unselected in the original
F77 reference. The original shared ESTUMP home was `strp/estump.f`; current
variants select `estb/estump.f90`.

Restore any individual historical file from the repository root, using its
recorded path. For example:

```sh
git show 9bc35a16d6dec5fc79f513ae524297077e5298e3:volume/r9clark_fvsMod.f90 > /private/tmp/r9clark_fvsMod.f90
```

This writes an inspection copy outside the source tree. Do not add a historical
alternative to a current source list without a separate scientific comparison.
[change-identity.json](change-identity.json) binds the exact deletion patch and
verification tools; [checksums.sha256](checksums.sha256) binds this evidence set.

## Individual task decisions

Every entry below is a completed deletion review and retirement. None was
selected by a supported variant before deletion. “Superseded” describes build
selection; it does not establish identical scientific behavior or prove why an
old development direction stopped.

### D04 — `canada/on/findag_for.f90`

**Selected home:** [canada/on/findag.f90](../../../../canada/on/findag.f90)
(1 variant).
**Retired size:** 56 lines / 1,322 bytes.

Superseded ON FINDAG alternative; no explicit abandonment note found.

The retired HTCALC call omits basal area, DBH and diameter percentile supplied by the selected routine. Selected FINDAG also initializes AGET and HTMAX. Preserve as a different historical implementation, not an equivalent copy.

[Full replacement diff](differences/D04.patch). Per-file hashes, DATA
literals and recovery command: `D04` in [retirement.json](retirement.json).

### D05 — `canada/on/unblnk.f90`

**Selected home:** [base/unblnk.f90](../../../../base/unblnk.f90)
(24 variants).
**Retired size:** 26 lines / 724 bytes.

Intentional Canadian utility clone, now excluded; already excluded in the pinned original upstream build.

ADJUSTL plus LEN_TRIM preserves embedded spaces. The selected shared utility removes every space. This difference predates cleanup and is not changed here.

[Full replacement diff](differences/D05.patch). Per-file hashes, DATA
literals and recovery command: `D05` in [retirement.json](retirement.json).

### D06 — `canada/bc/unblnk.f90`

**Selected home:** [base/unblnk.f90](../../../../base/unblnk.f90)
(24 variants).
**Retired size:** 26 lines / 724 bytes.

Intentional BC utility clone, now excluded; independently checked against BC selection.

Same historical embedded-space behavior as D05. BC already selected shared base/unblnk in the pinned original upstream build.

[Full replacement diff](differences/D06.patch). Per-file hashes, DATA
literals and recovery command: `D06` in [retirement.json](retirement.json).

### D07 — `metric/dbsqlite/dbstreesin.f90`

**Selected home:** [dbsqlite/dbstreesin.f90](../../../../dbsqlite/dbstreesin.f90)
(24 variants).
**Retired size:** 282 lines / 8,406 bytes.

Older metric SQLite reader superseded in build selection by the shared reader.

Old interface has 30 arguments; selected shared interface has 33, adding RCULL, IDECAY and IWDLDSTMS. Old ABIRTH uses colreal rather than shared colint. Intel DLLIMPORT/ALIAS directives are also historical. The live 30-argument metric caller remains unchanged; bug 2 stays open.

[Full replacement diff](differences/D07.patch). Per-file hashes, DATA
literals and recovery command: `D07` in [retirement.json](retirement.json).

### D08 — `canada/fire/on/fmcrowe.f90`

**Selected home:** [fire/vbase/fmcrowe.f90](../../../../fire/vbase/fmcrowe.f90)
(22 variants).
**Retired size:** 565 lines / 17,415 bytes.

Unselected ON eastern-crown alternative; shared eastern routine is selected.

Old planted-species mapping includes indices 69-72, VARVER/VVER branches and an eight-argument FMSVL2 call. Shared code has a ten-argument call and additional initialization. Bug 1 remains unfixed.

[Full replacement diff](differences/D08.patch). Per-file hashes, DATA
literals and recovery command: `D08` in [retirement.json](retirement.json).

### D09 — `base/myopen_pc.f90`

**Selected home:** [base/myopen.f90](../../../../base/myopen.f90)
(24 variants).
**Retired size:** 194 lines / 6,027 bytes.

Legacy PC/compiler-specific OPEN implementation; current Windows and CMake recipes also select generic MYOPEN.

Old ISTAT=5 maps REPLACE to UNKNOWN and activates CARRIAGECONTROL=CCNTRL. Generic MYOPEN retains REPLACE. Retiring the unused alternative does not change current platform selection; no claim is made about unsupported historical compiler behavior.

[Full replacement diff](differences/D09.patch). Per-file hashes, DATA
literals and recovery command: `D09` in [retirement.json](retirement.json).

### D10 — `canada/on/r9init.f90`

**Selected home:** [volume/NVEL/r9init.f90](../../../../volume/NVEL/r9init.f90)
(23 variants).
**Retired size:** 704 lines / 33,497 bytes.

Older ON volume initializer superseded by selected NVEL library implementation.

Retired R9_MHTS lacks MTOPP, declares VOLEQ length 11 rather than 10, uses forest-3 softwood BFMIND=7 rather than 9 and fixed BFTOPD values 7.6/9.6 rather than MTOPP-derived defaults. Full coefficient/DATA and replacement diff are retained.

[Full replacement diff](differences/D10.patch). Per-file hashes, DATA
literals and recovery command: `D10` in [retirement.json](retirement.json).

### D11 — `volume/r9clark_fvsMod.f90`

**Selected home:** [volume/NVEL/r9clark.f90](../../../../volume/NVEL/r9clark.f90)
(23 variants).
**Retired size:** 1674 lines / 53,279 bytes.

Former operational NVEL workaround, later superseded by explicit return to canonical NVEL; not established as abandoned research.

Introduced and selected in a19c41b48da27e74585218ac3cd6e91bb86c8fc8 (2024-03-04); ON switched in d495d604f5afa481ce90c396f7834f0e8f9d8012 (2024-06-26). Source lists returned to NVEL in 08e833b2855133d9f01dff48307d7932a8a34433 (2025-08-28). Local r9dib and r9ht guard Y=0 when (1-17.3/totht)<0.005748 and p>14; selected versions evaluate the power directly there. Other NVEL routines already have related guards. VOLEQ lengths differ (11/10). Preserve exact diff; deferred numerical investigation is not a fix or a demonstrated model-impact claim.

[Full replacement diff](differences/D11.patch). Per-file hashes, DATA
literals and recovery command: `D11` in [retirement.json](retirement.json).

### D12a — `ca/estump.f90`

**Selected home:** [estb/estump.f90](../../../../estb/estump.f90)
(24 variants).
**Retired size:** 59 lines / 1,349 bytes.

Older variant-specific stump bookkeeping superseded by the selected shared ESTUMP.

Hard-coded aspen species index 44; selected ESTUMP uses ESASID(VARACD,INDXAS), records stump age, and handles a full MAXTRE list by matching/accumulating records. Retired code does not implement that complete shared capacity logic. Preserve each individual diff and DATA statement.

[Full replacement diff](differences/D12a.patch). Per-file hashes, DATA
literals and recovery command: `D12a` in [retirement.json](retirement.json).

### D12b — `cr/estump.f90`

**Selected home:** [estb/estump.f90](../../../../estb/estump.f90)
(24 variants).
**Retired size:** 59 lines / 1,349 bytes.

Older variant-specific stump bookkeeping superseded by the selected shared ESTUMP.

Hard-coded aspen species index 20; selected ESTUMP uses ESASID(VARACD,INDXAS), records stump age, and handles a full MAXTRE list by matching/accumulating records. Retired code does not implement that complete shared capacity logic. Preserve each individual diff and DATA statement.

[Full replacement diff](differences/D12b.patch). Per-file hashes, DATA
literals and recovery command: `D12b` in [retirement.json](retirement.json).

### D12c — `tt/estump.f90`

**Selected home:** [estb/estump.f90](../../../../estb/estump.f90)
(24 variants).
**Retired size:** 77 lines / 1,967 bytes.

Older variant-specific stump bookkeeping superseded by the selected shared ESTUMP.

Hard-coded aspen species index 6; selected ESTUMP uses ESASID(VARACD,INDXAS), records stump age, and handles a full MAXTRE list by matching/accumulating records. Retired code does not implement that complete shared capacity logic. Preserve each individual diff and DATA statement.

[Full replacement diff](differences/D12c.patch). Per-file hashes, DATA
literals and recovery command: `D12c` in [retirement.json](retirement.json).

### D12d — `ut/estump.f90`

**Selected home:** [estb/estump.f90](../../../../estb/estump.f90)
(24 variants).
**Retired size:** 59 lines / 1,348 bytes.

Older variant-specific stump bookkeeping superseded by the selected shared ESTUMP.

Hard-coded aspen species index 6; selected ESTUMP uses ESASID(VARACD,INDXAS), records stump age, and handles a full MAXTRE list by matching/accumulating records. Retired code does not implement that complete shared capacity logic. Preserve each individual diff and DATA statement.

[Full replacement diff](differences/D12d.patch). Per-file hashes, DATA
literals and recovery command: `D12d` in [retirement.json](retirement.json).

### D12e — `wc/estump.f90`

**Selected home:** [estb/estump.f90](../../../../estb/estump.f90)
(24 variants).
**Retired size:** 60 lines / 1,362 bytes.

Older variant-specific stump bookkeeping superseded by the selected shared ESTUMP.

Hard-coded aspen species index 26; selected ESTUMP uses ESASID(VARACD,INDXAS), records stump age, and handles a full MAXTRE list by matching/accumulating records. Retired code does not implement that complete shared capacity logic. Preserve each individual diff and DATA statement.

[Full replacement diff](differences/D12e.patch). Per-file hashes, DATA
literals and recovery command: `D12e` in [retirement.json](retirement.json).

### D13 — `canada/newmist/dmcwak.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,927 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13.patch). Per-file hashes, DATA
literals and recovery command: `D13` in [retirement.json](retirement.json).

### D13-02 — `canada/newmist/dmcwbm.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,935 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-02.patch). Per-file hashes, DATA
literals and recovery command: `D13-02` in [retirement.json](retirement.json).

### D13-03 — `canada/newmist/dmcwci.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,934 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-03.patch). Per-file hashes, DATA
literals and recovery command: `D13-03` in [retirement.json](retirement.json).

### D13-04 — `canada/newmist/dmcwcr.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 225 lines / 6,052 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable. This CR file also retains commented subvariant initialization history; full diff/Git recovery includes those comments.

[Full replacement diff](differences/D13-04.patch). Per-file hashes, DATA
literals and recovery command: `D13-04` in [retirement.json](retirement.json).

### D13-05 — `canada/newmist/dmcwec.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,934 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-05.patch). Per-file hashes, DATA
literals and recovery command: `D13-05` in [retirement.json](retirement.json).

### D13-06 — `canada/newmist/dmcwem.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,936 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-06.patch). Per-file hashes, DATA
literals and recovery command: `D13-06` in [retirement.json](retirement.json).

### D13-07 — `canada/newmist/dmcwib.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 95 lines / 2,522 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Already copies CRWDTH rather than recalculating crown width. Unlike selected generic DMCW, this historical copy lacks local I/ISPI/D/H/CL/BAREA declarations despite IMPLICIT NONE. It is uncompiled, not a newly introduced live defect.

[Full replacement diff](differences/D13-07.patch). Per-file hashes, DATA
literals and recovery command: `D13-07` in [retirement.json](retirement.json).

### D13-08 — `canada/newmist/dmcwkt.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,929 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-08.patch). Per-file hashes, DATA
literals and recovery command: `D13-08` in [retirement.json](retirement.json).

### D13-09 — `canada/newmist/dmcwnc.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,941 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-09.patch). Per-file hashes, DATA
literals and recovery command: `D13-09` in [retirement.json](retirement.json).

### D13-10 — `canada/newmist/dmcwni.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,935 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-10.patch). Per-file hashes, DATA
literals and recovery command: `D13-10` in [retirement.json](retirement.json).

### D13-11 — `canada/newmist/dmcwso.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,927 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-11.patch). Per-file hashes, DATA
literals and recovery command: `D13-11` in [retirement.json](retirement.json).

### D13-12 — `canada/newmist/dmcwtt.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,928 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-12.patch). Per-file hashes, DATA
literals and recovery command: `D13-12` in [retirement.json](retirement.json).

### D13-13 — `canada/newmist/dmcwut.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,926 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-13.patch). Per-file hashes, DATA
literals and recovery command: `D13-13` in [retirement.json](retirement.json).

### D13-14 — `canada/newmist/dmcwwc.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 172 lines / 4,955 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-14.patch). Per-file hashes, DATA
literals and recovery command: `D13-14` in [retirement.json](retirement.json).

### D13-15 — `canada/newmist/dmcwws.f90`

**Selected home:** [canada/newmist/dmcw.f90](../../../../canada/newmist/dmcw.f90)
(1 variant).
**Retired size:** 151 lines / 3,943 bytes.

Unselected regional crown-width implementation in the Canadian newmist collection; the BC mistletoe pathway itself remains active.

Recalculates crown width using Moeur/CVCW-derived equations, BH1/BINT2/BH2 species coefficient tables and factors 1.02479/1.03064; selected BC generic DMCW copies CRWDTH. Each coefficient table is preserved separately; regional coefficients are not interchangeable.

[Full replacement diff](differences/D13-15.patch). Per-file hashes, DATA
literals and recovery command: `D13-15` in [retirement.json](retirement.json).

## Why the historical material remains recoverable

D05/D06 were intentional Canadian clones, described in the retained
[2018 change note](../../../../changeNotes/2018.06.22_gdixon.txt). Their
embedded-space behavior differs from shared UNBLNK. Both original and current
root source lists already select shared UNBLNK, and KEYOPN/FILOPN reach that
shared routine. Removal neither introduces that behavior nor restores the old
Canadian behavior. A future filename-behavior proposal needs its own test and
compatibility decision.

D09 preserves an old PC/compiler-specific design, rather than a demonstrated
current platform dependency. Current Make and CMake selection uses shared
MYOPEN, including the Windows recipe. There is no claim that its OPEN behavior
matches every unsupported historical compiler.

D11 was a working library workaround, not merely an unused research sketch.
[history.json](history.json) retains the commits and source-list excerpts that
selected it and later returned to canonical NVEL. The two local power guards
are absent at the corresponding sites in both selected current and original
NVEL; related guards exist elsewhere in the library. Whether the dropped
guards are still needed is unknown. The deferred investigation is recorded as
an **unconfirmed concern** in [FOUND_BUGS.md](../../../../FOUND_BUGS.md#6-deferred-investigation-historical-r9-clark-power-guard).
The exact workaround is recoverable; no guard or other model fix was ported.

For D12, five separate diffs preserve the variant species indices and stump
bookkeeping. No replacement algorithm was inferred from similarity alone.

For D13, 14 files have the same normalized non-DATA statement body, while
retaining regional `BH1`, `BINT2`, and `BH2` coefficient tables. All 42 DATA
statements are stored separately by file. D13-04 additionally contains old
commented CR subvariant initialization; its diff and Git recovery preserve
those comments. D13-07 has no DATA tables and already copies CRWDTH; its local
declarations differ from the selected generic implementation. None of these
facts supports deleting the active BC newmist pathway. The historical headers
cite Moeur (1981), CVCW, and 1994/2008 NISI revisions. No explicit scientific
abandonment decision was found for the regional alternatives; their current
build exclusion and exact recoverability support this bounded retirement.

## Historical manifests

Two Canadian lists remain unchanged and unused by current repository recipes:

| Historical list | Resolve entries from its own directory: missing before → after | Newly retired references |
| --- | --- | --- |
| `canada/bin/FVSbcc_notReady.txt` | 637 → 638 of 712 | `../bc/unblnk.f90` |
| `canada/bin/FVSon_sourceList.txt` | 479 → 481 of 521 | `../fire/on/fmcrowe.f90`, `../on/unblnk.f90` |

The verifier also tests the historical root-bin interpretation; neither list
adds a reference to these 28 files under that interpretation. CMake's
commented ON glob names the distinct **root** `bin/FVSon_sourceList.txt`.
It does not select the same-named Canadian legacy list. The receipt records
that resolution explicitly. These already incomplete historical lists are
retained; repairing or deleting them is separate work.

## Builds, comparisons and limitations

Eight fresh compilations succeeded: original and pre-deletion IE, BC, ON and
SN. All four candidate binaries were reused explicitly after their complete
selected input hashes and flags matched the pre-deletion builds. Candidate IE
and SN still ran in separate processes and directories. Reuse is not counted
as a new compilation.

The platform was macOS 26.6.2 arm64, GNU Fortran/GCC 15.2.0, GNU Make 3.81.
[builds.json](builds.json) records tool versions, exact commands, flags,
input inventories, binary hashes and compressed compiler logs. The same
numerical/trap flags are used throughout; `-fdec`, `-std=legacy`,
`-fallow-argument-mismatch`, Unix C selection and PIC are the documented build
accommodations. Only the two original Canadian source manifests receive
missing link inputs, exactly as retained in [reference-build.patch](reference-build.patch).
Original Fortran/C sources are unmodified. [source-integrity.json](source-integrity.json)
checks every original/current Git blob, both NVEL snapshots and patch replay.

| Case | States run | Pairwise comparisons passed | Meaning |
| --- | --- | --- | --- |
| `ie-iet01` | Original, current, candidate | 3 | All 7 outputs, all SQLite schemas/tables/typed rows, baseline exit/errors preserved |
| `sn-snt01` | Original, current, candidate | 3 | All 8 outputs and baseline exit/errors preserved |
| `bc-StandStructure` | Original, current | 1 | Same invalid-keyword failure before simulation |
| `bc-YSM-SkyRanch` | Original, current | 1 | Same invalid-keyword failure before simulation |
| `on-Hardwood` | Original, current | 1 | Same invalid-keyword failure before simulation |

All **nine** comparison reports pass. IE retains STOP 20 and its existing
reported errors/warnings; SN retains STOP 10. These baseline statuses are
compared, not hidden. Canadian decks retain STOP 20, invalid-keyword diagnostics,
and no generated simulation summary/database. [canadian-boundary.json](canadian-boundary.json)
records those six runs. Candidate BC/ON executions are not claimed: their
selected inputs match current, but existing bugs still prevent meaningful
BC/ON simulation verification.

The [existing exact comparison contract](../../README.md#exact-comparison-contract)
is unchanged: only recognized run metadata and stable CaseID aliases are
normalized. Scientific values have no tolerance or rounding. Every SQLite
schema/table/typed row and output inventory is compared. These cases do not
cover every variant, every R9 equation, or all historical coefficient sets.
The all-24-variant input identity proves this retirement did not alter the
selected implementation; it is not a new global proof that every current
scientific branch matches original F77.

## Verification infrastructure and controls

The shared [verify_retirement.py](../../verify_retirement.py) accepts a per-file
plan, rather than adding another hard-coded family-specific retirement script.
It checks all 24 selected input sets, other model sources/includes, 34 build
recipes, selected-source INCLUDE references, legacy manifest consumers and
original/current/live NVEL integrity. [retirement-controls.json](retirement-controls.json)
records rejection of a selected-source edit, an unselected-model edit and a
reintroduced retired file. Controls altered temporary snapshot copies only;
those bytes were restored before the final successful verification.

[record_evidence.py](../../record_evidence.py) needed a narrow adjustment because
it previously required the unbuilt D07 reader for historical signature tracing.
Only that exact retired filename can now be absent after verifying exclusion
from all 24 lists. Missing live sources, selection of the absent reader, and
incorrect live argument counts still fail. It also identifies Canadian boundary
runs by variant, allowing SN runs in the same receipt. No comparison rule or
model interface changed. [interface-controls.json](interface-controls.json)
records original-size historical and current/candidate live signatures plus
three rejection controls. Bugs 1 and 2 remain demonstrably present; bug 5's
Canadian input boundary remains reproduced. No confirmed model bug was fixed.

## Retained artifacts and replay

[runs.json](runs.json) records inputs, process return codes and every generated
output hash. [artifacts.tar.gz](artifacts.tar.gz) retains the raw inputs,
outputs, stdout, stderr and exit-code files; [artifacts.json](artifacts.json)
binds its SHA256 and size. All nine comparisons were repeated from a fresh
extraction, with all unmodified raw input hashes verified. The replay reports
were identical to the original reports; see [replay.json](replay.json).

From the repository root, verify `checksums.sha256` within this evidence
directory, extract the artifact archive into a fresh directory, then replay:

```sh
python3 tests/parity/compare_outputs.py \
  --runs tests/parity/evidence/2026-11-13-d04-d13/runs.json \
  --left original --right candidate --case ie-iet01 \
  --artifacts-root /private/tmp/mfvs-d04-d13-replay/runs \
  --report /private/tmp/mfvs-d04-d13-replay/ie-comparison.json
```

Rebuild from the recorded commits using `prepare_builds.py` with
`--variants ie bc on sn`, run the five recorded cases with `run_cases.py`, and
run `verify_retirement.py --evidence-dir` against the new receipts. The latter
requires access to the recorded temporary snapshots, or freshly reconstructed
snapshots. Source/receipt checks are not claimed to run without those inputs.
Artifact comparisons can be replayed independently of the temporary binaries.

The earlier P01–P03 and D01–D03 evidence bytes are unchanged. Their checksums
were captured before this batch and checked again at completion. D14 and all
C/M/R/U/B/L tasks remain pending. Any future fixes from FOUND_BUGS.md require
separate authorization and before/after evidence.
