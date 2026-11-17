# Found Bugs

The first five confirmed model defects below were found during the 2026-11 consolidation of duplicated routines. Every one is already present in the pre-modernization source (commit `c2105365`), so none was introduced by the conversion to free-form Fortran or by the consolidation. M02a–M07a resolve the interfaces in section 1 and the missing arguments in section 2, and remove DBSIN's part of section 5; M12a subsequently repairs BC INITRE/MISIN's buffers in section 5, and M17a repairs ON INITRE's buffers. The remaining defects are deliberately retained. These interface repairs define behavior where the original calls were undefined and cannot establish simulation parity on those paths. Sections 6–8 separately record a reproduced R9 Clark power trap, a historical documentation discrepancy, and ESADDT data-flow concerns requiring follow-up. Section 9 records a reproduced SQLite resource defect with scientific-output impact still unmeasured. Section 10 records a restart-file difference that modernization itself introduced, now corrected. Sections 11 and 12 record two original defects that read undefined values and so make restart files vary between runs. Sections 13 and 14 record an invalid stub diagnostic and a fuel-model fallback defect found during the safe reapplication; neither was fixed. Sections 15–18 record DGDRIV tripling/bounds defects and undefined local reads found during L05; all were preserved. Section 19 records conditional undefined local reads in KT CRATET; these were also retained. Sections 20 and 21 record two defects in the former metric copy of EVLDX found during the M01 audit; the M01a merge carried both into `base/evldx.f90` unfixed. Sections 22–24 were found during the M02–M07 audits: section 22 is an output difference in BC and ON that the free-form conversion introduced, six of whose FORMATs are now corrected by M05a/M07a; sections 23 and 24 are original defects retained in the shared STATS and FMDOUT.

Unless a later status or location is given, file and line references are to the tree at commit `b5657da3` plus the then-uncommitted `VARIANTCD` change. Bugs 1 and 2 have the same root cause: FVS calls external procedures with no explicit interface, so gfortran cannot check argument lists across files. Moving these routines into modules would let the compiler catch this class of bug.

Sections 25–29 were found during the bounded M08–M14 audits on 15 November
2026 at `ed5e92fb`. All are present in the pinned original F77 reference and
remain unfixed. Temporary block checks reproduce the report/classification,
diagnostic and zero-division cases described below; they do not establish
Canadian integrated simulation coverage.

Sections 30–32 were found during the bounded M15–M21 audits on 15 November
2026 at `6dcf25a6`. All three are present in the pinned original F77
reference and remain unfixed. Direct original/current SUMOUT, SQLite and
sampling-field checks reproduce them without relying on Canadian keyword
input or claiming integrated simulation parity.

---

## 1. `FMSVL2` is called with the wrong number of arguments in BC and ON

**Variants:** BC, ON
**Status:** interface mismatch resolved 15 November 2026 under M07a. BC's
`FMSVL2` now accepts ten arguments; every variant uses the ten-argument call
in `fire/base/fmdout.f90`. BC still ignores crown ratio and the live/dead
flag. Direct BC eight/ten-argument tests match original F77 and the immediate
baseline for all species, height/merchantability cases and debug settings.
ON's original call was undefined, so the repair cannot claim parity with it.
The description below records the pre-repair mismatch.

The snag-volume entry point `FMSVL2` has two incompatible definitions. The shared one in `fire/base/fmsvol.f90:82` takes ten arguments `(JSP,XD,XH,XHT,VOL2HT,CRWNRTO,LIVEDEAD,LMERCHIN,DEBUG,IOUT)`. The BC one in `canada/fire/bc/fmsvol.f90:68` takes eight `(JSP,XD,XH,XHT,VOL2HT,LMERCHIN,DEBUG,IOUT)`, without crown ratio or the live/dead flag. Each variant compiles callers written for the other version.

In **ON**, the shared ten-argument entry is linked, but `metric/fire/base/fmdout.f90:228` calls it with eight arguments. The callee therefore receives `LMERCH` as the crown ratio, `DEBUG` as the live/dead character, and `JOSTND` (a non-zero unit number) as the merchantable-volume switch. Its `DEBUG` and `IOUT` arguments were never passed.

In **BC**, the eight-argument entry is linked, but 26 call sites in shared code use the ten-argument form:

- `base/svcwd.f90` (259, 261)
- `fire/base/fmcrbout.f90` (128)
- `fire/base/fmcwd.f90` (190, 242, 284)
- `fire/base/fmevmon.f90` (294, 309, 580, 590, 666, 737)
- `fire/base/fmsalv.f90` (204, 215)
- `fire/base/fmscut.f90` (104, 136, 164)
- `fire/base/fmsvout.f90` (102, 110, 115, 120, 201, 209, 214, 219)
- `fire/vbase/fmcrowe.f90` (274)

Here the callee reads the crown ratio as the merchantable-volume switch, the `'L'`/`'D'` character as `DEBUG`, and the caller's merchantable flag as the output unit.

The behavior is undefined in both cases. The most likely visible effect is that snag and down-wood volumes switch between merchantable and total volume according to an unrelated value. Because the BC and ON test decks never get past keyword input (see bug 5), no test has exercised these paths.

A third copy of the eight-argument call, formerly in `canada/fire/on/fmcrowe.f90:277`, was not compiled by any variant and was retired under D08 on 13 November 2026. Its original source is available with `git show 9bc35a16:canada/fire/on/fmcrowe.f90`. M07a resolves the live mismatched calls described above.

**Repair completed under M07a:**
1. Give the BC `FMSVL2` the ten-argument interface; it can accept `CRWNRTO` and `LIVEDEAD` and ignore them.
2. Change the metric `fmdout` call to the ten-argument form, passing `ICR(I)` and `'L'` as `fire/base/fmdout.f90:244` does.
3. Merge the `fmdout` twin after the interface fix. The unused ON `fmcrowe` retirement is already complete under D08.

The twin was kept until the BC interface was repaired, then removed under M07a.

---

## 2. Metric `INTREE` calls `DBSTREESIN` with 30 of its 33 arguments

**Variants:** BC, ON
**Status:** missing arguments resolved 15 November 2026 under M02a. All
variants now use `base/intree.f90` and its 33-argument call. Real SQLite
input in both BC and ON verifies cull, decay/stem fields and metric units.
The absent metric decay-code correction is still retained behind
`LMETRICVAR`; it needs a separate model correction. The description below
records the old interface defect.

`dbsqlite/dbstreesin.f90:5` gained three trailing arguments for cull, decay code and woodland stems: `RCULL`, `IDECAY`, `IWDLDSTMS`. `base/intree.f90:135` passes them as `CULL(I)`, `DECAYCD(I)`, `WDLDSTEM(I)`, but the metric copy at `metric/base/intree.f90:136` still passes only the original 30.

`DBSTREESIN` sets all three to zero unconditionally (lines 78–80) and reads them from the `Cull`, `DecayCd` and `WdldStem` columns when present (lines 217–225). In BC and ON those writes go through addresses that were never passed. A scratch build confirmed the effect. With bug 5 fixed, ON crashed with a segmentation fault while reading tree records from the database. When the three arguments were also passed, the ON test ran to completion.

The metric copy also lacked the decay-code correction that the imperial reader applies after the call; M02a preserves that metric behavior. `metric/dbsqlite/dbstreesin.f90`, an older 30-argument version, was not compiled by any variant and was retired under D07 on 13 November 2026. Its original source is available with `git show 9bc35a16:metric/dbsqlite/dbstreesin.f90`. M02a changes the caller's interface; the shared callee is unchanged.

P03 source tracing on 13 November 2026 confirmed 33 versus 30 arguments in both the original `c2105365` source and mFVS before M02a. Earlier documentation counted 31 versus 28, omitting `ABIRTH` and `LBIRTH` from both totals.

**Remaining correction:** decide and port the metric decay-code correction.
Passing `CULL(I)`, `DECAYCD(I)` and `WDLDSTEM(I)` and merging the twin are complete.

---

## 3. `ESADDT` bins seedlings by hard-coded species indices that are wrong for several variants

**Variants:** AK, CI, EM, IE, KT (the former ESTB source profile)

When ADDTREES uses an external establishment model, `ESADDT` writes the stand to a `.es1` file for that program. Fields 13–20 hold trees per acre of seedlings (DBH ≤ 1 in.) for eight species. The species are chosen by fixed variant species indices in `estb/esaddt.f90:151–168`. Those indices follow the IE/CI/KT species order, which other variants don't share, and the Douglas-fir index is wrong even in IE order.

| `.es1` field | Intended species | Index used | IE, CI, KT | EM | AK |
|---|---|---|---|---|---|
| 13 | ABGR (grand fir) | 4 | GF | LM | TA |
| 14 | ABLA (subalpine fir) | 9 | AF | AF | LP |
| 15 | LAOC (western larch) | 2 | WL | WL | AF |
| 16 | PICO (lodgepole pine) | 7 | LP | LP | BE |
| 17 | PIMO (western white pine) | 1 | WP | WB | SF |
| 18 | PIEN (Engelmann spruce) | 8 | ES | ES | SS |
| 19 | PIPO (ponderosa pine) | 10 | PP | PP | RC |
| 20 | PSME (Douglas-fir) | 5 | **WH** | **LL** | WS |

Douglas-fir is index 3 in IE, CI, KT and EM. In all of them, the PSME field therefore reports another species (western hemlock or subalpine larch) and Douglas-fir seedlings are not reported at all. In EM, the ABGR and PIMO fields also report the wrong species, and in AK every field does. Only runs that use the external-model form of ADDTREES are affected, through the trees the external model returns.

C10 centralizes all 24 variants in `estb/esaddt.f90`. Compile-time `IDF` preserves index 5 for the five variants above and index 3 for former STRP consumers; every other hard-coded bin is unchanged. Actual original/current/candidate `.es1` exports match for all variants. This relocation does not extend the confirmed scope above to every new consumer of the shared filename, or establish that the former STRP indices are scientifically correct. No species-bin correction was applied.

**Suggested fix:** select species by code (`JSP` or `FIAJSP`) rather than by index, and decide whether the external model applies to AK at all.

---

## 4. Mistletoe height-growth defaults for species 11 and up are written into the wrong array

**Variants:** CA, OC, WS

In `mistoe/misintca.f90` (CA and OC) and `mistoe/misintws.f90` (WS), the default mistletoe height-growth proportions are given by five DATA blocks. Only the first block, rows 1–10, names `AHGP`. The other four (`misintca.f90:176, 188, 200, 212` and `misintws.f90:230, 242, 254, 266`) are labelled `ADGP`, the diameter-growth array, which already has its own DATA blocks for those rows.

Initializing an element twice is not standard Fortran, but gfortran accepts it without a warning, even with `-Wall`, and keeps the *first* value; this was checked with a small test program. So `ADGP` keeps its intended values, and `AHGP` rows 11 to `MAXSP` are never initialized and stay at 0.0 instead of the intended 1.0 (no effect).

`MISHGF` (`mistoe/mishgf.f90:147`) returns `HGPDMR(species, DMR+1)`, forces 1.0 only for uninfected trees, and the height-growth routines multiply by it:
- `ca/htgf.f90:219`
- `oc/htgf.f90:240`
- `ws/htgf.f90:429` and `ws/htgf.f90:745`

As a result, **every infected tree (DMR ≥ 1) of a host species with index 11 or above gets zero height growth**, unless a MistHMod keyword supplies values. The affected hosts are:
- **CA and OC:** KP, LP, LM, JP, SP, WP, PP
- **WS:** WP, PM, SF, KP, FP, CP, LM, MP, GP, WE, GB, BD, MH

**Suggested fix:** rename the four mislabelled blocks in each file to `AHGP`. This changes CA, OC and WS results whenever infected trees of those species are present.

---

## 5. Canadian keyword readers pass seven-field buffers to the twelve-field KEYRDR

**Variants:** BC, ON
**Status:** BC's INITRE/MISIN buffers repaired 15 November 2026 as the
isolated prerequisite for M12a. M17a repairs ON's INITRE on the same date
by widening only its three ARRAY/LNOTBK/KARD declarations from 7 to 12.
M17b/M17d subsequently share only DESIGN through INITDESIGN; keyword
field references and other handlers are unchanged.

`KEYRDR` (`base/keyrdr.f90`) declares `ARRAY(12)`, `KARD(12)` and `LNOTBK(12)` (lines 35, 39, 44) and fills all twelve. The original BC/ON readers declare seven-element arrays, so an ordinary keyword record overwrites the five elements past the end. In `INITRE` this evidently overwrites the neighboring `KEYWRD` variable:
- every keyword reads back as eight NUL bytes;
- each record produces `FVS01 ERROR: INVALID KEYWORD WAS SPECIFIED`;
- the run ends with `STOP 20`.

**Original F77 BC and ON cannot process keyword files at `c2105365`.**
Both variants now read keywords and produce additional model output, so
their pristine-F77 suite comparisons intentionally differ. Upstream commit `08e833b2` (28 August 2025) widened
KEYRDR from 7 to 12 fields without changing the metric and BC copies.

Compiled readers at the audit and their current status:

| File | Status after M17a | Variants |
|---|---|---|
| `canada/bc/initre.f90` | All three buffers widened | BC |
| `metric/vbase/initre.f90` | All three buffers widened | ON |
| `metric/newmist/misin.f90` | All three buffers widened | BC |

The same defect was in pre-modernization `canada/bc/keyopn.f`, `metric/base/ksstag.f` and `metric/clim/clin.f`. The consolidation replaced those with the shared 12-element versions, so it no longer exists there. M05a also retires `metric/dbsqlite/dbsin.f90`; BC/ON now use the shared DBSIN with `KARD(12)`. Direct DBSIN tests exercise the real twelve-field KEYRDR.

Eleven uncompiled files had the same defect (in `archive/bgc/`, `metric/dfb/`, `metric/estb/`, `metric/ppbase/`, `metric/rd/` and `metric/wwpb/`). They were removed with the other unbuilt directories on 14 November 2026.

A pre-M05a scratch build with the then-four compiled files widened to 12 read the BC and ON keyword files correctly.

M12a's full-MISIN fixtures exercise 20 real twelve-field KEYRDR cases with
caller-buffer canaries and bounds checking. Outputs, threshold bits, NEWMOD
reset/enable and all 30 MISKEY slots match original F77 and the immediate
baseline after widening only their corresponding buffers. These controls
avoid the original undefined writes; they do not establish equivalence with
the overrunning calls.

**Buffer repair complete:** M17a supplies ON's separate interface repair.
A shared field-count constant remains a possible follow-up. MISTPRT and
DESIGN are shared; complete INITRE/MISIN merges still require their bounded
audits. DBSIN and the missing DBSTREESIN arguments are already handled under
M05a and M02a.

---

## What the regression tests show for BC and ON

The original BC/ON decks stop at keyword input. After M17a, the whole
original-reference suite reports 22/24 variants matching: both Canadian
variants now run farther and differ from that undefined original behavior.
The 22 U.S. variants match original F77 model outputs.

For M17a/M17b/M17d and M18a–M21a, all eleven BC generated files match the
immediate baseline at `62db12b5`; all five ON generated files match that
baseline with only its three INITRE buffer declarations widened. BC
StandStructure still fails later, while YSM-SkyRanch and ON Hardwood produce
summaries. Guarded twelve-field KEYRDR/DESIGN cases also match native F77
and the immediate baseline exactly. These are controlled comparisons, not
pristine-original Canadian simulation parity or fixes to the remaining
model defects.

R02 subsequently repairs the BC snag-call interface (section 42), allowing
StandStructure's projections to finish with its expression warnings and
`STOP 20`, and repairs ON's snag-volume trap (section 46). Those changes are
separate correctness repairs; the pristine-original comparison remains 22/24.

An earlier, pre-M05a scratch build with bugs 5 and 2 fixed showed two further,
unexplained problems:

- **ON:** the `Hardwood` test runs to completion. Its starting stand values match `tests/FVSon/Hardwood.sum.save`, as do the post-treatment stand values in later rows. But from the second row on, the `.sum` file's year and growth columns hold wrong values, which points to a further summary-output problem.
- **BC:** both tests still end with exit status 127. `StandStructure` writes no output, and `YSM-SkyRanch` output stops right after the mistletoe keywords (`NEWSPRED`).

Those earlier discrepancies have not been fully traced. The current BC
comparison above supersedes that older input-stage observation. The ON
summary-stride and fresh-schema defects are now recorded in sections 30–31
and remain unfixed.

---

## 6. R9 Clark inverse-height power underflow

**Status:** R9HT repaired on 16 November 2026 in a separate correctness step
before O04. O02 reproduced the inherited SN stop; the follow-up identified
underflow in the inverse-height power. The historical fixed threshold was
not reinstated, and R9DIB is unchanged.
**Demonstrated scope:** SN using the bundled native inventory record below.
**Potential wider scope:** variants selecting NVEL `r9clark.f90` (all current
variants except BC), when the relevant R9 equation path is used.

D11 found that the formerly selected `volume/r9clark_fvsMod.f90` guards the power term in `r9dib` and `r9ht`: when `(1.0-17.3/totht) < 0.005748` and `p > 14`, it sets `Y=0` instead of evaluating `(1.0-17.3/totht)**p`. Before this repair, selected NVEL evaluated that power directly at these two sites. Related guards exist elsewhere in NVEL; this is not evidence that the whole library lacks underflow handling. The pinned original NVEL `38548071` also evaluates these two powers directly.

The local workaround entered build selection in `a19c41b4` (4 March 2024); ON selected it in `d495d604` (26 June 2024). The lists returned to canonical NVEL in `08e833b2` (28 August 2025), before the pinned original F77 reference. The available history establishes that selection change, but does not explain whether these two guards were deliberately dropped, made unnecessary, or overlooked.

O02's larger inventory sweep reached a trap on `Stand_CN=3009010602`
(`Stand_ID=08030101310002`, inventory year 2014, 40 input tree records) in
`tests/FVSie/FVS_Data.db`, whose native variant is SN. A separate single-stand
deck reproduces it: read this Stand_CN from both FVS_StandInit and FVS_TreeInit,
request ten cycles and `FMIN/POTFIRE/FUELOUT/SNAGSUM/END`, then PROCESS/STOP.
The database records were not changed. With O01's Mac toolchain and default
trap flags, both pre-repair main `b344e862` / NVEL `a73e697e` and original main
`c2105365` / NVEL `38548071` terminate with `SIGILL` (process return code -4).
Symbolized backtraces locate `Y=(1.0-17.3/totHt)**p` in R9HT at current
`volume/NVEL/r9clark.f90:1285` and original `r9clark.f:1286`, through
R9CLARK, VOLINIT/VOLINITNVB, NATCRS and VOLS. All four partial output files
from the larger SN sweep match between original and current code after the
existing comparator's run-header and CaseID masks.

The follow-up captured `totHt=17.3999996`, `p=17.8098202`, and the original
single-precision base `0.00574713945`. A probe with underflow halting disabled
confirmed the IEEE underflow flag: the power is about `1.248068e-40`, below
`TINY(real)=1.17549435e-38`. Its subtraction does not change the bits of
`X=0.00484667020` in this case. The R8 outside-bark coefficients select this
exponent; the table and input records were left unchanged.

The repair checks the logarithm of the original real-valued base in double
precision, before evaluating the power. Positive powers below normal real
range use zero for the vanishing term; every normal-range result keeps the
original single-precision expression. This avoids both the demonstrated
trap and the old threshold's suppression of some representable values.
No floating-point trap flags were changed. The historical workaround remains
available with `git show 9bc35a16:volume/r9clark_fvsMod.f90`.

`tests/parity/check_nvel_repairs.py --component r9ht` checks 378 exact result
comparisons across three taper segments and both sides of the normal-range
boundary, including 117 cases where original F77 raises underflow. Only the
fixture's original oracle permits underflow; repaired calls halt on it.
The formerly failing stand and all nine populated SN inventory stands now
finish with exit code zero. All 24 standard-suite variants match the immediate
pre-repair baseline; the 22 U.S. variants still match pristine original F77,
with the existing BC/ON comparison limits retained. No masks were changed.

O10's direct VOLINITNVB cases (16 November 2026) also reached R9DIB's
unguarded power: a 9-inch, 12-foot SN tree with equation `811CLKE611` stops
with the same underflow trap in both the pre-change and current NVEL. Those
cases now use plausible diameter/height pairs; R9DIB remains unrepaired.

---

## 7. Historical NC crown-mapping comment disagrees with its table

**Status:** stale documentation, not a demonstrated numerical defect; recorded during C04 on 14 November 2026.
**Variant:** NC, species 12 (coast redwood).

The old `fire/nc/fmcrow.f90` comment says coast redwood maps to equation 3, but the twelfth `ISPMAP` value is 19. Original F77 `c2105365:fire/nc/fmcrow.f` has the same comment and value (lines 71–74). `fire/base/fmcroww.f90` identifies equation 3 as Douglas-fir and equation 19 as giant sequoia/coast redwood, which supports treating the old comment as stale rather than changing the table.

C04 preserves mapping **19**, along with every other KT/NC table entry, in shared `fire/base/fmcrow.f90`. Its comment makes the historical discrepancy explicit. Direct original/current/candidate tests exercise every species with the actual crown equations. No numerical correction is proposed or applied. Revisit the equation assignment only if a separate scientific review supplies evidence that the table itself is wrong.

---

## 8. Deferred ESADDT investigation: locals on short-option/error paths

**Status:** source-level data-flow concerns found during C10 on 14 November 2026. Reachability from valid keyword input and numerical impact have not been demonstrated.
**Potential scope:** every ESADDT consumer; all current variants select the shared implementation after C10.

`estb/esaddt.f90` assigns `IPYR` only when `IABS(NP) >= 2`, but otherwise defaults `IMETH` to 1 and can reach the export that writes `IPYR`. Determine whether OPGET can return fewer than two parameters for a valid ADDTREES request before treating this as a demonstrated input-path bug.

`IKEEP` is assigned by the `.es2` READ, but a blank external command, an open/write failure, or end-of-file can branch to label 101 and then reach `IF (IKEEP .NE. 1)` at label 202 without that READ succeeding. The subsequent CLOSE can also target a unit that was not successfully opened. No initialization or error-path guard was added during consolidation.

Both patterns are already present in original F77 `c2105365:estb/esaddt.f` (IPYR assignment/use around lines 58–59/129; IKEEP READ/cleanup around 184/190–194) and its STRP twin. C10's tests cover configured two-parameter exports, successful local regeneration responses, no pending requests and the unsupported-method branch. They do not establish safe error recovery. The executable body was preserved after resolving the species-index parameter.

Later work should trace OPGET's parameter-count contract and the unit/IKEEP lifecycle, reproduce a reachable case, and measure the effect before selecting defaults or cleanup changes. Keep any correction separate from original-F77 parity work. Bug 3's species-bin defects also remain unfixed.

---

## 9. FIA cut-list and after-treatment writers orphan prepared statements

**Status:** reproduced on 14 November 2026 in original F77 `c2105365`, immediate baseline `fb27261e`, and the C13–C18 candidate. Deliberately unfixed. This is a resource-lifecycle defect; no scientific-output change or full-engine memory-growth measurement is claimed.
**Scope:** all 24 variants select these SQLite writers; both FIA reports must be active for the consecutive-writer reproduction.

`vbase/cuts.f90` calls `DBS_FIAVBC_CUTLST` followed by `DBS_FIAVBC_ATRTLS`. Both writers prepare an INSERT statement, reset it after each row, and return after COMMIT without finalizing it. `fsql3_prepare_` in `dbsqlite/fvsqlite3.c` overwrites the single `stmtset[dbnum]` pointer. Preparing the after-treatment INSERT therefore loses the pointer to the cut-list statement. `DBSCLOSE` finalizes only the currently tracked statement; that cannot release the orphan. The close wrapper also clears its database slot even if `sqlite3_close` fails.

The controlled IE fixture calls the actual writers in this order with real repository SQLite and an already established CaseID. It supplies case-number/error-reporting context only. SQLite reports **two** outstanding statements after the pair, **one** after normal `fsql3_finalize`, and `SQLITE_BUSY` (5) from direct close attempts at both points. All three source states produce these same observations. The test finalizes the orphan through separate harness teardown so it can release its test database; no model source is repaired. Single-writer retention alone is not the demonstrated defect, because the existing close routine can finalize the last tracked statement.

To reproduce, call both writers on one open output database with both reports enabled and an established CaseID. Count statements with `sqlite3_next_stmt`, finalize the tracked statement through `fsql3_finalize`, count again, and try closing the connection. The original writer sequence and C wrapper already contain the leak; the binding helper preserves it. Later work should measure repeated-report resource growth and review statement ownership across prepare/finalize/close before choosing a fix. Do not combine that correction with F77-preserving cleanup.

---

## 10. Corrected: restart files differed from original F77 after `b5657da3`

**Status:** found and corrected 14 November 2026. Introduced by the consolidation, not inherited. No listing, summary, tree list or database output was affected.
**Variants:** the 17 US variants that use the partial establishment model (all except AK, CI, EM, IE and KT), and by the same change BC and ON.

Original F77 has two versions of `ESFLTR`. `estb/esfltr.f` (AK, CI, EM, IE, KT) tallies `TPAAINV`, the inventory trees per acre on each plot. `strp/esfltr.f` (every other variant) is otherwise the same routine but never touches `TPAAINV`. Commit `b5657da3` removed the `strp` copy and pointed all variants at the `estb` one, so the other 19 variants began computing `TPAAINV`.

`TPAAINV` is read in only one place, `ak/estab.f90`, so no result changed. But `pg/putstd.f90` saves it in restart files, where it showed up as eleven reals per stand (one per inventory plot of the test stands) that are all zero in original F77.

**Fix:** `estb/esfltr.f90` now tallies `TPAAINV` only for AK, CI, EM, IE and KT, through a compile-time test on `VARIANTCD`. Restart files of all variants match original F77 again, apart from the two original defects below. The test exists only to keep that match; whether every variant should tally `TPAAINV` is a separate, model-changing question.

---

## 11. TT takes its geographic location code from a table that is never filled

**Status:** found 14 November 2026 while tracing restart files. Present in original F77 (`c2105365:tt/forkod.f`, lines 35 and 88). Repaired 16 November 2026 as part of prerequisite R01 in `OPTIMIZATION_TASKS.md`.
**Variant:** TT.

`tt/forkod.f90` declared `KFOR(4)` but gave it no values, then executed `IGL = KFOR(IFOR)` whenever a valid forest code was used. Every other variant's `FORKOD` fills `KFOR` with a DATA statement. `IGL` therefore held whatever was in memory: for example 1798184300 in one build of the original code and 1806851564 in another build of the same code.

TT never reads `IGL`, so results were unaffected. Since consolidation, TT compiles shared DGF and REGENT files that read `IGL`, but only in WS's DGF and BC's REGENT branches. The value is saved in restart files (the 25th integer of each stand's record), which is why TT restart files never matched between builds.

**Repair:** `data KFOR/4*1/` keeps the value `GRINIT` assigns (`IGL = 1`), as UT's FORKOD does. In the suite's TT restart file, only the five stands' `IGL` words changed: 20 bytes, from 1861606772 to 1. All other TT outputs match the immediate baseline. Original F77 still stores an undefined value, so `tests/parity/check_suite.py` keeps the TT exception for comparisons with it. The exception now requires the working tree to store 1, which makes the comparison stricter than before.

---

## 12. Decay codes read from tree files are set from the previous record's history code

**Status:** found 14 November 2026 while tracing restart files. Present in original F77 (`c2105365:base/intree.f`). Deliberately unfixed. Effect on results not measured.
**Variants:** all, for stands whose tree records come from a tree file rather than a database.

`base/intree.f90` calls `DBSTREESIN` for each tree record and then corrects the decay code (lines 141–146): dead trees (`ITH` above 5) without a valid code get 3, live trees get 0. When trees come from a database, `DBSTREESIN` has just read `ITH`, so this works. When they come from a tree file, the record is not read until line 170, after the correction. The correction therefore uses `ITH` from the *previous* record, and for the first record `ITH` has no value at all.

The effects:
- a tree that follows a dead tree in the file gets decay code 3, and a dead tree gets 0 unless the tree before it is also dead;
- the first tree's code is undefined. With the same IE executable and the same input (`iet01.key` stopped in 2040), six runs gave the first tree, and the eight records later split from it, code 0 four times and code 3 twice.

`ie/vols.f90` resets the code to 0 for live trees at the point where it uses it, and the regression outputs are identical between runs, so the visible effect appears limited to dead trees' codes. That has not been measured. The codes are saved in restart files; `check_suite.py` accepts a 0-versus-3 difference in a single integer there for this reason.

The metric copy `metric/base/intree.f90` has no such correction at all (see bug 2).

**Suggested fix:** move the correction to after the record has been read, for both input paths. Then remove the decay-code exception from `check_suite.py`.

---

## 13. BMIN uses dummy arguments belonging to a different ENTRY

**Status:** confirmed by source inspection on 14 November 2026; present in original F77 `c2105365:base/exbm.f`. Runtime impact has not been measured. Deliberately unfixed.
**Variants:** all 24 select `base/exbm.f90`; the unsupported pine-beetle keyword path calls BMIN when keyword echo is enabled.

`BMIN(LKECHO)` writes using `JOPPRT` and `IRECNT`, which are dummy arguments of `BMPPIN(IREAD,IRECNT,JOPPRT)`, not BMIN. They are undefined when BMIN is called. Its format also expects an A8 field before I6, while the supplied values are `IRECNT,NOBM`. Source inspection of the pinned F77 and current stub confirms both defects; no integrated failure or numerical effect is claimed.

The proposed BMPPIN deletion would turn the two arguments into uninitialized local integers and change this already-invalid diagnostic path. BMPPIN and its declarations were retained during cleanup. A later, separate fix should obtain the correct output unit and record count explicitly and align the format with the argument list, with an unsupported-keyword fixture against a deliberately corrected reference.

---

## 14. Fuel-model failsafe leaves the dominant-model result at -1

**Status:** reproduced at block level on 14 November 2026 in original F77 `c2105365:fire/base/fmdyn.f` and extracted `fire/base/fmmodsel.f90`. Reachability from valid full-model inputs and model-output impact have not been demonstrated. Deliberately unfixed.
**Variants:** all 24 select FMDYN; the same tail is used by FMSHRUB for CA, NC, OC and WS.

The failsafe sets `FMOD(1)=8`, its weight to 1 and the warning flag when no dominant model is available, but leaves the return value `FMD=-1`. The later static-model branch then overwrites `FMOD(1)` with that -1. The same statements occur in the original FMDYN and FMSHRUB copies.

A temporary fixture compiled the original F77 tail without changing its statements and compared it with FMMODSEL. Fourteen cases, including ordinary weights, truncation, the 1e-6 threshold and zero weights in both dynamic/static modes, matched exactly. With all weights zero, both return FMD=-1; FMOD(1) is 8 in dynamic mode and -1 in static mode, with one warning request. This isolates the fallback behavior and does not establish a reachable valid-input simulation failure.

A later investigation should reproduce the fallback from a full model run before assigning impact. Any correction should set the dominant result consistently with the intended fallback and be verified separately from F77-preserving consolidation.

---

## 15. DGDRIV tripling limits use the parent record instead of the triple

**Variants:** CI and UT (test and assignment); EM and IE (test)
**Location:** the `ITRIPU`/`ITRIPL` growth-limit blocks in `vbase/dgdriv.f90`

The original `c2105365` copies already have these indices. CI (species 17,
19) and UT (17–19, 22) calculate `GDIF = DG(I)-WKI` and write `DG(I)` after
calculating a triple's growth. EM (11, 13–16, 19) and IE (19, 22) also test
`DG(I)` but write the triple. CR, from which these blocks were copied, tests
and writes the triple itself. Consequently the documented one-third limit
can be bypassed: with `WKI=1`, parent `DG(I)=1` and triple growth 2, the test
is `0 > 0.33` and the triple remains 2 rather than being limited to 1.33.
This is a source-level/synthetic confirmation, not a measured regression-suite
impact. L05 preserves all original indices. A separate model fix should use
the relevant triple in both the test and assignment, with affected-species
and tripling regressions.

---

## 16. WC/PN DGDRIV overwrites the bounds on a dubbed diameter increment

**Variants:** WC, PN
**Location:** missing-increment branch after label 215 in `vbase/dgdriv.f90`

The original WC routine, also used by PN, first computes an unscaled increment,
limits it to inside-bark DBH and calls `DGBND`. It then overwrites `DG(I)` with
`SQRT(D*D+EXP((WK2(I)+OLDRN(I)))*SCALE)-D`, without applying either bound again.
The same sequence is present in `c2105365:wc/dgdriv.f`. For example, `D=10`,
`EXP(WK2+OLDRN)=100`, `SCALE=4` gives an initial increment of about 4.14 but
an overwrite of about 12.36, exceeding even `D`. This confirms a possible
bound violation synthetically; its footprint in real stands is unmeasured.
L05 preserves the overwrite. A separate fix should establish the intended
scaling order and then apply the appropriate bounds to the final value.

---

## 17. BC DGDRIV can read LTEM before assigning it

**Variant:** BC
**Location:** calibration label 193 in `vbase/dgdriv.f90`

`LTEM(ISPC)` is set only after the empirical-Bayes correction, but the
insufficient-sample path reaches label 193 without that assignment and reads
it to set `CTEM(IREFI)`. This path and the automatic local declaration are
present in `c2105365:canada/bc/dgdriv.f`. `CTEM` is never subsequently read,
so no scientific-output impact has been demonstrated. BC's supplied test
stops at keyword input (bug 5). L05 retains the assignments and read; a
separate cleanup should initialize the flag before all paths to its use,
or establish that the unused marker can be removed.

---

## 18. OC/OP DGDRIV prints an unassigned ORGANON status flag

**Variants:** OC, OP
**Location:** ORGANON error diagnostic, FORMAT 125, in `vbase/dgdriv.f90`

When `IERROR=1` and debugging is enabled, the routine prints local logical
`STATUS`, which is neither assigned nor passed to `EXECUTE`. Both original
`c2105365` routines have the same declaration and read. This is a diagnostic
reliability defect; no model-output effect has been established. L05 retains
it. A separate change should identify the intended status value or remove
that diagnostic field, with an ORGANON error-path test.

---

## 19. KT CRATET conditionally reads unassigned or stale local values

**Variant:** KT
**Location:** KT height-dubbing branches in `vbase/cratet.f90`
**Status:** confirmed by source tracing in current code and original F77
`c2105365:kt/cratet.f`; simulation impact has not been isolated. Unfixed.

`IERRCK` is assigned only the value 1 when `STEP1 <= 0`, but is read in the
small-tree fallback test when the discriminant is positive as well. There
is no initial assignment or reset, so that read can use an undefined value
or a value retained from an earlier record/species. Fortran does not require
short-circuit evaluation of the surrounding `.or.`/`.and.` expressions.
The intended lifetime of this flag needs to be established before changing it.

`XN` is assigned only after the `K1 < 3`/`NOHTDREG` bypass, but the live-tree
dubbing diagnostic prints it on the default-equation path too. With debugging
enabled, a first insufficient-sample/default species can therefore print an
undefined value; later species can print a preceding species' sample count.
This second read affects diagnostics; no model-value impact is claimed for it.

L06 preserves both conditional assignments and reads, along with the original
pre-DBCHK initialization of `RAD` and `SOLN`. A separate repair should define
`IERRCK`'s lifetime and provide a defined diagnostic count, with cases covering
both discriminant signs, successive species and insufficient height samples.

---

## 20. BC and ON cannot evaluate an expression that contains a number

**Variants:** BC, ON
**Location:** `MXCNST` in `base/evldx.f90` (before the M01a merge,
`metric/base/evldx.f90:55`)
**Status:** reproduced by a direct call on 14 November 2026; present in
`c2105365:metric/base/evldx.f`. Unfixed; M01a kept the limit for BC and ON.

Numbers in `IF` and `COMPUTE` expressions are stored from the top of `PARMS`
downward and loaded with instruction code 1000 plus the index. Upstream
commit `86463e16` (20 April 2025) raised `MAXPRM` from 5999 to 7999 and
changed the test in `base/evldx.f` from `INSTR.LT.7000` to `INSTR.LT.9000`.
The metric copy kept 7000. The first 2,000 constants of a run get codes
7000–8999, which BC and ON reject with return code 2.

A driver linked with the BC objects compiled `YEAR GE 5` to codes
`101 8999 7` and `ALGEVL` returned 2, which `EVMON` reports as `FVS21 ERROR:
AN EXPRESSION WAS INCORRECTLY COMPUTED; ANSWER IS IGNORED`. The same objects
with the imperial routine of that date (`3b15e43a:base/evldx.f90`) returned 0
and true. The BC and ON decks stop earlier, at keyword input (bug 5), so no
test shows this.

**Suggested fix:** remove `MXCNST` and test against 9000 (or `MAXPRM`+1000)
in every variant.

---

## 21. Metric EVLDX applies the wrong factor to several values

**Variants:** BC, ON
**Location:** unit-code tables (`IUTST1`–`IUTST4`) and in-place conversions
in `base/evldx.f90` (before the M01a merge, `metric/base/evldx.f90`)
**Status:** confirmed by source tracing against `base/algkey.f90` and the
routines that set each value; all present in `c2105365`. Unfixed. Which unit
each value should have is for the maintainer to settle.

BC and ON convert results by position in fixed tables. Some positions hold
a different kind of value from the one the factor suits:

| Value | Factor applied | Stored value |
|---|---|---|
| `LAT` (137) | ÷ `ACRtoHA` | Latitude, degrees |
| `BHTWTBA` (119) | ÷ `ACRtoHA` | Height, feet |
| `BSCUFT` (120) | ÷ `ACRtoHA` | Cubic feet per acre |
| `AMERBIO` (224) | ÷ `ACRtoHA` | Tons per acre; no other biomass or carbon variable is converted |
| `CROWNIDX` (422) | × `FTtoM` | Wind speed, miles per hour; the fire report uses `MItoKM` |
| `DBHDIST(a,7)` | × `INtoCM` | Total of the attribute (trees or volume per acre), not a diameter |
| `SNAGS(1,…)`, `SNAGS(2,…)` | × `FT3pACRtoM3pHA` | Trees and basal area per acre |
| `ACORNS(1,…)` | × `LBtoKG` ÷ `ACRtoHA` | Number per acre, not pounds |

Paired variables are also treated differently before and after thinning:
`BCCF`, `BSDIMAX`, `BSDI`, `BRDEN` and `BRDEN2` are divided by `ACRtoHA`,
while `ACCF`, `DCCF`, `ASDIMAX`, `ASDI`, `ARDEN` and `ARDEN2` are not.

Variables added after the tables were written are returned in imperial
units: `MAI` (145), `BDBHWTBA` (147), `BSDI2` (150), `BGMD` (159),
`ADBHWTBA` (216), `ASDI2` (218), `AHTWTBA` (219), `ASCUFT` (220), `RSCUFT`
(221), `AGMD` (238), `CRBASEHT` (426) and the stratum heights (440–443).

**Suggested fix:** correct the unit codes in the tables, extend the tables to
the newer variables, give `DBHDIST`, `SNAGS` and `ACORNS` a factor per
attribute, and decide the unit of each density index once for both the
before- and after-thinning forms.

---

## 22. Introduced by the free-form conversion: split FORMAT text prints a stray apostrophe in BC and ON

**Variants:** BC, ON
**Status:** found 15 November 2026 during the M05 and M07 audits and
reproduced with a two-line test program. Not in `c2105365`; introduced when
the files were converted to free form. M05a and M07a remove the first two
files below, restoring six FORMATs to their original F77 text. Direct
DBSIN/FMDOUT comparisons confirm the exercised messages. The other thirteen
FORMATs remain unfixed. The full regression decks cannot reach these
messages because BC/ON stop at keyword input (bug 5).

At the audit baseline, nineteen FORMAT statements split a text item
across two lines with no comma between the halves:

```fortran
 1120     FORMAT ('********  ERROR: ',A,' CAN NOT PREFORM'
     &      ' SUBSTITUTIONS IN SQL COMMAND: ',A)
```

gfortran reads that as two text items with a missing comma (a legacy
extension) and prints them one after the other. The conversion wrote the
break as `'&` followed by `&'`. In free form that joins the two quotes into
`''`, which is one apostrophe inside a single text item. The original prints
`CAN NOT PREFORM SUBSTITUTIONS`; the converted file prints
`CAN NOT PREFORM' SUBSTITUTIONS`.

| File | Formats |
|---|---|
| `metric/dbsqlite/dbsin.f90` (retired M05a; corrected) | 1120, 2710, 2810 |
| `metric/fire/base/fmdout.f90` (retired M07a; corrected) | 702, 802, 902 |
| `metric/fire/base/fmin.f90` | 5610 |
| `fire/base/fmsrecord_mod.f90` (moved by M09a; retained) | 201 (formerly metric FMSOUT 200) |
| `metric/fire/vbase/fmfout.f90` | 318 |
| `metric/fire/vbase/fmpofl.f90` | 132 |
| `metric/newmist/misprt.f90` | 905 |
| `canada/bc/becset.f90` | 31, 34, 41 (two breaks), 61, 71 |
| `canada/bc/sitset.f90` | 57 |
| `canada/on/morts.f90` | 351 |
| `clim/base/clin.f90` | 9106, the BC/ON branch only |

The audit searched every Fortran file for a line ending in `'&` followed by
a line starting with `&'` and found these sites and no others. Wherever the
imperial twin has the same message, it has the comma.

**Suggested fix:** end the first line with `',&`. Text items separated by a
comma print exactly as the original did. Retiring a metric copy in favour of
the imperial file (M05a, M07a) removes its sites as well.

---

## 23. Metric STATS converts the percent sampling error and leaves the error in units unconverted

**Variants:** BC, ON
**Location:** `base/stats.f90`, the metric trees, cubic-volume and basal-area
blocks
**Status:** found 15 November 2026 during the M04 audit by reading the
source; present in `c2105365:metric/base/stats.f`. Unfixed; M04a retains it
in the shared routine's metric branches.

The last two columns of the sample-point table are the sampling error in
percent (`SEP`) and in units (`SEU`). The metric copy multiplies or divides
`SEP` by the unit factor (`ACRtoHA`, `FT3pACRtoM3pHA`, `FT2pACRtoM2pHA`) and
writes `SEU` as computed, per acre. A percentage has no unit to convert, so
both columns are wrong, in the printed table and in the `FVS_Stats_Stand`
rows.

Two smaller points in the same routine:

- The species table prints the `BFV` total as net merchantable cubic metres,
  but the value sent to the database column `BoardM` is multiplied by zero.
  For ON, where `BFV` holds merchantable volume, the report and the database
  disagree.
- STATS passes `LABELS(I)` and `IDIST(1,I)` to `DBSSTATS` for
  every species row, with `I` up to the number of species present. The arrays
  have six entries after M04a (the former metric copy had four). `DBSSTATS`
  does not read them for species rows, so nothing visible follows, but a bounds-checked
  build would stop on a stand with more than six species.

**Suggested fix:** apply the factor to `SEU` and not to `SEP`; decide whether
`BoardM` should carry the merchantable volume the report shows; pass a fixed
element for the unused arguments.

---

## 24. BC and ON do not produce the all-fuels report that FUELOUT requests

**Variants:** BC, ON
**Location:** the metric branch in `fire/base/fmdout.f90` with
`fire/base/fmfuelreq_mod.f90`, called by option 15 in both FMIN readers.
**Status:** found 15 November 2026 during the M07 audit by reading the
source; present in `c2105365`. Unfixed. M07a preserves this rule in the
shared routine; direct drivers exercise it, while Canadian integrated
simulation acceptance remains limited by bug 5. M08d/M08e subsequently
extract the DWDCVOUT/FUELOUT requests without changing these contracts.

The metric branch of FMDOUT decides whether to produce each report from the
begin variable alone: the report is skipped when it is greater than zero.

```fortran
LPRINT = .true.
if (IFLALB .gt.0) LPRINT = .false.
```

The metric FMIN sets the down-wood variables to −1 for `DWDVLOUT` and
`DWDCVOUT`, which this test accepts. For `FUELOUT` it sets `IFLALB = IY(1)`,
the inventory year, as the imperial FMIN does for a begin-and-end-year test.
With any inventory year above zero the test fails in every cycle, so neither
the printed all-fuels report nor the `DBSFUELS` rows are written, although
the keyword echo says `THE ALL FUELS REPORT WILL BE PRINTED`. The report
appears only if `FUELOUT` is read while the inventory year is still zero.

The imperial FMDOUT tests `IYR` against `IFLALB` and `IFLALE`, and its FMIN
sets all three reports' variables to the inventory year and that year plus
999.

**Suggested fix:** use the imperial year-window test in FMDOUT and the
imperial settings in the metric FMIN for all three reports. M07a deliberately
retains the old rule. M08 confirms DWDVLOUT's paired flag contract; it is an
audit, not the model correction. Correct the producer and consumer together
in separate model-fix work. Changing either alone turns the down-wood reports
off or leaves the all-fuels report off.

---

## 25. The metric burn-conditions report rejects the begin year that BURNREPT sets

**Variants:** BC, ON.
**Location:** the metric branch in `fire/vbase/fmburnreport_mod.f90`, called
by `metric/fire/vbase/fmfout.f90`, with option 4 in `metric/fire/base/fmin.f90`.
**Status:** confirmed during M10 by source tracing and block checks in
current code and `c2105365:metric/fire/vbase/fmfout.f` /
`c2105365:metric/fire/base/fmin.f`. Unfixed; M10a preserves it in the helper.

BURNREPT assigns `IFMBRB = IY(1)` and `IFMBRE = IY(1)+999`. The metric
FMFOUT section executes only when `IFMBRB < 0` and `IFIRE >= 0`, ignoring
the end year. An inventory year of zero or a positive calendar year
therefore suppresses both DBSFMBURN and the burn-conditions text, despite
the keyword echo promising the report. The imperial section accepts the
current year within the requested window. This is analogous to bug 24,
but affects a different report and uses a strict negative-flag test.

Thirty-six block cases per copy cover begin flags -1/0/2020, IFIRE -1/0/1,
FMD zero/nonzero and database-only/text output. Each copy matches its F77
section; the metric cases with begin 0 or 2020 produce no report. These
checks supply report inputs directly and do not demonstrate full-engine
reachability beyond the existing keyword-input blocker.

**Suggested fix:** adopt a consistent begin/end-year contract in the input
handler and consumer, with explicit zero-year and out-of-window cases.
Keep that result-changing correction separate from M10a's shared section.

---

## 26. Metric potential-fire database conditions use the moderate flag for both rows

**Variants:** BC, ON.
**Location:** `fire/vbase/fmpotcond_mod.f90`, called by the metric FMPOFL.
**Status:** confirmed during M11 in current code and
`c2105365:metric/fire/vbase/fmpofl.f`. Unfixed; full simulation impact has
not been measured.

The preceding FMOIS loop leaves IDPL equal to 2. Before M11a, the subsequent
IDPM=1,2 database loop tested `PRESVL(IDPL,1)` but copied moisture from
`PRESVL(IDPM,2:8)`. The shared helper retains this by setting IPRES to the
incoming IDPL in the metric branch. Both rows are controlled by the moderate
override flag. A severe-only override is ignored; a moderate-only override
also makes the severe row use its inactive PRESVL values. The fire
calculation and text condition rows test their respective scenario flags,
so the database can disagree with both. The imperial database loop correctly
tests IDPM.

Block drivers with the exact original/current loops match all eight records
from the four override-flag combinations. They set the documented incoming
IDPL=2 and supply distinguishable FMMOIS defaults and PRESVL rows, isolating
the flag-selection error without running the fire model.

**Suggested fix:** test `PRESVL(IDPM,1)` in this loop and verify both
asymmetric override cases against the text and calculation inputs. Preserve
the old selection during M11a; apply this correction separately.

---

## 27. BC mistletoe diameter classes are non-monotonic

**Variant:** BC; ON selects the mistletoe stub and is unaffected.
**Location:** the metric class-index branch in `mistoe/misclass_mod.f90`,
called by `metric/newmist/misprt.f90` after M13a.
**Status:** reproduced during M13 in current code and
`c2105365:metric/newmist/misprt.f`. Unfixed.

The routine converts DBH to integer centimetres and computes:

```fortran
IVAL = IFIX(DBH(ITREE)*INtoCM)
IDBH = IVAL / 5 + MOD(IVAL,5)
IDBH = MAX(1,MIN(20,IDBH))
```

The remainder adds zero through four classes instead of locating a tree in
one five-centimetre interval. Diameters 3.25, 4.25 and 5.25 cm go to classes
3, 4 and 1, respectively; 10.25 cm goes to class 2 and 45.25 cm to class 9.
These disagree with the printed five-centimetre headings. Counts, infection,
mortality and DMR/DMI contributions are assigned to the wrong columns in
the size summary and species/size detail, including database size summaries.
The imperial remainder modulo 2 does not generalize to modulo 5.

One hundred non-boundary diameters give identical original/current indices
and reproduce the decreasing classes. The tests isolate classification;
no integrated BC stand-output footprint has been measured. The subsequent
1–20 clamp and summary folding of classes 11–20 do not correct the problem.

**Suggested fix:** define the exact interval endpoints and use monotonic
five-centimetre binning, testing every boundary and both summary/detail
tables. Keep the existing formula during M13a's structural extraction.

---

## 28. BC biocontrol keywords have diagnostic and dispatch defects

**Variant:** BC.
**Location:** DMBCI/DMBCA handlers in `metric/newmist/misin.f90`, with
`canada/newmist/dmcycl.f90` as the application consumer.
**Status:** found during M12. All three defects are present in
`c2105365:metric/newmist/misin.f`; DMCYCL's original consumer also selects
activity 2010. Unfixed. M12a repairs BC's separate field-width defect (bug 5)
and preserves these biocontrol handlers.

- DMBCI's unconditional WRITE supplies keyword, agent index, species text
  and REAL mortality values. FORMAT 2902 also expects an integer species
  code between the text and the mortality values, but ISPL is missing from
  the argument list. Executing the extracted original/current WRITE and
  FORMAT with defined agent data makes GNU Fortran stop with an
  INTEGER/REAL type error. Valid agent definition therefore cannot finish
  that echo, even when keyword-buffer overflow is bypassed.
- DMBCA sets `IKD = 2004` and passes it to OPNEW/OPNEWC. DMCYCL searches
  for activity 2010; 2004 belongs to MISTHMOD, whose MISHGF consumer expects
  a species plus six growth modifiers rather than three application values.
  This producer/consumer mismatch is source-confirmed; a full run's wrong
  growth or application effect has not been measured.
- DMBCA tests local IPRMPT, which is declared but never assigned anywhere in
  this MISIN. The PARMS branch therefore depends on an undefined value;
  moreover the reader's earlier nonzero-KODE handling rejects negative
  return codes before this handler. Source tracing confirms the missing
  assignment/dispatch, not a measured distribution of runtime outcomes.

**Suggested fix:** isolate the biocontrol repairs from consolidation: add
the missing diagnostic argument, align the application activity with 2010,
and define PARMS/error dispatch before using IPRMPT. Verify with defined
twelve-field input, real scheduling consumers and each supported agent.

---

## 29. ESUCKR divides by zero when a sprout report has no generated sprouts

**Variants:** BC/ON (`metric/strp/esuckr.f90`) and AK/CI/EM/IE/KT
(`estb/esuckr.f90`). The 17 variants selecting `strp/esuckr.f90` have a
zero-total guard.
**Status:** found during M14; reproduced in the report-tail statements of
all three original/current copies. Unfixed; a valid full-engine case has
not been run.

ESUCKR can enter with removed stump records, then generate no sprouts
because removal density is too small or a multiplier/equation suppresses
them. TPATOT remains zero. With printing enabled, the metric and ESTB
tails unconditionally execute `WTAVEHT = WTAVEHT/TPATOT` after initializing
WTAVEHT to zero. This gives an invalid 0/0 result for the total DBSSPRT row,
or a floating-point trap. STRP instead sets the total mean to zero when
TPATOT is zero. Temporary original/current tail drivers with defined zero
totals trap in metric/ESTB and return zero in STRP.

The imperial ESTB/STRP tails also pass `TPASUM(I)` and `HTAVE(I)` after the
species loop, when I is NSPSPE+1. Those actual array elements are out of
bounds. DBSSPRT ignores these two values for the total row (TOT=2), so no
result impact is claimed; bounds checking can still reject the call. Metric
passes element 1 instead. This additional tail issue is source-confirmed.

**Suggested fix:** apply a defined zero-total mean and pass valid elements
for unused total-row arguments, with a no-sprouts report/database case.
Keep the repair separate from M14a's height/DBH extraction.

---

## 30. Metric DISPLY passes a 20-row stride for the 22-row summary array

**Variants:** BC, ON.
**Location:** `metric/vbase/disply.f90` calls SUMOUT with I20=20;
`metric/vbase/sumout.f90` and its shared text writer
`vbase/sumoutrec_mod.f90` declare `IOSUM(I20,LENG)`, while selected
`common/OUTCOM.f90` declares `IOSUM(22,MAXCY1)`.
**Status:** confirmed during M18 by source tracing and direct
original/current routine calls. Present in `c2105365` (including its
`common/OUTCOM.F77`). Unfixed; full-engine impact was not measured.

DISPLY stores cycles in columns of a 22-row array. SUMOUT instead advances
20 elements between columns. The first cycle is read correctly, but every
later cycle starts two elements earlier per preceding column. In cycle 2,
the printed/database year and age come from the preceding column's rows
21 and 22; its tree count comes from the actual second column's year.
The per-cycle SUMTAB arrays still use their proper cycle index, mixing
these shifted values with the correctly indexed basal-area/QMD data.

Direct calls to the unmodified original F77 and current metric SUMOUT,
using a defined two-column input `IOSUM(i,j)=100*j+i` and the caller's
I20=20, both pass year/age 101/102 for the first DBSSUMRY call and 121/122
for the second, where the actual second column contains 201/202. The
database call is captured to isolate the stride issue from bug 31. This
demonstrates argument indexing, not successful Canadian simulation input.

**Suggested fix:** pass the true leading dimension, 22, from metric DISPLY,
while retaining the metric record's existing 20-field selection and units.
Test two or more cycles, final-cycle rows and text/database agreement in a
separate result-changing repair; do not combine it with M18a's writer
extraction.

---

## 31. Ontario's ordinary Summary table cannot accept its own INSERT

**Variants:** ON, when ISUMARY=1 creates a fresh summary table.
**Location:** the first routine in `metric/vdbsqlite/dbssumry.f90`.
**Status:** confirmed during M21 with actual original/current DBSSUMRY
routines linked to SQLite. Present in `c2105365`. Unfixed; DBSSUMRY2's
separate ISUMARY=2 record is outside this audit.

ON selects `FVS_Summary_East_Metric`. Its CREATE and INSERT disagree:

| Volume group | CREATE columns | INSERT columns |
|---|---|---|
| Standing | TCuM, MCuM, CCuM | MCuM, SCuM, NCuM |
| Removed | RTCuM, RMCuM, RCCuM | RMCuM, RSCuM, RNCuM |

On a fresh database, preparation fails with `table
FVS_Summary_East_Metric has no column named SCuM`. DBSSUMRY sets ISUMARY=0
and returns without a row, disabling later summary writes too. Temporary
drivers using defined, already converted arguments reproduce the same
schema, diagnostic and empty table in original F77 and current code. The
corresponding BC ordinary Summary record and imperial IE/CS records each
write their row. A pre-existing table with INSERT-compatible columns can
avoid this particular failure; the defect is in the routine's fresh schema.

**Suggested fix:** choose and document the ON column names/volume meanings,
align CREATE with INSERT and handle existing database schemas explicitly.
Verify all six volume fields and output-switch behavior. Keep this schema
change separate from M21a's common binding-prefix extraction.

---

## 32. Metric stand input echoes positive basal-area factors with a density factor

**Variants:** BC, ON, for positive BASAL_AREA_FACTOR input with keyword echo.
**Location:** sampling-field conversion and echo in
`vdbsqlite/dbsstanddesign_mod.f90`, called by metric DBSSTANDIN after M20a.
**Status:** confirmed during M20 using the original/current blocks with
real SQLite input, including NULL controls. Present in `c2105365`. Unfixed;
this is an echo defect, not a demonstrated change in stored sampling state.

The reader correctly converts nonnegative BASAL_AREA_FACTOR from m2/ha to
ft2/acre with M2pHAtoFT2pACR. Negative values represent inverse plot area
and correctly use division by HAtoACR instead. After assignment to BAF,
the echo unconditionally prints `BAF/ACRtoHA`, which reverses only the
negative-value density conversion.

An input of 4.5 stores BAF about 19.602348, then prints 48.4 instead of
4.5. The original and current blocks produce exactly the same state bits
and echo. Negative, zero and SQL NULL cases were checked separately;
the presence flags prevent NULL input from replacing the existing state.
The metric INITRE DESIGN handler already chooses the echo conversion by
BAF sign, using FT2pACRtoM2pHA for the nonnegative case.

**Suggested fix:** use the same sign-dependent BAF echo conversion as
DESIGN, with positive/negative/zero/NULL and echo-off tests. Keep the
diagnostic change separate from M20a's sampling-field extraction.

---

## 33. Coastal DGCONS initializes attenuation by equation instead of species

**Variants:** OP, PN and WC.
**Location:** `ATTEN(KSP) = OBSERV(JSPC,IB)` in `vbase/dgf.f90:873`
(DGFPAC; KSP is the equation index JSPC for OP/PN/WC). Originally
`op/dgf.f90:523`, `pn/dgf.f90:516` and `wc/dgf.f90:498`.
**Status:** reproduced during the complete L04 audit on 15 November 2026
with actual original F77 and current DGCONS routines. Present in `c2105365`;
deliberately unfixed. No integrated growth-impact measurement is claimed.

DGCONS loops over native species ISPC but stores ATTEN at the mapped growth
equation index JSPC. OP/PN have 39 native species and a 20-row equation bank
with row 17 unused; WC has 39 species and a 19-row bank. DGDRIV subsequently
reads `ATTEN(ISPC)` for the species' calibration prior, rather than looking
it up through MAPSPC. Thus multiple species overwrite the same equation
slot, some populated slots contain another species' observation count,
and many native slots retain their incoming value.

Direct bounds-checked calls initialize all ATTEN values to -999, provide
defined site/forest/aspect/correction state, then call each native DGCONS.
Original and current results agree exactly:

| Variant | Native ATTEN slots still -999 | Count |
|---|---|---:|
| OP, PN | 17,21–39 | 20 each |
| WC | 20–39 | 20 |

The sentinel demonstrates missing writes; it is not a claim that a normal
model run starts with -999. Existing initialization or earlier state can
mask the omission. DGDSQ is also equation-indexed in these routines, but
DGF reads it by equation index, so that assignment is a different contract.

**Suggested fix:** assign `ATTEN(ISPC)=OBSERV(JSPC)`, leaving native
observation types and the equation-indexed DGDSQ behavior intact. Verify
all 39 native slots and calibration results in a separate model-changing
repair; do not include this correction in DGF consolidation.

---

## 34. Ontario DGF reuses the last predicted tree's bark ratio

**Variant:** ON.
**Location:** bark calculation at `vbase/dgf.f90:9595` (DGFON; was
`canada/on/dgf.f90:360`), followed by the separate final loop's use at
line 9797.
**Status:** reproduced during L04 with actual original/current DGF and
Ontario BRATIO. Present in `c2105365`; deliberately unfixed. The direct
routine comparison does not establish Canadian integrated simulation parity.

The first tree loop calculates each tree's grown diameter, calls BRATIO
and stores its bark-adjusted diameter increment in TEMD. After that loop,
the final loop calculates `DDS=DIAGR*(2.0*DIAM(I)*BARK+DIAGR)` using the
single scalar BARK left by the last successfully predicted tree. It neither
retains each tree's ratio nor recalculates it. A later species can therefore
change an earlier tree's stored growth even when that earlier tree's own
predictors and diameter increment are unchanged.

A defined two-tree direct case uses native species 1 (jack pine) and 4
(red pine), each D=10 inches and H=60 feet, with fixed stand BA/QMD/site/
competition and neutral correction factors. The disabled hybrid branch
remains disabled. Original F77 and current DGF give identical results:

| First tree's context | WK2 for species 1 |
|---|---:|
| Predicted alone | 2.5945756 |
| Followed by species 4 | 2.5589375 |

Actual BRATIO returns 0.9609000 for species 1 and 0.9260800 for species 4.
The changed final-loop multiplier accounts for the earlier tree's different
result. This controlled state demonstrates the data-flow defect, not its
frequency or total effect in field inventories.

**Suggested fix:** retain each tree's bark ratio from the original grown-
diameter call and use that ratio in the final conversion. Recomputing it
at the original diameter is a different change because some Ontario bark
equations depend on diameter. Test mixed species, diameter-dependent bark,
corrections, skipped trees and the inactive hybrid contract separately from
structural DGF sharing.

---

## 35. Western Sierra DGF's upper growth bound does not reach WK2

**Variant:** WS, species 1–3,5–8,11,13,18,22,24,28–33,40,42,43,
when the calculated DDS exceeds DDSMAX and ICR is positive.
**Location:** `vbase/dgf.f90:7290–7313` (DGFWS; was `ws/dgf.f90:732–755`).
**Status:** reproduced during L04 with actual original/current DGF and
native WS bark. Present in `c2105365`; deliberately unfixed. No field-case
prevalence or integrated projection effect was measured.

DGF applies its lower clamp, stores DDS in WK2 at line 7290, then calculates
the species/crown/DBH upper bound and reduces local DDS. WK2 is not written
again. Debug mode can report that the bound is being invoked while callers
still receive the value from before that bound.

A direct bounds-checked species-1 case uses D=10, ICR=50, defined stand/
point state and COR(1)=1 to reach the upper-bound path. Density outputs are
controlled, and the actual DGF/BRATIO routines are unchanged. Original and
current both store WK2=5.2058067, above the calculated bound
`-3.91+1.42*ALOG(50*10)=4.9147434`. This is a controlled routine case, not
a claim that its correction factor represents a particular production run.

**Suggested fix:** store the final bounded value after the upper-limit
logic, while preserving the existing skipped-diameter behavior. Verify
every listed species, the ICR<=0 bypass, both sides of each bound and the
interaction with the lower clamp in a separate result-changing repair.
Moving that store during consolidation would silently fix the model bug.

## 36. DGF diagnostics print crown ratio, BAL and AK's PFCOMP2 that some branches never set

**Variants:** IE (juniper 15,16), SO (juniper 11, red alder 22), TT
(juniper 4,11,12), OP/PN/WC red alder (CR, BAL and PCCF(IPCCF) in the
8000F line), and AK (PFCOMP2 in the PF MODIFIER line for species outside
the permafrost group 4–7,13,16–23); debug output only.
**Location:** the end-of-tree debug writes at `vbase/dgf.f90:1678` (TT),
`vbase/dgf.f90:2715` (IE), `vbase/dgf.f90:768` (coastal 8000F) and
`vbase/dgf.f90:5895` (SO), and the PF MODIFIER write at
`vbase/dgf.f90:7868` (AK).
**Status:** found by the L04 old/new routine comparisons on 15 November
2026 (AK during L04o). Present in `c2105365`; deliberately unfixed.

These branches compute DDS without assigning the CR/BAL locals that the
shared diagnostic prints. AK assigns PFCOMP2 only for permafrost species
but prints it for every species. If such a tree is the first one DGF
processes in a call, the values are whatever the stack holds; later trees
print the previous tree's values. All model state and WK2 are unaffected, and every
compared common block matched bit for bit. Only the DEBUG text differs
between builds whose stack layout differs (for example `CR=-0.0000`,
`CR=*******` or `CR=    NaN` where the original printed `CR= 0.0000`;
AK prints `PFCOMP2 NaN` where the original printed `0.00000000`).

**Suggested fix:** assign CR, BAL and PFCOMP2 (or print only defined
values) in these branches, in a separate diagnostic-only change. Until then,
comparisons of DGF debug output must expect these fields to vary.

## 37. MORTS reads unset FIXMORT flags and diagnostic work values

**Variants:** AK for the control-flow defect; the shared MORTS consumers
for the pre-loop FIXMORT diagnostic; OP/PN/WC for the Pacific diagnostics.
**Location:** `ak/morts.f90`, the FIXMORT block and following reallocation;
`vbase/morts.f90`, FIXMRT and MORPAC.
**Status:** reproduced during L01–L03 on 15 November 2026. The same reads
are present in pinned original F77 (`c2105365`). Deliberately unfixed.

AK assigns KBIG and KPOINT only inside the scheduled FIXMORT activity loop,
then tests them outside that loop. With no scheduled activity they, and the
other activity locals used by the selected branch, can be unset. A scratch
extraction with identical executable statements entered the reallocation
branch on later cycles of the ordinary AK deck, printed garbage flag values,
and sorted IWORK1/WORK3 where the immediate baseline did not. Summaries and
database rows matched in these cases; wider model impact was not measured.
AK therefore retains its original selected `ak/morts.f90`; its extraction
is deferred until this separate model defect has an authorized resolution.

FIXMORT also prints ITODO before starting the loop that defines it. In
OP/PN/WC, the `IN MORTS EQ I,XCHECK,...` diagnostic prints XCHECK even when
the establishment branch that assigns it was skipped. The final per-tree
diagnostic prints RELDBH1(I) for records whose calculation was skipped
because of zero probability, or OP's already-computed ORGANON mortality.
Those entries can be unset or stale. Raw old/shared debug comparisons
reproduced differences in precisely these fields while model files matched.

For focused comparison only, temporary copies initialize ITODO, and Pacific
XCHECK/RELDBH1, to the same sentinel on both sides. This establishes a
separate diagnostic comparison contract; repository code and the whole-suite
comparator are unchanged. It does not legitimize masking AK's control-flow
differences or changing any defined diagnostic value.

**Suggested fix:** separately define AK's no-activity reallocation behavior
and initialize its flags at the appropriate point. Print the activity count
or a defined counter before FIXMORT, and restrict Pacific diagnostics to
values assigned on the executed path. Test these changes separately from
consolidation, including repeated cycles and scheduled/unscheduled activities.

## 38. EM REGENT calls a different DUBSCR argument contract

**Variant:** EM.
**Location:** REGEM in `vbase/regent.f90` (formerly `em/regent.f90`), the
small-tree transition call; `em/dubscr.f90`.
**Status:** source-confirmed during L03. Both contracts are present in
pinned original F77. Deliberately preserved; prevalence and total projection
impact have not been measured.

REGENT calls `DUBSCR(ISPC,DNEW,HK,BA,CR,TPCCF,AVH,RMAI)`, the eight-argument
contract used by IE/CI. EM's selected routine instead declares
`DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)`. Thus the callee's crown-ratio output aliases
the caller's stand BA, while the intended crown-ratio argument occupies the
unused TPCT slot; the final two actual arguments are extra. DUBSCR writes its
CR argument, so this call can overwrite stand basal area. Consolidation keeps
the call unchanged and leaves this one legacy interface implicit instead of
inventing a corrected contract.

**Suggested fix:** separately determine the intended EM transition equation
and argument mapping, then add a matching explicit interface and targeted
before/after model tests. Merely making the signatures compile together
would not establish the intended scientific behavior.

## 39. REGENT calibration prints unset tree and table indices

**Variants:** OP, PN, WC and UT.
**Location:** REGPAC in `vbase/regent.f90`, the calibration section's
`1ST-K` and `2ND-K` diagnostics (formerly `op/regent.f90` and
`wc/regent.f90`), and REGUT's `SI,RELSI,X,ISPC,J` diagnostic (formerly
`ut/regent.f90`).
**Status:** reproduced in immediate-baseline/shared real-engine comparisons
with REGENT debugging enabled. Present in pinned original F77; unfixed.

The initial LSTART calibration path selects the tree with I but prints K
after its two SMHGDG calls. K is assigned in the separate growth path,
which has not yet executed on the first call. The index printed in these
two diagnostics consequently varies with stack layout. The paired probes
matched model outputs and common-block state while printing different K
values. Temporary comparison copies initialize only this diagnostic index
on both sides; repository routines and the suite comparator are unchanged.

UT's initial calibration diagnostic similarly prints J without assigning it
on that path. The paired runs printed 1 and 0 with otherwise matching state
and output. J is defined by species-table searches on the growth path; the
calibration diagnostic cannot rely on those earlier calls. Temporary copies
initialize this diagnostic local consistently too.

**Suggested fix:** print the calibration loop's I, or explicitly assign the
intended index on this path, in a separate diagnostic change. Preserve the
existing SMHGDG calls, arithmetic, random draws and period conversion.

## 40. CS/SN fuel-model selection reads unassigned moisture weights

**Variants:** CS, SN

**Location:** `fire/vbase/fmcfmd_cs.f90:119-136`, shared by CS/SN under
F13. Former locations: `fire/cs/fmcfmd.f90:119-136` and
`fire/sn/fmcfmd.f90:119-136` at `6943c310`.

**Status:** reproduced during a fresh source audit on 15 November 2026 at
`1cf093f3`. Present in original F77 `c2105365`; deliberately unfixed.

For hardwood and mixed forest types (`IFFEFT=1:3`) with `SMALL<=6`, the dry
branch assigns only `MOISWT(9)` and the wet branch assigns only `MOISWT(8)`.
Both elements are then read to fill `EQWT(8:9)`. The local `MOISWT` array has
no initialization, so the other weight is undefined or left over from an
earlier call. `EQWT`'s initialization does not initialize `MOISWT`.

**Confirmation:** temporary drivers called the actual CS/SN FMCFMD routines
and their actual forest-type classifiers in both original F77 and current
code. Only DBCHK and the downstream FMDYN were stubbed; the latter captured
its incoming weights. With `IFORTP=504`, `SMALL=2`, and gfortran
`-O0 -finit-real=snan -fcheck=all`, moisture 0.10 produced a non-finite
weight 8, and moisture 0.30 produced a non-finite weight 9, in all four
original/current variant runs. NaN initialization exposes the undefined
read; ordinary builds can instead supply arbitrary or stale values. This
checks the weights passed to FMDYN, not whole-model fire-output impact.

**Suggested fix:** initialize both moisture weights on every applicable
call before selecting the dry, wet or interpolated case. Verify the fuel
weights and resulting fire behavior separately from structural cleanup.

## 41. CS/SN redcedar fuel selection divides unweighted heights by tree density

**Variants:** CS, SN

**Location:** `fire/vbase/fmcfmd_cs.f90:154-184`, shared by CS/SN under
F13. Former locations: `fire/cs/fmcfmd.f90:154-175` and
`fire/sn/fmcfmd.f90:154-175` at `6943c310`.

**Status:** reproduced during the fresh audit on 15 November 2026 at
`1cf093f3`. Present in original F77 `c2105365`; deliberately unfixed.

The eastern-redcedar branch adds each record's height to `RCHT`, adds its
trees per acre (`FMPROB`) to `RCTPA`, and divides the first sum by the
second. The numerator omits the density weight. Consequently, the height
used at the 4.5/7.5-foot fuel thresholds depends on density and on how an
otherwise identical stand is divided into tree records.

**Confirmation:** the actual original/current CS/SN routines and forest-type
classifiers were exercised with `IFORTP=181`, species 2, and 10-foot trees;
a capturing FMDYN stub reported the input weights. One record representing
1 tree/acre selected weight 4=1 and weight 6=0; at 10 trees/acre it selected
4=0 and 6=1. More directly, one record representing 2 trees/acre gave
weights `(0.16667,0.83333)`, while two identical records representing
1 tree/acre each gave `(1,0)`. All four original/current variant runs
agreed. These are direct fuel-candidate checks, not an integrated measure
of fire-output impact.

**Suggested fix:** confirm the intended average-height definition, then
accumulate `HT(I)*FMPROB(I)` before dividing by total density. Review the
zero-density case at the same time: CS guards the division, whereas SN
divides unconditionally. Keep this model change separate from merging the
two routine definitions.

## 42. BC snag volume calls the ten-argument CFTOPK with nine arguments

**Variants:** BC

**Location:** `canada/fire/bc/fmsvol.f90:124-126`, calling
`ie/cftopk.f90:5` selected by `bin/FVSbc_sourceList.txt`.

**Status:** confirmed by source inspection and compiler diagnostics during
the fresh audit on 15 November 2026 at `1cf093f3`. Present in original F77
`c2105365:canada/fire/bc/fmsvol.f:118`. Repaired by R02 on 16 November 2026.

BC calls `CFTOPK(ISPC,D,H,VN,VM,VMAX,LCONE,BARK,IHT)`, but the selected
routine expects `(ISPC,D,H,TCF,MCF,SCF,VMAX,LCONE,BARK,ITHT)`. The missing
sawtimber-cubic argument shifts every subsequent actual argument: VMAX is
used as SCF, a logical is passed as VMAX, a real as LCONE, an integer as
BARK, and ITHT has no actual argument. The call is therefore undefined.
It is reached when `CTKFLG` and `LTKIL` are true; BC CFVOL does not change
CTKFLG after FMSVOL initializes it from LTKIL.

**Confirmation:** temporary compilations put the actual caller and callee
in one translation unit with their selected BC includes. For both pinned
original F77 and current sources, gfortran diagnosed the LOGICAL-to-REAL,
REAL-to-LOGICAL and INTEGER-to-REAL argument mismatches. No repository
source or compiler flags were changed.

**Repair:** FMSVOL now passes a separate `SCF=0.0` and the complete ten-argument
list, checked by an explicit interface. BC CFVOL/MIN returns total and
merchantable cubic volume, without a separate sawtimber-cubic product. The
existing total/merchantable correction, Kozak calculation and truncation
sequence are retained; no new sawtimber standard or equation is introduced.

Before repair, StandStructure traps with arguments shifted into VMAX, LCONE,
BARK and ITHT. After repair its projections finish with the existing expression
warnings (`STOP 20`); YSM-SkyRanch remains identical to its immediate baseline.
The other 23 standard-suite variants match exactly. Additional BC snag/down-wood
decks finish, and direct entry/height/merchantability cases pass with bounds
checking and signaling-NaN initialization. The pristine-original suite still
reports only the BC/ON differences. The independent live-tree VOLS call sites
remain under section 54.

## 43. ON planted white pine is omitted from pine crown cover in fuel selection

**Variants:** ON

**Location:** `fire/vbase/fmcfmd_ls.f90:345-379`. Former location:
`canada/fire/on/fmcfmd.f90:345-379` at `6943c310`.

**Status:** reproduced during F12 consolidation on 15 November 2026.
Present in original F77 `c2105365`; deliberately unfixed.

The red/white-pine cover group includes planted white pine (species 70)
in its basal area, but its `PINECRA` calculation counts only species 3–5.
Consequently, planted white pine contributes no pine crown cover. In a
mixed stand, the `PINECOV/PERCOV < 0.5` test can select candidate fuel model
8 where the equivalent native white-pine record (species 5) selects
candidate model 9.
ON's species table identifies 70 as planted white pine and 5 as white pine.

**Confirmation:** temporary drivers compared actual original F77,
baseline `6943c310` and shared FMCFMD. Only DBCHK and the downstream FMDYN
were stubbed; FMDYN captured its incoming candidate weights. Two 12-inch
records represented 100 white-pine trees/acre and 25 red-maple trees/acre,
both with 20-foot crown widths. With stand cover 59.4045%, average height
40 feet, wind 6, habitat type 1, no dead surface fuel and no activity fuel,
species 5 produced `(EQWT(8),EQWT(9))=(0,1)` while species 70 produced
`(1,0)`. All three implementations agreed. This confirms a difference in
fuel-candidate weights; integrated fire-output impact was not measured.

**Suggested fix:** review the plantation-species treatment in the crown
cover and understory tests, then include the appropriate native aliases
consistently. Validate resulting fuel selection as a separate model change.

## 44. ON CRNMULT processing uses an unset species index

**Variants:** ON

**Location:** `canada/on/crown.f90:98-130`.

**Status:** reproduced during F15–F20 verification on 16 November 2026.
Present in original F77 `c2105365` (`canada/on/crown.f:116-146`);
deliberately unfixed.

For each scheduled CRNMULT activity, CROWN reads the keyword parameters with
OPGET but never assigns `ISPCC = INT(PRM(1))`, which the other variants'
CROWN routines do immediately after OPDONE. `ISPCC` is an unset local, so
the species/group/all-species branch is chosen from whatever value is on the
stack, and `CRNMLT(ISPCC)`, `DLOW(ISPCC)`, `DHI(ISPCC)` and `ICFLG(ISPCC)` can
be written outside their bounds. The keyword's species field is ignored.

**Confirmation:** ON's Hardwood deck with two CRNMULT keywords and DEBUG
added completed with HEAD `1fefe63c` but stopped with a segmentation fault
at line 126 (`CRNMLT(ISPCC) = PRM(2)`) when routines elsewhere in the
executable changed the stack contents. The regression decks do not use
CRNMULT. Scientific impact of the original run's stray writes was not
measured.

**Suggested fix:** add `ISPCC = INT(PRM(1))` after `call OPDONE(I,IDT)`,
as in the US CROWN routines, and verify CRNMULT runs as a separate model
change.

## 45. PN/WC/OP HTGF diagnostics print an unassigned variable

**Variants:** PN, WC, OP

**Location:** `wc/htgf.f90:31,380-381` (PN and WC) and
`op/htgf.f90:32,406-407`.

**Status:** reproduced during F18 verification on 16 November 2026.
Present in original F77 `c2105365` (`wc/htgf.f:51,410` and
`op/htgf.f:54,435`); deliberately unfixed.

The redwood height-growth diagnostic writes `'MAP=',MAPPHD`, but `MAPPHD` is
declared and never assigned. The printed value is stack contents, so it
changes whenever another routine's stack frame changes. Model results are
unaffected; debug output is not reproducible.

**Confirmation:** decks with DEBUG enabled for HTGF matched line for line
between HEAD `1fefe63c` and the F18 working tree except for the `MAP=`
values (2,030, 2,014 and 185 lines in PN, WC and OP).

**Suggested fix:** remove `MAPPHD` from the diagnostic, or print the
intended mapping value.

## 46. ON snag-volume reports stop with a floating-point trap

**Variants:** ON

**Location:** `ie/cftopk.f90:95-107` (BEHRE calls and the top-kill
volume ratio) and `base/behre.f90:26`, reached from
`fire/base/fmsvol.f90:144` by the snag list (FMSOUT, SNAGOUT keyword) and
the down-wood report (FMDOUT).

**Status:** reproduced during F21–F22 verification on 16 November 2026 at
`94c172ea`. Original F77 `c2105365` cannot reach this path, because its ON
decks stop at keyword input (bug 5). Diagnosed and repaired by R02 on
16 November 2026.

ON's Hardwood regression deck with `FMIN`, `SNAGOUT` and `END` added before
`PROCESS` stops with a trapped invalid floating-point operation while
computing snag merchantable volume for top-killed snags. Depending on the
stand, the trap is either in BEHRE's logarithms or in the ratio
`SCF*VOLTK/VOLT` that follows. The regression decks do not request snag
reports for ON. BC's own regression deck stops on the same path, via its
nine-argument call (bug 42).

**Confirmation:** the HEAD build and the F21–F24 working tree stop at the
same statement with identical output up to that point.

**Cause and repair:** ON's older CFVOL/OCFVOL contracts do not return the
separate SCF that the shared FMSVOL now passes to CFTOPK. OCFVOL also returns
positive `0.0001` total/merchantable placeholders below DBHMIN. A reproduced
snag with D=0.100147352 inches and H=4.70686865 feet has TOPD=3.937 inches.
Its positive merchantable placeholder reaches BEHRE with HTMRCH=-0.00758577324
and `AHAT*HTMRCH+BHAT=-0.000192962121`, outside the logarithm's domain.
Separately, undefined positive SCF enters an unsupported sawtimber correction
using uninitialized Canadian sawtimber standards or a zero integral.

FMSVOL now sets the unused Canadian SCF to zero and sets MCF to zero below
the existing DBHMIN threshold before top-kill correction. Total cubic volume,
the FFE tiny-tree cone floor, equations, standards and U.S. paths are unchanged.
No logarithm clamp, epsilon denominator or disabled trap was added.
Hardwood with SNAGOUT/down-wood reporting now finishes. Its ordinary standard
suite remains identical to the BC-repaired baseline for all 24 variants;
the pristine-original result remains 22/24, with only BC/ON differing.

## 47. BC prints undefined volume-equation codes after an invalid VOLEQNUM

**Variants:** BC

**Location:** reached from the VOLEQNUM handler (`vbase/initrevol_mod.f90`;
formerly `canada/bc/initre.f90`) through `volume/voleqdef.f90`; the
printed values appear in the listing's NATIONAL VOLUME ESTIMATOR LIBRARY
EQUATION NUMBERS table. Source of the undefined values not diagnosed.

**Status:** reproduced during F21 verification on 16 November 2026 at
`94c172ea`. Original F77 `c2105365` cannot reach this path for BC (bug 5).
Deliberately unfixed.

On BC's StandStructure deck, adding `VOLEQNUM` with an invalid cubic-foot
equation number for all species prints the expected "INVALID VOLUME
EQUATION NUMBER: THE DEFAULT CUBIC FT EQUATION NUMBERS WILL BE USED"
message, but the equation table then contains 112 non-printable bytes
instead of codes or `---`. An invalid board-foot code for one species
leaves 7 such bytes. The bytes look like memory addresses and differ
between builds: apart from them, the HEAD and F21 builds produced
identical output up to BC's later StandStructure stop. Without VOLEQNUM
the table prints `---`.

**Suggested fix:** trace how the default BC equation numbers are assigned
after VOLEQNUM resets VEQNNC/VEQNNB, find the undefined source, and
initialize it. Treat as a separate model change.

## 48. ON MCDEFECT labels species-group gross volume as pulpwood

**Variant:** ON.

**Location:** `INVMCDEFECT` in `vbase/initrevol_mod.f90`, formerly the
MCDEFECT handler in `metric/vbase/initre.f90`.

**Status:** reproduced during F21b verification on 16 November 2026 against
baseline `c12c9be1`. The same branch and FORMAT are present in original F77
`c2105365:metric/vbase/initre.f`. Deliberately unfixed.

An immediate MCDEFECT request for a species group includes ON in the
`CS/LS/NE/SN` branch and uses FORMAT 4332, which calls the result
"PULPWOOD VOLUME DEFECT". The all-species and single-species ON branches
instead use their own "GROSS TOTAL VOLUME DEFECT" messages. A two-species
group reproduced the discrepancy in both baseline and extracted builds;
the defect arrays and scheduled-operation bytes matched exactly. This is
a diagnostic inconsistency; no changed calculation was demonstrated.

**Suggested fix:** give ON's species-group echo the same gross-total-volume
wording as its other MCDEFECT forms in a separate diagnostic change.

## 49. R8PREPCOEF skips a fresh coefficient record using undefined previous keys

**Status:** confirmed and repaired on 16 November 2026 during the separate
correctness work preceding O04. Present in NVEL `a73e697e` and pinned original
`38548071`; not introduced by modernization.

**Location:** `volume/NVEL/r8clkdib.f90:R8PREPCOEF`. It is selected by all
23 variants that use NVEL; BC uses its separate volume implementation.
Callers are R8CLKDIB, HT2TOPD, R8VOL2 and R8CLKHT, including calls that switch
between COEFFS and COEFFST or reset the record before coefficient preparation.

The routine compared SPEC/EQN/GEOA with uninitialized local SPECPR/EQNPR/GEOAPR
and filled COEFFS only when a key differed. Those locals have no SAVE or
initialization. Matching bytes left on the stack can therefore skip the
entire fill, even when the caller supplies a different record. Saving just
the keys would retain that destination-identity defect: a matching equation
does not establish that the current caller's record contains its coefficients.

Two consecutive calls for `810CLK0100` with separate zeroed records reproduce
the failure under default GNU Fortran 15.2 settings. The first receives
R=24.20656, A4=-0.41002, P=7.15667 and FIXDI=3.5; the second retains zeros,
with both error flags zero. Pinned original F77 fails the same regression.
A static-local diagnostic build also fails; an optimized component build
behaves differently. This demonstrates compiler-sensitive coefficient loss,
without claiming that every integrated simulation previously encountered it.

The repair removes the previous-key condition and fills the caller's record
on every valid call. Lookup tables, searches, normalization, error paths,
success error-flag behavior and branch-specific assignments are unchanged.
Geometry and other fields outside those assignments remain caller-owned;
the repair neither clears the whole record nor adds saved/shared state.

`tests/parity/check_nvel_repairs.py --component r8prep` verifies independent
records, total/four-inch equation alternation, species aliases, untouched
geometry and invalid-equation errors under default and static-local builds.
All 24 standard-suite variants match the immediate R9HT-repaired baseline,
and all completed nine-stand SN inventory outputs also match exactly. The
22 U.S. variants still match pristine original F77 on the standard suite;
the documented Canadian comparison limits remain. No masks were changed.

## 50. STKVAL reads coefficient row zero before falling back

**Status:** confirmed and repaired on 16 November 2026 as a separate
correctness step before accepting O04.

**Location:** the four stocking-stage coefficient reads in `vbase/stkval.f90`.
All 24 variants select this routine. In CS/LS/NE/SN/ON, FIA 299 has
`TAB3(299,2)=0`; western variants explicitly replace that row selector with 8.
The same lookup/fallback ordering is present in original F77 `c2105365`.

Each stage read `TAB2(TAB3(IFIA,2),1:2)` before checking for zero coefficients
and falling back to the unknown-species row. A zero row therefore accesses
TAB2 outside its declared `1:36` bounds before that check can protect it.
This is reachable with SN's native `OS` species, mapped to FIA 299 in
`sn/blkdat.f90`, rather than requiring an invalid species code.

A derived first-stand `snt01` deck, limited to one cycle with its tree species
fields changed to native `OS`, reproduces the invalid access when only
STKVAL is compiled with bounds checking. Before repair it stops with
`Index '0' ... of array 'tab2' below lower bound of 1` (exit 2); after repair
it completes (exit 0). Normal builds before/after produce exactly matching
listing, summary, tree-list and database outputs on this Mac. That agreement
does not make the former out-of-bounds read defined or portable.

The repair resolves a zero coefficient selector to the existing FIA-999
fallback before indexing TAB2, at each of the four sites. Nonzero selectors,
the existing zero-coefficient fallback, species/forest-type mappings,
accumulation order and all coefficient values are unchanged. This introduces
no new stocking equation or species mapping.

`tests/parity/check_stkval.py` uses bounds checking and a frozen reference
with only this correctness repair applied. It checks 60 cases per variant
(1,440 total), exact result bits, size/stocking classes and debug output,
including empty input, excluded trees, blank/padded codes and both stocking
branches. Separate eastern cases require FIA 299's stocking totals/classes
to match the established FIA-999 fallback while retaining its own forest-type
mapping. All 24 full-suite variants match the preceding NVEL-repaired
baseline; the pristine-F77 suite remains 22/24 with the known BC/ON limits.

## 51. VOLINITNVB passes NSVB an equation whose eleventh character is undefined

**Status:** confirmed on 16 November 2026 while preparing O06–O08, and
repaired the same day as part of prerequisite R01 (before O10). Present in
NVEL `d0e634fa`/`70fbb02` and pinned original `38548071`
(`CHARACTER*11 VOLEQ,NVBEQN` in `volinit.f`); not introduced by
modernization or by the O06–O08 parsing changes.

**Location:** `volume/NVEL/volinit.f90:VOLINITNVB`, the branch for a VOLEQ
that is not an NSVB equation. All 23 variants that use NVEL select it; BC
uses its separate volume implementation. NVEL's own `vollibcs.f90` entry
points call the same routine.

`NVBEQN` is a `character(len=11)` local. Its only assignment is through
`NVB_DefaultEq`, whose dummy `NVBEQ` is `character(len=10)`, so
`NVBEQN(11:11)` is never defined before NVBC receives it. NVBC and SPEQCOEF
test `LEN_TRIM(VOLEQ)` and that character. A `'P'` sets STDORG=1, so slash
pine (111) and loblolly pine (131) use the planted-stand rows and other
species fall back to Jenkins-group or species-999 coefficients. A digit
triggers NVBC's four-digit-species reset, which with FIASPCD supplied leaves
a blank there. Blanks, NUL and other characters leave results unchanged.

A temporary driver called VOLINITNVB for an SN region 8 tree (10 in. DBH,
60 ft, default `811CLKE` equations) after setting only that byte. With
`' '` or NUL, the aboveground dry biomass `DRYBIO(1)` was 447.5671 (131),
622.7170 (111) and 572.9487 (316). With `'P'` it was 473.3537, 567.0146
and 549.4144; `VOL(1)` was unchanged. A disposable instrumented build saw
many byte values at this site in IE `ie`/`iet01`, PN `pnt01` and SN
`snt01`/`sn`, including 80 (`'P'`) and digits in SN.

No FVS output effect has been demonstrated. In this branch FVS takes its
volumes from VOLINIT, and it uses the NSVB biomass only under FIAVBC
(`BIODRYIN = BIODRY` when `LFIANVB`). FIAVBC gives every non-woodland
species an NSVB equation, which takes the other branch; its woodland
species are Jenkins group 10, for which NVB_DefaultEq returns before NVBC.
Direct NVEL callers receive the affected biomass.

**Repair:** VOLINITNVB now sets `NVBEQN = ' '` immediately before calling
NVB_DefaultEq. NVB_DefaultEq, NVBC, SPEQCOEF and the deliberate `'P'` that
NVBEQDEF writes for FIAVBC planted stands are unchanged. Results equal the
former behavior with a blank, NUL, digit or other non-`P` byte.

`tests/parity/check_nvel_repairs.py --component nvbeqn` fills the stack with
blank, `P`, `0`, `7`, NUL and 255 before each of 120 default-equation calls.
These cover five regions, 30 species, two tree sizes and live/dead trees; all
return ERRFLAG 0. The repaired routine gives identical bits for every fill,
equal to the frozen pre-repair routine with a blank. A `P` fill changes
the pre-repair results in 114 cases, which shows the fill reaches the byte.
The other fills change none.

All 24 standard-suite variants match the immediate baseline (bug 11 already
applied), and the pristine-original suite remains 22/24 with the known
BC/ON limits. FIAVBC versions of IE `iet01` and SN `snt01` (natural and
planted origin) also produce identical outputs before and after the repair.

## 52. NVB_RefSpcData leaves its outputs undefined for species codes below 10

**Status:** confirmed on 16 November 2026 during O10's direct checks; not
fixed. Present in NVEL `9655442` and pinned original `38548071`
(`nsvb.f` NVB_RefSpcData and GetRegnWF).

**Location:** `volume/NVEL/nsvb.f90:NVB_RefSpcData`, used by GetRegnWF in
the same file and by VOLINITNVB in `volinit.f90`. All 23 variants that use
NVEL select them; NVEL's own `vollibcs.f90` entry points call them too.

For a species code below 10, NVB_RefSpcData sets `ERRFLG = 6` and returns
before assigning SPGRPCD, WDSG, SFTHRD, CF, SPGRNWF or SPDRYWF. An unknown
code of 10 or more instead receives species 999's values. GetRegnWF ignores
the error flag and copies its own unassigned SPGRNWF into WtFac and DeadWF.
VOLINITNVB's DBH-only branch uses the unassigned CF, SPGRNWF and SPDRYWF in
the same way. Its default-equation branch tests SPGRPCD after the lookup has
failed.

**Confirmation:** in a pre-change NVEL build, two direct VOLINIT cruise-weight
cases (`CTYPE='C'`) stopped with a floating-point trap at GetRegnWF's
`DeadWF = WtFac*...`. One supplied species 1 (`CONSPEC='1   '`). The other
used R6's default equation `616BEHW000`, whose species field gives 0 when
CONSPEC is not numeric. NVBC rejects codes below 10 before its own
GetRegnWF calls.

**FVS reachability:** not demonstrated. FVS passes VOLINITNVB the variant's
FIA code with cruise type `F` or `I`. Reaching the defect needs a missing or
one-digit FIA code on a DBH-only tree or a failed default-equation lookup.
Direct NVEL callers can reach it with cruise type `C` or such a code. The
O09–O12 direct cases avoid these inputs, because no comparison of them is
meaningful.

**Suggested fix:** return species 999's values for codes below 10, as for
other unknown codes, or define every output before returning. Callers
should also honor `ERRFLG`. This is a separate correctness change.

## 53. R1KEMP's cubic sawlog volume subtracts an unassigned percentage

**Status:** confirmed on 16 November 2026 during O10's direct checks; not
fixed. Present in NVEL `9655442` and pinned original `38548071`
(`r1kemp.f`, the NONSAW lines).

**Location:** `volume/NVEL/r1kemp.f90`, reached through DVEST for region 1
equations `102`–`106...DVE`. These are, for example, IE's defaults for grand
fir and several other species (`102DVEW017`, `102DVEW108`, `102DVEW106`).
All 23 variants that use NVEL select it.

The statements that set NONSAW (`NONSAW = IDEF`) are commented out, but
`CBSAW = CBGRS - NONSAW / 100.0 * CBGRS` still uses it. NONSAW is an unsaved
local, so `VOL(5)`, the cubic sawlog volume, depends on whatever integer the
stack holds.

**Confirmation:** during O10's direct checks the current NVEL build gave
`VOL(5)` = 2.0423009 in one run and 2.0629303 in another for
`102DVEW017` (6-inch tree), consistent with NONSAW of 1 and 0. In 80 runs of
the same executable, 14 differed, only in `VOL(5)` and only for that
equation. The pre-change build's 100 runs happened to agree, because each
build leaves different values on the stack.

**FVS impact:** none demonstrated. FVS uses `VOL(1)`, `VOL(2)`, `VOL(4)`,
`VOL(7)` and `VOL(10)` from NVEL, and VOLINITNVB does not use `VOL(5)`.
Direct NVEL callers receive the undefined value. As a test-only control,
`check_nvel_parsing.py` sets NONSAW to 0 in both of its temporary builds, so
it can compare `VOL(5)` meaningfully. Repository sources are unchanged.

**Suggested fix:** decide NONSAW's intended source. With no defect input,
`NONSAW = 0` (sawlog volume equal to gross cubic volume) appears to be the
intent. This is a separate correctness change.

## 54. Canadian live-tree VOLS still calls CFTOPK with nine arguments

**Variants:** BC and ON

**Location:** `canada/bc/vols.f90:143`, `canada/on/vols.f90:155`, and the
ten-argument interface in `ie/cftopk.f90`.

**Status:** confirmed by source inspection and compiler diagnostics during
R02 on 16 November 2026. The same calls occur in pinned original F77.
Deliberately unfixed: R02 repairs the separate snag callers in sections 42/46.

Both live-tree callers still pass `(ISPC,D,H,VN,VM,VMAX,LCONE,BARK,ITRUNC(I))`,
omitting SCF and shifting the remaining arguments as in section 42. Compiling
ON's selected VOLS and CFTOPK together diagnoses LOGICAL/REAL/INTEGER argument
mismatches. The calls require top kill and positive VMAX; BC's current CFVOL
sets VMAX to zero, so its normal path does not reach this call. Runtime impact
on ON live trees has not been measured. Standard-deck completion does not
verify this separate broken-top path.

**Suggested fix:** separately establish the Canadian live-tree volume
contracts and pass a complete, explicitly checked interface. Do not bundle
that model change with the snag-volume optimization.

## 55. ON's shared snag wrapper passes the newer NATCRS interface to the older entry

**Variant:** ON

**Location:** `fire/base/fmsvol.f90` calls NATCRS for METHC 6/10, but
`canada/on/varvol.f90:NATCRS` implements the older thirteen-argument entry.

**Status:** confirmed by source inspection and a combined-translation-unit
compiler check during O16 on 16 November 2026. The mismatch is inherited from
the pre-modernization sources. Deliberately unfixed; the standard ON deck uses
the native OCFVOL path instead, and runtime impact for METHC 6/10 is unmeasured.

The shared call supplies the newer twenty-argument total/merchantable/sawtimber,
biomass and live/dead contract. ON expects total, merchantable and board-foot
volumes followed immediately by species and geometry. SCF and BBFV therefore
shift the old species and diameter arguments, with additional incompatible
positions later. gfortran diagnoses the REAL/INTEGER/LOGICAL mismatches when
the actual caller and callee are compiled together.

**Suggested fix:** reconcile ON's older NATCRS product meanings and argument
contract as a separate model repair. O17 leaves this path unchanged and does
not reuse its output. R02's supported native Canadian paths remain covered.

## 56. FVSVOL debug output prints the undefined retired I1 local

**Variants:** all 22 U.S. variants selecting `vvolume/fvsvol.f90`.

**Location:** NATCRS's cubic and board-foot debug records in
`vvolume/fvsvol.f90:283-284,444-445`.

**Status:** reproduced during O17 on 16 November 2026, including different
debug records from two fresh runs of the identical immediate-baseline binary.
Present in pinned original F77. Deliberately unfixed; no model calculation
uses this retired local.

The former I1 arguments were replaced by initialized HTLOG/HTREF in 2022 and
the I1 assignments were commented out. Both FVSVOL debug WRITE statements
still print I1, so their final integer depends on leftover stack contents.
One observed record prints 1 in the baseline and 0 after O17, while an
unchanged-baseline repeat differs later in the same field. Ordinary model
outputs and restart files match.

The extended FVSVOL debug comparison therefore gives I1 a test-only zero in
both temporary builds. This controls an undefined diagnostic input without
changing the comparator, masking a field or shipping a model/diagnostic fix.
Other defined debug output must still match exactly. Uncontrolled native
FVSVOL debug output remains unsuitable for exact whole-file comparison.

**Suggested fix:** remove the retired I1 field from the diagnostic text and
argument list as a separate diagnostic repair.
