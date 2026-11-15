# Found Bugs

The first five confirmed model defects below were found during the 2026-11 consolidation of duplicated routines. Every one is already present in the pre-modernization source (commit `c2105365`), so none was introduced by the conversion to free-form Fortran or by the consolidation. M02a–M07a resolve the interfaces in section 1 and the missing arguments in section 2, and remove DBSIN's part of section 5; the remaining defects are deliberately retained. These interface repairs define behavior where the original calls were undefined and cannot establish simulation parity on those paths. Sections 6–8 separately record an unconfirmed historical numerical concern, a historical documentation discrepancy, and ESADDT data-flow concerns requiring follow-up. Section 9 records a reproduced SQLite resource defect with scientific-output impact still unmeasured. Section 10 records a restart-file difference that modernization itself introduced, now corrected. Sections 11 and 12 record two original defects that read undefined values and so make restart files vary between runs. Sections 13 and 14 record an invalid stub diagnostic and a fuel-model fallback defect found during the safe reapplication; neither was fixed. Sections 15–18 record DGDRIV tripling/bounds defects and undefined local reads found during L05; all were preserved. Section 19 records conditional undefined local reads in KT CRATET; these were also retained. Sections 20 and 21 record two defects in the former metric copy of EVLDX found during the M01 audit; the M01a merge carried both into `base/evldx.f90` unfixed. Sections 22–24 were found during the M02–M07 audits: section 22 is an output difference in BC and ON that the free-form conversion introduced, six of whose FORMATs are now corrected by M05a/M07a; sections 23 and 24 are original defects retained in the shared STATS and FMDOUT.

Unless a later status or location is given, file and line references are to the tree at commit `b5657da3` plus the then-uncommitted `VARIANTCD` change. Bugs 1 and 2 have the same root cause: FVS calls external procedures with no explicit interface, so gfortran cannot check argument lists across files. Moving these routines into modules would let the compiler catch this class of bug.

Sections 25–29 were found during the bounded M08–M14 audits on 15 November
2026 at `ed5e92fb`. All are present in the pinned original F77 reference and
remain unfixed. Temporary block checks reproduce the report/classification,
diagnostic and zero-division cases described below; they do not establish
Canadian integrated simulation coverage.

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

## 5. BC and ON keyword-field arrays have 7 slots, but `KEYRDR` fills 12, so BC and ON cannot read keyword files

**Variants:** BC, ON

`KEYRDR` (`base/keyrdr.f90`) declares `ARRAY(12)`, `KARD(12)` and `LNOTBK(12)` (lines 35, 39, 44) and fills all twelve. Several BC/ON routines still declare these as 7-element arrays, so every keyword record overwrites the five elements past the end. In `INITRE` this evidently overwrites the neighboring `KEYWRD` variable:
- every keyword reads back as eight NUL bytes;
- each record produces `FVS01 ERROR: INVALID KEYWORD WAS SPECIFIED`;
- the run ends with `STOP 20`.

**BC and ON cannot process any keyword file, at `c2105365` or now.** Upstream commit `08e833b2` (28 August 2025) widened `KEYRDR` from 7 to 12 fields without changing the metric and BC copies.

Files still affected and compiled:

| File | Declarations | Variants |
|---|---|---|
| `canada/bc/initre.f90` | lines 54, 60, 64 | BC |
| `metric/vbase/initre.f90` | lines 52, 59, 63 | ON |
| `metric/newmist/misin.f90` | lines 121, 126, 131 | BC |

The same defect was in pre-modernization `canada/bc/keyopn.f`, `metric/base/ksstag.f` and `metric/clim/clin.f`. The consolidation replaced those with the shared 12-element versions, so it no longer exists there. M05a also retires `metric/dbsqlite/dbsin.f90`; BC/ON now use the shared DBSIN with `KARD(12)`. Direct DBSIN tests exercise the real twelve-field KEYRDR; the three remaining compiled readers above still block Canadian simulation.

Eleven uncompiled files had the same defect (in `archive/bgc/`, `metric/dfb/`, `metric/estb/`, `metric/ppbase/`, `metric/rd/` and `metric/wwpb/`). They were removed with the other unbuilt directories on 14 November 2026.

A pre-M05a scratch build with the then-four compiled files widened to 12 read the BC and ON keyword files correctly.

**Suggested fix:** widen the remaining arrays to 12, preferably through a shared named constant so `KEYRDR` and its callers cannot drift apart again. Then merge the `initre` and `misin` twins. DBSIN and the missing `DBSTREESIN` arguments (bug 2) are already handled under M05a and M02a.

---

## What the regression tests show for BC and ON

Because of bug 5, the BC and ON test decks (`tests/FVSbc`, `tests/FVSon`) stop at keyword input in both `c2105365` and the current tree. Their outputs match only because both fail the same way, so the regression comparison says nothing about BC or ON simulation results. The other 22 variants' test decks run normally.

A scratch build with bugs 5 and 2 fixed showed two further, unexplained problems:

- **ON:** the `Hardwood` test runs to completion. Its starting stand values match `tests/FVSon/Hardwood.sum.save`, as do the post-treatment stand values in later rows. But from the second row on, the `.sum` file's year and growth columns hold wrong values, which points to a further summary-output problem.
- **BC:** both tests still end with exit status 127. `StandStructure` writes no output, and `YSM-SkyRanch` output stops right after the mistletoe keywords (`NEWSPRED`).

Neither has been traced. Expect more BC/ON defects once these five are fixed and the test decks can run.

---

## 6. Deferred investigation: historical R9 Clark power guard

**Status:** unconfirmed numerical concern; no demonstrated failure on valid tree inputs and no fix authorized.
**Potential scope:** variants selecting NVEL `r9clark.f90` (all current variants except BC), when the relevant R9 equation path is used.

D11 found that the formerly selected `volume/r9clark_fvsMod.f90` guards the power term in `r9dib` and `r9ht`: when `(1.0-17.3/totht) < 0.005748` and `p > 14`, it sets `Y=0` instead of evaluating `(1.0-17.3/totht)**p`. The selected NVEL implementation evaluates that power directly at these two sites. Related guards exist elsewhere in NVEL; this is not evidence that the whole library lacks underflow handling. The pinned original NVEL `38548071` also evaluates these two powers directly.

The local workaround entered build selection in `a19c41b4` (4 March 2024); ON selected it in `d495d604` (26 June 2024). The lists returned to canonical NVEL in `08e833b2` (28 August 2025), before the pinned original F77 reference. The available history establishes that selection change, but does not explain whether these two guards were deliberately dropped, made unnecessary, or overlooked.

Before proposing a fix, reproduce a reachable valid-input case, measure any exception or output effect, and establish why the library selection changed. A small positive power base and large exponent can motivate an underflow investigation, but this cleanup does not establish a model bug or a safe replacement threshold. Keep any eventual correction separate from original-F77 parity work.

The workaround remains available with `git show 9bc35a16:volume/r9clark_fvsMod.f90`. No guard was added to NVEL and no active model code changed.

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

**Status:** found 14 November 2026 while tracing restart files. Present in original F77 (`c2105365:tt/forkod.f`, lines 35 and 88). Deliberately unfixed.
**Variant:** TT.

`tt/forkod.f90` declares `KFOR(4)` but gives it no values, then executes `IGL = KFOR(IFOR)` (line 79) whenever a valid forest code is used. Every other variant's `FORKOD` fills `KFOR` with a DATA statement. `IGL` therefore holds whatever was in memory: for example 1798184300 in one build of the original code and 1806851564 in another build of the same code.

No routine that TT compiles reads `IGL`, so results are unaffected. The value is saved in restart files (the 25th integer of each stand's record), which is why TT restart files never match between builds. `tests/parity/check_suite.py` ignores that one integer for TT.

**Suggested fix:** add `data KFOR/4*1/`, which keeps the value `GRINIT` assigns (`IGL = 1`). Then remove the TT exception from `check_suite.py`.

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
| `metric/fire/base/fmsout.f90` | 200 |
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
`metric/fire/base/fmin.f90:800`
**Status:** found 15 November 2026 during the M07 audit by reading the
source; present in `c2105365`. Unfixed. M07a preserves this rule in the
shared routine; direct drivers exercise it, while the BC/ON simulation
decks still cannot read keywords (bug 5).

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
**Location:** the first report section in `metric/fire/vbase/fmfout.f90`,
with option 4 in `metric/fire/base/fmin.f90`.
**Status:** confirmed during M10 by source tracing and block checks in
current code and `c2105365:metric/fire/vbase/fmfout.f` /
`c2105365:metric/fire/base/fmin.f`. Unfixed.

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
**Location:** the first-cycle DBSFMPFC loop in `metric/fire/vbase/fmpofl.f90`.
**Status:** confirmed during M11 in current code and
`c2105365:metric/fire/vbase/fmpofl.f`. Unfixed; full simulation impact has
not been measured.

The preceding FMOIS loop leaves IDPL equal to 2. The subsequent IDPM=1,2
database loop tests `PRESVL(IDPL,1)` but copies moisture from
`PRESVL(IDPM,2:8)`. Both database rows are thus controlled by the moderate
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
**Location:** class accumulation in `metric/newmist/misprt.f90`.
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
activity 2010. Unfixed. The existing field-width defect (bug 5) is separate.

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
