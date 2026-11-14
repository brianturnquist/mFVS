# Found Bugs

The five confirmed defects below were found during the 2026-11 consolidation of duplicated routines. Every one is already present in the pre-modernization source (commit `c2105365`), so none was introduced by the conversion to free-form Fortran or by the consolidation. They are **deliberately left unfixed** for now so that model outputs stay identical to `c2105365` while modernization is finished. Each fix below will change outputs for the variants listed, so each should be made as its own commit with a before/after comparison. Sections 6 and 7 separately record an unconfirmed historical numerical concern and a historical documentation discrepancy.

File and line references are to the tree at commit `b5657da3` plus the uncommitted `VARIANTCD` change. Bugs 1 and 2 have the same root cause: FVS calls external procedures with no explicit interface, so gfortran cannot check argument lists across files. Moving these routines into modules would let the compiler catch this class of bug.

---

## 1. `FMSVL2` is called with the wrong number of arguments in BC and ON

**Variants:** BC, ON
**Blocks consolidation of:** `metric/fire/base/fmdout.f90` (left as a twin of `fire/base/fmdout.f90`)

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

A third copy of the eight-argument call, formerly in `canada/fire/on/fmcrowe.f90:277`, was not compiled by any variant and was retired under D08 on 13 November 2026. Its exact recovery identity and replacement diff are in the [D04–D13 report](tests/parity/evidence/2026-11-13-d04-d13/README.md). The live mismatched calls above remain unchanged.

**Suggested fix:**
1. Give the BC `FMSVL2` the ten-argument interface; it can accept `CRWNRTO` and `LIVEDEAD` and ignore them.
2. Change the metric `fmdout` call to the ten-argument form, passing `ICR(I)` and `'L'` as `fire/base/fmdout.f90:244` does.
3. Merge the `fmdout` twin after the interface fix. The unused ON `fmcrowe` retirement is already complete under D08.

Merging `fmdout` before this fix is a compile error, because one file cannot call the same external two different ways. That is why the twin was kept.

---

## 2. Metric `INTREE` calls `DBSTREESIN` with 30 of its 33 arguments

**Variants:** BC, ON
**Blocks consolidation of:** `metric/base/intree.f90` (left as a twin of `base/intree.f90`)

`dbsqlite/dbstreesin.f90:5` gained three trailing arguments for cull, decay code and woodland stems: `RCULL`, `IDECAY`, `IWDLDSTMS`. `base/intree.f90:135` passes them as `CULL(I)`, `DECAYCD(I)`, `WDLDSTEM(I)`, but the metric copy at `metric/base/intree.f90:136` still passes only the original 30.

`DBSTREESIN` sets all three to zero unconditionally (lines 78–80) and reads them from the `Cull`, `DecayCd` and `WdldStem` columns when present (lines 217–225). In BC and ON those writes go through addresses that were never passed. A scratch build confirmed the effect. With bug 5 fixed, ON crashed with a segmentation fault while reading tree records from the database. When the three arguments were also passed, the ON test ran to completion.

The metric copy also lacks the decay-code correction that `base/intree.f90:141–146` applies after the call. `metric/dbsqlite/dbstreesin.f90`, an older 30-argument version, was not compiled by any variant and was retired under D07 on 13 November 2026. Its exact recovery identity and replacement diff are in the [D04–D13 report](tests/parity/evidence/2026-11-13-d04-d13/README.md). The live caller and shared callee remain unchanged.

P03 source tracing on 13 November 2026 confirmed 33 versus 30 arguments in both the original `c2105365` source and current mFVS. Earlier documentation counted 31 versus 28, omitting `ABIRTH` and `LBIRTH` from both totals. This is a documentation correction only; the missing trailing arguments and the model code are unchanged. See the [P01–P03 verification report](tests/parity/evidence/2026-11-13-p01-p03/README.md).

**Suggested fix:** pass `CULL(I)`, `DECAYCD(I)` and `WDLDSTEM(I)`, and port the decay-code correction. Then merge the `intree` twin, which was attempted during consolidation and reverted for this reason.

---

## 3. `ESADDT` bins seedlings by hard-coded species indices that are wrong for several variants

**Variants:** AK, CI, EM, IE, KT (every variant that compiles `estb/esaddt.f90`)

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

**BC and ON cannot process any keyword file, at `c2105365` or now.**

Files still affected and compiled:

| File | Declarations | Variants |
|---|---|---|
| `canada/bc/initre.f90` | lines 54, 60, 64 | BC |
| `metric/vbase/initre.f90` | lines 52, 59, 63 | ON |
| `metric/dbsqlite/dbsin.f90` | line 73 (`KARD` only) | BC, ON |
| `metric/newmist/misin.f90` | lines 121, 126, 131 | BC |

The same defect was in pre-modernization `canada/bc/keyopn.f`, `metric/base/ksstag.f` and `metric/clim/clin.f`. The consolidation replaced those with the shared 12-element versions, so it no longer exists there.

Uncompiled files with the same defect:
- `archive/bgc/src/bgcin.f90`
- `metric/dfb/dfbin.f90`
- `metric/estb/esin.f90`
- `metric/ppbase/ppin.f90`
- `metric/rd/rdin.f90`, `rdinca.f90`, `rdincr.f90`, `rdinie.f90`, `rdinso33.f90`, `rdinut.f90`
- `metric/wwpb/bmppin.f90`

A scratch build with the four compiled files widened to 12 read the BC and ON keyword files correctly.

**Suggested fix:** widen the arrays to 12, preferably through a shared named constant so `KEYRDR` and its callers cannot drift apart again. Then merge the `initre`, `dbsin` and `misin` twins. Fix this together with bug 2: once keywords are read, ON reaches the `DBSTREESIN` call and crashes.

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

The [D04–D13 report](tests/parity/evidence/2026-11-13-d04-d13/README.md), D11 replacement diff, historical selection receipt, and exact pre-deletion Git recovery command preserve the workaround. No guard was added to NVEL and no active model code changed.

---

## 7. Historical NC crown-mapping comment disagrees with its table

**Status:** stale documentation, not a demonstrated numerical defect; recorded during C04 on 14 November 2026.
**Variant:** NC, species 12 (coast redwood).

The old `fire/nc/fmcrow.f90` comment says coast redwood maps to equation 3, but the twelfth `ISPMAP` value is 19. Original F77 `c2105365:fire/nc/fmcrow.f` has the same comment and value (lines 71–74). `fire/base/fmcroww.f90` identifies equation 3 as Douglas-fir and equation 19 as giant sequoia/coast redwood, which supports treating the old comment as stale rather than changing the table.

C04 preserves mapping **19**, along with every other KT/NC table entry, in shared `fire/base/fmcrow.f90`. Its comment makes the historical discrepancy explicit. Direct original/current/candidate tests exercise every species with the actual crown equations. No numerical correction is proposed or applied. Revisit the equation assignment only if a separate scientific review supplies evidence that the table itself is wrong.
