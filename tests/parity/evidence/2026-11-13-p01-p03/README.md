# P01–P03 verification receipt

Completed: 13 November 2026, America/Denver. The simulator's raw timestamps use
UTC and show 14 November; they are retained as emitted, with metadata
normalization described below.

P01 reference preparation and P02's one-deck IE comparison passed. P03's
Canadian boundary investigation is complete; BC/ON simulation parity remains
blocked. No scientific model source, interface, input deck, or reference output
was repaired. Model bug fixes remain deferred in
[FOUND_BUGS.md](../../../../FOUND_BUGS.md).

## Exact states

| State | Main revision | NVEL revision |
|---|---|---|
| Original fixed-form reference | `c210536536d81fd9b46dccb9a571f0de1e55e944` | `38548071d5aa652bb90c7f111f86b427f798a1c9` |
| Immediate current baseline | `42d1d2504dbc6162adf8cf4cd8071fa3b0d332f8` | `a73e697e220ffd0f792aa7ac10d84c911edb7ad7` |
| Verification-only candidate | Working-file snapshot based on `42d1d250` | Same current NVEL revision |

The candidate's selected model/build inputs are identical to current mFVS.
Its binary was therefore reused explicitly, rather than separately compiled;
the IE candidate ran in its own directory and process. This establishes the
initial three-state comparison procedure. It does not test a scientific
cleanup patch, because none has been made.

The original reference retains its own original NVEL, including differences
from the current library. No current NVEL substitution was made.

## P01 — Reference preparation

Six builds succeeded: original and current IE, BC, and ON. Candidate binary
reuse for identical inputs is recorded separately in [builds.json](builds.json).

The original main snapshot's 3,210 tracked blobs were checked against git;
only `bin/FVSbc_sourceList.txt` and `bin/FVSon_sourceList.txt` differ. The exact
[reference-build.patch](reference-build.patch) adds the four documented database
link files to both Canadian manifests and `wdbkwtdata.inc` to BC. All 282
original NVEL blobs match their pinned git revision. All 3,100 current main
blobs and 285 current NVEL blobs match their own revisions. See
[source-integrity.json](source-integrity.json).

These are linkage-only accommodations already described in
[Contributing.md](../../../../Contributing.md). Original equations, declarations,
interfaces, and control flow remain unchanged. The patch is retained for exact
reconstruction of the reference build inputs.

Environment: macOS arm64; GNU Fortran and GNU C 15.2.0; GNU Make 3.81;
SQLite CLI 3.51.0. Full version strings and executable paths are in `builds.json`.

Shared Fortran flags:

```text
-fPIC -g -cpp -DCMPgcc -Wall -Wno-integer-division
-ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace
-fdec -std=legacy -fallow-argument-mismatch
```

Shared C/C++ flags:

```text
-fPIC -std=gnu17 -DANSI -DCMPgcc -DSQLITE_THREADSAFE=0
-DSQLITE_OMIT_LOAD_EXTENSION -Dunix
```

The original numerical/trap flags are retained, with common PIC and
legacy-language compatibility settings. No optimization or precision change
was introduced. The runtime warns that the requested denormal-operand trap is
unsupported on this platform; that same warning is retained in every run.
Archive builds use `unknown` version macros because they contain no git
metadata; exact source/binary identities remain in the receipts.

## P02 — IE iet01

The original `iet01.key` and `iet01.tre` bytes match the current/candidate
inputs. All three independent runs produced the same seven artifacts under the
comparison contract:

- `iet01.out`: full listing.
- `iet01.sum`: all summary headers and records.
- `iet01.trl`: complete tree listing.
- `iet01_Out.db`: every schema object and row in all 17 tables.
- `stdout.txt`, `stderr.txt`, and `returncode.txt`.

| Comparison | Result |
|---|---|
| Original vs current | PASS — [receipt](compare-ie-original-current.json) |
| Original vs candidate | PASS — [receipt](compare-ie-iet01-original-candidate.json) |
| Current vs candidate | PASS — [receipt](compare-ie-iet01-current-candidate.json) |

Each database contains 4 cases, 50 summary records, and 14,178 rows across all
17 tables. Simulation records progress through multiple cycles. This is a
comparison of actual simulated results, unlike the Canadian input failures.

Every IE run ends with STOP 20 and the same FVS_Error records: four FVS09
plot-count warnings and one FVS04 invalid-parameter error. The listing explicitly
says `* this thinpres should cause an error` immediately before its invalid
THINPRSC record. This is intentional error-path coverage in the existing deck,
not evidence of a newly discovered model defect. The keyword, its error, and
the exit status were retained and compared, not fixed or filtered out.

### Comparison behavior

Only identified run metadata is normalized: recognized header date/time and
version fields, FVS_Cases build/run metadata, and known CaseID UUID spelling.
CaseID relationships and multiplicity remain significant. SQLite physical row
order is ignored; schema, types, NULLs, all values, and exact REAL bits are
compared. Text whitespace and all nonmetadata bytes remain significant. See
the [tool contract](../../README.md#exact-comparison-contract).

All ten deliberate mismatch controls returned comparator CLI exit status 1:

| Mutation | Rejected |
|---|---|
| Scientific year in summary text | Yes |
| Record count in a `-999` header | Yes |
| Added text whitespace | Yes |
| Timestamp-shaped text outside a metadata header | Yes |
| One ULP in a REAL QMD value, retaining REAL type | Yes |
| SQLite schema addition | Yes |
| Child row assigned to the wrong existing CaseID | Yes |
| Duplicate database row | Yes |
| Missing tree-list artifact | Yes |
| Changed process exit status | Yes |

The negative controls update capture hashes after mutation, so failures come
from semantic/content comparison rather than stale checksums. Full results are
in [comparator-controls.json](comparator-controls.json). Separate
[metadata-controls.json](metadata-controls.json) checks show that legitimate
IE/ON header metadata changes are accepted while title/variant changes are
still rejected.

## P03 — Canadian verification boundary

Original and current versions both exhibit the following, with matching
artifacts after narrow metadata normalization:

| Deck | Exit status in both | Invalid-keyword errors in each | Summary/output database generated |
|---|---:|---:|---|
| BC StandStructure | 20 | 300 | None |
| BC YSM-SkyRanch | 20 | 461 | None |
| ON Hardwood | 20 | 30 | None |

The listings contain eight-NUL keyword fields: 296 occurrences for
StandStructure, 461 for YSM-SkyRanch, and 30 for Hardwood, in each version.
Input databases are unchanged. See [canadian-boundary.json](canadian-boundary.json)
and the three matching-failure comparisons:
[StandStructure](compare-bc-StandStructure-original-current.json),
[YSM-SkyRanch](compare-bc-YSM-SkyRanch-original-current.json),
[Hardwood](compare-on-Hardwood-original-current.json).

Source-bound tracing of both original and current code confirms:

- Bug 5: KEYRDR declares twelve slots, while selected Canadian INITRE and
  related keyword declarations still have seven. The observed failures are
  consistent with the documented keyword-buffer defect.
- Bug 2: shared DBSTREESIN declares **33** arguments, while metric INTREE
  passes **30**, ending with ABIRTH and LBIRTH. The three omitted arguments
  are RCULL, IDECAY, and IWDLDSTMS. The unused older metric implementation
  also has 30 arguments. Previous documentation's 31/28 totals omitted two
  arguments from both counts; only those documentation totals were corrected.
- Bug 1: BC FMSVL2 has 8 arguments, shared FMSVL2 has 10, metric FMDOUT
  passes 8, and the shared SVCWD caller passes 10. The incompatible selected
  interfaces remain unchanged.

Argument lists, declaration excerpts, and exact source hashes are retained in
[static-interfaces.json](static-interfaces.json). Bugs 1 and 2 were inspected
statically; this task did not fix bug 5 to reach and invoke their undefined
paths. Existing reports of behavior after experimental repairs were not
reclassified as new evidence from this run.

**P03 completes the boundary audit only.** Matching input failures do not
establish BC/ON simulation parity. Model repairs remain deferred; M02, M05,
M07, M12, M17 and other affected consolidations still need adequate simulation
coverage before acceptance.

## Retained evidence and replay

- [builds.json](builds.json): states, dirty status at snapshot time, tools,
  flags, commands, per-variant source hashes, logs, and binary hashes.
- `build-*.log.gz`: complete compiler/link output for all six builds.
- [reference-build.patch](reference-build.patch): exact reference link patch.
- [runs.json](runs.json): input/output hashes, commands, timing, modifications,
  and actual exit status for all nine runs.
- `compare-*.json`: per-file and per-table comparison results.
- [artifacts.json](artifacts.json): archive size and SHA256.
- [artifacts.tar.gz](artifacts.tar.gz): raw run inputs, every generated output,
  stdout, stderr, and exit status. This remains usable after temporary builds
  are removed; follow the [replay instructions](../../README.md#replay-retained-artifacts).

No broader variant parity claim is made. No model bug fix or source cleanup
was performed. The working checkout's selected IE/BC/ON model/build inputs
still match the captured candidate; all changes for these tasks are verification
support, receipts, tracker updates, and factual documentation corrections.
