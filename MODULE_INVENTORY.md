# Module migration inventory and ownership

Phase 1 source/design inventory, 17 November 2026. Its source reference is
main `d31a9ff0d0a51e1f0622d0a9b8d431dd310b54c4`, with NVEL
`2503c0de71cb07b7d04c4d2edba38a3ff27237fc`. No simulation procedure, COMMON
declaration, equation, initialization or restart layout changes in this phase.

## Scope and regeneration

The principal migration covers the internal Fortran selected by all 24 live
`bin/FVSxx_sourceList.txt` manifests, including metric paths, linked extension
stubs, existing modules and the executable/library adapters. GNU Make and the
GNU CMake path remain supported build inputs. Compiler-specific Windows/Intel
aliases and annotations remain compatibility boundaries; native Windows and
Intel execution were not verified here. MOD06 must verify dependency changes
on each build path before a migration uses them.

NVEL and ORGANON implementation internals, SQLite, FOFEM and CFIM remain named
library boundaries. Their selected Fortran procedures, shared state and import
sites are indexed, but this principal phase does not rewrite their internals.
The `vorganon`/volume/database/fire adapters on the mFVS side remain in scope.
Standalone NVEL SDK/Visual Studio targets and unselected sources are outside
this migration; a live-manifest exclusion does not authorize their removal.

The Make executable and per-variant shared library use the same manifests;
CMake additionally groups SQLite/FOFEM into libraries. The clients in
`tests/APImemTables`, `tests/APIviaR` and `tests/AllCycleActivities`, and the
C shims in `base/apisubsc.c`, are external API consumers. Packaging must retain
the executable, library and public calling contracts.

Regenerate or inspect the inventory without retaining a generated archive:

```sh
python3 -B tests/parity/module_inventory.py
python3 -B tests/parity/module_inventory.py --procedure ISTFNB
python3 -B tests/parity/module_inventory.py --common DMMIST
python3 -B tests/parity/module_inventory.py --json
python3 -B tests/parity/test_module_inventory.py
```

The [indexer](tests/parity/module_inventory.py) reads every live manifest and
its include lookup view. Each definition has a source/line/scope identity,
native variants and a migration family. It records signatures, ENTRY groups,
interfaces, module USE/visibility, DEC ABI annotations, direct calls, known
function references and passed procedures. Each COMMON source layout retains
ordered members, declarations, native include sources, consumers, reference
sites, direct assignments, possible call/I/O mutations and proposed ownership.
SAVE/DATA/declaration-initialized locals, derived types, module objects and
EQUIVALENCE sites are also indexed. Repeated COMMON fragments are joined in
source order; differently named fields in another scope remain separate layouts.

This is a conservative lexical index. CPP alternatives remain visible, and
native PRGPRM declarations give the variant-dependent dimensions. A possible
mutation is an audit site, not proof of INTENT(OUT). It does not prove alias
safety, branch reachability or readonly DATA tables. The named migration batch
must check those contracts with source inspection and the compiler before it
adds INTENT, changes storage or updates callers. No interfaces are generated
from guessed types. New source roots and unowned COMMON names fail the index.

## Procedure families and compatibility boundaries

The current union contains 1,541 selected paths and 1,355 Fortran compilation
sources. It has 2,123 scoped units: 1,903 subroutines, 104 functions, 87 BLOCK
DATA units, 28 modules and one program. There are 366 ENTRY declarations and
31 statement functions. These counts include selected library boundaries and
different native definitions of the same external name.

| Inventory family | Scoped units | Migration treatment |
|---|---:|---|
| MOD09 API | 60 | Executable, command-line, C/Fortran and memory-table adapters; preserve symbols and argument conventions. |
| MOD10 helpers | 4 | Small shared helpers; the first bounded batch is ISTFNB only. |
| MOD11 database | 80 | Input/output bindings and resources, one writer/reader family at a time. |
| MOD11 input/report/expression | 66 | Keyword readers, reports and expression handling. |
| MOD12 equations | 487 | Native equation/volume/dispatch families, preserving selected species layouts. |
| MOD13 core | 288 | Stand/cycle control, treatments, tree updates and initialization. |
| MOD13 restart | 22 | Stop/restart adapters and primitive packing; preserve formats. |
| MOD14 extensions | 515 | Separate fire, establishment, sprouts, mistletoe/NISI, root disease, cover, climate, economics and ORGANON adapters. |
| NVEL boundary | 423 | Preserve library internals and their existing module/data contracts. |
| ORGANON boundary | 178 | Preserve library internals, native arrays and persistent helper state. |

MOD08/09/25 are also cross-cutting requirements: a family assignment never
waives ENTRY, callback, exported-symbol or BLOCK DATA work. Expand each family
by named source/routine batches before migration.

Duplicate names across disjoint variant manifests are native implementations,
not duplicate definitions to coalesce. `DGCONS` illustrates an intentional
signature split: the common executable include has no argument, while EM's
include accepts IDTYPE. The index attaches both to the selected DGF host with
their own variant sets. DUBSCR, NATCRS and volume entry contracts likewise
remain selected/native contracts. A module must not unify them by padding or
casting arguments.

Name resolution also distinguishes COMMON/component arrays, local statement
functions and external functions: for example BC's BB coefficient array is
not NVEL's BB function. The real procedure arguments found in the live source
are RANN, ESRANN and SVRANN, passed at 98 sites. BACHLO's RANGEN dummy calls
need a checked callback contract under MOD08. Keep each stream's identity,
argument mutation and draw sequence. EXMIST's unreachable CBUFF references
belong to its ENTRY-buffer contract review, rather than an inferred callback
migration. Compiler/runtime calls and CFIM_DRIVER/FM_FOFEM/SQLite calls are
named boundaries in the index.

All existing top-level/ENTRY Fortran symbols are compatibility candidates,
including names not marked DLLEXPORT in source: GNU shared libraries expose
legacy externals without those annotations. Keep external adapters until
MOD09 accounts for their consumers. Preserve C BIND names, DEC aliases, hidden
character lengths, kinds, by-reference/value conventions and exported COMMON
backing storage. The inventory includes 1,317 distinct top-level procedure
names before ENTRY names; that is not a claim that each is a documented API.

## Shared-state ownership and lifetime

The index finds 99 named COMMON blocks in 103 source/scope layouts, assembled
from 116 COMMON statements. In particular DMMIST's fourteen statements append
one layout. Native dimensions stay tied to their selected PRGPRM, and TAPER
and SITEFPRM keep their separate declaration/alias views.

The owners below are proposed declaration/storage owners. They do not move
initialization into a module declaration. Preserve the existing linked process
storage during the principal migration, and retain the current process/stand/
cycle reset boundaries. A block can mix values with different reset times.
The source index supplies its actual writers and possible mutation sites;
each block batch must resolve member-level resets before moving declarations.

| Proposed owner | Complete COMMON membership |
|---|---|
| MOD22 tree/work | ARRAYS, WORKCM |
| MOD23 stand/cycle/input | CONTRL, CONCHR, PLOT, PLTCHR, OPCOM, OPCHR, GLBLCNTL, GLBLCNTLC, KEYCOM, CKEYFN, INTREECOM, QFACOM |
| MOD19 reports | OUTCOM, OUTCHR, SUMTAB, SCREEN, FVSSTDCM, GENRCM |
| MOD19 visualization | SVDATA, SVDEAD, SVRCOM |
| MOD19 database | DBSCOM, DBSCHR, DBSTK, DBCHR |
| MOD21 growth/calibration | COEFFS, CALCOM, CALDEN, CICOM, HTCAL, MULTCM, PDEN, SSTGMC, STDSTK |
| MOD21 regional/volume | BCPLT1, BCPLT2, BECREF, CRCOEFFS, CRCOM, DGCOEFFS, VARCOM, VARCHR, KOTCOM, V23DUB, V2PLOT, SECOM, TAPER, VOLMET, VOLSTD, VOLCHR, FVSVOLCOM |
| MOD20 fire/fuels | CFIMCOM, CRBCOM, FMCOM, FMFCOM, FMOPCM, FMSVCM, FMBLK2, CWDCOM |
| MOD20 establishment | ESCOM2, ESCOMN, ESHAP, ESHAP2, ESHOOT, ESTCOR, ESTREE, ESWSBW, HVDNCM |
| MOD20 mistletoe/NISI | MISTL, MISTL2, MISTL3, DMBCCM, DMMIST |
| MOD20 root disease | PRPDAT, PRPDATD, RDADD, RDARRY, RDCOM, RDCRY |
| MOD20 climate | CLIMATE, CLIMCHR |
| MOD20 cover | CVCOM |
| MOD20 economics | ECON, ECON1, ECONSAVE |
| MOD20 other extensions | EMCOM, FFCOM, TWIGCOM |
| MOD24 separate RNG streams | RANCOM, ESRNCM, DMRNCM, RRANN |
| ORGANON boundary/shared adapters | ORGANON, GGCOM, INFO, SITEFPRM |
| MOD16 unit-system selector | METRIC |

PRGPRM, FMPARM and declaration-only parameter definitions form the configuration
layer. Preserve compiled VARIANTCD/LMETRICVAR and every native value/kind/bound;
runtime VARACD is not a substitute before GRINIT. Parameter statements and
derived-type definitions are retained in the include index. DATA alone does
not make a table immutable.

There are 873 scopes with saved, DATA-initialized or module-owned objects and
38 scopes with EQUIVALENCE. Persistent local objects retain their procedure
owner and existing process lifetime; they are not automatically promoted to
module variables. Host-associated state remains attached to its host. The
three mutable NVEL module owners are DEBUG_MOD, R8VLIST and VOLINPUT_MOD;
their imported users/read/write sites are indexed separately. CHARMOD,
CLKCOEF_MOD and MRULES_MOD supply types, rather than mutable instances.
Existing mFVS modules can still use COMMON within their procedures, so an
empty module-global list does not establish a stateless procedure.

Initialization owners include the selected BLKDAT/CUBRDS/FMCBLK/RDBLK1 units,
KEYWDS, SVBLKD, ESBLKD, DMBLKD, DBSBLKD and SETGLBLCNTL. Preserve their linkage
and startup timing. GRINIT, keyword processing and extension initialization
own stand setup; cycle drivers and extension routines own later updates.
RANSED/RANNGET/RANNPUT and the establishment/SVS/root-disease equivalents
preserve separate seed/reset contracts. Do not replace an initialization
owner with a DATA/default initializer merely because both produce zero in
one test.

Database/file state requires both Fortran and foreign owners. DBSCOM/DBSTK,
CONTRL and GLBLCNTL carry handles, units, case/stand controls and filenames;
`dbsqlite/fvsqlite3.c` owns the process connection/statement arrays dbset and
stmtset. DBSTABLES getters share that statement slot with writers. Preserve
DBSOPEN/DBSCLOSE, stand/case creation, API stop-point access and error cleanup.
CFIM/FOFEM/SQLite private state remains in its named library boundary.

## Aliases, serialization and deferred contracts

GETSTD/PUTSTD alias IOSUM through ROSUM, seeds through two-real buffers,
IDTREE through RDTREE, and WK6 through real/logical/integer work arrays.
IOSUM is 22 rows while the legacy ROSUM view is 20; preserve the existing
packing and transfer counts until MOD17 has an explicit compatibility design.
An alias view is not permission to change a restart format or array stride.

The serializer map also includes `pg/putgetsubs`, cover/economics/fire
GET/PUT pairs, `clim/base/clputget`, `dbsqlite/dbsppget`/`dbsppput`, mistletoe
MSPPGT/MSPPPT, root-disease RDPPGT/RDPPPT and native VARGET/VARPUT. Their actual
primitive transfer calls and buffer arguments are in the procedure index.
Other alias sites include compression/statistics/tree workspaces, label/
expression buffers, BC fitted tables, establishment plot arrays, SVS work
buffers, metric stand input, root-disease seed packing and HTGF tables.
No migration may create a second backing store and synchronize copies.

The corrected reference retains known defects. These are bounded exclusions
or prerequisites for their affected batches, not prerequisites for ISTFNB:

- Bug 66: transaction failure/ownership behavior before database resource-state
  migration or a batch that changes those error paths.
- Bugs 52/53 and the remaining R9DIB issue in bug 6: undefined/error inputs in
  the NVEL boundary. Preserve the documented direct-test controls and limits.
- Bug 13 and the remaining undefined/debug paths in bugs 8, 17–19, 36–39,
  45–47 and 56: review the particular ENTRY/procedure batch; do not add casts,
  blanket initialization or permissive interfaces to force it through.
- Bug 63: GNU initialization flags miss locals in procedures with ENTRY;
  ordinary output agreement is not a proof that those locals are defined.
- Bug 60: RDPR's saved LYAREA is deliberately absent from restart data. The
  first report in a new process starts from zero. Existing IE restarted and
  continuous runs also differ in six tables; preserve each reference path
  rather than claiming those two modes are equivalent.

Deterministic equation/unit/report defects can remain frozen during an
independent structural batch. Its affected paths still need defined,
repeatable comparisons. Record any newly established defect separately.

## Target module layers and first pilot

Arrows mean imports. Callback contracts belong below implementations, and
lifecycle code initializes data owners, keeping the proposed dependencies
acyclic:

```mermaid
flowchart TD
  API[External compatibility adapters] --> Run[Stand and cycle orchestration]
  Run --> IO[Input and reporting]
  Run --> Ext[Extension procedures]
  Run --> Eq[Native equations]
  Ext --> Eq
  Ext --> State[State owners and callback contracts]
  IO --> State
  Eq --> State
  IO --> Helpers[Leaf helpers]
  Eq --> Helpers
  Eq --> Foreign[Foreign-library adapters]
  IO --> Foreign
  Foreign --> State
  State --> Config[Native configuration and types]
```

The MOD07 pilot is **ISTFNB only**, selected by all 24 variants. It is a
default-integer function of one `character(len=*)` argument, with one internal
call at `vbase/sumhed.f90`. It has no includes, callees, ENTRY, COMMON, SAVE,
DATA, RNG or argument writes. ALGSLP was considered but has 164 call sites in
46 source files; it is a later batch.

Use a new `base/istfnb_mod.f90` containing ISTFNB in ISTFNB_MOD, with private
defaults and only ISTFNB public. Update SUMHED with `use ISTFNB_MOD, only:
ISTFNB`, removing its conflicting local type declaration of that name. Keep
`base/istfnb.f90` as the external compatibility wrapper, importing the module
function under a local alias and preserving its integer result and assumed-
length character ABI. Leave ISTLNB, SUMHED's COMMON declarations and its
formatting unchanged. Both sources remain selected in every manifest.

Pilot assessment ran 87,381 direct strings against Fortran VERIFY, including
zero length, blanks, ordinary characters, tabs and NULs, with input preservation
and diagnostic flags. Five title/header cases compiled and ran through the
real SUMHED in all 24 variants; only REVISE was a fixed test stub. Screen
output is not part of the suite's normal file comparison, so MOD07 must retain
a focused SUMHED output check as well as the full corrected/immediate suites.
Add its incompatible-call compiler control during MOD07. No procedure has
been moved and no module or state owner has been implemented in Phase 1.
