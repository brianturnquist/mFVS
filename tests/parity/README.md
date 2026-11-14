# Temporary original-F77 comparisons

These reusable tools build isolated original/current/candidate versions and
compare their outputs. Test results and build copies are disposable. Keep the
cleanup checklist and bug descriptions; do not commit generated reports,
archives, logs, compiler dumps or run metadata.

The original reference is main revision
`c210536536d81fd9b46dccb9a571f0de1e55e944` with NVEL revision
`38548071d5aa652bb90c7f111f86b427f798a1c9`. Set the immediate pre-change main
revision explicitly. Both references must already be available locally.
Use Python 3.12+, GNU Fortran, a GNU C compiler, GNU make and Git.

## Run a comparison

From the repository root, choose temporary paths outside the repository:

```sh
python3 tests/parity/prepare_builds.py \
  --work-dir /private/tmp/mfvs-check-build \
  --evidence-dir /private/tmp/mfvs-check-results \
  --current-revision PRE_CHANGE_COMMIT --variants ie --jobs 4

python3 tests/parity/run_cases.py \
  --evidence-dir /private/tmp/mfvs-check-results --cases ie-iet01

python3 tests/parity/compare_outputs.py \
  --runs /private/tmp/mfvs-check-results/runs.json \
  --left original --right candidate --case ie-iet01 \
  --report /private/tmp/mfvs-check-results/comparison.json
```

Request every affected variant and its cases. Compare all three pairs:
original/current, original/candidate and current/candidate. Add focused cases
when the standard deck does not reach the changed branch. The existing
`--evidence-dir` option names the temporary result directory; it does not
require retaining those files.

The comparator checks output inventory, exit status, text and every SQLite
schema/typed row. It normalizes only recognized version/date/time headers and
known CaseID UUIDs; REAL values are compared exactly. Review program errors
and simulation progress as well as the comparison result. BC/ON currently
stop during keyword input, so matching those failures does not demonstrate
simulation parity. Leave model bug fixes for separate work.

`check_comparator.py` can check the comparator against deliberately changed
IE outputs using the same temporary paths. Source snapshots omit historical
generated evidence, and the build preparer rejects output paths inside this
repository.

When finished, discard both temporary directories. Update the checklist and
record newly found bugs in [FOUND_BUGS.md](../../FOUND_BUGS.md). Do not create
another results archive or completion report.
