"""Reproduce the deferred FIA statement-lifecycle concern without model edits."""
from __future__ import annotations
import argparse
import json
from pathlib import Path
import subprocess
from check_consolidation import fixed_form
from prepare_builds import ROOT, sha256


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    parser.add_argument('--work-dir', type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    direct = json.loads((evidence/'targeted-C14.json').read_text())
    fixture = ROOT/'tests/parity/fixtures/c13-c14-sqlite-lifecycle.c'
    runs = []
    for state in ('original', 'current', 'candidate'):
        record = next(x for x in direct['runs'] if x['state'] == state and x['variant'] == 'ie')
        command = record['commands'][-1].copy()
        old = Path(command[command.index('-I')+2])
        fixed = state == 'original'
        directory = args.work_dir.resolve()/'runs/targeted/lifecycle'/state
        directory.mkdir(parents=True, exist_ok=False)
        # Extend the retained direct driver only with observations and a
        # consecutive CUTLST/ATRTLS sequence, matching CUTS's call order.
        text = old.read_text()
        indent = '      ' if fixed else '  '
        declaration = 'integer :: fsql3_open,fsql3_close,fsql3_finalize'
        text = text.replace(declaration, declaration+'\n'+indent+'integer :: test_sqlite_lifecycle,NSTMT\n'+indent+'integer :: test_sqlite_cleanup')
        chain = """
  ICYC = 1
  RC = fsql3_open(IoutDBref,'chain.db'//CHAR(0))
  if (RC.ne.0) stop 94
  call DBS_FIAVBC_CUTLST
  call DBS_FIAVBC_ATRTLS
  RC = test_sqlite_lifecycle(IoutDBref,NSTMT)
  write(*,*) 'CHAIN BEFORE FINALIZE',NSTMT,RC
  RC = fsql3_finalize(IoutDBref)
  RC = test_sqlite_lifecycle(IoutDBref,NSTMT)
  write(*,*) 'CHAIN AFTER FINALIZE',NSTMT,RC
  RC = test_sqlite_cleanup(IoutDBref)
  RC = fsql3_close(IoutDBref)
  if (RC.ne.0) stop 95
"""
        marker = indent+"open(31,file='state.bin'"
        assert marker in text
        text = text.replace(marker, (fixed_form(chain) if fixed else chain)+marker, 1)
        driver = directory/old.name
        driver.write_text(text)
        obj = directory/'probe.o'
        ccommand = ['/opt/homebrew/bin/gcc-15', '-I', str(ROOT/'dbsqlite'), '-c', str(fixture), '-o', str(obj)]
        subprocess.run(ccommand, check=True, capture_output=True)
        command[command.index(str(old))] = str(driver)
        command[-1] = str(directory/'check')
        command.insert(-2, str(obj))
        result = subprocess.run(command, cwd=directory, capture_output=True)
        (directory/'build.log').write_bytes(result.stdout+result.stderr)
        if result.returncode:
            raise RuntimeError(result.stderr.decode(errors='replace'))
        result = subprocess.run([str(directory/'check')], cwd=directory, capture_output=True)
        assert result.returncode == 0, result.stderr
        (directory/'stdout.txt').write_bytes(result.stdout)
        (directory/'stderr.txt').write_bytes(result.stderr)
        observations = [line.strip().split() for line in result.stdout.decode().splitlines() if 'CHAIN' in line]
        assert observations == [['CHAIN','BEFORE','FINALIZE','2','5'],['CHAIN','AFTER','FINALIZE','1','5']], observations
        runs.append({'state':state, 'directory':str(directory), 'driver_sha256':sha256(driver),
                     'probe_sha256':sha256(fixture), 'binary_sha256':sha256(directory/'check'),
                     'compiled_inputs':record['compiled_inputs'], 'commands':[ccommand, command],
                     'returncode':result.returncode, 'observations':observations})
    (evidence/'sqlite-lifecycle.json').write_text(json.dumps({'runs':runs, 'equal':True,
        'status':'Pre-existing statement leak reproduced with normal CUTS writer order; no scientific-output impact demonstrated; deliberately unfixed'}, indent=2)+'\n')
    print('PASS: all three states retain two statements after CUTLST/ATRTLS, one after normal finalization; close returns SQLITE_BUSY=5')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
