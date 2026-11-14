"""Check each C13-C18 extraction against literal F77 and immediate baseline.

Run this at each migration boundary. The candidate is the working source at
that boundary, while the retained build receipt identifies both baselines.
Real SQLite code handles every bind/step; only model context is supplied by
the FIA fixture. Mortality uses the complete selected public routine.
"""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
from pathlib import Path
import re
import shlex
import shutil
import sqlite3
import subprocess

from check_consolidation import fixed_form
from compare_outputs import value_token
from prepare_builds import ROOT, selected_inputs, sha256


def execute(command: list[str], directory: Path) -> bytes:
    result = subprocess.run(command, cwd=directory, capture_output=True)
    log = result.stdout + result.stderr
    if result.returncode:
        (directory / 'failed-build.log').write_bytes(log)
        raise RuntimeError(f"Build failed in {directory}: {log[-4000:].decode(errors='replace')}")
    return log


def literal_database(path: Path) -> dict:
    """Compare exact schemas and typed rows, including the fixture's CaseID.

    No UUID or report-header normalization is needed for these direct tests.
    REAL values use the same exact hexadecimal tokens as the main comparator.
    """
    with sqlite3.connect(path.resolve().as_uri() + '?mode=ro', uri=True) as connection:
        schema = connection.execute('select type,name,tbl_name,sql from sqlite_master order by type,name').fetchall()
        tables = {}
        for name, in connection.execute("select name from sqlite_master where type='table' order by name"):
            cursor = connection.execute('select * from "' + name + '"')
            rows = sorted(json.dumps([value_token(v) for v in row]) for row in cursor)
            tables[name] = {'columns': [c[0] for c in cursor.description], 'rows': len(rows),
                            'sha256': hashlib.sha256('\n'.join(rows).encode()).hexdigest()}
        return {'schema': schema, 'tables': tables, 'user_version': connection.execute('pragma user_version').fetchone()[0]}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    parser.add_argument('--baseline-evidence', type=Path, required=True)
    parser.add_argument('--work-dir', type=Path, required=True)
    parser.add_argument('--task', choices=[f'C{i}' for i in range(13, 19)], required=True)
    parser.add_argument('--variants', nargs='+')
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((args.baseline_evidence / 'builds.json').read_text())
    # The previous candidate is the committed, immediate pre-cleanup baseline.
    snapshots = {'original': Path(builds['states']['original']['snapshot']),
                 'current': Path(builds['states']['candidate']['snapshot']), 'candidate': ROOT}
    plan = json.loads((evidence / 'plan.json').read_text())
    task = next(t for t in plan['tasks'] if t['id'] == args.task)
    # Check all selected model/build inputs against HEAD, rather than assuming
    # that the previous working snapshot became the immediate baseline.
    baseline_inputs = {b['variant']: b['inputs'] for b in builds['builds'] if b['state'] == 'candidate'}
    entries = subprocess.check_output(['git', '-C', str(ROOT), 'ls-tree', '-rz', plan['before_revision']]).split(b'\0')
    blobs = {}
    for entry in entries:
        if entry:
            metadata, name = entry.split(b'\t', 1)
            if metadata.split()[1] == b'blob':
                blobs[name.decode()] = metadata.split()[2].decode()
    checked = set()
    for v in task['consumers']:
        assert selected_inputs(snapshots['current'], v) == baseline_inputs[v]
        for relative in baseline_inputs[v]:
            if relative in blobs and relative not in checked:
                data = (snapshots['current'] / relative).read_bytes()
                assert hashlib.sha1(b'blob '+str(len(data)).encode()+b'\0'+data).hexdigest() == blobs[relative], relative
                checked.add(relative)
    compiler = builds['tools']['fortran']['path']
    flags = shlex.split(builds['fflags']) + ['-fcheck=bounds']
    report = {'task': args.task, 'baseline_revision': plan['before_revision'],
              'baseline_builds_sha256': sha256(args.baseline_evidence / 'builds.json'),
              'flags': flags, 'context': 'Real selected writers/SQLite or full mortality routine; see retained driver',
              'runs': [], 'comparisons': []}
    fia = args.task in ('C13', 'C14')
    template_path = Path(__file__).parent / 'fixtures' / ('c13-c14-fia-bindings.f90.in' if fia else 'c15-c18-mortality.f90.in')
    report['fixture_sha256'] = sha256(template_path)
    report['baseline_selected_git_blobs_verified'] = len(checked)
    for variant in task['consumers']:
        if args.variants and variant not in args.variants:
            continue
        identities = {}
        for state, snapshot in snapshots.items():
            manifest = snapshot / 'bin' / f'FVS{variant}_sourceList.txt'
            paths = [(manifest.parent / p).resolve() for p in manifest.read_text().split()]
            by_name = {p.stem.upper(): p for p in paths}
            fixed = state == 'original'
            directory = args.work_dir.resolve() / 'targeted-build' / args.task / state / variant
            directory.mkdir(parents=True, exist_ok=True)
            headers = directory / 'includes'
            headers.mkdir(exist_ok=True)
            classification = snapshot / 'bin/fortran_includes.txt'
            includes = set(classification.read_text().split()) if classification.exists() else set()
            copied = []
            for p in paths:
                if p.suffix.lower() in ('.f77', '.inc', '.h') or str(p.relative_to(snapshot)) in includes:
                    shutil.copy2(p, headers / p.name)
                    copied.append(p)
            if fia:
                sources = [by_name[n] for n in ('DBS_FIAVBC_TRLS', 'DBS_FIAVBC_ATRTLS', 'DBS_FIAVBC_CUTLST')]
            else:
                sources = [p for p in paths if str(p.relative_to(snapshot)).lower().replace('.f90', '.f') in
                           {s.lower().replace('.f90', '.f') for s in task['consumers'][variant]}]
                assert len(sources) == 1
            if task['helper'] in by_name:
                sources.append(by_name[task['helper']])
            driver = template_path.read_text()
            if not fia:
                driver = driver.replace('@ROUTINE@', sources[0].stem.upper())
            for include in re.findall(r"include '([^']+)'", driver):
                driver = driver.replace(f"'{include}'", f"'{by_name[Path(include).stem.upper()].name}'")
            driver_path = directory / ('driver.f' if fixed else 'driver.f90')
            driver_path.write_text(fixed_form(driver) if fixed else driver)
            commands = []
            cobjects = []
            cinputs = []
            if fia:
                cinputs = [snapshot / 'dbsqlite' / n for n in ('fvsqlite3.c', 'sqlite3.c')]
                # Compile each exact C input/flag identity once; retain the
                # commands and hashes in every consumer's receipt.
                identity = hashlib.sha256((''.join(sha256(p) for p in cinputs) + builds['cflags'] + builds['tools']['c']['version']).encode()).hexdigest()
                cdir = args.work_dir.resolve() / 'sqlite-objects' / identity
                cdir.mkdir(parents=True, exist_ok=True)
                for p in cinputs:
                    obj = cdir / (p.stem + '.o')
                    command = [builds['tools']['c']['path'], *shlex.split(builds['cflags']), '-c', str(p), '-o', str(obj)]
                    commands.append(command)
                    if not obj.exists():
                        execute(command, cdir)
                    cobjects.append(obj)
            command = [compiler, *flags, '-I', str(headers), str(driver_path), *map(str, sources), *map(str, cobjects), '-o', str(directory / 'check')]
            commands.append(command)
            log = execute(command, directory)
            logpath = evidence / f'extract-{args.task}-{state}-{variant}.log.gz'
            logpath.write_bytes(gzip.compress(log, mtime=0))
            inputs = {str(p.relative_to(snapshot)): sha256(p) for p in sources + copied + cinputs}
            for case in ([0] if fia else range(1, 13)):
                run_dir = args.work_dir.resolve() / 'runs/targeted' / args.task / state / variant / str(case)
                run_dir.mkdir(parents=True, exist_ok=False)
                shutil.copy2(driver_path, run_dir / driver_path.name)
                run = subprocess.run([str(directory / 'check'), str(case)], cwd=run_dir, capture_output=True, timeout=45)
                (run_dir / 'stdout.txt').write_bytes(run.stdout)
                (run_dir / 'stderr.txt').write_bytes(run.stderr)
                (run_dir / 'returncode.txt').write_text(str(run.returncode)+'\n')
                if run.returncode:
                    raise RuntimeError(f'Fixture failed: {args.task}/{state}/{variant}/{case}: {run.stderr[-2000:]}')
                outputs = {p.name: sha256(p) for p in sorted(run_dir.iterdir()) if p.name != driver_path.name and p.suffix != '.db'}
                if fia:
                    dbs = sorted(run_dir.glob('*.db'))
                    assert len(dbs) == 12
                    outputs['databases'] = {p.name: literal_database(p) for p in dbs}
                    # Require populated real tables; equality of empty DBs
                    # would not validate any binding or selection behavior.
                    counts = {}
                    for p in dbs:
                        with sqlite3.connect(p) as connection:
                            tables = connection.execute("select name from sqlite_master where type='table'").fetchall()
                            counts[p.name] = {t[0]: connection.execute('select count(*) from "'+t[0]+'"').fetchone()[0] for t in tables}
                    assert all(not counts[f'writer{w}-mode{m}.db'] for w in (1,2,3) for m in (0,1))
                    for w in (1,2,3):
                        expected = (6 if w == 1 else 3)
                        assert list(counts[f'writer{w}-mode2.db'].values()) == [expected], counts
                        assert list(counts[f'writer{w}-mode3.db'].values()) == [4 if w == 1 else 3], counts
                    outputs['row_counts'] = counts
                key = (state, case)
                identities[key] = outputs
                report['runs'].append({'state': state, 'variant': variant, 'case': case, 'directory': str(run_dir),
                                       'driver_sha256': sha256(driver_path),
                                       'binary_sha256': sha256(directory / 'check'), 'compiled_inputs': inputs,
                                       'commands': commands, 'outputs': outputs, 'returncode': run.returncode})
        for case in ([0] if fia else range(1, 13)):
            for left, right in (('original', 'current'), ('original', 'candidate'), ('current', 'candidate')):
                equal = identities[left, case] == identities[right, case]
                report['comparisons'].append({'variant': variant, 'case': case, 'left': left, 'right': right, 'passed': equal})
                if not equal:
                    (evidence / f'targeted-{args.task}.json').write_text(json.dumps(report, indent=2)+'\n')
                    raise RuntimeError(f'Output mismatch: {args.task}/{variant}/{case}: {left}/{right}')
        print(args.task, variant, 'passed', flush=True)
    report['passed'] = all(c['passed'] for c in report['comparisons'])
    (evidence / f'targeted-{args.task}.json').write_text(json.dumps(report, indent=2)+'\n')
    print(args.task, len(report['runs']), 'runs;', len(report['comparisons']), 'exact comparisons passed', flush=True)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
