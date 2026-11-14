"""Replay retained C13-C18 raw captures and comparisons from their archive."""
from __future__ import annotations
import argparse
import json
from pathlib import Path
import tarfile
import tempfile
from check_extractions import literal_database
from compare_outputs import compare_directories
from prepare_builds import sha256


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    archive = json.loads((evidence/'artifacts.json').read_text())
    assert sha256(evidence/archive['file']) == archive['sha256']
    report = {'archive_sha256_verified': True, 'integrated': [], 'direct': [], 'lifecycle': []}
    with tempfile.TemporaryDirectory(prefix='mfvs-extraction-replay-') as temporary:
        root = Path(temporary)
        with tarfile.open(evidence/archive['file']) as stream:
            stream.extractall(root, filter='data')
        runs = json.loads((evidence/'runs.json').read_text())
        entries = {(r['state'],r['case']):r for r in runs['runs']}
        for path in sorted((evidence/'comparisons').glob('*.json')):
            recorded = json.loads(path.read_text())
            left, right = [entries[recorded[s],recorded['case']] for s in ('left','right')]
            directories = [root/'runs'/r['state']/r['case'] for r in (left,right)]
            for r,d in zip((left,right),directories):
                for name, identity in r['outputs'].items():
                    assert sha256(d/name) == identity['sha256'], (path,name)
                for name,digest in runs['cases'][r['case']]['inputs'].items():
                    if name not in r['modified_inputs']:
                        assert sha256(d/name) == digest
            replayed = compare_directories(*directories,list(left['outputs']),list(right['outputs']))
            replayed.update({'case':recorded['case'], 'left':recorded['left'], 'right':recorded['right'],
                             'returncodes':[left['returncode'],right['returncode']]})
            assert json.loads(json.dumps(replayed)) == recorded, path
            assert replayed['equal']
            report['integrated'].append({'file':str(path.relative_to(evidence)), 'identical':True})
        for task in (f'C{i}' for i in range(13,19)):
            direct = json.loads((evidence/f'targeted-{task}.json').read_text())
            actual = {}
            for r in direct['runs']:
                d = root/'runs/targeted'/task/r['state']/r['variant']/str(r['case'])
                driver = next(d.glob('driver.*'))
                assert sha256(driver) == r['driver_sha256']
                for name,digest in r['outputs'].items():
                    if name not in ('databases','row_counts'):
                        assert sha256(d/name) == digest, (task,r['state'],name)
                if 'databases' in r['outputs']:
                    # JSON round-trip canonicalizes schema tuple/list shapes.
                    databases = json.loads(json.dumps({p.name:literal_database(p) for p in d.glob('*.db')}))
                    assert databases == r['outputs']['databases']
                actual[r['state'],r['variant'],r['case']] = r['outputs']
            for pair in direct['comparisons']:
                a,b = [actual[pair[s],pair['variant'],pair['case']] for s in ('left','right')]
                assert pair['passed'] and a == b
            report['direct'].append({'task':task,'runs':len(direct['runs']),'comparisons':len(direct['comparisons']),'identical':True})
        lifecycle = json.loads((evidence/'sqlite-lifecycle.json').read_text())
        for r in lifecycle['runs']:
            d = root/'runs/targeted/lifecycle'/r['state']
            assert sha256(next(d.glob('driver.*'))) == r['driver_sha256']
            observed = [line.strip().split() for line in (d/'stdout.txt').read_text().splitlines() if 'CHAIN' in line]
            assert observed == r['observations']
            report['lifecycle'].append({'state':r['state'],'identical':True})
    report['passed'] = True
    (evidence/'replay.json').write_text(json.dumps(report,indent=2)+'\n')
    print('PASS: archive integrity and exact replay of',len(report['integrated']),'integrated comparisons, all six direct fixtures and lifecycle evidence')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
