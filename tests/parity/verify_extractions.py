"""Verify extraction scope, literal executable bodies, selection and recovery.

Unlike whole-file consolidation, these tasks retain the public variant
routines. Expand each new helper call back into its literal body and compare
the complete executable caller with both original F77 and immediate baseline.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile

from prepare_builds import ROOT, selected_inputs, sha256


def normalized(path: Path) -> str:
    lines = []
    for line in path.read_text().splitlines():
        if path.suffix.lower() == '.f':
            if line[:1].upper() in ('C', '*', '!'):
                continue
            line = line[:5].strip() + line[6:72]
        # Preserve every character within quoted literals. Only Fortran
        # whitespace, comments, continuation markers and keyword case vary.
        parts = re.findall(r"'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\"|[^'\"]+", line)
        for part in parts:
            if part.startswith(("'", '"')):
                lines.append(part)
            else:
                code, marker, _ = part.partition('!')
                lines.append(re.sub(r'\s+|&', '', code).upper())
                if marker:
                    break
    return ''.join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    parser.add_argument('--baseline-evidence', type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    plan = json.loads((evidence / 'plan.json').read_text())
    builds = json.loads((args.baseline_evidence / 'builds.json').read_text())
    original = Path(builds['states']['original']['snapshot'])
    baseline = Path(builds['states']['candidate']['snapshot'])
    report = {'before_revision': plan['before_revision'], 'tasks': [], 'passed': False}
    db = normalized(ROOT / 'dbsqlite/dbs_fiavbc_bindtree.f90')
    db = db[db.index('DDBH=DBH(I)'):db.rindex('RETURNEND')]
    mortality = normalized(ROOT / 'vbase/mortalloc.f90')
    mortality = mortality[mortality.index('NPASS=IFIX(TOKILL/PASS1)+1'):]
    allowed = {s for t in plan['tasks'] for s in t['sources']}
    helpers = {t['canonical'] for t in plan['tasks']}
    manifests = {f'bin/FVS{v}_sourceList.txt' for t in plan['tasks'] for v in t['consumers']}
    model_count = 0
    entries = subprocess.check_output(['git', '-C', str(ROOT), 'ls-tree', '-rz', plan['before_revision']]).split(b'\0')
    selected_baseline = {b['variant']: b['inputs'] for b in builds['builds'] if b['state'] == 'candidate'}
    blobs = {}
    for entry in entries:
        if not entry:
            continue
        metadata, name = entry.split(b'\t', 1)
        if metadata.split()[1] != b'blob':
            continue
        relative = name.decode()
        blobs[relative] = metadata.split()[2].decode()
        if Path(relative).suffix.lower() not in ('.f90', '.f', '.f77', '.inc', '.c', '.h') or relative.startswith('tests/') or relative in allowed:
            continue
        data = (ROOT / relative).read_bytes()
        assert hashlib.sha1(b'blob '+str(len(data)).encode()+b'\0'+data).hexdigest() == blobs[relative], relative
        model_count += 1
    checked = set()
    for variant, inputs in selected_baseline.items():
        assert selected_inputs(baseline, variant) == inputs
        for relative in inputs:
            if relative in blobs and relative not in checked:
                data = (baseline / relative).read_bytes()
                assert hashlib.sha1(b'blob '+str(len(data)).encode()+b'\0'+data).hexdigest() == blobs[relative], relative
                checked.add(relative)
    report['baseline_selected_git_blobs_verified'] = len(checked)
    report['other_model_sources_includes_unchanged'] = model_count
    report['nvel_head'] = subprocess.check_output(['git', '-C', str(ROOT/'volume/NVEL'), 'rev-parse', 'HEAD'], text=True).strip()
    assert report['nvel_head'] == plan['before_nvel']
    assert not subprocess.check_output(['git', '-C', str(ROOT/'volume/NVEL'), 'status', '--porcelain'])
    report['nvel_unchanged'] = True
    for task in plan['tasks']:
        item = {'id': task['id'], 'helper': task['canonical'], 'callers': []}
        for relative in task['sources']:
            old = normalized(baseline / relative)
            upstream = normalized(original / relative.replace('.f90', '.f'))
            candidate = normalized(ROOT / relative)
            public_signature = candidate[:candidate.index('IMPLICITNONE')]
            assert public_signature == old[:old.index('IMPLICITNONE')]
            assert public_signature == upstream[:upstream.index('IMPLICITNONE')]
            if task['id'] in ('C13', 'C14'):
                marker = {'dbs_fiavbc_trls': 'IF(IVBCTRELST.EQ.0)RETURN',
                          'dbs_fiavbc_atrtls': 'IF(IVBCATRLST.EQ.0)RETURN',
                          'dbs_fiavbc_cutlst': 'IF(IVBCCUTLST.EQ.0)RETURN'}[Path(relative).stem]
                call = 'CALLDBS_FIAVBC_BINDTREE(I,ITRNK,COLNUMBER,IRET)'
                expanded = candidate[candidate.index(marker):].replace(call, db)
                expected_calls = 2 if relative.endswith('dbs_fiavbc_trls.f90') else 1
                assert candidate.count(call) == expected_calls
            else:
                marker = 'IF(DEBUG)WRITE(JOSTND,20)ICYC,TOKILL'
                # The earlier free-form modernization replaced exactly this
                # terminal labeled DO with DO/END DO, retaining its 150
                # CONTINUE target for the internal GO TO. No math changes.
                assert upstream.count('DO150I=1,ITRN') == 1
                assert upstream.count('150CONTINUE') == 1
                upstream = upstream.replace('DO150I=1,ITRN', 'DOI=1,ITRN').replace('150CONTINUE', '150CONTINUEENDDO')
                call = 'CALLMORTALLOC(TOKILL,DEBUG,SUMKIL,PASS1,TEMKIL,JPASS,EFFTR,TEMWK2)RETURNEND'
                expanded = candidate[candidate.index(marker):].replace(call, mortality)
                assert candidate.count(call) == 1
            assert expanded == old[old.index(marker):], relative+' baseline executable mismatch'
            assert expanded == upstream[upstream.index(marker):], relative+' original executable mismatch'
            item['callers'].append({'source': relative, 'before_sha256': sha256(baseline/relative),
                                    'original_sha256': sha256(original/relative.replace('.f90','.f')),
                                    'after_sha256': sha256(ROOT/relative),
                                    'public_signature_unchanged': public_signature,
                                    'expanded_executable_sha256': hashlib.sha256(expanded.encode()).hexdigest(),
                                    'original_and_baseline_executable_equal': True,
                                    'original_loop_normalization': 'DO 150 I=1,ITRN / 150 CONTINUE becomes DO I=1,ITRN / 150 CONTINUE / END DO' if task['id'] not in ('C13','C14') else None,
                                    'recovery': 'git show '+plan['before_revision']+':'+relative})
        report['tasks'].append(item)
    # Replay the six saved, sequential patches and validate the source hashes
    # at every task boundary, then compare the final tree with the checkout.
    with tempfile.TemporaryDirectory(prefix='mfvs-extraction-replay-') as temporary:
        replay = Path(temporary)
        for relative in allowed | manifests:
            p = replay / relative
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_bytes((baseline / relative).read_bytes())
        for task in plan['tasks']:
            command = ['patch', '--batch', '-p1', '-i', str(evidence / (task['id']+'.patch'))]
            result = subprocess.run(command, cwd=replay, capture_output=True)
            assert not result.returncode, result.stdout+result.stderr
            recorded = json.loads((evidence/(task['id']+'-extraction.json')).read_text())
            for relative, digest in recorded['after_sources'].items():
                assert sha256(replay/relative) == digest, (task['id'], relative)
        for relative in allowed | helpers | manifests:
            assert (replay/relative).read_bytes() == (ROOT/relative).read_bytes(), relative
    report['six_stage_patch_replay'] = True
    report['selection'] = {}
    for variant in sorted(selected_baseline):
        selected = selected_inputs(ROOT, variant)
        added = set(selected)-set(selected_baseline[variant])
        expected_helpers = {t['canonical'] for t in plan['tasks'] if variant in t['consumers']}
        assert added == expected_helpers, (variant, added)
        assert not set(selected_baseline[variant])-set(selected), variant
        changed = {p for p in selected if p in selected_baseline[variant] and selected[p] != selected_baseline[variant][p]}
        expected_callers = {s for t in plan['tasks'] for s in t['consumers'].get(variant, [])}
        assert changed == expected_callers | {f'bin/FVS{variant}_sourceList.txt'}, (variant, changed)
        paths = (ROOT/f'bin/FVS{variant}_sourceList.txt').read_text().split()
        assert len(paths) == len(set(paths))
        for helper in expected_helpers:
            assert paths.count('../'+helper) == 1
            name = Path(helper).stem.upper()
            definitions = sum(len(re.findall(r'(?im)^\s*subroutine\s+'+name+r'\b', (ROOT/'bin'/p).read_text(errors='replace'))) for p in paths if p.endswith('.f90'))
            assert definitions == 1, (variant, name, definitions)
        report['selection'][variant] = {'added_helpers': sorted(added), 'changed_inputs': sorted(changed), 'helper_definitions_once': True}
    report['lines_before'] = sum(len((baseline/s).read_text().splitlines()) for s in allowed)
    report['lines_after'] = sum(len((ROOT/s).read_text().splitlines()) for s in allowed | helpers)
    report['lines_removed_net'] = report['lines_before']-report['lines_after']
    report['passed'] = True
    (evidence/'extractions.json').write_text(json.dumps(report, indent=2)+'\n')
    print('PASS: expanded original/baseline statements, six-stage patch replay, all 24 selections and protected model inputs')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
