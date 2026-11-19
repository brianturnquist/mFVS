"""Regenerate the MOD03/MOD04 source index from every live variant manifest.

This is a conservative source index, not a Fortran semantic analyser. Function
references are resolved against known definitions; array/function ambiguities,
CPP alternatives and argument mutation through calls still need the named
batch's compiler/source audit. Include layouts remain separate source records.
Nothing is written to the checkout: --json is an optional stdout export, and
--procedure/--common expose the detailed index without retaining generated data.
"""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
from functools import lru_cache
import json
from pathlib import Path
import re
import sys

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
WORD = re.compile(r"\b[A-Z][A-Z0-9_]*\b", re.I)
STRINGS = re.compile(r"'([^']|'')*'|\"([^\"]|\"\")*\"")
DECL = re.compile(r"^(integer|real|double\s+precision|logical|character|complex|type\s*\(|class\s*\()", re.I)
START = re.compile(r"\b(subroutine|function|program|block\s*data|module)\s+([A-Z]\w*)", re.I)
REGIONS = {"ak", "bc", "bm", "ca", "ci", "cr", "cs", "ec", "em", "ie", "kt", "ls",
           "nc", "ne", "oc", "on", "op", "pn", "sn", "so", "tt", "ut", "wc", "ws"}
LEAVES = {"algslp", "algexp", "alglim", "algmod", "algpnt", "algprc", "algout", "algarg",
          "sort", "sorts", "sortin", "sortre", "istlnb", "istfnb", "istfnb_mod", "rdanuw", "fvs_truncate"}
API = {"main", "apisubs", "apisubsc", "cmdline", "dbstables", "version"}
RUNTIME = {"SLEEP", "GET_COMMAND_ARGUMENT", "DATE_AND_TIME", "RANDOM_SEED",
           "RANDOM_NUMBER", "FLUSH", "SYSTEM_CLOCK", "SYSTEM", "GETPID"}
FOREIGN = {"CFIM_DRIVER": "boundary:CFIM", "FM_FOFEM": "boundary:FOFEM"}
OWNERS = {
    "MOD22:tree-work": "ARRAYS WORKCM",
    "MOD23:stand-cycle": "CONTRL CONCHR PLOT PLTCHR OPCOM OPCHR GLBLCNTL GLBLCNTLC KEYCOM CKEYFN INTREECOM QFACOM",
    "MOD19:reports": "OUTCOM OUTCHR SUMTAB SCREEN FVSSTDCM GENRCM",
    "MOD19:stand-visualization": "SVDATA SVDEAD SVRCOM",
    "MOD19:database": "DBSCOM DBSCHR DBSTK DBCHR",
    "MOD21:growth-calibration": "COEFFS CALCOM CALDEN CICOM HTCAL MULTCM PDEN SSTGMC STDSTK",
    "MOD21:regional-volume": "BCPLT1 BCPLT2 BECREF CRCOEFFS CRCOM DGCOEFFS VARCOM VARCHR KOTCOM V23DUB V2PLOT SECOM TAPER VOLMET VOLSTD VOLCHR FVSVOLCOM",
    "MOD20:fire": "CFIMCOM CRBCOM FMCOM FMFCOM FMOPCM FMSVCM FMBLK2 CWDCOM",
    "MOD20:establishment": "ESCOM2 ESCOMN ESHAP ESHAP2 ESHOOT ESTCOR ESTREE ESWSBW HVDNCM",
    "MOD20:mistletoe": "MISTL MISTL2 MISTL3 DMBCCM DMMIST",
    "MOD20:root-disease": "PRPDAT PRPDATD RDADD RDARRY RDCOM RDCRY",
    "MOD20:climate": "CLIMATE CLIMCHR",
    "MOD20:cover": "CVCOM",
    "MOD20:economics": "ECON ECON1 ECONSAVE",
    "MOD20:other-extensions": "EMCOM FFCOM TWIGCOM",
    "MOD24:random-streams": "RANCOM ESRNCM DMRNCM RRANN",
    "boundary:ORGANON-state": "ORGANON GGCOM INFO SITEFPRM",
    "SP04:configuration": "METRIC",
}


def family(path: str) -> str:
    """Total, reviewable assignment; unknown new source roots fail the audit."""
    parts = path.lower().split("/")
    stem = Path(path).stem.lower()
    if path.startswith("volume/NVEL/"):
        return "boundary:NVEL"
    if parts[0] == "organon":
        return "boundary:ORGANON"
    if Path(path).suffix.lower() in (".c", ".cpp", ".h"):
        if parts[0] == "dbsqlite":
            return "boundary:SQLite-wrapper" if stem == "fvsqlite3" else "boundary:SQLite"
        if parts[:2] == ["fire", "fofem"]:
            return "boundary:FOFEM"
        if parts[:2] == ["fire", "cfim"]:
            return "boundary:CFIM"
        return "MOD09:C-API"
    if stem in API:
        return "MOD09:API"
    if "fire" in parts:
        return "MOD14:fire"
    if any(p in ("dbsqlite", "vdbsqlite") for p in parts):
        return "MOD11:database"
    for domain in ("estb", "strp", "mistoe", "newmist", "rd", "covr", "clim", "econ", "organon"):
        if any(p in (domain, "v" + domain) for p in parts):
            return "MOD14:" + domain
    if parts[0] == "pg":
        return "MOD13:restart"
    if (parts[0] in ("volume", "vvolume", "fiavbc") or parts[0] in {"v" + r for r in REGIONS}
            or any(p in REGIONS for p in parts[:-1])):
        return "MOD12:equations"
    if parts[0] in ("base", "vbase") or parts[:2] in (["metric", "base"], ["metric", "vbase"]):
        if stem in LEAVES:
            return "MOD10:helpers"
        if stem.startswith(("initre", "intree", "key", "displ", "sumout", "sumhed", "stats", "fvsstd", "ev", "alg")):
            return "MOD11:input-report-expression"
        return "MOD13:core"
    if parts[0] in ("common", "canada", "metric"):
        return "MOD21:variant-state"
    raise ValueError(f"Unassigned live source: {path}")


def split_fields(text: str, separator: str = ",") -> list[str]:
    masked = STRINGS.sub(lambda m: " " * len(m[0]), text)
    result, start, depth = [], 0, 0
    for index, char in enumerate(masked):
        depth += (char == "(") - (char == ")")
        if char == separator and depth == 0:
            result.append(text[start:index].strip())
            start = index + 1
    result.append(text[start:].strip())
    return [field for field in result if field]


@lru_cache(maxsize=None)
def statements(path: str) -> tuple[tuple[int, str], ...]:
    """Join free-form continuations without mistaking quoted ! or ; for code."""
    result, pending, first, quote = [], "", 0, ""
    for line_number, line in enumerate((ROOT / path).read_text(encoding="latin1").splitlines(), 1):
        if line.lstrip().startswith("#"):
            result.append((line_number, line.strip()))
            continue
        if not pending:
            first = line_number
        code, index = "", 0
        while index < len(line):
            char = line[index]
            if char == quote:
                if index + 1 < len(line) and line[index + 1] == quote:
                    code += line[index:index + 2]
                    index += 2
                    continue
                quote = ""
            elif not quote:
                if char == "!":
                    break
                if char in "'\"":
                    quote = char
            code += char
            index += 1
        code = code.strip()
        if not code:
            continue
        code = code.lstrip("&") if pending else code
        continued = code.endswith("&")
        if continued:
            code = code[:-1]
        pending += code + ("" if quote else " ")
        if not continued:
            for statement in split_fields(pending.strip(), ";"):
                result.append((first, re.sub(r"^\d+\s+", "", statement)))
            pending, quote = "", ""
    if pending:
        raise ValueError(f"Unfinished continuation: {path}:{first}")
    return tuple(result)


def declarations(code: str) -> dict[str, str]:
    if not DECL.match(code):
        return {}
    if "::" in code:
        body = code.split("::", 1)[1]
    else:
        old = re.match(r"^(?:integer|real|double\s+precision|logical|character|complex|type|class)"
                       r"\s*(?:\([^)]*\)|\*\s*(?:\d+|\([^)]*\)))?\s+(.+)$", code, re.I)
        if not old:
            return {}
        body = old[1]
    return {match[1].upper(): code for item in split_fields(body)
            if (match := re.match(r"([A-Z]\w*)", item, re.I))}


def starts_type(code: str) -> bool:
    return bool(re.match(r"^type\b\s*(?:,[^:]*)?(?:::)?\s*\w+\s*$", code, re.I))


def common_blocks(code: str) -> list[tuple[str, list[str]]]:
    if not re.match(r"^common\b", code, re.I):
        return []
    body = re.sub(r"^common\s*", "", code, flags=re.I)
    matches = list(re.finditer(r"/\s*(\w*)\s*/", body))
    if not matches:
        return [("<BLANK>", [re.match(r"\w+", s)[0].upper() for s in split_fields(body)])]
    return [(match[1].upper() or "<BLANK>",
             [re.match(r"\w+", s)[0].upper() for s in split_fields(
                 body[match.end():matches[i + 1].start() if i + 1 < len(matches) else len(body)].strip(" ,"))])
            for i, match in enumerate(matches)]


def scan(path: str) -> tuple[list[dict], list[dict], list[dict]]:
    units, stack, layouts, directives = [], [], [], []
    interfaces, type_depth = 0, 0
    for line, code in statements(path):
        lower = code.lower()
        if code.startswith("#"):
            directives.append({"line": line, "code": code})
            continue
        if re.match(r"^(abstract\s+)?interface\b", lower):
            interfaces += 1
        if interfaces:
            if stack:
                stack[-1]["interfaces"].append({"line": line, "code": code})
            if re.match(r"^end\s*interface\b", lower):
                interfaces -= 1
            continue
        if starts_type(code):
            type_depth += 1
        if type_depth:
            if stack:
                stack[-1]["derived_types"].append({"line": line, "code": code})
            if re.match(r"^end\s*type\b", lower):
                type_depth -= 1
            continue
        match = START.search(STRINGS.sub(lambda m: " " * len(m[0]), code))
        if (match and not re.match(r"^(end|call|if|module\s+procedure)\b", lower)
                and not STRINGS.sub("", code[:match.start()]).strip().startswith("!")):
            kind, name = re.sub(r"\s+", " ", match[1].lower()), match[2].upper()
            args = re.match(r"\s*\(([^)]*)\)", code[match.end():])
            unit = {"id": f"{path}:{line}:{name}", "source": path, "line": line,
                    "name": name, "kind": kind, "parent": stack[-1]["id"] if stack else None,
                    "arguments": [s.upper() for s in split_fields(args[1])] if args else [],
                    "signature": code, "family": family(path), "entries": [], "interfaces": [],
                    "declarations": {}, "derived_types": [], "includes": [], "uses": [], "external": [], "code": []}
            units.append(unit)
            stack.append(unit)
            continue
        if re.match(r"^end\s*(?:(subroutine|function|program|module|block\s*data)\b.*)?$", lower):
            if not stack:
                raise ValueError(f"Unmatched end: {path}:{line}")
            stack.pop()
            continue
        for name, members in common_blocks(code):
            layouts.append({"name": name, "members": members, "source": path, "line": line,
                            "scope": stack[-1]["id"] if stack else None, "code": code})
        if not stack:
            continue
        unit = stack[-1]
        unit["code"].append((line, code))
        unit["declarations"].update(declarations(code))
        if match := re.match(r"include\s*['\"]([^'\"]+)", code, re.I):
            unit["includes"].append(match[1])
        if match := re.match(r"entry\s+(\w+)\s*(?:\(([^)]*)\))?", code, re.I):
            unit["entries"].append({"name": match[1].upper(), "line": line,
                                    "arguments": split_fields((match[2] or "").upper())})
        if re.match(r"use\b", lower):
            unit["uses"].append(code)
        if re.search(r"\bexternal\b", lower):
            body = code.split("::")[-1] if "::" in code else re.sub(r"^external\s*", "", code, flags=re.I)
            unit["external"].extend(re.match(r"\w+", s)[0].upper() for s in split_fields(body))
    if stack or interfaces or type_depth:
        raise ValueError(f"Unclosed source scopes: {path}: {[u['name'] for u in stack]}")
    return units, layouts, directives


def build_index() -> dict:
    selected, views = defaultdict(set), {}
    for manifest in sorted((ROOT / "bin").glob("FVS??_sourceList.txt")):
        variant = manifest.name[3:5]
        paths = [(manifest.parent / name).resolve().relative_to(ROOT).as_posix() for name in manifest.read_text().split()]
        views[variant] = {Path(path).name.upper(): path for path in paths}
        for path in paths:
            selected[path].add(variant)
    include_paths = {(ROOT / name).resolve().relative_to(ROOT).as_posix() for name in
                     (ROOT / "bin/fortran_includes.txt").read_text().split()}
    files, units, layouts, empty = {}, [], [], []
    for path, variants in sorted(selected.items()):
        record = {"variants": sorted(variants), "family": family(path), "include": path in include_paths}
        record["abi_directives"] = [{"line": i, "code": line.strip()}
                                    for i, line in enumerate((ROOT / path).read_text(encoding="latin1").splitlines(), 1)
                                    if re.search(r"!DEC\$.*\b(DLLEXPORT|DLLIMPORT|ALIAS)\b", line, re.I)]
        files[path] = record
        if Path(path).suffix.lower() not in (".f90", ".inc"):
            continue
        if path in include_paths or Path(path).suffix.lower() == ".inc":
            decls, types, depth = {}, [], 0
            for line, code in statements(path):
                if starts_type(code):
                    depth += 1
                if depth:
                    types.append({"line": line, "code": code})
                    if re.match(r"^end\s*type\b", code, re.I):
                        depth -= 1
                    continue
                decls.update(declarations(code))
                for name, members in common_blocks(code):
                    layouts.append({"name": name, "members": members, "source": path, "line": line,
                                    "scope": None, "code": code})
            record["declarations"] = decls
            record["derived_types"] = types
            record["parameters"] = [{"line": line, "code": code} for line, code in statements(path)
                                    if re.search(r"\bparameter\b", code, re.I)]
            continue
        parsed, blocks, cpp = scan(path)
        units.extend(parsed)
        layouts.extend(blocks)
        record["cpp"] = cpp
        if not parsed:
            empty.append(path)
    if empty:
        raise ValueError(f"No unit recognized in live compilation sources: {empty}")
    # Repeated COMMON statements append storage in one scope. DMMIST has
    # fourteen fragments; they are one layout, not fourteen alternatives.
    joined = {}
    for block in layouts:
        key = (block["name"], block["source"], block["scope"])
        if key not in joined:
            joined[key] = {**block, "members": [], "statements": []}
        joined[key]["members"].extend(block["members"])
        joined[key]["statements"].append({"line": block["line"], "code": block["code"]})
    layouts = list(joined.values())
    by_id = {u["id"]: u for u in units}
    definitions = defaultdict(list)
    for unit in units:
        unit["variants"] = files[unit["source"]]["variants"]
        # Executable include fragments can define ENTRY points (DGCONS).
        # Attach those to their actual containing procedure, not to every
        # contained child that inherits the parent's declaration includes.
        fragments = {views[v][inc.upper()] for inc in unit["includes"] for v in unit["variants"]}
        for source in sorted(fragments):
            for line, code in statements(source):
                if match := re.match(r"entry\s+(\w+)\s*(?:\(([^)]*)\))?", code, re.I):
                    unit["entries"].append({"name": match[1].upper(), "line": line, "source": source,
                                            "variants": sorted(set(unit["variants"]) & set(files[source]["variants"])),
                                            "arguments": split_fields((match[2] or "").upper())})
        definitions[unit["name"]].append(unit["id"])
        for entry in unit["entries"]:
            if unit["id"] not in definitions[entry["name"]]:
                definitions[entry["name"]].append(unit["id"])
    function_names = {u["name"] for u in units if u["kind"] == "function"}
    function_names.update(e["name"] for u in units if u["kind"] == "function" for e in u["entries"])
    function_names.update(match[1].upper() for u in units for declaration in u["interfaces"]
                          if (match := re.search(r"\bfunction\s+(\w+)", declaration["code"], re.I)))
    for unit in units:
        ancestors, parent = [unit], unit["parent"]
        while parent:
            ancestors.append(by_id[parent])
            parent = by_id[parent]["parent"]
        imported, seen = set(), set()
        pending = [(inc, variant) for ancestor in ancestors
                                     for inc in ancestor["includes"] for variant in unit["variants"]]
        while pending:
            name, variant = pending.pop()
            path = views[variant].get(name.upper())
            if path is None:
                raise ValueError(f"Include outside selected view: {unit['id']}: {name} ({variant})")
            if (path, variant) in seen:
                continue
            seen.add((path, variant))
            imported.add(path)
            for _, code in statements(path):
                if match := re.match(r"include\s*['\"]([^'\"]+)", code, re.I):
                    pending.append((match[1], variant))
        unit["include_sources"] = sorted(imported)
        unit["host_scopes"] = [a["id"] for a in ancestors]
        # Shared declarations and host associations are indexed conservatively;
        # retain each native layout rather than merging its storage declaration.
        visible = [b for b in layouts if b["source"] in imported or b["scope"] in {a["id"] for a in ancestors}]
        unit["common"] = sorted({b["name"] for b in visible})
        names = {name for b in visible for name in b["members"]}
        entry_arguments = {name for entry in unit["entries"] for name in entry["arguments"]}
        arrays = {name for name, declaration in unit["declarations"].items()
                  if re.search(r"\bdimension\s*\(", declaration, re.I)
                  or re.search(r"\b" + name + r"\s*\(", declaration.split("::")[-1], re.I)}
        for _, code in unit["code"]:
            if re.match(r"^dimension\b", code, re.I):
                arrays.update(re.findall(r"\b(\w+)\s*\(", code.upper()))
        local_functions = []
        for line, code in unit["code"]:
            match = re.match(r"^(\w+)\s*\(([^()]*)\)\s*=(?!=)", code, re.I)
            if (match and match[1].upper() in unit["declarations"]
                    and all(re.fullmatch(r"[A-Z]\w*", arg, re.I) for arg in split_fields(match[2]))
                    and match[1].upper() not in arrays | names | set(unit["arguments"])):
                local_functions.append({"name": match[1].upper(), "line": line,
                                        "arguments": split_fields(match[2].upper()), "code": code})
        unit["statement_functions"] = local_functions
        local_names = {f["name"] for ancestor in ancestors for f in ancestor.get("statement_functions", [])}
        words, assigned, io_mutations, calls, data, saves, aliases = set(), set(), set(), [], [], [], []
        params, initialized = set(), set()
        for line, code in unit["code"]:
            masked = STRINGS.sub(lambda m: " " * len(m[0]), code).upper()
            if match := re.match(r"^(DATA|SAVE|EQUIVALENCE)\b", masked):
                {"DATA": data, "SAVE": saves, "EQUIVALENCE": aliases}[match[1]].append({"line": line, "code": code})
            if "PARAMETER" in masked:
                params.update(WORD.findall(masked))
            if DECL.match(code):
                if "::" in masked:
                    initialized.update(re.findall(r"(?:^|,)\s*(\w+)\s*(?:\([^)]*\))?\s*=(?!=)", masked.split("::", 1)[1]))
                continue
            if re.match(r"^(INCLUDE|USE|COMMON|DIMENSION|IMPLICIT|EXTERNAL|DATA|SAVE|EQUIVALENCE|PARAMETER)\b", masked):
                continue
            words.update(WORD.findall(masked))
            if match := re.match(r"(?:IF\s*\(.*\)\s*)?(\w+)\s*(?:\([^=]*\))?(?:%\w+(?:\([^=]*\))?)*\s*=(?!=)", masked):
                assigned.add(match[1])
            if re.match(r"^(READ|OPEN|INQUIRE|CLOSE|REWIND|BACKSPACE|ENDFILE)\b", masked):
                io_mutations.update(WORD.findall(masked))
            for match in re.finditer(r"\bCALL\s+(\w+)\b\s*(\()?|\b([A-Z]\w*)\s*\(", masked):
                target = match[1] or match[3]
                if match[3] and masked[:match.start(3)].rstrip().endswith("%"):
                    continue  # derived-type component/array, not a global procedure
                if not match[1] and target in arrays | names:
                    continue
                if (not match[1] and target not in function_names
                        and target not in local_names and target not in unit["external"]
                        and target not in unit["declarations"]
                        and not target.startswith("FSQL3_")):
                    continue
                has_arguments = bool(match[2] or match[3])
                depth, end = int(has_arguments), match.end()
                while depth and end < len(masked):
                    depth += (masked[end] == "(") - (masked[end] == ")")
                    end += 1
                actuals = split_fields(code[match.end():end - 1]) if has_arguments else []
                if (not match[1] and unit["declarations"].get(target, "").lower().startswith("character")
                        and len(actuals) == 1 and ":" in actuals[0]):
                    continue  # scalar character substring, not a function
                calls.append({"name": target, "line": line, "arguments": actuals,
                              "boundary": ("statement-function" if target in local_names
                                           else "procedure-dummy" if target in unit["arguments"]
                                           else "entry-dummy" if target in entry_arguments
                                           else "SQLite" if target.startswith("FSQL3_")
                                           else "compiler-runtime" if target in RUNTIME
                                           else FOREIGN.get(target)),
                              "definitions": [] if target in local_names else [uid for uid in definitions[target]
                                              if set(by_id[uid]["variants"]) & set(unit["variants"])]})
        imported_declarations = set()
        for source in sorted(imported):
            imported_declarations.update(files[source].get("declarations", {}))
            for line, code in statements(source):
                masked = STRINGS.sub(lambda m: " " * len(m[0]), code).upper()
                if match := re.match(r"^(DATA|SAVE|EQUIVALENCE)\b", masked):
                    {"DATA": data, "SAVE": saves, "EQUIVALENCE": aliases}[match[1]].append(
                        {"line": line, "source": source, "code": code})
                if "PARAMETER" in masked:
                    params.update(WORD.findall(masked))
                if DECL.match(code) and "::" in masked:
                    initialized.update(re.findall(r"(?:^|,)\s*(\w+)\s*(?:\([^)]*\))?\s*=(?!=)", masked.split("::", 1)[1]))
        unit["common_references"] = sorted(words & names)
        unit["common_assignments"] = sorted(assigned & names)
        unit["possible_io_mutations"] = sorted(io_mutations & names)
        unit["possible_call_mutations"] = sorted(names & {word for call in calls for arg in call["arguments"] for word in WORD.findall(arg.upper())})
        unit["calls"], unit["data"], unit["save"], unit["equivalence"] = calls, data, saves, aliases
        declared = set(unit["declarations"]) | imported_declarations
        saved_names = {word for s in saves for word in WORD.findall(s["code"].upper())}
        for initializer in data:
            masked = STRINGS.sub(lambda m: " " * len(m[0]), initializer["code"]).upper()
            objects = " ".join(masked.split("/")[::2])
            indices = set(re.findall(r"\b(\w+)\s*=", objects))
            saved_names.update(set(WORD.findall(objects)) - indices)
        if any(s["code"].lower().strip() == "save" for s in saves) or unit["kind"] == "module":
            saved_names.update(declared)
        unit["persistent_locals"] = sorted((saved_names | initialized) & declared - params - names - set(unit["arguments"]) - entry_arguments - {unit["name"]})
        external = {name for ancestor in ancestors for name in ancestor["external"]}
        unit["procedure_arguments"] = [{"callee": call["name"], "line": call["line"], "position": i + 1,
                                         "procedure": arg.upper(), "definitions": definitions.get(arg.upper(), [])}
                                        for call in calls for i, arg in enumerate(call["arguments"])
                                        if re.fullmatch(r"[A-Z]\w*", arg, re.I)
                                        and (arg.upper() in external
                                             or arg.upper() in definitions and arg.upper() not in unit["declarations"]
                                             and arg.upper() not in names)]
        unit["implicit"] = [code for _, code in unit["code"] if re.match(r"^implicit\b", code, re.I)]
        unit["visibility"] = [code for _, code in unit["code"] if re.match(r"^(private|public)\b", code, re.I)]
        unit["_access"] = {"references": sorted(words), "assignments": sorted(assigned),
                           "possible_io_mutations": sorted(io_mutations),
                           "possible_call_mutations": sorted({word for call in calls for arg in call["arguments"] for word in WORD.findall(arg.upper())})}
        unit["persistent_access"] = {key: sorted(set(values) & set(unit["persistent_locals"]))
                                     for key, values in unit["_access"].items()}
        del unit["code"]
    for block in layouts:
        owners = [owner for owner, members in OWNERS.items() if block["name"] in members.split()]
        if len(owners) != 1:
            raise ValueError(f"COMMON needs one reviewed owner: {block['name']}: {owners}")
        block["proposed_owner"] = owners[0]
        block["storage_lifetime"] = "existing linked process storage; preserve current stand/cycle reset sites"
        block["variants"] = files[block["source"]]["variants"]
        block["consumers"] = [u["id"] for u in units if block["name"] in u["common"]
                              and (block["source"] in u["include_sources"] or block["scope"] in u["host_scopes"])]
        block["declarations"] = {name: files[block["source"]].get("declarations", {}).get(name)
                                  or by_id.get(block["scope"], {}).get("declarations", {}).get(name)
                                  for name in block["members"]}
        # Kozak's LOG/VLM use ordinary implicit typing. Record that fact
        # explicitly rather than inventing declarations or treating missing
        # declarations as interchangeable with another TAPER layout.
        owner = by_id.get(block["scope"])
        if owner and not owner["implicit"]:
            for name, declaration in block["declarations"].items():
                if not declaration:
                    kind = "integer" if name[0] in "IJKLMN" else "real"
                    block["declarations"][name] = f"implicit default {kind} :: {name}"
    module_state = []
    for module in (u for u in units if u["kind"] == "module" and u["persistent_locals"]):
        members, users = set(module["persistent_locals"]), []
        for unit in units:
            hosts = [by_id[uid] for uid in unit["host_scopes"]]
            imports = [use for host in hosts for use in host["uses"]
                       if re.match(r"use\s*(?:,.*?::\s*)?" + module["name"] + r"\b", use, re.I)]
            if not imports and module["id"] not in unit["host_scopes"]:
                continue
            # Only names actually visible through ONLY/renaming are counted.
            visible = {name: name for name in members} if module["id"] in unit["host_scopes"] else {}
            for use in imports:
                if "only:" not in use.lower().replace(" ", ""):
                    visible.update({name: name for name in members})
                else:
                    for item in split_fields(re.split(r"only\s*:", use, flags=re.I)[1]):
                        local, _, original = item.upper().partition("=>")
                        original = original.strip() or local.strip()
                        if original in members:
                            visible[local.strip()] = original
            users.append({"unit": unit["id"], **{key: sorted({visible[word] for word in values if word in visible})
                                                 for key, values in unit["_access"].items()}})
        module_state.append({"module": module["name"], "source": module["source"],
                             "declarations": {name: module["declarations"].get(name) for name in sorted(members)},
                             "lifetime": "module process storage; existing initialization and reset sites retained",
                             "users": users})
    for unit in units:
        unit["host_persistent_access"] = []
        for uid in unit["host_scopes"][1:]:
            owner = by_id[uid]
            if owner["kind"] == "module" or not owner["persistent_locals"]:
                continue
            # Local declarations hide host objects of the same name. Module
            # objects are handled above with their USE/ONLY/rename contracts.
            visible = set(owner["persistent_locals"]) - set(unit["declarations"])
            unit["host_persistent_access"].append({"owner": uid, **{
                key: sorted(set(values) & visible) for key, values in unit["_access"].items()}})
    for unit in units:
        del unit["_access"]
    return {"files": files, "units": units, "common_layouts": layouts, "module_state": module_state,
            "method": "conservative lexical source index; CPP alternatives retained; no semantic alias proof"}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", action="store_true", help="Print the complete source index to stdout")
    parser.add_argument("--procedure", help="Show a definition/ENTRY contract and all indexed callers")
    parser.add_argument("--common", help="Show native layouts and their consumers/access sites")
    args = parser.parse_args()
    index = build_index()
    if args.json:
        print(json.dumps(index, indent=1))
    elif args.procedure:
        name = args.procedure.upper()
        print(json.dumps({"definitions": [u for u in index["units"] if u["name"] == name or any(e["name"] == name for e in u["entries"])],
                          "callers": [{"unit": u["id"], "variants": u["variants"], **call}
                                      for u in index["units"] for call in u["calls"] if call["name"] == name]}, indent=1))
    elif args.common:
        name = args.common.upper()
        members = {m for b in index["common_layouts"] if b["name"] == name for m in b["members"]}
        print(json.dumps({"layouts": [b for b in index["common_layouts"] if b["name"] == name],
                          "access": [{"id": u["id"], "variants": u["variants"], "equivalence": u["equivalence"],
                                      **{k: sorted(set(u[k]) & members) for k in
                                         ("common_references", "common_assignments", "possible_call_mutations", "possible_io_mutations")}}
                                     for u in index["units"] if name in u["common"]]}, indent=1))
    else:
        print(json.dumps({"selected_paths": len(index["files"]), "units": len(index["units"]),
                          "families": dict(sorted(Counter(u["family"] for u in index["units"]).items())),
                          "entries": sum(len(u["entries"]) for u in index["units"]),
                          "common_blocks": len({b["name"] for b in index["common_layouts"]}),
                          "common_layouts": len(index["common_layouts"]),
                          "common_statements": sum(len(b["statements"]) for b in index["common_layouts"]),
                          "persistent_scopes": sum(bool(u["persistent_locals"]) for u in index["units"]),
                          "alias_scopes": sum(bool(u["equivalence"]) for u in index["units"]),
                          "method": index["method"]}, indent=1))


if __name__ == "__main__":
    main()
