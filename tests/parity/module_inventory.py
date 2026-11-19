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


def manifest_views() -> dict[str, dict[str, str]]:
    """Each variant's selected files by upper-case basename (its include view)."""
    views = {}
    for manifest in sorted((ROOT / "bin").glob("FVS??_sourceList.txt")):
        paths = [(manifest.parent / name).resolve().relative_to(ROOT).as_posix() for name in manifest.read_text().split()]
        views[manifest.name[3:5]] = {Path(path).name.upper(): path for path in paths}
    return views


def build_index() -> dict:
    selected, views = defaultdict(set), manifest_views()
    for variant, view in views.items():
        for path in view.values():
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


# ---------------------------------------------------------------------------
# SP01: compile-time variant inputs (configuration inventory).

SIZE_NAMES = ("MAXSP", "MAXTRE", "MAXTP1", "MAXPLT", "NSPSPE", "MXFRCDS")
IDENTITY_NAMES = ("VARIANTCD", "LMETRICVAR", "VARACD")
CONFIG_WORD = re.compile(r"\b(" + "|".join(SIZE_NAMES + IDENTITY_NAMES) + r")\b", re.I)
SERIALIZERS = {"pg/putstd.f90", "pg/getstd.f90"}
RELATION = r"(?:\.(?:eq|ne|lt|le|gt|ge)\.|==|/=|<=?|>=?)"


def config_use(code: str, name: str) -> tuple[str, str]:
    """Classify one statement's use of a compile-time variant input as (kind, use).

    Kinds follow SP01: storage dimension, active extent, identity test or
    initializer. A size in a declaration is storage; in executable code it is an
    extent the run actually uses. This is lexical: SP05 still checks each site.
    """
    low = STRINGS.sub(lambda m: " " * len(m[0]), code).lower()
    word = name.lower()
    size = name.upper() in SIZE_NAMES
    head = low.split("::")[0]
    if re.match(r"parameter\b", low) or (DECL.match(code) and re.search(r"\bparameter\b", head)):
        return ("storage dimension" if size else "identity test", "parameter expression")
    if re.match(r"data\b", low):
        literal = bool(re.search(r"/[^/]*\b\d+\s*\*", low))
        return "initializer", "DATA with literal repeat count" if literal else "DATA"
    if DECL.match(code) and "::" in low and re.search(r"\(\s*/|\[", low.split("::", 1)[1]) \
            and re.search(r"=\s*(\(\s*/|\[)", low.split("::", 1)[1]):
        return "initializer", "declaration initializer"
    if DECL.match(code) or re.match(r"(dimension|common)\b", low):
        return ("storage dimension", "declaration") if size else ("identity test", "declaration")
    if not size:
        if name.upper() == "VARACD" and re.search(r"\bvaracd\s*=(?!=)", low):
            return "identity test", "runtime assignment"
        return "identity test", "runtime test" if name.upper() == "VARACD" else "compile-time test"
    if re.match(r"do\b[^=]*=", low) and re.search(r",[^,]*\b" + word + r"\b", low.split("=", 1)[1]):
        return "active extent", "loop bound"
    if re.search(RELATION + r"\s*\(?\s*" + word + r"\b|\b" + word + r"\s*\)?\s*" + RELATION, low):
        return "active extent", "bound check"
    if re.search(r":\s*" + word + r"\b", low):
        return "active extent", "array section"
    if re.match(r"call\b", low):
        return "active extent", "call argument"
    return "active extent", "other expression"


def configuration_inventory(index: dict, collisions: dict | None = None) -> dict:
    """Every compile-time variant input in the live sources, classified for SP01."""
    views = manifest_views()
    files = index["files"]
    values = {}
    for variant, view in views.items():
        found = {}
        for _, code in statements(view["PRGPRM.F90"]):
            if re.match(r"parameter\b", code, re.I):
                inner = re.sub(r"^parameter\s*\((.*)\)\s*$", r"\1", code, flags=re.I)
                for item in split_fields(inner):
                    name, _, value = item.partition("=")
                    found[name.strip().upper()] = value.strip()
        values[variant] = found
    names = sorted({name for found in values.values() for name in found})
    varying = {name: {variant: values[variant].get(name) for variant in sorted(values)}
               for name in names if len({found.get(name) for found in values.values()}) > 1}
    per_variant_includes = {}
    for name in sorted({name for view in views.values() for name in view}):
        paths = sorted({view[name] for view in views.values() if name in view})
        if len(paths) > 1 and files[paths[0]]["include"]:
            per_variant_includes[name] = {path: files[path]["variants"] for path in paths}
    block_data = defaultdict(dict)
    for unit in index["units"]:
        if unit["kind"] == "block data":
            block_data[unit["name"]][unit["source"]] = unit["variants"]
    prgprm = {view["PRGPRM.F90"] for view in views.values()}
    sites = []
    for path, record in sorted(files.items()):
        if not path.endswith(".f90") or path in prgprm:
            continue
        for line, code in statements(path):
            if code.startswith("#"):
                continue
            for name in sorted({w.upper() for w in CONFIG_WORD.findall(STRINGS.sub("", code))}):
                kind, use = config_use(code, name)
                flags = [flag for flag, test in (("restart", path in SERIALIZERS),
                                                 ("API", record["family"].startswith("MOD09"))) if test]
                sites.append({"name": name, "kind": kind, "use": use, "flags": flags,
                              "source": path, "line": line, "code": code})
    # Link-time selection: files that not every variant links.
    colliding_sources = ({impl["source"] for family in collisions["families"] for impl in family["implementations"]}
                         if collisions else set())
    selection = defaultdict(list)
    for path, record in sorted(files.items()):
        if len(record["variants"]) == len(views) or record["include"]:
            continue
        if path in prgprm:
            continue
        if path.startswith("volume/NVEL/"):
            group = "NVEL (every variant but BC)"
        elif re.match(r"(base|fire/base)/ex\w+\.f90$", path):
            group = "extension stub"
        elif path in colliding_sources:
            group = "alternate implementation"
        elif Path(path).suffix.lower() in (".c", ".cpp", ".h"):
            group = "foreign source"
        else:
            group = "variant-only source (no name conflict)"
        selection[group].append(path)
    # Line counts comparable with the plan's audit (code lines, comments removed).
    lines, line_files = Counter(), defaultdict(set)
    patterns = {"do ...,MAXSP": r"\bdo\b.*=.*,\s*MAXSP\b", "do ...,MAXTRE": r"\bdo\b.*=.*,\s*MAXTRE\b"}
    for path in files:
        if not path.endswith(".f90") or path in prgprm:
            continue
        for text in (ROOT / path).read_text(encoding="latin1").splitlines():
            code = text.split("!")[0]
            for name in {w.upper() for w in CONFIG_WORD.findall(code)}:
                lines[name] += 1
                line_files[name].add(path)
            for label, pattern in patterns.items():
                if re.search(pattern, code, re.I):
                    lines[label] += 1
                    line_files[label].add(path)
    totals = Counter((site["name"], site["kind"], site["use"]) for site in sites)
    return {
        "parameters": varying,
        "per_variant_includes": per_variant_includes,
        "block_data": {name: dict(sources) for name, sources in sorted(block_data.items())},
        "link_selection": {group: paths for group, paths in sorted(selection.items())},
        "sites": sites,
        "totals": [{"name": n, "kind": k, "use": u, "statements": c} for (n, k, u), c in sorted(totals.items())],
        "kind_totals": dict(sorted(Counter(site["kind"] for site in sites).items())),
        "flagged": dict(sorted(Counter(f"{site['name']} {flag}" for site in sites for flag in site["flags"]).items())),
        "code_lines": {label: {"lines": lines[label], "files": len(line_files[label])} for label in sorted(lines)},
    }


# ---------------------------------------------------------------------------
# SP02: external names defined more than once across the 24 manifests.

def argument_type(unit: dict, name: str) -> str:
    """Normalized type and rank of a dummy argument, e.g. real, integer[1]."""
    declaration = unit["declarations"].get(name)
    if not declaration:
        return ("integer" if name[0] in "IJKLMN" else "real") if not unit["implicit"] else "undeclared"
    if "::" in declaration:
        head, body = declaration.split("::", 1)
    else:
        match = re.match(r"^\s*(\w+(?:\s+precision)?(?:\s*\*\s*\d+|\s*\([^)]*\))?)(.*)$", declaration, re.I)
        head, body = match[1], match[2]
    base = re.sub(r",\s*intent\s*\([^)]*\)", "", head.lower()).replace(" ", "")
    dimension = re.search(r"dimension\s*\(([^)]*)\)", head, re.I)
    base = re.sub(r",?dimension\([^)]*\)", "", base)
    item = next((f for f in split_fields(body) if re.match(name + r"\b", f, re.I)), "")
    shape = re.match(name + r"\s*\((.*)\)", item, re.I)
    rank = len(split_fields((shape or dimension)[1])) if (shape or dimension) else 0
    base = {"real*4": "real", "integer*4": "integer", "doubleprecision": "real*8",
            "real(kind=8)": "real*8", "real(8)": "real*8"}.get(base, base)
    return f"{base}[{rank}]" if rank else base


def collision_kind(name: str, sources: list[str]) -> tuple[str, str]:
    """Return (kind, plan task) for a colliding name.

    Precedence follows the plan's task split: extension stubs (SP08); fire,
    including metric fire twins (SP11); establishment, including metric
    sprouting (SP09); the named remaining collisions (SP12); other metric twins
    and the variant equation families (SP10).
    """
    if any(re.match(r"(base|fire/base)/ex\w+\.f90$", s) for s in sources):
        return "extension stub/real", "SP08"
    if any("/fire/" in "/" + s for s in sources):
        return "fire per-variant", "SP11"
    if (any(part in ("estb", "strp", "vestb", "vstrp") for s in sources for part in s.split("/")[:-1])
            or name.startswith("ES")):
        return "establishment", "SP09"
    if any(s.endswith("DGCONS_ENTRY.f90") for s in sources) or name in ("RDBLK1", "VOLS"):
        return "remaining", "SP12"
    if any(s.startswith("metric/") for s in sources):
        return "metric twin", "SP10"
    return "variant equation family", "SP10"


def implementation_tag(source: str, variants: list[str], name: str) -> str:
    """Suffix for an implementation's proposed name: its variant or family."""
    parts = source.split("/")
    stem = Path(source).stem.lower()
    if len(variants) == 1:
        return variants[0].upper()
    if stem.startswith(name.lower()) and stem != name.lower():
        return stem[len(name):].strip("_").upper()
    if "_" in stem:
        return stem.rsplit("_", 1)[1].upper()
    if re.match(r"(base|fire/base)/ex\w+\.f90$", source):
        return "STUB"
    if parts[0] == "metric":
        return "METRIC"
    for part in parts[:-1]:
        if part in REGIONS:
            return part.upper()
    return parts[-2].upper() if len(parts) > 1 else "BASE"


def collision_inventory(index: dict) -> dict:
    """SP02 family table: implementations, signatures, callers and batch order."""
    units = index["units"]
    by_id = {u["id"]: u for u in units}
    children = defaultdict(list)
    for unit in units:
        if unit["parent"]:
            children[unit["parent"]].append(unit["id"])

    def subtree(uid: str) -> list[dict]:
        found, pending = [], [uid]
        while pending:
            current = pending.pop()
            found.append(by_id[current])
            pending.extend(children[current])
        return found

    definitions = defaultdict(list)
    for unit in units:
        # Module procedures are namespaced by their module; only external
        # procedures, ENTRY points and named BLOCK DATA meet at link time.
        if unit["parent"] is not None or unit["kind"] in ("module", "program"):
            continue
        definitions[unit["name"]].append({"unit": unit["id"], "source": unit["source"], "entry": False,
                                          "variants": unit["variants"], "arguments": unit["arguments"]})
        for entry in unit["entries"]:
            definitions[entry["name"]].append({"unit": unit["id"], "source": entry.get("source") or unit["source"],
                                               "entry": True, "variants": entry.get("variants") or unit["variants"],
                                               "arguments": entry["arguments"]})
    colliding = {name: impls for name, impls in definitions.items() if len({i["source"] for i in impls}) > 1}
    existing = set(definitions)
    families = []
    for name, impls in sorted(colliding.items()):
        kind, task = collision_kind(name, [i["source"] for i in impls])
        rows, signatures = [], []
        for impl in impls:
            unit = by_id[impl["unit"]]
            scope = subtree(impl["unit"])
            types = [argument_type(unit, argument) for argument in impl["arguments"]]
            signatures.append(tuple(types))
            tag = implementation_tag(impl["source"], impl["variants"], name)
            # NVEL already uses NAME_XX for some of its own routines (FORMCL_BM
            # and others), so fall back to NAME_FVS_XX where NAME_XX is taken.
            proposed = f"{name}_{tag}" if f"{name}_{tag}" not in existing else f"{name}_FVS_{tag}"
            rows.append({"proposed": proposed, "source": impl["source"], "unit": impl["unit"],
                         "entry": impl["entry"], "variants": impl["variants"], "arguments": impl["arguments"],
                         "types": types, "entries": [e["name"] for e in unit["entries"]],
                         "persistent_locals": sorted({n for u in scope for n in u["persistent_locals"]}),
                         "data_statements": sum(len(u["data"]) for u in scope)})
        if len({len(s) for s in signatures}) > 1:
            status = "argument count"
        elif len(set(signatures)) == 1:
            status = "same"
        elif len({tuple(re.sub(r"\[\d+\]$", "", t) for t in s) for s in signatures}) == 1:
            status = "rank"
        else:
            status = "types"
        callers = sorted({(u["id"], tuple(u["variants"])) for u in units for c in u["calls"] if c["name"] == name})
        depends = sorted({c["name"] for impl in impls for u in subtree(impl["unit"]) for c in u["calls"]
                          if c["name"] in colliding and c["name"] != name})
        families.append({"name": name, "kind": kind, "task": task, "signature": status,
                         "implementations": rows, "depends_on": depends,
                         "callers": [{"unit": uid, "variants": list(v)} for uid, v in callers]})
    # Leaf-first levels over the collision call graph; a cycle shares a level.
    by_name = {f["name"]: f for f in families}
    order, low, stack, on_stack, components = {}, {}, [], set(), []

    def strongconnect(node: str) -> None:
        order[node] = low[node] = len(order)
        stack.append(node)
        on_stack.add(node)
        for target in by_name[node]["depends_on"]:
            if target not in order:
                strongconnect(target)
                low[node] = min(low[node], low[target])
            elif target in on_stack:
                low[node] = min(low[node], order[target])
        if low[node] == order[node]:
            component = []
            while True:
                member = stack.pop()
                on_stack.discard(member)
                component.append(member)
                if member == node:
                    break
            components.append(component)

    sys.setrecursionlimit(max(sys.getrecursionlimit(), 10000))
    for name in sorted(by_name):
        if name not in order:
            strongconnect(name)
    level = {}
    for component in components:  # Tarjan emits components callee-first
        members = set(component)
        deps = {d for m in component for d in by_name[m]["depends_on"]} - members
        value = 1 + max((level[d] for d in deps), default=-1)
        for member in component:
            level[member] = value
            by_name[member]["level"] = value
            by_name[member]["cycle"] = sorted(members) if len(members) > 1 else []
    batches = []
    for task in ("SP10", "SP11", "SP08", "SP09", "SP12"):
        for value in sorted({f["level"] for f in families if f["task"] == task}):
            batches.append({"task": task, "level": value,
                            "names": [f["name"] for f in families if f["task"] == task and f["level"] == value]})
    # A proposed name must be unique within its family and must not already
    # be an external name somewhere in the union.
    clashes = sorted({r["proposed"] for f in families for r in f["implementations"]
                      if len({x["source"] for x in f["implementations"] if x["proposed"] == r["proposed"]}) > 1
                      or r["proposed"] in existing})
    same_variant = sorted(f["name"] for f in families
                          if max(Counter(v for r in f["implementations"] for v in r["variants"]).values()) > 1)
    return {"families": families, "batches": batches, "name_clashes": clashes,
            "same_variant_duplicates": same_variant,
            "summary": {"names": len(families),
                        "extra_definitions": sum(len({r["source"] for r in f["implementations"]}) - 1 for f in families),
                        "by_kind": dict(sorted(Counter(f["kind"] for f in families).items())),
                        "by_signature": dict(sorted(Counter(f["signature"] for f in families).items())),
                        "levels": max((f["level"] for f in families), default=-1) + 1}}


def print_configuration(report: dict) -> None:
    print("PRGPRM values that differ by variant:")
    for name, values in report["parameters"].items():
        print(f"  {name}: " + ", ".join(sorted(set(values.values()))))
    print("Per-variant includes: " + ", ".join(f"{name} ({len(paths)} versions)"
                                                for name, paths in report["per_variant_includes"].items()))
    print(f"BLOCK DATA: {sum(len(s) for s in report['block_data'].values())} units under "
          f"{len(report['block_data'])} names; per-variant names: " +
          ", ".join(f"{name} x{len(s)}" for name, s in report["block_data"].items() if len(s) > 1))
    print("Link-time selection (files not linked by every variant):")
    for group, paths in report["link_selection"].items():
        print(f"  {group}: {len(paths)}")
    print(f"Sites: {len(report['sites'])} statements; by kind: " +
          ", ".join(f"{k} {v}" for k, v in report["kind_totals"].items()))
    for row in report["totals"]:
        print(f"  {row['name']:<10} {row['kind']:<18} {row['use']:<32} {row['statements']}")
    print("Flagged: " + ", ".join(f"{k} {v}" for k, v in report["flagged"].items()))
    print("Code lines (comments removed): " + ", ".join(
        f"{k} {v['lines']}/{v['files']} files" for k, v in report["code_lines"].items()))


def print_collisions(report: dict) -> None:
    summary = report["summary"]
    print(f"{summary['names']} colliding names, {summary['extra_definitions']} extra definitions; "
          f"same-variant duplicates: {report['same_variant_duplicates'] or 'none'}; "
          f"proposed-name clashes: {report['name_clashes'] or 'none'}")
    print("By kind: " + ", ".join(f"{k} {v}" for k, v in summary["by_kind"].items()))
    print("Signatures: " + ", ".join(f"{k} {v}" for k, v in summary["by_signature"].items()))
    print("Batch order (leaf first):")
    for batch in report["batches"]:
        print(f"  {batch['task']} level {batch['level']}: " + " ".join(batch["names"]))
    print("Signature mismatches:")
    for family in report["families"]:
        if family["signature"] != "same":
            shapes = Counter(f"{len(r['types'])}:" + ",".join(r["types"]) for r in family["implementations"])
            print(f"  {family['name']} ({family['signature']}): " + " | ".join(
                f"{'/'.join(sorted(r['proposed'].rsplit('_', 1)[1] for r in family['implementations'] if f'{len(r['types'])}:' + ','.join(r['types']) == shape))} "
                f"({shape.split(':', 1)[0]} args)" for shape in shapes))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", action="store_true", help="Print the complete source index to stdout")
    parser.add_argument("--procedure", help="Show a definition/ENTRY contract and all indexed callers")
    parser.add_argument("--common", help="Show native layouts and their consumers/access sites")
    parser.add_argument("--configuration", action="store_true",
                        help="SP01: classify every compile-time variant input (summary; --detail for all sites)")
    parser.add_argument("--collisions", action="store_true",
                        help="SP02: family table of names defined more than once (summary; --detail for JSON)")
    parser.add_argument("--detail", action="store_true", help="With --configuration/--collisions, print full JSON")
    args = parser.parse_args()
    index = build_index()
    if args.configuration or args.collisions:
        collisions = collision_inventory(index)
        report = configuration_inventory(index, collisions) if args.configuration else collisions
        if args.detail:
            print(json.dumps(report, indent=1))
        elif args.configuration:
            print_configuration(report)
        else:
            print_collisions(report)
    elif args.json:
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
