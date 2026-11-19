"""SP03 poisoned-storage mode: storage dimensions above the active extents.

The single program will size shared arrays for the largest configuration and
use each default's own extents at run time (SP05/SP06). A loop or read left on
the storage dimension would then reach slots the default never fills. This
mode exposes such sites in a disposable build tree:

1. Each variant's PRGPRM storage sizes are raised to the largest values in the
   union (MAXSP 108, MAXTRE 6000, MAXPLT 500).
2. A generated POISON_INACTIVE routine, one per variant, fills the inactive
   tail of every COMMON array declared with one of those sizes: signalling NaNs
   for reals, -HUGE for integers and ACHAR(127) for characters. Logical arrays
   cannot be poisoned and are listed as such.
3. INITRE calls it right after GRINIT, so each stand starts with poisoned
   inactive slots; a real read of one traps under the build's existing
   -ffpe-trap flags, and an integer or character read changes output.

The tail is filled at run time, not with -finit-* flags, which skip locals of
procedures with ENTRY (bug 63) and crash on vbase/morts.f90 (bug 58). Only
COMMON storage declared in include files is covered: module variables, SAVE
locals and COMMON declared inside a procedure are not. Restarted runs read
their storage from the restart file and are not re-poisoned.

Until SP05 and SP06 move the loops and checks to active extents, raising the
storage sizes also raises those bounds, and the MAXSP-shaped DATA statements
no longer match, so real variants are not expected to build or pass yet.
`check_poison_storage.py` proves the mechanism on a planted fixture.
"""

from __future__ import annotations

from pathlib import Path
import re

from module_inventory import ROOT, build_index, common_blocks, declarations, manifest_views, split_fields, statements

STORAGE = {"MAXSP": 108, "MAXTRE": 6000, "MAXPLT": 500}
HOOK = re.compile(r"^(\s*)call\s+GRINIT\s*$", re.I | re.M)
INITRE_SOURCES = ("vbase/initre.f90", "metric/vbase/initre.f90", "canada/bc/initre.f90")


def native_values(prgprm: Path) -> dict[str, int]:
    """The default's own sizes, read from its PRGPRM before any rewrite."""
    values = {}
    for _, code in statements(str(prgprm)):
        if re.match(r"parameter\b", code, re.I):
            for item in split_fields(re.sub(r"^parameter\s*\((.*)\)\s*$", r"\1", code, flags=re.I)):
                name, _, value = item.partition("=")
                if name.strip().upper() in STORAGE and value.strip().isdigit():
                    values[name.strip().upper()] = int(value)
    return values


def raise_storage(prgprm: Path, storage: dict[str, int]) -> None:
    text = prgprm.read_text(encoding="latin1")
    for name, value in storage.items():
        text, count = re.subn(r"(parameter\s*\(\s*" + name + r"\s*=\s*)\d+", rf"\g<1>{value}", text, flags=re.I)
        if count != 1:
            raise ValueError(f"{prgprm}: expected one {name} parameter, found {count}")
    prgprm.write_text(text, encoding="latin1")


def array_shapes(include: Path) -> list[tuple[str, str, list[str]]]:
    """(name, declared type, dimension texts) for every COMMON array in an include."""
    members, declared = [], {}
    for _, code in statements(str(include)):
        declared.update(declarations(code))
        for _, names in common_blocks(code):
            members.extend(names)
    shapes = []
    for name in members:
        declaration = declared.get(name)
        if not declaration:
            continue
        head, body = declaration.split("::", 1) if "::" in declaration else (declaration, declaration)
        item = next((f for f in split_fields(body) if re.match(name + r"\b", f, re.I)), "")
        dims = re.match(name + r"\s*\((.*)\)\s*$", item, re.I)
        attribute = re.search(r"dimension\s*\(([^)]*)\)", head, re.I)
        if dims or attribute:
            shapes.append((name, head.strip().lower(), split_fields((dims or attribute)[1])))
    return shapes


def poison_statements(shapes: list[tuple[str, str, list[str]]], native: dict[str, int],
                      storage: dict[str, int]) -> tuple[list[str], list[str]]:
    """Assignments filling each array's inactive tail, and arrays left unpoisoned."""
    lines, skipped = [], []
    for name, kind, dims in shapes:
        positions = [i for i, d in enumerate(dims) if d.replace(" ", "").upper() in
                     set(storage) | {f"1:{s}" for s in storage}]
        if not positions:
            if any(re.search(r"\b(" + "|".join(storage) + r")\b", d, re.I) for d in dims):
                skipped.append(f"{name}({','.join(dims)}): derived storage extent")
            continue
        if kind.startswith("logical"):
            skipped.append(f"{name}: logical")
            continue
        if kind.startswith(("real*8", "double")) or re.match(r"real\s*\(\s*(kind\s*=\s*)?8\s*\)", kind):
            value = "ieee_value(1.0d0, ieee_signaling_nan)"
        elif kind.startswith("real"):
            value = "ieee_value(1.0, ieee_signaling_nan)"
        elif kind.startswith("integer"):
            value = f"-huge({name}(" + ",".join(f"lbound({name},{i + 1})" for i in range(len(dims))) + "))"
        elif kind.startswith("character"):
            value = f"repeat(achar(127), len({name}))"
        else:
            skipped.append(f"{name}: {kind}")
            continue
        for position in positions:
            size = dims[position].replace(" ", "").upper().split(":")[-1]
            if native[size] >= storage[size]:
                continue
            section = ",".join(f"NATIVE_{size}+1:{size}" if i == position else ":" for i in range(len(dims)))
            lines.append(f"    {name}({section}) = {value}")
    return lines, skipped


def poison_routine(includes: list[tuple[str, list[str]]], native: dict[str, int],
                   storage: dict[str, int] = STORAGE) -> tuple[str, list[str]]:
    """Fortran for POISON_INACTIVE. Each include gets its own contained routine
    that repeats a real consumer's include order, so names resolve as they do
    in compiled code and no two includes are forced into one scope."""
    parts, calls, skipped = [], [], []
    for number, (path, prefix) in enumerate(includes, 1):
        lines, missed = poison_statements(array_shapes(Path(path)), native, storage)
        skipped.extend(f"{Path(path).name}: {item}" for item in missed)
        if not lines:
            continue
        calls.append(f"  call POISON_{number}")
        parts.append("\n".join([f"  subroutine POISON_{number}",
                                "    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_signaling_nan",
                                "    implicit none",
                                *(f"    include '{name}'" for name in prefix),
                                *(f"    integer, parameter :: NATIVE_{s} = {native[s]}" for s in storage),
                                *lines,
                                f"  end subroutine POISON_{number}"]))
    source = "\n".join(["! Generated by tests/parity/poison_storage.py for a disposable build tree.",
                        "subroutine POISON_INACTIVE", "  implicit none", *calls, "contains", *parts,
                        "end subroutine POISON_INACTIVE", ""])
    return source, skipped


def variant_includes(index: dict, view: dict[str, str], variant: str) -> list[tuple[str, list[str]]]:
    """Each COMMON include the variant selects, with a consumer's include prefix."""
    result = []
    sources = sorted({b["source"] for b in index["common_layouts"] if b["scope"] is None} & set(view.values()))
    for source in sources:
        name = Path(source).name.upper()
        for unit in index["units"]:
            direct = [n.upper() for n in unit["includes"]]
            if variant in unit["variants"] and name in direct:
                prefix = unit["includes"][:direct.index(name) + 1]
                result.append((source, prefix))
                break
    return result


def apply(tree: Path, variants: list[str], storage: dict[str, int] = STORAGE) -> dict[str, list[str]]:
    """Poison a candidate snapshot in place; return the unpoisoned arrays per variant."""
    index, views = build_index(), manifest_views()
    report = {}
    for variant in variants:
        view = views[variant]
        prgprm = tree / view["PRGPRM.F90"]
        native = native_values(prgprm)
        includes = [(str(tree / source), prefix) for source, prefix in variant_includes(index, view, variant)]
        source, skipped = poison_routine(includes, native, storage)
        target = tree / "tests" / "poison" / variant / "poison_inactive.f90"
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(source)
        manifest = tree / "bin" / f"FVS{variant}_sourceList.txt"
        manifest.write_text(manifest.read_text().rstrip("\n") + f"\n../tests/poison/{variant}/poison_inactive.f90\n")
        report[variant] = skipped
    # PRGPRM files and INITRE hooks are shared by name across variants; rewrite once.
    for prgprm in {tree / views[v]["PRGPRM.F90"] for v in variants}:
        raise_storage(prgprm, storage)
    for relative in INITRE_SOURCES:
        path = tree / relative
        text = path.read_text(encoding="latin1")
        text, count = HOOK.subn(r"\g<0>\n\g<1>call POISON_INACTIVE", text)
        if count != 1:
            raise ValueError(f"{relative}: expected one GRINIT call, found {count}")
        path.write_text(text, encoding="latin1")
    return report
