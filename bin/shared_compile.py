"""Reuse audited GNU compilation units without sharing their runtime storage.

Make and CMake call this as a compiler launcher. Each binary still links every
selected object in its original position. Cache entries are immutable; a local
object is unlinked before any compiler can write it, including forced rebuilds.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time

sys.dont_write_bytecode = True


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


@contextmanager
def locked(path: Path):
    """OS locks survive competing builds and are released after a crashed job."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a+b") as stream:
        if os.name == "nt":
            import msvcrt
            stream.write(b"\0")
            stream.flush()
            while True:
                try:
                    stream.seek(0)
                    msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
                    break
                except OSError:
                    time.sleep(0.05)
        else:
            import fcntl
            fcntl.flock(stream.fileno(), fcntl.LOCK_EX)
        try:
            yield
        finally:
            if os.name == "nt":
                stream.seek(0)
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)


def compiler_identity(command: str, cache: Path) -> dict:
    executable = Path(shutil.which(command) or command).resolve()
    info = executable.stat()
    key = hashlib.sha256(str((str(executable), info.st_mtime_ns, info.st_size)).encode()).hexdigest()
    path = cache / "compilers" / (key + ".json")
    with locked(path.with_suffix(".lock")):
        if not path.exists():
            version = subprocess.check_output([command, "--version"], text=True)
            target = subprocess.check_output([command, "-dumpmachine"], text=True).strip()
            path.write_text(json.dumps({"path": str(executable), "sha256": digest(executable),
                                        "version": version, "target": target,
                                        "host": sys.platform}))
        return json.loads(path.read_text())


def option_paths(arguments: list[str], option: str) -> list[Path]:
    paths = []
    for index, arg in enumerate(arguments):
        if arg == option:
            paths.append(Path(arguments[index + 1]))
        elif arg.startswith(option) and len(arg) > len(option):
            paths.append(Path(arg[len(option):]))
    return paths


def without_outputs(arguments: list[str], identity: bool = False) -> list[str]:
    """Output/dependency destinations do not change code or its ABI.

    Search paths and definitions may differ across variants. They are replaced
    in the identity by the actual preprocessed text, resolved headers and input
    module bytes, so unused -D/-I differences cannot hide a changed input.
    """
    result = []
    skip = False
    pairs = {"-o"}
    if identity:
        pairs.update(("-MF", "-MT", "-MQ", "-I", "-J", "-isystem", "-iquote", "-D", "-U"))
    for arg in arguments:
        if skip:
            skip = False
            continue
        if arg in pairs:
            skip = True
            continue
        if arg == "-c" or (identity and arg in ("-MD", "-MMD", "-MP")):
            continue
        if identity and any(arg.startswith(p) and arg != p for p in ("-MF", "-MT", "-MQ", "-I", "-J", "-D", "-U")):
            continue
        if identity and Path(arg).suffix.lower() in (".c", ".f90"):
            result.append(Path(arg).name)
        else:
            result.append(arg)
    return result


def fortran_inputs(source: Path, arguments: list[str]) -> tuple[dict[str, str], list[str], list[str]]:
    """Read Fortran INCLUDEs and module interfaces in the compiler's lookup order.

    Audited NVEL includes contain no CPP directives. CPP in the source itself
    is handled by the caller's actual preprocessing pass. Unsupported submodule
    units are left to the compiler rather than guessed into a reusable entry.
    """
    files = {}
    texts = []
    search = option_paths(arguments, "-I")

    def visit(path: Path) -> None:
        path = path.resolve()
        if str(path) in files:
            return
        files[str(path)] = digest(path)
        text = path.read_bytes().decode("latin1")
        texts.append(text)
        for name in re.findall(r"(?im)^\s*include\s*['\"]([^'\"]+)['\"]", text):
            candidates = [path.parent / name, *(p / name for p in search), Path.cwd() / name]
            included = next((p for p in candidates if p.is_file()), None)
            if included is None:
                raise ValueError(f"unresolved INCLUDE {name}")
            if len({digest(p) for p in candidates if p.is_file()}) > 1:
                raise ValueError(f"ambiguous INCLUDE {name}")
            if re.search(r"(?m)^\s*#", included.read_text(errors="replace")):
                raise ValueError(f"CPP inside Fortran INCLUDE {included}")
            visit(included)

    visit(source)
    text = "\n".join(texts)
    if re.search(r"(?im)^\s*submodule\b", text):
        raise ValueError("submodule compilation is not in the audited reuse set")
    provides = re.findall(r"(?im)^\s*module\s+(?!procedure\b|function\b|subroutine\b)(\w+)", text)
    uses = re.findall(r"(?im)^\s*use\b\s*(?:,\s*(?:non_)?intrinsic\s*)?(?:::\s*)?(\w+)", text)
    # Paths locate inputs; their bytes and names establish equivalence across
    # flat Make directories and CMake's variant-specific include directories.
    inputs = {Path(name).name + ":" + value: value for name, value in files.items()}
    for name in uses:
        candidates = [p / (name.lower() + ".mod") for p in [Path.cwd(), *search, *option_paths(arguments, "-J")]]
        module = next((p for p in candidates if p.is_file()), None)
        if module is None:
            raise ValueError(f"unresolved module {name}")
        if len({digest(p) for p in candidates if p.is_file()}) > 1:
            raise ValueError(f"ambiguous module {name}")
        inputs[name.lower() + ".mod"] = digest(module)
    return inputs, [name.lower() + ".mod" for name in provides], uses


def install(source: Path, destination: Path, link: bool = False) -> None:
    """Publish a complete file; never modify an inode shared with the cache."""
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_name(destination.name + f".{os.getpid()}.tmp")
    temporary.unlink(missing_ok=True)
    try:
        if link:
            try:
                os.link(source, temporary)
            except OSError:
                shutil.copy2(source, temporary)
        else:
            shutil.copy2(source, temporary)
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)


def compile_cached(command: list[str], cache: Path, kind: str, hardlink: bool = False) -> int:
    source = next(Path(a) for a in reversed(command) if Path(a).suffix.lower() in (".c", ".f90"))
    output = Path(command[command.index("-o") + 1])
    # This also protects old hard links when the compiler or eligibility changes.
    output.unlink(missing_ok=True)
    # These options consume mutable external profiles/plugins or produce extra
    # files beyond the audited object/module/dependency outputs.
    unsupported = ("-fprofile", "-fauto-profile", "-fplugin", "-gsplit-dwarf",
                   "-save-temps", "-fstack-usage", "-ftest-coverage", "--coverage",
                   "-include-pch", "-fsyntax-only")
    if any(arg.startswith(unsupported) or arg.startswith("@") for arg in command[1:]):
        return subprocess.call(command)
    compiler = compiler_identity(command[0], cache)
    if "GNU" not in compiler["version"] and "gcc (" not in compiler["version"]:
        return subprocess.call(command)
    # Keep -MD/-MF/-MT on this pass: a CMake cache hit still needs a fresh
    # dependency file naming this consumer's object, not the first producer's.
    preprocessed = subprocess.run([command[0], *without_outputs(command[1:]), "-E"],
                                  capture_output=True)
    if preprocessed.returncode:
        return subprocess.call(command)
    text = preprocessed.stdout.decode("latin1")
    headers = {}
    header_hashes = {}
    volatile = re.compile(rb"\b__(?:DATE|TIME|TIMESTAMP)__\b")
    marker = re.compile(r'^#\s+\d+\s+"([^"]+)"', re.M)
    for match in marker.finditer(text):
        path = Path(match[1])
        if path.is_file():
            if path not in header_hashes:
                if volatile.search(path.read_bytes()) or path.with_name(path.name + ".gch").exists():
                    return subprocess.call(command)
                header_hashes[path] = digest(path)
            value = header_hashes[path]
            headers[path.name + ":" + value] = value
    # Directory names in #line records are not tokens. __FILE__ expansions
    # remain in the text, and therefore still distinguish path-sensitive code.
    text = marker.sub(lambda m: m[0].replace(m[1], "<working-directory>" if m[1].endswith("//")
                                            else Path(m[1]).name), text)
    identity = {"compiler": compiler, "flags": without_outputs(command[1:], True),
                "source": digest(source), "preprocessed": text, "headers": headers,
                "launcher": digest(Path(__file__)), "kind": kind}
    modules = []
    module_directory = (option_paths(command[1:], "-J") or [Path.cwd()])[0]
    if kind == "nvel":
        try:
            inputs, modules, _ = fortran_inputs(source, command[1:])
        except ValueError:
            # A new dependency outside the audited set must compile normally.
            output.unlink(missing_ok=True)
            return subprocess.call(command)
        identity["fortran_inputs"] = inputs
    key = hashlib.sha256(json.dumps(identity, sort_keys=True).encode()).hexdigest()
    entry = cache / kind / key
    with locked(entry.with_suffix(".lock")):
        metadata = entry / "entry.json"
        valid = metadata.exists() and (entry / "object.o").exists()
        if valid:
            stored = json.loads(metadata.read_text())
            valid = digest(entry / "object.o") == stored["object"]
            valid = valid and all((entry / name).is_file() and digest(entry / name) == value
                                  for name, value in stored.get("modules", {}).items())
        if not valid:
            # A previous cache hit may have installed a hard link here.
            output.unlink(missing_ok=True)
            print("SHARED MISS " + source.name + ": " + shlex.join(command), flush=True)
            result = subprocess.call(command)
            if result:
                return result
            entry.parent.mkdir(parents=True, exist_ok=True)
            scratch = Path(tempfile.mkdtemp(prefix=key + ".", dir=entry.parent))
            try:
                shutil.copy2(output, scratch / "object.o")
                (scratch / "object.o").chmod(0o644 if os.name == "nt" else 0o444)
                module_hashes = {}
                for name in modules:
                    shutil.copy2(module_directory / name, scratch / name)
                    module_hashes[name] = digest(scratch / name)
                (scratch / "entry.json").write_text(json.dumps({"object": digest(output),
                                                               "modules": module_hashes}))
                if entry.exists():
                    shutil.rmtree(entry)
                scratch.rename(entry)
            finally:
                if scratch.exists():
                    shutil.rmtree(scratch)
        else:
            print("SHARED HIT " + source.name, flush=True)
            # GNU's Fortran preprocessing pass does not emit INCLUDE/USE
            # dependencies. Recreate this consumer's .d file on a cache hit.
            if kind == "nvel" and "-MF" in command:
                dependencies = subprocess.run([command[0], *without_outputs(command[1:]), "-M"],
                                              stdout=subprocess.DEVNULL)
                if dependencies.returncode:
                    return dependencies.returncode
        for name in modules:
            destination = module_directory / name
            if not destination.exists() or digest(destination) != digest(entry / name):
                install(entry / name, destination)
                os.utime(destination, None)
        linked = hardlink and os.name != "nt"
        install(entry / "object.o", output, link=linked)
        if not linked:
            # CMake owns object timestamps. Its ordinary dependency machinery
            # must see this consumer's completed compilation as current.
            os.utime(output, None)
        # Make can retain immutable hard links without changing other consumers'
        # object timestamps. Its link rules also depend on this local receipt.
        output.with_name(output.name + ".stamp").touch()
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache-dir", type=Path, required=True)
    parser.add_argument("--kind", choices=("sqlite", "fofem", "nvel"))
    parser.add_argument("--groups", default="sqlite")
    parser.add_argument("--hardlink", action="store_true")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    source = next((Path(a) for a in reversed(command) if Path(a).suffix.lower() in (".c", ".f90")), None)
    kind = args.kind
    if source and not kind:
        if source.name in ("sqlite3.c", "fvsqlite3.c"):
            kind = "sqlite"
        elif source.parent.name == "fofem" and source.suffix == ".c":
            kind = "fofem"
        elif source.parent.name == "NVEL" and source.suffix.lower() == ".f90":
            kind = "nvel"
    if not source or "-o" not in command or "-c" not in command or kind not in args.groups.split(","):
        if kind and "-c" in command and "-o" in command:
            Path(command[command.index("-o") + 1]).unlink(missing_ok=True)
        os.execvp(command[0], command)
    return compile_cached(command, args.cache_dir.resolve(), kind, args.hardlink)


if __name__ == "__main__":
    sys.exit(main())
