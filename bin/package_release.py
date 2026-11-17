"""Strip release copies while preserving developer binaries and optional symbols."""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def package(inputs: list[Path], output: Path, symbols: Path | None,
            strip: str, objcopy: str, dsymutil: str) -> None:
    sources = [p.resolve(strict=True) for p in inputs]
    output = output.resolve()
    if len({p.name for p in sources}) != len(sources):
        raise ValueError("package inputs must have unique names")
    if any((output / p.name).resolve() == p for p in sources):
        raise ValueError("the release directory must differ from developer binaries")
    if symbols:
        symbols = symbols.resolve()
        if symbols == output or any(symbols == p.parent for p in sources):
            raise ValueError("symbols need their own output directory")
        symbols.mkdir(parents=True, exist_ok=True)
    output.mkdir(parents=True, exist_ok=True)
    for source in sources:
        before = digest(source)
        magic = source.read_bytes()[:4]
        macho = magic in (b"\xcf\xfa\xed\xfe", b"\xfe\xed\xfa\xcf", b"\xca\xfe\xba\xbe", b"\xbe\xba\xfe\xca")
        if not macho and magic != b"\x7fELF" and magic[:2] != b"MZ":
            raise ValueError(f"unsupported executable format: {source}")
        # Work on a private copy even if the compiler cache uses hard links.
        # Neither strip nor objcopy ever receives the developer binary as output.
        with tempfile.TemporaryDirectory(prefix=".package-", dir=output) as temporary:
            copied = Path(temporary) / source.name
            shutil.copy2(source, copied)
            copied.chmod(copied.stat().st_mode | 0o200)
            if symbols:
                if macho:
                    destination = symbols / (source.name + ".dSYM")
                    if destination.exists():
                        raise ValueError(f"symbol bundle already exists: {destination}")
                    subprocess.run([dsymutil, str(source), "-o", str(destination)], check=True)
                else:
                    debug = symbols / (source.name + ".debug")
                    subprocess.run([objcopy, "--only-keep-debug", str(source), str(debug)], check=True)
            strip_flags = ["-S", "-x"] if macho else ["--strip-debug"]
            subprocess.run([strip, *strip_flags, str(copied)], check=True)
            if symbols and not macho:
                subprocess.run([objcopy, "--add-gnu-debuglink=" + str(debug), str(copied)], check=True)
            copied.replace(output / source.name)
        if digest(source) != before:
            raise RuntimeError(f"developer binary changed while packaging: {source}")
        print(f"PACKAGED {source.name}: {source.stat().st_size} -> {(output / source.name).stat().st_size} bytes", flush=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--symbols-dir", type=Path)
    parser.add_argument("--strip", default="strip")
    parser.add_argument("--objcopy", default="objcopy")
    parser.add_argument("--dsymutil", default="dsymutil")
    parser.add_argument("inputs", type=Path, nargs="+")
    args = parser.parse_args()
    package(args.inputs, args.output, args.symbols_dir, args.strip, args.objcopy, args.dsymutil)


if __name__ == "__main__":
    main()
