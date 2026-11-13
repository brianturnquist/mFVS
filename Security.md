# Security Policy

mFVS is a modernization of the USDA Forest Service's Forest Vegetation Simulator (FVS). It is maintained independently of the Forest Service, and this policy covers mFVS only. For official FVS releases, follow the [upstream security policy](https://github.com/USDAForestService/ForestVegetationSimulator/blob/main/Security.md).

## Supported versions

mFVS has not yet made a formal release. Only the latest commit on the `main` branch is supported, and fixes are made there. Tags such as `FS2026.3` and `2027.1.0` in this repository's history are upstream FVS releases from before modernization began; they are not maintained by mFVS.

Builds report their version from git. A version ending in `-dirty` was built from a working tree with uncommitted changes, and should not be used for production work or deliverables.

## What to treat as trusted input

FVS is a command-line simulation program, not a network service. It reads keyword files, tree data files and SQLite databases, and writes reports and databases. Keyword files are closer to scripts than to data files:

- **External commands.** The ADDTREES keyword can run an external establishment model. The command it runs is taken from the keyword file and executed by the operating system.
- **SQL.** Database keywords such as `StandSQL`, `TreeSQL`, `SQLIn` and `SQLOut` contain SQL statements that FVS executes against the input and output databases.
- **Fixed-size arrays.** FVS stores data in fixed-size arrays. Unexpected input can cause out-of-bounds memory access; [FOUND_BUGS.md](FOUND_BUGS.md) records an example triggered by ordinary keyword input in the BC and ON variants.

Run keyword files and databases only if they come from a source you trust, and review them as you would a script.

## Reporting a vulnerability

Please report suspected security vulnerabilities **privately**, using the repository's Security tab: **Report a vulnerability**. Do not open a public issue for them. Bugs that are not security-sensitive can be reported as ordinary GitHub issues.

A useful report includes:

- the mFVS commit or version string, from the output header or `git rev-parse --short HEAD`;
- the variant, operating system and compiler used;
- a description of the problem and its possible impact;
- the steps, keyword file and input data needed to reproduce it, if it is reproducible;
- the exact text of any error messages.

mFVS is maintained on a best-effort basis, so response times cannot be guaranteed. Reports will be acknowledged as soon as possible, and confirmed vulnerabilities will be fixed on `main` and noted in the commit history.

If the problem also affects official FVS, please report it to the FVS Staff as well, as described in the upstream security policy.
