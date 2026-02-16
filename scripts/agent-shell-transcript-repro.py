#!/usr/bin/env python3
import argparse
import re
import subprocess
import sys
from pathlib import Path


COMMAND_PREFIX = "**Command:** ["


def extract_commands(text: str) -> list[str]:
    commands = []
    in_command = False
    buffer: list[str] = []

    for line in text.splitlines():
        if not in_command:
            idx = line.find(COMMAND_PREFIX)
            if idx == -1:
                continue
            in_command = True
            remainder = line[idx + len(COMMAND_PREFIX) :]
            if "]" in remainder:
                before, _ = remainder.split("]", 1)
                commands.append(before.strip())
                in_command = False
                continue
            buffer.append(remainder)
        else:
            if "]" in line:
                before, _ = line.split("]", 1)
                buffer.append(before)
                command = "\n".join(buffer).strip()
                if command:
                    commands.append(command)
                buffer = []
                in_command = False
            else:
                buffer.append(line)

    return [command for command in commands if command]


def run_commands(commands: list[str]) -> int:
    total = len(commands)
    for idx, command in enumerate(commands, start=1):
        print(f"==> [{idx}/{total}] {command}")
        sys.stdout.flush()
        match = re.match(r"^(?P<shell>\S+)\s+-lc\s+(?P<body>.+)$", command, re.DOTALL)
        if match:
            shell = match.group("shell")
            body = match.group("body")
            result = subprocess.run([shell, "-lc", body])
        else:
            result = subprocess.run(command, shell=True)
        if result.returncode != 0:
            print(f"Command failed with exit code {result.returncode}", file=sys.stderr)
            return result.returncode
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Replay tool commands extracted from an agent-shell transcript.",
    )
    parser.add_argument("transcript", type=Path, help="Path to the transcript file")
    parser.add_argument(
        "--pattern",
        default=r"\./scripts/agent-shell-.*-repro\.sh",
        help="Regex to filter commands (default: \\./scripts/agent-shell-.*-repro\\.sh)",
    )
    parser.add_argument(
        "--run",
        action="store_true",
        help="Execute matched commands (default: print only)",
    )
    parser.add_argument(
        "--max",
        type=int,
        default=0,
        help="Limit number of commands executed/printed (0 = no limit)",
    )

    args = parser.parse_args()
    if not args.transcript.exists():
        print(f"Transcript not found: {args.transcript}", file=sys.stderr)
        return 1

    text = args.transcript.read_text(encoding="utf-8")
    commands = extract_commands(text)

    if args.pattern:
        regex = re.compile(args.pattern)
        commands = [command for command in commands if regex.search(command)]

    if args.max > 0:
        commands = commands[: args.max]

    if not commands:
        print("No matching commands found.")
        return 1 if args.run else 0

    if not args.run:
        for idx, command in enumerate(commands, start=1):
            print(f"# {idx}")
            print(command)
            print()
        return 0

    return run_commands(commands)


if __name__ == "__main__":
    raise SystemExit(main())
