#!/usr/bin/env python

from __future__ import print_function
import os
import subprocess


def main():
    cwd = os.getcwd()
    root = subprocess.check_output(
        ["git", "rev-parse", "--show-toplevel"]).strip()
    lines = subprocess.check_output(
        ["git", "status", "--porcelain"]).splitlines()
    relative_root = os.path.relpath(cwd, root)
    for line in lines:
        status, filepath = line.strip().split(" ", 1)
        if status == "D":
            continue

        print(os.path.relpath(filepath, relative_root))


if __name__ == "__main__":
    main()
