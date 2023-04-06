#!/usr/bin/env python3

import sys
import os
from pathlib import Path

name = sys.argv[2]
root = Path(sys.argv[1]) / name
count = int(sys.argv[3])

root.mkdir(parents=True, exist_ok=True)

package = f"""\
// swift-tools-version: 5.7
import PackageDescription

let package = Package(
    name: "{name}",
    targets: [\
"""

for i in range(0, count):
    if i == 0:
        stanza = f"""
        .target(
            name: "Module{i}",
            dependencies: []),\
"""
    else:
        stanza = f"""
        .target(
            name: "Module{i}",
            dependencies: ["Module{i - 1}"]),\
"""

    package += stanza

    source_file = root / "Sources" / f"Module{i}" / f"Module{i}.swift"
    source_file.parent.mkdir(parents=True)
    source_file.write_text(f"// Module{i}\n")


package += """
    ]
)
"""

(root / "Package.swift").write_text(package)
(root / ".gitignore").write_text(".build\n")
