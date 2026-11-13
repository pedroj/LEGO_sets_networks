#!/bin/bash
# Compile the Swift classifier on the Mac and compare it with the categories that
# classify_part.R stored in parts_index.rds (via lego.sqlite). Expect 0 mismatches.
set -e
cd "$(dirname "$0")/.."
B=$(mktemp -d)
swiftc -O -o "$B/verify" LEGOExplorer/Core/SQLiteDB.swift LEGOExplorer/Core/PartClassifier.swift tools/verify/main.swift -lsqlite3
"$B/verify" "${1:-LEGOExplorer/Resources/lego.sqlite}"
