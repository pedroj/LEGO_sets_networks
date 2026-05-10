#!/usr/bin/env python3
"""Extract thumbnail from BrickLink Studio .io files."""

import sys
import zipfile
import json
from pathlib import Path

PASSWORD = b"soho0909"

def extract_thumbnail(io_path: str, output_path: str | None = None) -> str:
    io_path = Path(io_path)
    if not io_path.exists():
        print(f"Error: {io_path} not found", file=sys.stderr)
        sys.exit(1)

    with zipfile.ZipFile(io_path) as z:
        try:
            info_data = z.read(".info", pwd=PASSWORD)
            info = json.loads(info_data)
            version = info.get("version", "").replace("\\r", "")
            parts = info.get("total_parts", "?")
            print(f"Studio {version} — {parts} parts")
        except Exception:
            pass

        thumb_data = z.read("thumbnail.png", pwd=PASSWORD)

        if output_path is None:
            output_path = io_path.with_suffix(".png")
        else:
            output_path = Path(output_path)

        output_path.write_bytes(thumb_data)
        print(f"Thumbnail saved to {output_path}")
        return str(output_path)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <file.io> [output.png]")
        sys.exit(1)
    output = sys.argv[2] if len(sys.argv) > 2 else None
    extract_thumbnail(sys.argv[1], output)
