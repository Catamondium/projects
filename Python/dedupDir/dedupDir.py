#!/usr/bin/env python3
import hashlib
from pathlib import Path
import argparse


def md5(file):
    """md5 hash a file"""
    hash_md5 = hashlib.md5()
    with file.open("rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def dedup(path, recurse=True):
    """Deduplicate exact file matches from directory"""
    dels = 0
    uniques = dict()
    glob = path.rglob if recurse else path.glob
    fs = [x for x in glob('*') if not x.is_dir()]

    for f in fs:
        size = f.stat().st_size
        if size in uniques:
            if md5(f) in uniques[size]:
                f.unlink()
                dels += 1
            else:
                uniques[size].append(md5(f))
        else:
            uniques[size] = [md5(f)]
    return dels


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--recurse', '-r', action='store_true',
                        help="dedupliate recursively")
    parser.add_argument("dirs", type=Path, nargs='*',
                        help="Directories to deduplicate")
    args = parser.parse_args()

    acc = 0
    for f in args.dirs:
        acc += dedup(f, args.recurse)
    print(f"{acc} deletions")
