#!/usr/bin/env python3
import hashlib
from pathlib import Path
from os import getcwd
from collections import defaultdict


def md5(file):
    """md5 hash a file"""
    hash_md5 = hashlib.md5()
    with file.open("rb") as f:
        for chunk in iter(lambda: f.read(64 * hash_md5.block_size), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def dedup(path, recurse=True):
    """Deduplicate exact file matches from directory"""
    uniques = defaultdict(set)
    glob = path.rglob if recurse else path.glob
    fs = {x for x in glob('*') if not x.is_dir() and not x.is_symlink()}

    dels = set()
    for f in fs:
        size = f.stat().st_size
        fhash = md5(f)
        if fhash in uniques[size]:
            dels.add(f)
            f.unlink()
        else:
            uniques[size].add(fhash)
    return dels


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--recurse', '-r', action='store_true',
                        help="dedupliate recursively")
    parser.add_argument("dirs", type=Path, nargs='*', default=[Path(getcwd())],
                        help="Directories to deduplicate")
    args = parser.parse_args()

    acc = 0
    for f in args.dirs:
        acc += len(dedup(f, args.recurse))
    print(f"{acc} deletions")
