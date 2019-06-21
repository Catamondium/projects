#!/usr/bin/env python3
import hashlib
from pathlib import Path


def md5(file):
    """md5 hash a file"""
    hash_md5 = hashlib.md5()
    with file.open("rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def dedup(path, recurse=True):
    """Deduplicate exact file matches from directory"""
    uniques = dict()
    glob = path.rglob if recurse else path.glob
    fs = [x for x in glob('*') if not x.is_dir()]

    for f in fs:
        size = f.stat().st_size
        if size in uniques:
            if md5(f) in uniques[size]:
                f.unlink()
            else:
                uniques[size].append(md5(f))
        else:
            uniques[size] = [md5(f)]


if __name__ == "__main__":
    target = './dir'
    dedup(Path(target))
