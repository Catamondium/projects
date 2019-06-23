#!/usr/bin/env python3
from pathlib import Path, PurePath
import dropbox
import argparse


def read_token(tok='creds.secret'):
    """
    Read client token relative to script
    """
    # access client secret relative to script
    fpath = Path(__file__).resolve().parent / tok
    with open(fpath, 'r') as f:
        return f.readline().strip()


def pair(arg):
    return arg.split(',')


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Upload directory to Dropbox recursively")
    parser.add_argument("pair", type=pair, nargs='+',
                        help="LOCAL,DROP directory pairs")
    args = parser.parse_args()

    for local, drop in args.pair:
        client = dropbox.Dropbox(read_token())
        # enumerate local files recursively
        for i in Path(local).rglob("*"):
            local_path = i.root / i

            if local_path.is_dir():
                continue

            relative_path = local_path.relative_to(local)
            drop_path = str(Path(drop) / relative_path)

            # upload the file
            with open(local_path, 'rb') as f:
                client.files_upload(f.read(), drop_path,
                                    mode=dropbox.files.WriteMode("overwrite"))
