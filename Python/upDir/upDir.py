#!/usr/bin/env python3
from pathlib import Path, PurePath
import dropbox
import argparse


def readToken(tok='creds.secret'):
    """
    Read client token relative to script
    """
    # access client secret relative to script
    fpath = Path(__file__).resolve().parent / tok
    with open(fpath, 'r') as f:
        return f.readline().strip()


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Upload directory to Dropbox recursively")
    parser.add_argument("local", metavar="Local target directory")
    parser.add_argument("drop", metavar="Dropbox destination directory")
    args = parser.parse_args()

    local_directory = args.local
    dropbox_destination = args.drop

    client = dropbox.Dropbox(readToken())
    # enumerate local files recursively
    for i in Path(local_directory).rglob("*"):
        local_path = i.root / i

        relative_path = local_path.relative_to(local_directory).resolve()
        drop_path = str(Path(dropbox_destination) / relative_path)

        # upload the file
        with open(local_path, 'rb') as f:
            client.files_upload(f.read(), drop_path,
                                mode=dropbox.files.WriteMode("overwrite"))
