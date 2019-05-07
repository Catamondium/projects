#!/usr/bin/env python3
from pathlib import Path, PurePath
import dropbox
import sys


def readToken(tok='creds.secret'):
    """
    Read client token relative to script
    """
    # access client secret relative to script
    fpath = Path(__file__).resolve().parent / tok
    with open(fpath, 'r') as f:
        return f.readline().strip()


def usage():
    print("params: (local_dir, dropbox_dir)")
    sys.exit(1)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        usage()

    local_directory, dropbox_destination = sys.argv[1:3]
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
