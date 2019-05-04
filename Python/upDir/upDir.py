#!/usr/bin/env python3
# based on: https://stackoverflow.com/questions/29189557/how-to-upload-complete-folder-to-dropbox-using-python
from pathlib import Path, PurePath  # import os
import dropbox
import sys

# get an access token, local (from) directory, and Dropbox (to) directory
# from the command-line


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
    for i in Path(local_directory).glob("**/*"):
        local_path = i.root / i

        relative_path = local_path.relative_to(local_directory)
        drop_path = Path(dropbox_destination) / relative_path

        # upload the file
        with open(local_path, 'rb') as f:
            client.files_upload(f.read(), drop_path,
                                mode=dropbox.files.WriteMode("overwrite"))
