#!/usr/bin/env python3
# based on: https://stackoverflow.com/questions/29189557/how-to-upload-complete-folder-to-dropbox-using-python
import os
import dropbox
import sys

# get an access token, local (from) directory, and Dropbox (to) directory
# from the command-line


def readToken(tok='creds.secret'):
    """
    Read client token relative to script
    """
    script = os.path.realpath(__file__)
    # access client secret relative to script
    scriptdir = os.path.dirname(script)
    fpath = os.path.join(scriptdir, tok)
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
    for root, dirs, files in os.walk(local_directory):
        for filename in files:

            # construct the full local path
            local_path = os.path.join(root, filename)

            # construct the full Dropbox path
            relative_path = os.path.relpath(local_path, local_directory)
            dropbox_path = os.path.join(dropbox_destination, relative_path)

            # upload the file
            with open(local_path, 'rb') as f:
                client.files_upload(f.read(), dropbox_path,
                                    mode=dropbox.files.WriteMode("overwrite"))
