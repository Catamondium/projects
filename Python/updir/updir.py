#!/bin/envrun
import dropbox
import pickle
from sys import stderr
from pathlib import Path
from webbrowser import open as wbopen
from tqdm import tqdm
from math import ceil

CHUNK_SIZE = int(4 * 1E6)

def loadcreds(location: Path):
    """
    Load application credentials
    """
    with open(location, 'r') as f:
        lines = list(map(str.strip, f))
        return lines[:2]


def connect(creds='creds.secret', access='client.secret') -> dropbox.Dropbox:
    """
    Authorises application w/ user and loads client
    """
    parent = Path(__file__).resolve().parent
    app = loadcreds(parent / creds)
    access_p = parent / access

    user = None
    if access_p.exists():
        with open(access_p, 'rb') as token:
            user = pickle.load(token)
    
    if not user:
        flow = dropbox.DropboxOAuth2FlowNoRedirect(*app)
        redirect = flow.start()
        print(f"Redirecting for authorisation: {redirect}\nCtl-C to continue...")
        wbopen(redirect)
        token = input("Copy access token here: ").strip()
        if not token:
            print("Error: bad input", file=stderr)
            exit(1)
        user = flow.finish(token)
        with open(access_p, 'wb+') as token:
            pickle.dump(user, token)
    return dropbox.Dropbox(user.access_token)

def pair(arg):
    return arg.split(',')


def large_upload(dbx, f, file_size, dest_path):
    upload_session_start_result = dbx.files_upload_session_start(
        f.read(CHUNK_SIZE))
    cursor = dropbox.files.UploadSessionCursor(
        upload_session_start_result.session_id, f.tell())
    commit = dropbox.files.CommitInfo(
        path=dest_path, mode=dropbox.files.WriteMode("overwrite"))
    while f.tell() < file_size:
        if ((file_size - f.tell()) <= CHUNK_SIZE):
            dbx.files_upload_session_finish(
                f.read(CHUNK_SIZE), cursor, commit)
        else:
            dbx.files_upload_session_append_v2(
                f.read(CHUNK_SIZE), cursor.session_id, cursor.offset)
            cursor.offset = f.tell()

def small_upload(dbx, f, file_size, dest_path):
    dbx.files_upload(f.read(), dest_path,
                    mode=dropbox.files.WriteMode("overwrite"))

def upload(dbx, f, file_size, dest_path):
    try:
        small_upload(
            dbx, f, file_size, dest_path)
    except:  # write timeout
        large_upload(
            dbx, f, file_size, dest_path)

def _main():
    import argparse
    parser = argparse.ArgumentParser("Upload directory to dropbox recursively")
    parser.add_argument("pair", type=pair, nargs='+',
                        help="LOCAL,DROP directory pairs")
    parser.add_argument("--dry", "-d", action='store_true',
                        help="Print changes w/o sending")
    args = parser.parse_args()

    client = connect()
    for local, drop in args.pair:
        # enumerate local files recursively
        length = sum(1 for x in Path(local).rglob("*"))
        for local_path in tqdm(Path(local).rglob("*"), desc="Files done", total=length):

            if local_path.is_dir():
                continue

            relative_path = local_path.relative_to(local)
            drop_path = str(Path(drop) / relative_path)

            if args.dry:
                print(f"{local_path} -> {drop_path}")
            else:
                # upload the file
                with open(local_path, 'rb') as f:
                    upload(client, f, local_path.stat().st_size, drop_path)


if __name__ == "__main__":
    _main()
