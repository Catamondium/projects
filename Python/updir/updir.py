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

def dbx_list(dbx, drop):
    files = []
    resp = dbx.files_list_folder(drop, recursive=True, include_non_downloadable_files=False)
    files += [x.path_display for x in resp.entries
                if isinstance(x, dropbox.files.FileMetadata)]
    while resp.has_more:
        resp = dbx.files_list_folder_continue(resp.cursor)
        files += [x.path_display for x in resp.entries
                    if isinstance(x, dropbox.files.FileMetadata)]
    return set(files)

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

def download(dbx, dnames, local, drop):
    for drop_path in tqdm(dnames, desc="Down"):
        relative_path = Path(drop_path).relative_to(drop)
        local_path = Path(local) / relative_path
        local_path.parent.mkdir(parents=True, exist_ok=True)
        local_path.touch(exist_ok=True)
        dbx.files_download_to_file(str(local_path), drop_path)

def _main():
    import argparse
    parser = argparse.ArgumentParser("Manage dropbox stuff")
    parser.add_argument("pair", type=pair, nargs='+',
                        help="LOCAL,DROP directory pairs")
    parser.add_argument("--dry", action='store_true',
                        help="Print changes w/o sending")

    parser.add_argument("--down", "-d", action='store_true',
                        help="Download recursively")
    parser.add_argument("--up", "-u", action='store_true',
                        help="Upload recursively")
    parser.add_argument("--sync", "-s", action='store_true',
                        help="Upload, then download difference")
    args = parser.parse_args()

    do_up = args.up or args.sync
    do_down = args.down or args.sync
    client = connect()
    for local, drop in args.pair:
        # enumerate local files recursively
        up = list(Path(local).rglob("*"))
        ups = set()
        if do_up:
            for local_path in tqdm(up, desc="Up"):

                if local_path.is_dir():
                    continue

                relative_path = local_path.relative_to(local)
                drop_path = str(Path(drop) / relative_path)
                ups.add(drop_path)
                if args.dry:
                    print(f"{local_path} -> {drop_path}")
                else:
                    # upload the file
                    with open(local_path, 'rb') as f:
                        upload(client, f, local_path.stat().st_size, drop_path)
        if do_down:
            downs = dbx_list(client, drop)
            diff = downs - ups
            download(client, diff, local, drop)


if __name__ == "__main__":
    _main()
