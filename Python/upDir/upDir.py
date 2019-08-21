#!/usr/bin/env python3
import dropbox
from pathlib import Path

CHUNK_SIZE = 4 * 1024 * 1024


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


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser("Upload directory to dropbox recursively")
    parser.add_argument("pair", type=pair, nargs='+',
                        help="LOCAL,DROP directory pairs")
    parser.add_argument("--dry", action='store_true',
                        help="Print changes w/o sending")
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

            if args.dry:
                print(f"{local_path} -> {drop_path}")
            else:
                # upload the file
                with open(local_path, 'rb') as f:
                    try:
                        client.files_upload(f.read(), drop_path,
                                            mode=dropbox.files.WriteMode("overwrite"))
                    except:  # write timeout
                        large_upload(
                            client, f, local_path.stat().st_size, drop_path)
