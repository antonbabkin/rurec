# AUTOGENERATED! DO NOT EDIT! File to edit: nbs/util.ipynb (unless otherwise specified).

__all__ = ['get_root_dir', 'download_file']

# Cell
from pathlib import Path
from urllib.parse import urlparse, unquote

import requests

# Cell
def get_root_dir():
    """Return `pathlib.Path` to project root directory.
    Root is identified by presence of specific files and directories in it."""
    root_members = {'rurec', 'tmp', 'data', 'settings.ini'}
    d = Path.cwd()
    while d != d.parent:
        if root_members < {x.name for x in d.iterdir()}:
            return d
        d = d.parent
    raise Exception(f'Can not find project root at or above "{Path.cwd()}"')

# Cell
def download_file(url, dir=None, fname=None, overwrite=False):
    """Download file from given `url` and put it into `dir`.
    Current working directory is used as default. Missing directories are created.
    File name from `url` is used as default.
    Return absolute pathlib.Path of the downloaded file."""

    if dir is None:
        dir = '.'
    dpath = Path(dir).resolve()
    dpath.mkdir(parents=True, exist_ok=True)

    if fname is None:
        fname = Path(urlparse(url).path).name
    fpath = dpath / fname

    if not overwrite and fpath.exists():
        print(f'File {fname} already exists.')
        return fpath

    with requests.get(url) as r:
        r.raise_for_status()
        with open(fpath, 'wb') as f:
            f.write(r.content)

    print(f'Downloaded file {fname}.')
    return fpath