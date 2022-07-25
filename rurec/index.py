#!/usr/bin/env python
# coding: utf-8

import os, pathlib

def init_symlinks():
    """Recreate symlinks of this project and all subprojects."""
    print('Initializing symlinks for the project "rurec".')
    root_dir = _dir_up()
    print(f'VERIFY! Project root directory: "{root_dir}"')
    
    _recreate_dir_symlink('nbs/rurec', '../rurec', root_dir)
    _recreate_dir_symlink('rurec/reseng', '../submodules/reseng/reseng', root_dir)
    from . import reseng
    _recreate_dir_symlink('rurec/pubdata', '../submodules/pubdata/pubdata', root_dir)
    _recreate_dir_symlink('data/pubdata', '../submodules/pubdata/data', root_dir)
    from . import pubdata
    _recreate_dir_symlink('rurec/infogroup', '../submodules/infogroup/infogroup', root_dir)
    from . import infogroup
    
    from .pubdata import index as pubdata_index
    pubdata_index.init_symlinks()
    
def _dir_up():
    """Return dir path two levels above current notebook or script."""
    try:
        caller_dir = pathlib.Path(__file__).parent.resolve()
    except Exception as e:
        if str(e) != "name '__file__' is not defined": raise
        caller_dir = pathlib.Path.cwd()
    return caller_dir.parent

def _recreate_dir_symlink(link, targ, root):
    """Remove and create new symlink from `link` to `targ`.
    `link` must be relative to `root`.
    `targ must be relative to directory containing `link`.
    """
    link = (root / link).absolute()
    assert (link.parent / targ).is_dir()
    link.unlink(missing_ok=True)
    link.symlink_to(pathlib.Path(targ), target_is_directory=True)
    link_res = link.resolve()
    assert link_res.is_dir()
    print(f'symlink: "{link.relative_to(root)}" -> "{link_res.relative_to(root)}"')

