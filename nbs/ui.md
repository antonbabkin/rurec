---
jupytext:
  formats: ipynb,md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.13.7
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

# User Interface

> Interact with the project and excute various tasks.

```{code-cell} ipython3
#default_exp ui
```

## Initialization

Run this section to initialize your local repository after cloning. It is safe to run multiple times.

+++

### Symbolic links

```{code-cell} ipython3
import os
from pathlib import Path
lib_dir = Path('../rurec')
assert lib_dir.exists() and lib_dir.is_dir()
lib_link = Path('rurec')
lib_link.unlink(missing_ok=True)
os.symlink(lib_dir, lib_link, target_is_directory=True)

for submod in ['reseng', 'infogroup']:
    submod_dir = Path(f'../submodules/{submod}/{submod}')
    assert submod_dir.exists() and submod_dir.is_dir()
    submod_link = lib_dir/submod
    submod_link.unlink(missing_ok=True)
    os.symlink(submod_dir, submod_link, target_is_directory=True)
    
# test imports
import rurec
import rurec.reseng
import rurec.infogroup
```

### Git configuration

```{code-cell} ipython3
from rurec.reseng.config import Paths
root = Paths().root
pwd = Path().resolve()

%cd {root}
!nbdime config-git --enable
!git config filter.jupyternotebook.clean "jupyter nbconvert --stdin --stdout --to=notebook --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --log-level=ERROR"
!git config filter.jupyternotebook.smudge cat
!git config filter.jupyternotebook.required true
!git config diff.jupyternotebook.command "git-nbdiffdriver diff --ignore-outputs --ignore-metadata --ignore-details"
%cd {pwd}
```

## Command line interface

Call with `python -m rurec.ui [args]`.

```{code-cell} ipython3
#export
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Perform tasks within Rural Economy project.')
    parser.add_argument('task', help='choose a task', choices=['build_parquet', 'add_rurality'])
    args = parser.parse_args()
    print(args.task, 'started')
    if args.task == 'build_parquet':
        from rurec import infogroup
        infogroup.build_parquet_dataset(21)
    elif args.task == 'add_rurality':
        from rurec import rurality
        rurality.build_parquet_dataset(11)
    print(args.task, 'finished')
```
