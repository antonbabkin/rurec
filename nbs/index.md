---
jupytext:
  formats: ipynb,md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.14.0
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

```{raw-cell}

---
title: "Rural economy"
format:
  html:
    code-fold: true
    ipynb-filters:
      - pubdata/reseng/nbd.py filter-docs
---
```

+++ {"jp-MarkdownHeadingCollapsed": true, "tags": ["nbd-docs"]}

Exploring economic dynamics in rural areas of the United States.

+++ {"tags": ["nbd-docs"]}

# Initialization

Run code in this section when you start working with the project.
It will create symbolic links necessary for file discovery within project directory structure.
If project is used as a library, importing code must call the `init()` function.

```{code-cell} ipython3
:tags: [nbd-module]

import os
import importlib
from pathlib import Path

def init():
    """Initialize project file structure by recreating symlinks to package and all submodule packages.
    Safe to run multiple times.
    """
    print('Initializing project "rurec" and submodules "reseng", "pubdata" and "infogroup"...')
    root_dir = _this_proj_root()
    print(f'  Project "rurec" root directory: "{root_dir}"')
    
    _recreate_dir_symlink('nbs/rurec', '../rurec', root_dir)
    
    _recreate_dir_symlink('rurec/reseng', '../submodules/reseng/reseng', root_dir)
    from rurec.reseng.index import init as reseng_init
    reseng_init()
    
    _recreate_dir_symlink('rurec/pubdata', '../submodules/pubdata/pubdata', root_dir)
    _recreate_dir_symlink('data/pubdata', '../submodules/pubdata/data', root_dir)
    from rurec.pubdata.index import init as pubdata_init
    pubdata_init()
    
    _recreate_dir_symlink('rurec/infogroup', '../submodules/infogroup/infogroup', root_dir)
    
    print('Initialization of "rurec" finished.\n')

def _this_proj_root():
    """Return abs path to this project's root dir."""
    try:
        # caller is "index.py" module
        caller_dir = Path(__file__).parent.resolve()
    except Exception as e:
        if str(e) != "name '__file__' is not defined": raise
        # caller is "index.ipynb" notebook
        caller_dir = Path.cwd()
    return caller_dir.parent

def _recreate_dir_symlink(link, targ, root):
    """Remove and create new symlink from `link` to `targ`.
    `link` must be relative to `root`.
    `targ` must be relative to directory containing `link`.
    Example: _recreate_dir_symlink('nbs/reseng', '../reseng', Path('/path/to/proj/root'))
    """
    link = (root / link).absolute()
    assert (link.parent / targ).is_dir()
    link.unlink(missing_ok=True)
    link.symlink_to(Path(targ), target_is_directory=True)
    link_res = link.resolve()
    assert link_res.is_dir()
    print(f'  symlink: "{link.relative_to(root)}" -> "{link_res.relative_to(root)}"')
```

+++ {"tags": ["nbd-docs"]}

Run initialization in the notebook.

```{code-cell} ipython3
:tags: [nbd-docs]

#| code-fold: false
init()
```

# Quick test

```{code-cell} ipython3
from rurec.pubdata import geography
geography.get_state_df(scale='20m').query('CONTIGUOUS').plot()
```

```{code-cell} ipython3
:tags: []

from rurec.pubdata import geography_cbsa
geography_cbsa.get_cbsa_delin_df(2020).query('CBSA_TITLE == "Madison, WI"')
```

```{code-cell} ipython3
:tags: []

from rurec.pubdata import bea_io
bea_io.get_naics_df().head()
```

```{code-cell} ipython3
:tags: []

from rurec import ag_output
ag_output.get_farm_sales_by_bea_detail(2017, 'national')
```

+++ {"tags": []}

# Reproduction: rurec.Rproj

- Start with the <rurec.Rproj> RStudio Project file.  
    - It keeps all the files associated with the project together e.g., data, scripts, results, and figures.
    - It lets you get back to where you left off: the same working directory, command history, and all the open files will persist across sessions.
    - The .Rproj file will also look for a <renv.lock> file to maintain environment version and structure, which makes the project Isolated, Portable, and Reproducible. 
        - Use command renv::status() to check differences between the project lockfile and the current library.
        - Use command renv::restore(lockfile = file.path(rprojroot::find_rstudio_root_file(), "renv.lock") ) to sync system and project package versions.
        - Use command renv::snapshot() if packages are added or updated. 
    
- Next navigate to the desired script or notebook using the Files tab (bottom right window in R), or File in computer tool bar (left top most), or ⌘O.  
    - For more on the relationships of project scripts and notebooks located in the /nbs folder see the *repo_structure* [schematic](https://docs.google.com/drawings/d/1z4iLABHF8wnfhSumAU7tXr68zDFd4wUfL8vclrVioBs/edit). 
    - As a rule simple .R scripts will generate functions and generate new data products. Whereas, .Rmd and .qmd notebooks will provide a narative with accompanying visualizations into a human readible and self contained .html output. 
        - The script <rural_typology_r_functions.R> is a repository for user generated functions used by all other project files.  
        - The script <rural_typology_r_data.R> downloads and imports all outside data from pubdata used in the project. 
     
        
        - The notebook <io_for_dummies.Rmd> is a reference page for all things Input-Output related. 
        - The notebook <national_connectedness.Rmd> generates hierarchical absorption matrices and spatial aglomeration data products. 
        - The notebook <connect_grant_example.qmd> a step by step toy example of the economic connectedness calculation process. 
        
        - The notebook <toy_stoy.Rmd> is depreciated. (a testing ground for Toy model specifications with numerical examples) 
        - The notebook <maps_io_similarity_ia.Rmd> is depreciated. (maps the various topological similarity index relationships for Iowa counties and Nebraska)
       
    - All data and generated R objects are stored in the /data and the /data/robjs files respectively.

+++

# Build this module

```{code-cell} ipython3
:tags: []

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('index.ipynb')
```
