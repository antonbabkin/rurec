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

# Rural economy

Exploring economic dynamics in rural areas of the United States.

+++ {"tags": []}

# Project structure
 
- Public datasets preparation: CBP, BDS, BEA IO tables, Census population estimates, NAICS classification.
- Geographic data: working with shapefiles of administrative units.
- Rurality definitions: Census, OMB, ERS.
- Agricultural and food industries (AFI) definitions.
- InfoGroup data preparation.
- Dynamics: summary statistics by geography and sector.

+++

# Recreate symlinks (Windows)

```{code-cell} ipython3
:tags: [nbd-module]

import os, pathlib

def init_symlinks():
    """Recreate symlinks of this project and all subprojects."""
    print('Initializing symlinks for the project "rurec".')
    root_dir = _dir_up()
    print(f'VERIFY! Project root directory: "{root_dir}"')
    
    _recreate_dir_symlink('nbs/rurec', '../rurec', root_dir)
    _recreate_dir_symlink('rurec/reseng', '../submodules/reseng/reseng', root_dir)
    from rurec import reseng
    _recreate_dir_symlink('rurec/pubdata', '../submodules/pubdata/pubdata', root_dir)
    _recreate_dir_symlink('data/pubdata', '../submodules/pubdata/data', root_dir)
    from rurec import pubdata
    _recreate_dir_symlink('rurec/infogroup', '../submodules/infogroup/infogroup', root_dir)
    from rurec import infogroup
    
    from rurec.pubdata import index as pubdata_index
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
```

```{code-cell} ipython3
init_symlinks()
```

# Quick test

```{code-cell} ipython3
from rurec.pubdata import geography
geography.get_state_df(scale='20m').query('CONTIGUOUS').plot()
```

```{code-cell} ipython3
from rurec import rurality
rurality.get_cbsa_delin_df(2020).query('CBSA_TITLE == "Madison, WI"')
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
    - As a rule simple .R scripts will generate function, download raw data, and import, clean, and generate new data products. Whereas, .Rmd and .qmd notebooks will provide a narative with accompanying visualizations into a human readible and self contained .html output. 
        - The script <rural_typology_r_functions.R> is a repository for user generated functions used by all other project files.  
        - The script <rural_typology_r_data_sources.R> downloads all outside data used in the project. 
        - The script <rural_typology_r_data_import.R> imports all downloaded data used in the project as R data objects.
        - The script <rural_typology_r_data_clean.R> cleans and reshapes imported data used in the project.
        - The script <rural_typology_r_data_generation.R> generates new data products (e.g., similarity index specifications) used in the project.       
        
        - The notebook <io_for_dummies.Rmd> is a reference page for all things Input-Output related. 
        - The notebook <toy_stoy.Rmd> is a testing ground for Toy model specifications with numerical examples. 
        - The notebook <maps_io_similarity_wi.Rmd> is an attempt at mapping the various topological similarity index relationships for Wisconsin counties. 
        - The notebook <maps_io_similarity_national.qmd> is an attempt at mapping the various topological similarity index relationships at the national level. 
    - All data and generated R objects are stored in the /data and the /data/robjs files respectively. 
    - To get a clear reset of all data and generated objects simply delete the /data folder.

+++

# Build this module

```{code-cell} ipython3
from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('index.ipynb')
```
