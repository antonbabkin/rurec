# Rural economy

Exploring economic dynamics in rural areas of the United States.


# Installation

Follow these steps to begin working on this project.

1. Install [Mamba](https://mamba.readthedocs.io), R, RStudio and Git.

2. Clone this repository with submodules.  
`git clone --recurse-submodules git@github.com:antonbabkin/rurec.git`

3. Use Mamba to create new Python environment from the spec file.  
`mamba env create --file environment.yml`  
This will download and install all necessary packages.
The environment includes Jupyter Lab and [Jupytext](https://jupytext.readthedocs.io) for working with notebooks.   

4. From Jupyter, run code in the "Initialization" section of `nbs/index.ipynb` notebook.
This will create symbolic links connecting submodules to the parent project.
Notebooks are stored in `.md` format, for the first time, right click to open as Notebook.

5. In RStudio, open project and choose `rurec.Rproj`.
Create a new `renv` environment and install R packages.
Required packages are specified in the `DESCRIPTION` file.  
`renv::init(bare = TRUE)`  
`renv::install()`



