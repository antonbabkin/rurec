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

+++ {"jp-MarkdownHeadingCollapsed": true, "tags": []}

# Reproduction: rurec.Rproj

- Start with the <rurec.Rproj> RStudio Project file.  
    - It keeps all the files associated with the project together e.g., data, scripts, results, and figures.
    - It lets you get back to where you left off: the same working directory, command history, and all the open files will persist across sessions.
    - The .Rproj file will also look for a <renv.lock> file to maintain environment version and structure, which makes the project Isolated, Portable, and Reproducible. 
    
- Next navigate to the desired script or notebook using the Files tab (bottom right window in R), or File in computer tool bar (left top most), or ⌘O.  
    - For more on the relationships of project scripts and notebooks see the *repo_structure* [schematic](https://docs.google.com/drawings/d/1z4iLABHF8wnfhSumAU7tXr68zDFd4wUfL8vclrVioBs/edit). 
    - For example, in the /nbs folder is an RMarkdown notebook <io_for_dummies.Rmd> that is a reference page for all things Input-Output related. 
    
    - As a rule simple .R scripts will generate function, download raw data, and import, clean, and generate new data products. Whereas, .Rmd and .qmd notebooks will provide a narative with accompanying visualizations into a human readible and self contained .html output. 
    
- To get a clear reset of all data and generated objects simply delete the /data folder. 
    

+++

::: {.hidden}
# Control panel
:::

```{code-cell} ipython3
#| echo: false
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
