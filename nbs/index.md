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

> Exploring economic dynamics in rural areas of the United States.

+++

## Research team

- Anton Babkin (UConn)
- Richard A. Dunn (UConn)
- Brent Hueth (UW-Madison)
- Thomas Flory (UW-Madison)

+++

## Installation

1. Clone this git project.
2. Create and activate conda environment from `environment.yml` file.
3. Pull resources from shared repository.

+++

# Project structure

- InfoGroup preparation: validate, clean, convert
- ERS codes preparation: download, combine, format
  - Other geo codes will be added as well
- Rurality: merge ERS codes into InfoGroup
- Dynamics: summary statistics by geography

+++

## Resource repository

+++

### List of registered resources

```{code-cell} ipython3
from rurec import resources

for r in resources.get():
    print(f'{r.key:30}{r.description}')
```

### Pull everything

```{code-cell} ipython3
resources.pull()
```

### Push everything

```{code-cell} ipython3
resources.push()
```
