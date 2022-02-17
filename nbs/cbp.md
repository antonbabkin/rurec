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

# County Business Patterns

```{code-cell} ipython3
:tags: [nbd-module]

import pandas as pd

from rurec.reseng.util import download_file
from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
```

```{code-cell} ipython3
:tags: [nbd-module]

def get_df(year):
    yr = str(year)[2:]
    f = download_file(f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}co.zip', nbd.root / 'data')
    df = pd.read_csv(f)
    return df
```

## Build this module

```{code-cell} ipython3
:tags: []

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('cbp.ipynb')
```
