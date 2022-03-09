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

# NAICS

North American Industry Classification System

[Homepage](https://www.census.gov/naics/) |
[Downloads](https://www.census.gov/naics/?48967)

The first two digits designate the economic sector, the third digit designates the subsector, the fourth digit designates the industry group, the fifth digit designates the NAICS industry, and the sixth digit designates the national industry.

```{code-cell} ipython3
:tags: [nbd-module]

import pandas as pd

from rurec.reseng.util import download_file
from rurec.reseng.nbd import Nbd

nbd = Nbd('rurec')
PATH = {
    'source': nbd.root/'data/naics/source/',
    'naics_codes': nbd.root/'data/bea_io/naics_codes.csv'
}
```

```{code-cell} ipython3
:tags: [nbd-module]

def download_source_files():
    base = 'https://www.census.gov/naics/'
    
    url = f'{base}2012NAICS/2012_NAICS_Index_File.xls'
    download_file(url, PATH['source'])

    url = f'{base}2012NAICS/2-digit_2012_Codes.xls'
    download_file(url, PATH['source'])
```

```{code-cell} ipython3
:tags: []

download_source_files()
```

```{code-cell} ipython3
:tags: [nbd-module]

def get_codes_df():
    df = pd.read_excel(PATH['source']/'2-digit_2012_Codes.xls', skiprows=2, header=None, usecols=[1, 2], dtype=str)
    df.columns = ['code', 'desc']

    df['digits'] = df['code'].str.len()
    df.loc[df['code'] == '31-33', 'digits'] = 2
    df.loc[df['code'] == '44-45', 'digits'] = 2
    df.loc[df['code'] == '48-49', 'digits'] = 2

    df.loc[df['digits'] == 2, 'code_2'] = df['code']
    df['code_2'] = df['code_2'].fillna(method='ffill')
    df.loc[df['digits'] == 3, 'code_3'] = df['code']
    df['code_3'] = df.groupby('code_2')['code_3'].fillna(method='ffill')
    df.loc[df['digits'] == 4, 'code_4'] = df['code']
    df['code_4'] = df.groupby('code_3')['code_4'].fillna(method='ffill')
    df.loc[df['digits'] == 5, 'code_5'] = df['code']
    df['code_5'] = df.groupby('code_4')['code_5'].fillna(method='ffill')
    df.loc[df['digits'] == 6, 'code_6'] = df['code']
    
    return df
```

Example: 6-digit industries in "115" subsector.

```{code-cell} ipython3
:tags: []

get_codes_df().query('code_3 == "115" and digits == 6')
```

Number of classes in each level.

```{code-cell} ipython3
:tags: []

get_codes_df()['digits'].value_counts().sort_index()
```

## Build this module

```{code-cell} ipython3
:tags: []

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('naics.ipynb')
```
