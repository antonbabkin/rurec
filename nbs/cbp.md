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

[Homepage](https://www.census.gov/programs-surveys/cbp.html) |
[CSV datasets](https://www.census.gov/programs-surveys/cbp/data/datasets.html)

**County files**

Record layouts:
[1986-1997](http://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/full-layout/county_layout_sic.txt),
[1998-2006](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/full-layout/county_layout.txt),
[2007-2013](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/county_layout.txt),
[2014](https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2014_record_layouts/county_layout_2014.txt),
[2015-2016](https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/county_layout_2015.txt),
[2017](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/2017_record_layouts/county_layout_2017.txt),
[2018-2019](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/2018_record_layouts/county-layout-2018.txt)

```{code-cell} ipython3
:tags: [nbd-module]

import pandas as pd

from rurec.reseng.util import download_file
from rurec.reseng.nbd import Nbd


nbd = Nbd('rurec')
PATH = {
    'root': nbd.root,
    'source': nbd.root/'data/cbp/source/',
    'county': nbd.root/'data/cbp/county.pq/'
}
```

```{code-cell} ipython3
:tags: []

pd.options.display.max_columns = 50
```

```{code-cell} ipython3
:tags: [nbd-module]

def get_source_file(year):
    path = PATH['source']/f'co/{year}.zip'
    if path.exists(): return path
    yr = str(year)[2:]
    url = f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}co.zip'
    download_file(url, path.parent, path.name)
    return path
```

*Columns over years.*  
`sic` was replaced with `naics` in 1998.  
`n1_4` was replaced with `n<5` in 2017.  
`emp_nf`, `qp1_nf` and `ap_nf` were added in 2007.  
`empflag` was removed in 2018.

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
df = {}
for y in range(1986, 2020):
    d = pd.read_csv(get_source_file(y), dtype=str, nrows=1)
    df[y] = pd.Series(True, index=d.columns.str.lower())
df = pd.concat(df, axis=1).fillna(False).replace({False: '', True: 'X'})
df
```

Not sure how SIC classification works. There are only 9 unique 3-digit codes (`'399/', '497/', '519/', '599/', '899/', '679/', '149/', '179/', '098/'`), which seems to little. Maybe it is not nested in the same sense as NAICS is.

```{code-cell} ipython3
:tags: [nbd-module]

employee_size_classes = ['1_4', '5_9', '10_19', '20_49', '50_99', '100_249', '250_499', '500_999', '1000', '1000_1', '1000_2', '1000_3', '1000_4']

def get_county_df(year):
    path = PATH['county']/f'{year}.pq'
    if path.exists():
        return pd.read_parquet(path, 'pyarrow')

    dt = {
        'fipstate': str,
        'fipscty': str,
        'naics': str,
        'emp': float,
        'qp1': float,
        'ap': float,
        'est': float
    }
    suf = (['1_4'] if year < 2017 else ['<5']) + employee_size_classes[1:]
    dt.update({f'n{s}': float for s in suf})
    if year < 1998:
        del dt['naics']
        dt['sic'] = str
    if year == 2015:
        dt = {k.upper(): v for k, v in dt.items()}

    df = pd.read_csv(get_source_file(year), usecols=dt.keys(), dtype=dt, na_values='N')

    if year == 2015:
        df.columns = df.columns.str.lower()
    if year >= 2017:
        df = df.rename(columns={'n<5': 'n1_4'})
    
    df['ind_digits'] = -1
    if year < 1998:
        s = df['sic']
        assert s.notna().all() and (s != '').all()
        df.loc[s == '----', 'ind_digits'] = 0
        df.loc[s.str[:2].str.isdigit() & (s.str[2:] == '--'), 'ind_digits'] = 2
        df.loc[s.str[:3].str.isdigit() & (s.str[3] == '/'), 'ind_digits'] = 3
        df.loc[s.str.isdigit(), 'ind_digits'] = 4
    else:
        s = df['naics']
        assert s.notna().all() and (s != '').all()
        df.loc[s == '------', 'ind_digits'] = 0
        df.loc[s.str[:2].str.isdigit() & (s.str[2:] == '----'), 'ind_digits'] = 2
        df.loc[s.str[:3].str.isdigit() & (s.str[3:] == '///'), 'ind_digits'] = 3
        df.loc[s.str[:4].str.isdigit() & (s.str[4:] == '//'), 'ind_digits'] = 4
        df.loc[s.str[:5].str.isdigit() & (s.str[5] == '/'), 'ind_digits'] = 5
        df.loc[s.str.isdigit(), 'ind_digits'] = 6

    df['ind_digits'] = df['ind_digits'].astype('int8')
    assert (df['ind_digits'] != -1).all()
    
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path, 'pyarrow', index=False)
    return df
```

```{code-cell} ipython3
:tags: []

year = 2010
df = get_county_df(year)
df.head()
```

## Rural business dynamics

+++

## Build this module

```{code-cell} ipython3
:tags: []

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('cbp.ipynb')
```
