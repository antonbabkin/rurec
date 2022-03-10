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

```{code-cell} ipython3
:tags: [nbd-module]

import typing

import pandas as pd
import pyarrow

from rurec.reseng.util import download_file
from rurec.reseng.nbd import Nbd

nbd = Nbd('rurec')
PATH = {
    'root': nbd.root,
    'source': nbd.root/'data/cbp/source/',
    'parquet': nbd.root/'data/cbp/parquet/'
}
```

+++ {"tags": []}

## Source files

U.S. record layouts:
[1986-1997](),
[1998-2006](),
[2007-2013](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/us_lfo_layout.txt),
[2014](),
[2015-2016](),
[2017](),
[2018-2019]()



County record layouts:
[1986-1997](http://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/full-layout/county_layout_sic.txt),
[1998-2006](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/full-layout/county_layout.txt),
[2007-2013](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/county_layout.txt),
[2014](https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2014_record_layouts/county_layout_2014.txt),
[2015-2016](https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/county_layout_2015.txt),
[2017](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/2017_record_layouts/county_layout_2017.txt),
[2018-2019](https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/2018_record_layouts/county-layout-2018.txt)

```{code-cell} ipython3
:tags: [nbd-module]

def get_source_file(geo: typing.Literal['us', 'state', 'county'], year: int):
    ext = 'txt' if geo == 'us' and year < 2008 else 'zip'
    path = PATH['source']/f'{geo}/{year}.{ext}'
    if path.exists(): return path

    yr = str(year)[2:]
    if geo == 'us':
        url = f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}us.{ext}'
    elif geo == 'state':
        url = f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}st.zip'
    elif geo == 'county':
        url = f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}co.zip'
    
    download_file(url, path.parent, path.name)
    
    return path
```

### Columns over years

`sic` was replaced with `naics` in 1998.  
`1_4` size class was renamed to `<5` in 2017.  
`...nf` (noise flags) were added in 2007.  
`empflag` was removed in 2018.

**U.S.**  
`lfo` was added in 2008.  
`f...` (Data Suppression Flag) removed in 2018.

**State**  
`lfo` was added in 2010.  
`f...` (Data Suppression Flag) removed in 2018.

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
# U.S.
df = {}
for y in range(1986, 2020):
    d = pd.read_csv(get_source_file('us', y), dtype=str, nrows=1)
    df[y] = pd.Series(True, index=d.columns.str.lower())
df = pd.concat(df, axis=1).fillna(False).replace({False: '', True: 'X'})
with pd.option_context('display.max_columns', 50, 'display.max_rows', 100):
    display(df)
```

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
# state
df = {}
for y in range(1986, 2020):
    d = pd.read_csv(get_source_file('state', y), dtype=str, nrows=1)
    df[y] = pd.Series(True, index=d.columns.str.lower())
df = pd.concat(df, axis=1).fillna(False).replace({False: '', True: 'X'})
with pd.option_context('display.max_columns', 50, 'display.max_rows', 100):
    display(df)
```

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
# county
df = {}
for y in range(1986, 2020):
    d = pd.read_csv(get_source_file('county', y), dtype=str, nrows=1)
    df[y] = pd.Series(True, index=d.columns.str.lower())
df = pd.concat(df, axis=1).fillna(False).replace({False: '', True: 'X'})
with pd.option_context('display.max_columns', 50):
    display(df)
```

Not sure how SIC classification works. There are only 9 unique 3-digit codes (`'399/', '497/', '519/', '599/', '899/', '679/', '149/', '179/', '098/'`), which seems too little. Maybe it is not nested in the same sense as NAICS is.

```{code-cell} ipython3
:tags: [nbd-module]

employee_size_classes = ['1_4', '5_9', '10_19', '20_49', '50_99', '100_249', '250_499', '500_999', '1000', '1000_1', '1000_2', '1000_3', '1000_4']

def get_df(geo: typing.Literal['us', 'state', 'county'], year: int):
    path = PATH['parquet']/f'{geo}/{year}/part.pq'
    if path.exists():
        return pd.read_parquet(path, 'pyarrow')

    # which columns to read and what data types, depending on year and geography
    dt = {
        'emp': float,
        'qp1': float,
        'ap': float,
        'est': float
    }

    if year < 1998:
        dt['sic'] = str
    else:
        dt['naics'] = str

    if geo == 'county':
        dt.update({'fipstate': str, 'fipscty': str})
    if geo == 'state':
        dt.update({'fipstate': str})
    
    if ((geo == 'us') and (year >= 2008)) or ((geo == 'state') and (year >= 2010)):
        dt.update({'lfo': str})
    
    suf = (['1_4'] if year < 2017 else ['<5']) \
        + (employee_size_classes[1:] if geo == 'county' else employee_size_classes[1:-4])
    dt.update({f'n{s}': float for s in suf})
    
    if (year == 2015) or ((geo, year) == ('us', 2006)):
        dt = {k.upper(): v for k, v in dt.items()}
    
    # numerical columns have "N" as N/A value
    na_val = {c: 'N' for c, t in dt.items() if t == float}

    df = pd.read_csv(get_source_file(geo, year), usecols=dt.keys(), dtype=dt, na_values=na_val)
    df.columns = df.columns.str.lower()
    
    if year >= 2017:
        df = df.rename(columns={'n<5': 'n1_4'})
    
    # add dummy lfo column for multi-year parquet dataset support
    if (geo in ['us', 'state']) and ('lfo' not in df):
        df['lfo'] = '-'
    
    df['ind_digits'] = -1
    if year < 1998:
        s = df['sic']
        assert s.notna().all() and (s != '').all()
        df.loc[s == '----', 'ind_digits'] = 0
        df.loc[s.str[:2].str.isdigit() & (s.str[2:] == '--'), 'ind_digits'] = 2
        pad = '/' if year == 1986 else '\\'
        df.loc[s.str[:3].str.isdigit() & (s.str[3] == pad), 'ind_digits'] = 3
        df.loc[s.str.isdigit(), 'ind_digits'] = 4
        df = df.rename(columns={'sic': 'industry'})
    else:
        s = df['naics']
        assert s.notna().all() and (s != '').all()
        df.loc[s == '------', 'ind_digits'] = 0
        df.loc[s.str[:2].str.isdigit() & (s.str[2:] == '----'), 'ind_digits'] = 2
        df.loc[s.str[:3].str.isdigit() & (s.str[3:] == '///'), 'ind_digits'] = 3
        df.loc[s.str[:4].str.isdigit() & (s.str[4:] == '//'), 'ind_digits'] = 4
        df.loc[s.str[:5].str.isdigit() & (s.str[5] == '/'), 'ind_digits'] = 5
        df.loc[s.str.isdigit(), 'ind_digits'] = 6
        df = df.rename(columns={'naics': 'industry'})

    df['ind_digits'] = df['ind_digits'].astype('int8')
    assert (df['ind_digits'] != -1).all()
    for d in [2,3,4,5]:
        df.loc[df['ind_digits'] == d, 'industry'] = df['industry'].str[:d]
    df.loc[df['industry'].str[0] == '-', 'industry'] = '-'
    
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path, 'pyarrow', index=False)
    return df


def build_all_parquet():
    for year in range(1986, 2020):
        for geo in ['us', 'state', 'county']:
            get_df(geo, year)
```

```{code-cell} ipython3
:tags: []

build_all_parquet()
```

With file structure like this (each year is stored `county/year/part.pq`), it is possible to read from multiple files treating them as [Parquet Dataset](https://arrow.apache.org/docs/python/dataset.html). Particularly useful can be the `filters` parameter - see filter specification [here](https://arrow.apache.org/docs/python/generated/pyarrow.parquet.read_table.html). Below are some examples.

```{code-cell} ipython3
:tags: []

part = pyarrow.dataset.partitioning(field_names=['year'])
# read subset of columns across all years
cols = ['year', 'fipstate', 'fipscty', 'industry', 'emp', 'est']
d = pd.read_parquet(PATH['parquet']/'county', 'pyarrow', columns=cols, partitioning=part)
# # filter by industry code
d = pd.read_parquet(PATH['parquet']/'county', 'pyarrow', columns=cols, partitioning=part,
                    filters=[('industry', '=', '-')])
# # filter by year and state
d = pd.read_parquet(PATH['parquet']/'county', 'pyarrow', columns=cols, partitioning=part,
                    filters=[('year', '>', 2000), ('fipstate', '=', '55')])
# US totals over time
d = pd.read_parquet(PATH['parquet']/'us', 'pyarrow', partitioning=part,
                    filters=[('lfo', '=', '-'), ('industry', '=', '-')])
```

```{code-cell} ipython3
:tags: [nbd-module]

def get_parquet(geo, cols=None, filters=None):
    path = PATH['parquet'] / geo
    part = pyarrow.dataset.partitioning(field_names=['year'])
    return pd.read_parquet(path, 'pyarrow', columns=cols, filters=filters,
                           partitioning=part)
```

```{code-cell} ipython3
:tags: []

get_parquet('state', ['year', 'est', 'emp', 'industry', 'lfo', 'fipstate'],
                 [('lfo', '=', '-'), ('industry', '=', '-'), ('fipstate', '=', '55')])
```

## Build this module

```{code-cell} ipython3
:tags: []

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('cbp.ipynb')
```
