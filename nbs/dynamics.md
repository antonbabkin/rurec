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

# Business dynamics

> How different slices of economy evolve over time.

```{code-cell} ipython3
:tags: []

import functools

import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from rurec import rurality
from rurec.pubdata import cbp, bds, naics, population, ers_rurality
from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
```

```{code-cell} ipython3
:tags: []

pd.options.display.max_colwidth = 300
```

## Data

GDP price deflator from BEA, downloaded from [FRED](https://fred.stlouisfed.org/series/A191RD3A086NBEA).

```{code-cell} ipython3
:tags: []

def get_deflator():
    d = pd.read_csv(nbd.root / 'data/A191RD3A086NBEA.csv').rename(columns={'DATE': 'year', 'A191RD3A086NBEA': 'deflator'})
    d['year'] = d['year'].str[:4].astype('int16')
    d = d.sort_values('year', ignore_index=True)
    # normalize latest year = 1
    d['deflator'] /= d['deflator'].iloc[-1]
    return d
```

```{code-cell} ipython3
:tags: []

get_deflator().set_index('year').plot(grid=True)
```

# Rurality

Business dynamics in rural areas, based on different definitions of rurality.

+++

## OMB definition

Rural = micropolitan and noncore. Non-rural = metropolitan.

For now, using ERS UI codes because I have them ready. They are a finer county level subdivision of CBSAs.

```{code-cell} ipython3
:tags: []

df = ers_rurality.get_ui_df().rename(columns=str.lower)
df['rural'] = ~df['ui_code'].isin(['1', '2'])
df = df[['fips', 'ui_year', 'rural']].rename(columns={'fips': 'stcty'})
rural = df
```

CODO: FIPS county code 999 probably means state-wide, and can not be classified as rural or non-rural.

```{code-cell} ipython3
:tags: []

def load_cbp():
    df = cbp.get_parquet('county', cols=['year', 'fipstate', 'fipscty', 'emp', 'est', 'ap'],
                         filters=[('industry', '=', '-')])
    df['stcty'] = df['fipstate'] + df['fipscty']
    del df['fipstate']
    del df['fipscty']
    return df
```

Timing. OMB classification is using data from decennial censuses. So if a county was classified as rural in 2013 revision of UI was rural from at least 2010, the year of preceeding decennial census.

```{code-cell} ipython3
:tags: []

df = load_cbp()

# deflate payroll
df = df.merge(get_deflator(), 'left', 'year')
df['ap'] /= df['deflator']

# add population
d = population.get_df()
d['stcty'] = d['st'] + d['cty']
del d['st']
del d['cty']
df = df.merge(d, 'left', ['year', 'stcty'])

# rural using constant definition
for y in [1993, 2003, 2013]:
    df['ui_year'] = y
    df = df.merge(rural, 'left', on=['ui_year', 'stcty']).rename(columns={'rural': f'rural_{y}'})

# rural using changing definition
df['ui_year'] = 2013
df.loc[df['year'] < 2010, 'ui_year'] = 2003
df.loc[df['year'] < 2000, 'ui_year'] = 1993
df = df.merge(rural, 'left', on=['ui_year', 'stcty']).rename(columns={'rural': 'rural_chng'})
```

```{code-cell} ipython3
:tags: []

agg = {}
for ry in [1993, 2003, 2013, 'chng']:
    rc = f'rural_{ry}'
    d = df.groupby(['year', rc])[['pop', 'emp', 'est', 'ap']].sum()
    d.columns.name = 'measure'
    d = d.stack().unstack(rc)
    d = d.rename(columns={False: 'nonrural', True: 'rural'})
    d['all'] = d.sum(1)
    agg[rc] = d
cbp_agg = agg = pd.concat(agg, axis=1, names=['rural_defn', 'rural'])
```

```{code-cell} ipython3
:tags: []

fig, ax = plt.subplots(1, 4, figsize=(24, 6))

colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
for ry, c in zip([1993, 2003, 2013, 'chng'], colors):
    t = agg[f'rural_{ry}']['rural'].unstack('measure')
    t = t.apply(lambda row: row / t.iloc[0], 1).add_suffix(f' {ry} rural')
    t.plot(ax=ax, subplots=True, color=c, grid=True)
    t = agg[f'rural_{ry}']['nonrural'].unstack('measure')
    t = t.apply(lambda row: row / t.iloc[0], 1).add_suffix(f' {ry} nonrural')
    t.plot(ax=ax, subplots=True, color=c, ls='--', grid=True)
    
fig.suptitle('Cumulative growth of population, employment, establishments and payroll');
```

```{code-cell} ipython3
:tags: []

pfit, pval = np.polynomial.polynomial.polyfit, np.polynomial.polynomial.polyval
fig, ax = plt.subplots(1, 4, figsize=(24, 6))

colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
t = agg['rural_2003']['rural'].unstack('measure')
t = (t / t.shift() * 100 - 100).add_suffix(f' {ry} rural')
t.plot(ax=ax, subplots=True, color=colors[0])
t = t.dropna().add_suffix(' trend').apply(lambda c: pval(c.index, pfit(c.index, c, 1)))
t.plot(ax=ax, subplots=True, color=colors[0], ls='--')

t = agg['rural_2003']['nonrural'].unstack('measure')
t = (t / t.shift() * 100 - 100).add_suffix(f' {ry} nonrural')
t.plot(ax=ax, subplots=True, color=colors[1])
t = t.dropna().add_suffix(' trend').apply(lambda c: pval(c.index, pfit(c.index, c, 1)))
t.plot(ax=ax, subplots=True, color=colors[1], ls='--', grid=True)

[a.axhline(c='black', lw=1) for a in ax]
fig.suptitle('Growth rate of population, employment, establishments and payroll');
```

```{code-cell} ipython3
:tags: []

fig, ax = plt.subplots(1, 4, figsize=(24, 6))

colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
for ry, c in zip([1993, 2003, 2013, 'chng'], colors):
    t = agg[f'rural_{ry}']
    t = t['rural'] / t['all'] * 100
    t = t.unstack().add_suffix(f' {ry}')
    t.plot(ax=ax, subplots=True, ylim=(0, 25), color=c, grid=True)
    
fig.suptitle('Share of population, employment, establishments and payroll in rural areas, %');
```

## BDS

```{code-cell} ipython3
:tags: []

df = bds.get_df('met')[['year', 'metro', 'firms', 'estabs', 'emp']]\
    .query('metro in ["M", "N"]')
df['rural'] = df['metro'].map({'N': 'rural', 'M': 'nonrural'})
del df['metro']
df = df.set_index(['year', 'rural'])
df.columns.name = 'measure'
df = df.stack().unstack('rural')
df['all'] = df.sum(1)
bds_agg = df
```

There are no decennial jumps in BDS series. Likely BDS is using a single revision of OMB definition, probably the latest.

```{code-cell} ipython3
:tags: []

bds_agg['rural'].unstack().plot(grid=True, subplots=True, layout=(1, 3), figsize=(18, 4), title='Rural businesses in BDS');
```

```{code-cell} ipython3
:tags: []

idx = pd.IndexSlice
colors = plt.rcParams['axes.prop_cycle'].by_key()['color']

d = bds_agg.loc[idx[:, ['estabs', 'emp']], :].stack().unstack(['measure', 'rural'])
d.columns = d.columns.to_flat_index()
ax = d.add_suffix(' BDS').plot(color=colors[0], grid=True, subplots=True, layout=(2, 3), figsize=(18, 8), title='BDS vs CBP')

d = cbp_agg['rural_2013'].loc[idx[:, ['est', 'emp']], :].stack().unstack(['measure', 'rural'])
d.columns = d.columns.to_flat_index()
d.add_suffix(' CBP').plot(ax=ax, color=colors[1], subplots=True);
```

Why difference between CBP and BDS?  
Short official [note](https://www.census.gov/programs-surveys/bds/documentation/comparability.html). Estabs may be lower in BDS because of de-duplication.

```{code-cell} ipython3
:tags: []

colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
df = cbp.get_parquet('us').query('industry == "-" and lfo == "-"')[['emp', 'est', 'year']].set_index('year')
ax = df.add_suffix(' CBP').plot(color=colors[0], subplots=True, layout=(1,2), figsize=(18, 6))
df = bds.get_df().set_index('year')[['emp', 'estabs']]
df.add_suffix(' BDS').plot(color=colors[1], ax=ax, subplots=True);
```

Is difference correlated with county?

```{code-cell} ipython3
:tags: []

df = bds.get_df('cty').query('year > 1985')
df['stcty'] = df['st'] + df['cty']
df = df[['year', 'stcty', 'emp', 'estabs']].rename(columns={'estabs': 'est'})
d = load_cbp().drop(columns='ap')
df = df.merge(d, 'outer', ['year', 'stcty'], suffixes=('_bds', '_cbp'), indicator=True)
```

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
d = df.groupby('year')[['est_bds', 'est_cbp']].sum()
d['est_cbp'] - d['est_bds']
```

```{code-cell} ipython3
:tags: []

df.head()
```

Before 2004 about 10% of the difference was non-merges on county code, but they cancel each other out, so are not the primary reason.

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
d = df.groupby(['year', '_merge'])[['est_bds', 'est_cbp']].sum()
d = d.sum(1).unstack()
d
```

```{code-cell} ipython3
:tags: []

d = df.query('_merge == "both"').groupby('year')[['est_bds', 'est_cbp']].sum()
d.plot()
```

```{code-cell} ipython3
:tags: []

d = df.query('_merge == "both" and year == 2019').copy()
d['bds/cbp'] = d['est_bds'] / d['est_cbp']
d['bds/cbp'].describe()
```
