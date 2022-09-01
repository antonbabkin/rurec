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

# Business dynamics

> How different slices of economy evolve over time.

```{code-cell} ipython3
:tags: []

import functools

import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pyarrow
import pyarrow.dataset

from rurec import rurality
from rurec.pubdata import geography, cbp, bds, naics, population, ers_rurality
from rurec.reseng.nbd import Nbd
from rurec.reseng.util import download_file
nbd = Nbd('rurec')
```

```{code-cell} ipython3
:tags: []

pd.options.display.max_colwidth = 300
```

# Data

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

# Rurality definition

County based OMB CBSA. Rural = nonmetro.

```{code-cell} ipython3
:tags: []

def get_county_rurality():
    """County classification to span 2000-2019 years of data.
    RURAL_2000 and RURAL_2010 use respective OMB revisions.
    RURAL_CHNG groups entire period into "rural", "nonrural" and "formerly rural".
    "nonrural->rural" is small in size (estab, emp) and is lumped together with "rural".
    """
    df = geography.get_county_df(2010, False)

    d = rurality.get_cbsa_delin_df(2003)[['STATE_CODE', 'COUNTY_CODE', 'METRO_MICRO']]
    df = df.merge(d, 'left')
    df['RURAL_2000'] = ~(df['METRO_MICRO'] == 'metro')
    del df['METRO_MICRO']

    d = rurality.get_cbsa_delin_df(2013)[['STATE_CODE', 'COUNTY_CODE', 'METRO_MICRO']]
    df = df.merge(d, 'left')
    df['RURAL_2010'] = ~(df['METRO_MICRO'] == 'metro')
    del df['METRO_MICRO']
    
    df['RURAL_CHNG'] = pd.Series(dtype=pd.CategoricalDtype(['rural', 'formerly rural', 'nonrural'], True))
    df.loc[df['RURAL_2010'], 'RURAL_CHNG'] = 'rural'
    df.loc[~df['RURAL_2000'] & ~df['RURAL_2010'], 'RURAL_CHNG'] = 'nonrural'
    df.loc[df['RURAL_2000'] & ~df['RURAL_2010'], 'RURAL_CHNG'] = 'formerly rural'

    return df[['STATE_CODE', 'COUNTY_CODE', 'RURAL_2000', 'RURAL_2010', 'RURAL_CHNG']]
```

```{code-cell} ipython3
:tags: []

get_county_rurality()[['RURAL_2000', 'RURAL_2010']].value_counts()
```

```{code-cell} ipython3
:tags: []

get_county_rurality()['RURAL_CHNG'].value_counts()
```

```{code-cell} ipython3
:tags: []

df = get_county_rurality()
d = cbp.get_df('county', 2010)\
    .rename(columns=str.upper)\
    .query('INDUSTRY == "-"')\
    .rename(columns={'FIPSTATE': 'STATE_CODE', 'FIPSCTY': 'COUNTY_CODE'})\
    [['STATE_CODE', 'COUNTY_CODE', 'EST', 'EMP', 'AP']]
df = df.merge(d, 'left')
```

```{code-cell} ipython3
:tags: []

df.groupby(['RURAL_2000', 'RURAL_2010'])[['EST', 'EMP', 'AP']].sum()
```

# BDS by rurality

Explore 2000-2019 trends in rural, nonrural and formerly rural from BDS data.

```{code-cell} ipython3
:tags: []

df = bds.get_df('cty')\
    .rename(columns=str.upper)\
    .rename(columns={'ST': 'STATE_CODE', 'CTY': 'COUNTY_CODE'})\
    .query('YEAR >= 2000')

df = df.merge(get_county_rurality())
```

- Nonrural grows faster than rural.
- Formerly rural is looks like a mix between rural and nonrural.
- Notable difference of rural is slower recover after great recession.
EMP has barely recovered, and EST is stuck at the decreased level.

```{code-cell} ipython3
:tags: []

t = df.groupby(['YEAR', 'RURAL_CHNG'])[['ESTABS', 'EMP']].sum()
t.columns.name = 'MEASURE'
t = t.unstack('RURAL_CHNG')
t.plot(subplots=True, layout=(2, 3), figsize=(16, 8), grid=True, title='Aggregate estab and emp by YEAR and RURAL_CHNG');
```

```{code-cell} ipython3
:tags: []

t = df.groupby(['YEAR', 'RURAL_CHNG'])[['ESTABS', 'EMP']].sum()
t.columns.name = 'MEASURE'
t = t.unstack('RURAL_CHNG')
t = t.apply(lambda col: col / col.iloc[0])
ax = t.plot(subplots=True, layout=(2, 3), figsize=(16, 8), grid=True, ylim=(0.8, 1.2),
            title='Growth in Aggregate estab and emp by YEAR and RURAL_CHNG (2000 = 100%)');
```

- Rural establishments are smaller in mean employment.

```{code-cell} ipython3
:tags: []

t = df.groupby(['YEAR', 'RURAL_CHNG'])[['ESTABS', 'EMP']].sum()
t.columns.name = 'MEASURE'
t = t['EMP'] / t['ESTABS']
t = t.unstack('RURAL_CHNG')
t.plot(subplots=True, layout=(1, 3), figsize=(16, 4), title='Mean EMP by YEAR and RURAL_CHNG', grid=True, ylim=(0, 20));
```

- Churn is decreasing over time everywhere.
- Rural has less churn than nonrural.
- Entry rate after GR is stuck steady at lower levels everywhere.
- In rural, unlike elsewhere, exit rate has not fallen enough to be offset by entry, so number of establishments has not recovered.

```{code-cell} ipython3
:tags: []

t = df.groupby(['YEAR', 'RURAL_CHNG'])[['ESTABS', 'ESTABS_ENTRY', 'ESTABS_EXIT']].sum()
t.columns.name = 'MEASURE'
t['ENTRY_RATE'] = t['ESTABS_ENTRY'] / t['ESTABS'] * 100
t['EXIT_RATE'] = t['ESTABS_EXIT'] / t['ESTABS'] * 100
t = t[['ENTRY_RATE', 'EXIT_RATE']]
t = t.unstack('RURAL_CHNG')
ax = t['ENTRY_RATE'].add_suffix(' ENTRY_RATE')\
    .plot(subplots=True, layout=(1, 3), figsize=(16, 4), title='Estab entry and exit rate by YEAR and RURAL_CHNG')
t['EXIT_RATE'].add_suffix(' EXIT_RATE').plot(subplots=True, ax=ax, ls='--', grid=True, ylim=(5, 15));
```

- Slightly less churn in jobs in rural than elsewhere.
- Patterns are similar, with slightly weaker net creation in rural post-GR.

```{code-cell} ipython3
:tags: []

t = df.groupby(['YEAR', 'RURAL_CHNG'])[['DENOM', 'JOB_CREATION', 'JOB_DESTRUCTION']].sum()
t.columns.name = 'MEASURE'
t['JOB_CREATION_RATE'] = t['JOB_CREATION'] / t['DENOM'] * 100
t['JOB_DESTRUCTION_RATE'] = t['JOB_DESTRUCTION'] / t['DENOM'] * 100
t = t[['JOB_CREATION_RATE', 'JOB_DESTRUCTION_RATE']]
t = t.unstack('RURAL_CHNG')
ax = t['JOB_CREATION_RATE'].add_suffix(' CREATION')\
    .plot(subplots=True, layout=(1, 3), figsize=(16, 4), title='Job creation and destruction rate by YEAR and RURAL_CHNG')
t['JOB_DESTRUCTION_RATE'].add_suffix(' DESTRUCTION').plot(subplots=True, ax=ax, ls='--', grid=True, ylim=(0, 20));
```

+++ {"tags": []}

# CBP by rurality

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

## Rural in CBP vs BDS

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

# Business dynamism county distribution

```{code-cell} ipython3
:tags: []

df = bds.get_df('cty').rename(columns=str.upper)
```

```{code-cell} ipython3
:tags: []

df['ESTABS_CHURN_RATE'] = df['ESTABS_ENTRY_RATE'] + df['ESTABS_EXIT_RATE']
```

```{code-cell} ipython3
:tags: []

df.columns
```

```{code-cell} ipython3
:tags: []

df['ESTABS_CHURN_RATE'].describe()
```

```{code-cell} ipython3
:tags: []

d = get_county_rurality().rename(columns={'STATE_CODE': 'ST', 'COUNTY_CODE': 'CTY'})
df = df.merge(d, 'left', ['ST', 'CTY'])
```

```{code-cell} ipython3
:tags: []

d = df.query('YEAR == 2002')
# df.plot.box(
```

```{code-cell} ipython3
:tags: []

d.groupby('RURAL_CHNG')['ESTABS_CHURN_RATE'].describe()
```

```{code-cell} ipython3
:tags: []

d.boxplot('ESTABS_CHURN_RATE', 'RURAL_2000')
```

```{code-cell} ipython3
:tags: []

d.boxplot('ESTABS_CHURN_RATE', 'RURAL_CHNG')
```

```{code-cell} ipython3
:tags: []

import matplotlib.pyplot as plt
```

```{code-cell} ipython3
:tags: []

plt.violinplot([
    d.loc[d['RURAL_2000'] == False, 'ESTABS_CHURN_RATE'].dropna(),
    d.loc[d['RURAL_2000'] == True, 'ESTABS_CHURN_RATE'].dropna(),
], showmedians=True)
```

```{code-cell} ipython3
:tags: []

plt.violinplot([
    d.loc[d['RURAL_2010'] == False, 'ESTABS_CHURN_RATE'].dropna(),
    d.loc[d['RURAL_2010'] == True, 'ESTABS_CHURN_RATE'].dropna(),
])
```

# Wage

## CBP

```{code-cell} ipython3
:tags: []

df = cbp.get_parquet('county', ['fipstate', 'fipscty', 'year', 'emp', 'ap'], [('industry', '==', '-')])
df = df.rename(columns=str.upper).rename(columns={'FIPSTATE': 'ST', 'FIPSCTY': 'CTY'})
d = get_county_rurality().rename(columns={'STATE_CODE': 'ST', 'COUNTY_CODE': 'CTY'})
df = df.merge(d, 'left', ['ST', 'CTY'])

d = get_deflator().rename(columns=str.upper)
df = df.merge(d, 'left', 'YEAR')
df['AP'] /= df['DEFLATOR']
```

```{code-cell} ipython3
:tags: []

tb = {}
for r in ['RURAL_2000', 'RURAL_2010']:
    t = df.groupby(['YEAR', r])[['EMP', 'AP']].sum()
    t = t['AP'] / t['EMP']
    t = t.unstack() * 1000
    tb[r] = (t[True] / t[False])
tb = pd.concat(tb, axis=1, names=['OMB rurality revision'])
```

```{code-cell} ipython3
:tags: []

fig, ax = plt.subplots(1, 2, figsize=(12, 4))
t.rename(columns={True: 'rural', False: 'urban'}).plot(ax=ax[0], title='Mean real wage, $2021\nCBP', grid=True);
tb.plot(ax=ax[1], legend=True, grid=True, title='Rural-to-urban mean wage ratio\nCBP');
```

## QCEW

### construction

[Data files](https://www.bls.gov/cew/downloadable-data-files.htm) |
[CSV layout](https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm)

```{code-cell} ipython3
:tags: []

def get_qcew_src(y):
    url = f'https://data.bls.gov/cew/data/files/{y}/csv/{y}_annual_singlefile.zip'
    src_dir = nbd.root / 'data/source/qcew'
    f = download_file(url, src_dir)
    return f
```

```{code-cell} ipython3
:tags: []

for y in range(1990, 2022):
    print(y, end=' ')
    get_qcew_src(y)
```

```{code-cell} ipython3
:tags: []

def build_qcew(y):
    pq_dir = nbd.root / 'data/qcew.pq'
    path = pq_dir / f'{y}/part.pq'

    src = get_qcew_src(y)

    cols = {
        'area_fips': str, 
        'agglvl_code': str, 
        'annual_avg_estabs': 'int64',
        'annual_avg_emplvl': 'int64', 
        'total_annual_wages': 'int64', 
        'taxable_annual_wages': 'int64',
        'annual_contributions': 'int64', 
        'annual_avg_wkly_wage': 'int64', 
        'avg_annual_pay': 'int64'
    }
    df = pd.read_csv(src, usecols=cols.keys(), dtype=cols)
    df = df.query('agglvl_code == "70"')
    del df['agglvl_code']

    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path, 'pyarrow', index=False)
```

```{code-cell} ipython3
:tags: []

for y in range(1990, 2022):
    print(y, end=' ')
    build_qcew(y)
```

```{code-cell} ipython3
:tags: [nbd-module]

def get_qcew_df(cols=None, filters=None):
    path = nbd.root / 'data/qcew.pq'
    part = pyarrow.dataset.partitioning(field_names=['year'])
    return pd.read_parquet(path, 'pyarrow', columns=cols, filters=filters,
                           partitioning=part)
```

### validation

```{code-cell} ipython3
:tags: []

df = get_qcew_df(['area_fips', 'year', 'annual_avg_emplvl', 'total_annual_wages'],
                 [('year', '==', 2020)])
assert not df['area_fips'].duplicated().any()
```

```{code-cell} ipython3
:tags: []

df = get_qcew_df(filters=[('area_fips', '==', '01001')])
df['st'] = df['area_fips'].str[:2]
df['cty'] = df['area_fips'].str[2:]
df = df.rename(columns={'annual_avg_emplvl': 'emp', 'total_annual_wages': 'ap', 'avg_annual_pay': 'wage_qcew'})
df = df[['st', 'cty', 'year', 'emp', 'ap', 'wage_qcew']]
df = df.rename(columns=str.upper)
```

```{code-cell} ipython3
:tags: []

d = cbp.get_parquet('county', ['fipstate', 'fipscty', 'year', 'emp', 'ap'], [('industry', '==', '-'), ('fipstate', '==', '01'), ('fipscty', '==', '001')])
d = d.rename(columns=str.upper).rename(columns={'FIPSTATE': 'ST', 'FIPSCTY': 'CTY'})
df = df.merge(d, 'inner', ['ST', 'CTY', 'YEAR'], suffixes=('_QCEW', '_CBP'))
```

```{code-cell} ipython3
df['AP_CBP'] *= 1000
```

```{code-cell} ipython3
:tags: []

df.set_index('YEAR')[['EMP_QCEW', 'EMP_CBP']].plot()
```

```{code-cell} ipython3
:tags: []

df.set_index('YEAR')[['AP_QCEW', 'AP_CBP']].plot()
```

### urban-rural wage

```{code-cell} ipython3
:tags: []

df = get_qcew_df(['area_fips', 'year', 'annual_avg_emplvl', 'total_annual_wages']).rename(columns=str.upper)
df['ST'] = df['AREA_FIPS'].str[:2]
df['CTY'] = df['AREA_FIPS'].str[2:]

d = get_county_rurality().rename(columns={'STATE_CODE': 'ST', 'COUNTY_CODE': 'CTY'})
df = df.merge(d, 'left', ['ST', 'CTY'])

d = get_deflator().rename(columns=str.upper)
df = df.merge(d, 'left', 'YEAR')
df['TOTAL_ANNUAL_WAGES'] /= df['DEFLATOR']
```

```{code-cell} ipython3
:tags: []

tb = {}
for r in ['RURAL_2000', 'RURAL_2010']:
    t = df.groupby(['YEAR', r])[['ANNUAL_AVG_EMPLVL', 'TOTAL_ANNUAL_WAGES']].sum()
    t = t['TOTAL_ANNUAL_WAGES'] / t['ANNUAL_AVG_EMPLVL']
    t = t.unstack()
    tb[r] = (t[True] / t[False])
tb = pd.concat(tb, axis=1, names=['OMB rurality revision'])
```

```{code-cell} ipython3
:tags: []

fig, ax = plt.subplots(1, 2, figsize=(12, 4))
t.rename(columns={True: 'rural', False: 'urban'}).plot(ax=ax[0], title='Mean real wage, $2021\nQCEW', grid=True);
tb.plot(ax=ax[1], legend=True, grid=True, title='Rural-to-urban mean wage ratio\nQCEW');
```
