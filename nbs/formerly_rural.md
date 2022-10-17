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

# Formerly rural

Classification of areas into three groups: rural (R), formerly rural (FR) and urban (U).

```{code-cell} ipython3
:tags: []

import numpy as np
import pandas as pd

from rurec.pubdata import geography, population
from rurec.pubdata import ers_rurality
from rurec.pubdata import naics
from rurec.pubdata import bds
from rurec import rurality
```

+++ {"tags": []}

# OMB metro

```{code-cell} ipython3
:tags: []

def rural_cbsa():
    df = geography.get_county_df(2010, False).rename(columns={'CODE': 'STCTY'})[['STCTY']]

    d = rurality.get_cbsa_delin_df(2003).query('METRO_MICRO == "metro"')
    d['STCTY'] = d['STATE_CODE'] + d['COUNTY_CODE']
    df['RURAL_2000'] = ~df['STCTY'].isin(d['STCTY'])

    d = rurality.get_cbsa_delin_df(2013).query('METRO_MICRO == "metro"')
    d['STCTY'] = d['STATE_CODE'] + d['COUNTY_CODE']
    df['RURAL_2010'] = ~df['STCTY'].isin(d['STCTY'])

    df.loc[df['RURAL_2000'] & df['RURAL_2010'], 'RURAL_CHNG'] = 'R'
    df.loc[df['RURAL_2000'] & ~df['RURAL_2010'], 'RURAL_CHNG'] = 'FR'
    df.loc[~df['RURAL_2000'] & df['RURAL_2010'], 'RURAL_CHNG'] = 'FU'
    df.loc[~df['RURAL_2000'] & ~df['RURAL_2010'], 'RURAL_CHNG'] = 'U'
    return df
```

```{code-cell} ipython3
:tags: []

# counties
df = rural_cbsa()
d = df.groupby(['RURAL_2000', 'RURAL_2010']).size()
d.unstack()
```

```{code-cell} ipython3
:tags: []

# population shares
df = rural_cbsa()
d = population.get_df().rename(columns=str.upper).query('YEAR == 2005')
d['STCTY'] = d['ST'] + d['CTY']
df = df.merge(d[['STCTY', 'POP']])
d = df.groupby(['RURAL_2000', 'RURAL_2010'])['POP'].sum()
d /= d.sum() / 100
d.unstack().round(1)
```

## Map of R/FR/FU/U.

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
df = geography.get_county_df(2010).rename(columns={'CODE': 'STCTY'})
df = df.merge(rural_cbsa(), 'left', 'STCTY')
df = df.merge(geography.get_state_df(False).rename(columns={'CODE': 'STATE_CODE'}), 'left', 'STATE_CODE')
df['RURAL_CHNG'] = df['RURAL_CHNG'].map({
    'R': 'Rural', 'FR': 'Formerly rural', 'FU': 'Formerly urban', 'U': 'Urban'
})
df.query('CONTIGUOUS').plot(column='RURAL_CHNG', legend=True, figsize=(24, 10));
```

```{code-cell} ipython3
---
jupyter:
  outputs_hidden: true
tags: []
---
df.query('ABBR == "WI"').explore(tiles=None, column='RURAL_CHNG', legend=True)
```

## Dynamics of est and emp

```{code-cell} ipython3
:tags: []

df = bds.get_df('cty')\
    .rename(columns=str.upper)\
    .query('YEAR.between(2000, 2019)')
df['STCTY'] = df['ST'] + df['CTY']
df = df.merge(rural_cbsa(), 'left', 'STCTY')

t = df.query('RURAL_CHNG != "FU"').groupby(['YEAR', 'RURAL_CHNG'])[['ESTABS', 'EMP']].sum()
t = t.stack().unstack([2, 1])
t = t.apply(lambda r: r / t.loc[2000, :], 1)
t
```

```{code-cell} ipython3
:tags: []

t1 = t['ESTABS']
t1.index = t1.index.astype(str)
t1.columns = ['Formerly Rural', 'Rural', 'Urban']
t1.plot(figsize=(12, 8), grid=True, title='Growth in the number of establishments (2000 = 1)');
```

```{code-cell} ipython3
:tags: []

t1 = t['EMP']
t1.index = t1.index.astype(str)
t1.columns = ['Formerly Rural', 'Rural', 'Urban']
t1.plot(figsize=(12, 8), grid=True, title='Growth in total employment (2000 = 1)');
```

## BDS in 2002 vs 2012

```{code-cell} ipython3
:tags: []

df = bds.get_df('cty')\
    .rename(columns=str.upper)\
    .query('YEAR.isin([2002, 2012])')

# county codes are constant between 2002 and 2012
df['STCTY'] = df['ST'] + df['CTY']
s0 = df.query('YEAR == 2002')['STCTY'].sort_values().to_list()
s1 = df.query('YEAR == 2012')['STCTY'].sort_values().to_list()
assert s0 == s1

d = rurality.get_cbsa_delin_df(2003).query('METRO_MICRO == "metro"')
d['STCTY'] = d['STATE_CODE'] + d['COUNTY_CODE']
df.loc[df['YEAR'] == 2002, 'RURAL'] = ~df['STCTY'].isin(d['STCTY'])
d = rurality.get_cbsa_delin_df(2013).query('METRO_MICRO == "metro"')
d['STCTY'] = d['STATE_CODE'] + d['COUNTY_CODE']
df.loc[df['YEAR'] == 2012, 'RURAL'] = ~df['STCTY'].isin(d['STCTY'])
assert df['RURAL'].notna().all()
df['RURAL'] = df['RURAL'].astype(bool)

d = df.pivot('STCTY', 'YEAR', 'RURAL')
d.loc[d[2002] & d[2012], 'RURAL_CHNG'] = 'R'
d.loc[d[2002] & ~d[2012], 'RURAL_CHNG'] = 'FR'
d.loc[~d[2002] & d[2012], 'RURAL_CHNG'] = 'FU'
d.loc[~d[2002] & ~d[2012], 'RURAL_CHNG'] = 'U'

df = df.merge(d.reset_index()[['STCTY', 'RURAL_CHNG']])
df.groupby('YEAR')['RURAL_CHNG'].value_counts().unstack()
```

```{code-cell} ipython3
df['RURAL_CHNG'] = df['RURAL_CHNG'].replace('FU', 'R')
```

```{code-cell} ipython3
:tags: []

t0 = df.groupby(['YEAR', 'RURAL_CHNG'])[['FIRMS', 'ESTABS', 'EMP']].sum().unstack('YEAR').astype(int)
t0
```

```{code-cell} ipython3
:tags: []

t0.apply(lambda r: r / t0.sum(), 1)
```

```{code-cell} ipython3
:tags: []

t = t0.stack(0)
t = t[2012] / t[2002]
t.unstack()
```

# RUCA tract crosswalk

Tracts that are missing from the 2000 RUCA table all have zero population.

```{code-cell} ipython3
RURAL_POP_PCT_THRESHOLD = 50
df = geography.get_tract_xwalk_time_df(2010)

# add RUCA rurality on both sides of the xwalk
d = ers_rurality.get_ruca_df().rename(columns={'FIPS': 'TRACT'})
d['RUCA_CODE'] = d['RUCA_CODE'].astype(float).astype(int).astype(str)
d['RURAL'] = d['RUCA_CODE'].isin(['1', '2', '3']).map({True: 'u', False: 'r'})
d = d[['YEAR', 'TRACT', 'RUCA_CODE', 'RURAL']].copy()
df = df.merge(d.query('YEAR == 2000').drop(columns='YEAR').add_suffix('_2000'), 'left', 'TRACT_2000')
df = df.merge(d.query('YEAR == 2010').drop(columns='YEAR').add_suffix('_2010'), 'left', 'TRACT_2010')

# assign dynamic rurality of 2000 tracts
d = df.groupby(['TRACT_2000', 'RURAL_2000', 'RURAL_2010'])['POP_PCT_2000'].sum()
d = d.unstack('RURAL_2010').fillna(0).reset_index()
assert not d['TRACT_2000'].duplicated().any()
d['RURAL_CHNG_2000'] = ''
d.loc[(d['RURAL_2000'] == 'r') & (d['r'] >= RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'R'
d.loc[(d['RURAL_2000'] == 'u') & (d['r'] >= RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'FU'
d.loc[(d['RURAL_2000'] == 'r') & (d['r'] <  RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'FR'
d.loc[(d['RURAL_2000'] == 'u') & (d['r'] <  RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'U'
df = df.merge(d[['TRACT_2000', 'RURAL_CHNG_2000']], 'left', 'TRACT_2000')

# assign dynamic rurality of 2010 tracts
d = df.groupby(['TRACT_2010', 'RURAL_2010', 'RURAL_2000'])['POP_PCT_2010'].sum()
d = d.unstack('RURAL_2000').fillna(0).reset_index()
assert not d['TRACT_2010'].duplicated().any()
d['RURAL_CHNG_2010'] = ''
d.loc[(d['r'] >= RURAL_POP_PCT_THRESHOLD) & (d['RURAL_2010'] == 'r'), 'RURAL_CHNG_2010'] = 'R'
d.loc[(d['r'] >= RURAL_POP_PCT_THRESHOLD) & (d['RURAL_2010'] == 'u'), 'RURAL_CHNG_2010'] = 'FR'
d.loc[(d['r'] <  RURAL_POP_PCT_THRESHOLD) & (d['RURAL_2010'] == 'r'), 'RURAL_CHNG_2010'] = 'FU'
d.loc[(d['r'] <  RURAL_POP_PCT_THRESHOLD) & (d['RURAL_2010'] == 'u'), 'RURAL_CHNG_2010'] = 'U'
df = df.merge(d[['TRACT_2010', 'RURAL_CHNG_2010']], 'left', 'TRACT_2010')
```

```{code-cell} ipython3
pd.crosstab(df['RURAL_CHNG_2000'].fillna('_N/A'), df['RURAL_CHNG_2010'].fillna('_N/A'), margins=True)
```

Example: Cass county, MI.

```{code-cell} ipython3
d = naics.find_concordance_group(df, 'TRACT_2000', 'TRACT_2010', ['26027000300'])
d = d[(d['POP_PCT_2000'] + d['POP_PCT_2010']).between(1, 199)]
d_cass = d
d['T0'] = d['RURAL_CHNG_2000'] + '\n' + d['TRACT_2000'] + '\n2000 ' + d['RURAL_2000'] + '[' + d['RUCA_CODE_2000'] + ']'
d['T1'] = d['RURAL_CHNG_2010'] + '\n' + d['TRACT_2010'] + '\n2010 ' + d['RURAL_2010'] + '[' + d['RUCA_CODE_2010'] + ']'
d['LINK'] = d['POP_PCT_2000'].astype(str) + 20*' ' + d['POP_PCT_2010'].astype(str)
display(naics.viz_concordance(d[['T0', 'T1', 'LINK']].values, 2000, 2010))
```

```{code-cell} ipython3
import folium

st = d_cass['TRACT_2000'].values[0][:2]
stcty = d_cass['TRACT_2000'].values[0][:5]
cty_shp = geography.get_county_df(2010).query('CODE == @stcty').geometry.iloc[0]

shp = geography.get_tract_df([2000], [st])[['CODE', 'geometry']].rename(columns={'CODE': 'TRACT_2000'})
d = d_cass[['TRACT_2000', 'RUCA_CODE_2000', 'RURAL_2000', 'RURAL_CHNG_2000']].drop_duplicates()
d = shp.merge(d, 'right')
m = d.explore(name='rural 2000', column='RURAL_2000')

shp = geography.get_tract_df([2010], [st])[['CODE', 'geometry']].rename(columns={'CODE': 'TRACT_2010'})
d = d_cass[['TRACT_2010', 'RUCA_CODE_2010', 'RURAL_2010', 'RURAL_CHNG_2010']].drop_duplicates()
d = shp.merge(d, 'right')
m = d.explore(m=m, name='rural 2010', column='RURAL_2010')

d = rurality.get_ua_df(2000)
d = d[d.intersects(cty_shp)]
m = d.explore(m=m, name='UA 2000', column='UATYP')

d = rurality.get_ua_df(2010)
d = d[d.intersects(cty_shp)]
m = d.explore(m=m, name='UA 2010', column='UATYP')

folium.LayerControl(collapsed=False).add_to(m)
m
```

# OMB vs RUCA

```{code-cell} ipython3
:tags: []

df = ers_rurality.get_ruca_df().rename(columns={'FIPS': 'TRACT'})
df['RUCA_CODE'] = df['RUCA_CODE'].astype(float).astype(int).astype(str)
df['RURAL_RUCA'] = df['RUCA_CODE'].isin(['1', '2', '3']).map({True: 'u', False: 'r'})
df['STCTY'] = df['TRACT'].str[:5]
df = df.merge(rural_cbsa(), 'left', 'STCTY')
```

```{code-cell} ipython3
:tags: []

# number of tracts
d = df.query('YEAR == 2010')
d.groupby(['RURAL_2010', 'RURAL_RUCA']).size().unstack()
```

```{code-cell} ipython3
:tags: []

# number of tracts
d = df.query('YEAR == 2010')
d = d.groupby(['RURAL_2010', 'RURAL_RUCA'])['POPULATION'].sum()
d /= d.sum() / 100
d.unstack().round(1).iloc[:, [1,0]]
```

+++ {"tags": []}

# Randomization of unknown locations

We can randomly assign R-FU-FR-U classes to tracts that undergo reshaping.
Or to firms where tract is unknown, but county is known.

## Tract reshape

```{code-cell} ipython3
:tags: []

df = geography.get_tract_xwalk_time_df(2010)

# add RUCA rurality on both sides of the xwalk
d = ers_rurality.get_ruca_df().rename(columns={'FIPS': 'TRACT'})
d['RUCA_CODE'] = d['RUCA_CODE'].astype(float).astype(int).astype(str)
d['RURAL'] = d['RUCA_CODE'].isin(['1', '2', '3']).map({True: 'u', False: 'r'})
d = d[['YEAR', 'TRACT', 'RUCA_CODE', 'RURAL']].copy()
df = df.merge(d.query('YEAR == 2000').drop(columns='YEAR').add_suffix('_2000'), 'left', 'TRACT_2000')
df = df.merge(d.query('YEAR == 2010').drop(columns='YEAR').add_suffix('_2010'), 'left', 'TRACT_2010')
```

Deterministic assignment of tracts.

```{code-cell} ipython3
:tags: []

RURAL_POP_PCT_THRESHOLD = 50
d = df.groupby(['TRACT_2000', 'RURAL_2000', 'RURAL_2010'])['POP_PCT_2000'].sum()
d = d.unstack('RURAL_2010').fillna(0).reset_index()
assert not d['TRACT_2000'].duplicated().any()
d['RURAL_CHNG_2000'] = ''
d.loc[(d['RURAL_2000'] == 'r') & (d['r'] >= RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'R'
d.loc[(d['RURAL_2000'] == 'u') & (d['r'] >= RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'FU'
d.loc[(d['RURAL_2000'] == 'r') & (d['r'] <  RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'FR'
d.loc[(d['RURAL_2000'] == 'u') & (d['r'] <  RURAL_POP_PCT_THRESHOLD), 'RURAL_CHNG_2000'] = 'U'
d['RURAL_CHNG_2000'].value_counts(True)
```

Random assignment of tracts: probability thresholds.

```{code-cell} ipython3
:tags: []

# assign dynamic rurality of 2000 tracts
d = df.groupby(['TRACT_2000', 'RURAL_2000', 'RURAL_2010'])['POP_PCT_2000'].sum()
d = d.unstack('RURAL_2010').fillna(0).reset_index()
assert not d['TRACT_2000'].duplicated().any()
d['RURAL_CHNG_THRSH_2000_R'] = 0
d.loc[d['RURAL_2000'] == 'r', 'RURAL_CHNG_THRSH_2000_R'] = d['r'] / 100
d['RURAL_CHNG_THRSH_2000_FU'] = d['r'] / 100
d['RURAL_CHNG_THRSH_2000_FR'] = 1
d.loc[d['RURAL_2000'] == 'u', 'RURAL_CHNG_THRSH_2000_FR'] = d['r'] / 100
thrsh_2000 = d
```

Example with randomly generated firm data.
Distribution over CHNG classes is very similar to deterministic assignment.

```{code-cell} ipython3
:tags: []

# some random firm data
n = 10000
d = pd.DataFrame({
    'TRACT_2000': np.random.choice(thrsh_2000['TRACT_2000'], n),
    'RND_DRAW': np.random.rand(n)
})
d = d.merge(thrsh_2000, 'left')
d.loc[d['RND_DRAW'] < d['RURAL_CHNG_THRSH_2000_R'], 'RURAL_CHNG_2000'] = 'R'
d.loc[d['RURAL_CHNG_2000'].isna() & (d['RND_DRAW'] < d['RURAL_CHNG_THRSH_2000_FU']), 'RURAL_CHNG_2000'] = 'FU'
d.loc[d['RURAL_CHNG_2000'].isna() & (d['RND_DRAW'] < d['RURAL_CHNG_THRSH_2000_FR']), 'RURAL_CHNG_2000'] = 'FR'
d.loc[d['RURAL_CHNG_2000'].isna(), 'RURAL_CHNG_2000'] = 'U'
d['RURAL_CHNG_2000'].value_counts(True)
```

## Unknown tract

Assigning 2010 counties using 2010 population as weights.

```{code-cell} ipython3
:tags: []

df = geography.get_tract_xwalk_time_df(2010)

# add RUCA rurality on both sides of the xwalk
d = ers_rurality.get_ruca_df().rename(columns={'FIPS': 'TRACT'})
d['RUCA_CODE'] = d['RUCA_CODE'].astype(float).astype(int).astype(str)
d['RURAL'] = d['RUCA_CODE'].isin(['1', '2', '3']).map({True: 'u', False: 'r'})
d = d[['YEAR', 'TRACT', 'RUCA_CODE', 'RURAL']].copy()
df = df.merge(d.query('YEAR == 2000').drop(columns='YEAR').add_suffix('_2000'), 'left', 'TRACT_2000')
df = df.merge(d.query('YEAR == 2010').drop(columns='YEAR').add_suffix('_2010'), 'left', 'TRACT_2010')
```

```{code-cell} ipython3
:tags: []

df['RURAL_CHNG_2010'] = (df['RURAL_2000'] + df['RURAL_2010']).map({'rr': 'R', 'ru': 'FR', 'ur': 'FU', 'uu': 'U'})
t = df.groupby('RURAL_CHNG_2010')['POP_2010'].sum()
t / t.sum()
```

```{code-cell} ipython3
:tags: []

df['STCTY_2010'] = df['TRACT_2010'].str[:5]
d = df.groupby(['STCTY_2010', 'RURAL_CHNG_2010'])['POP_2010'].sum()
d = d.unstack().fillna(0)
d = d.apply(lambda c: c / d.sum(1))
d['RURAL_CHNG_THRSH_2010_R'] = d['R']
d['RURAL_CHNG_THRSH_2010_FU'] = d['R'] + d['FU']
d['RURAL_CHNG_THRSH_2010_FR'] = d['R'] + d['FU'] + d['FR']
d = d.merge(df.groupby('STCTY_2010')['POP_2010'].sum(), 'left', left_index=True, right_index=True)
d = d.reset_index()
thrsh_2010 = d
```

Example with randomly generated firm data.

```{code-cell} ipython3
:tags: []

# some random firm data, weighted by county population
n = 10000
d = pd.DataFrame({
    'STCTY_2010': np.random.choice(thrsh_2010['STCTY_2010'], n, p=(thrsh_2010['POP_2010'] / thrsh_2010['POP_2010'].sum())),
    'RND_DRAW': np.random.rand(n)
})
d = d.merge(thrsh_2010, 'left')
d.loc[d['RND_DRAW'] < d['RURAL_CHNG_THRSH_2010_R'], 'RURAL_CHNG_2010'] = 'R'
d.loc[d['RURAL_CHNG_2010'].isna() & (d['RND_DRAW'] < d['RURAL_CHNG_THRSH_2010_FU']), 'RURAL_CHNG_2010'] = 'FU'
d.loc[d['RURAL_CHNG_2010'].isna() & (d['RND_DRAW'] < d['RURAL_CHNG_THRSH_2010_FR']), 'RURAL_CHNG_2010'] = 'FR'
d.loc[d['RURAL_CHNG_2010'].isna(), 'RURAL_CHNG_2010'] = 'U'
d['RURAL_CHNG_2010'].value_counts(True)
```
