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

# 2022 RDC conference

## Proposal

**Title: Business dynamics in rural, nonrural and formerly rural America**  
Authors: Anton Babkin, Richard A. Dunn, Brent Hueth

There has been a well-documented decline in economic dynamism in the United States over the past three decades.
This phenomenon is posited as one of the leading causes of the concomitant decline in productivity growth rates as the Schumpeterian forces of creative destruction that drive allocative efficiency gains weaken.
Declining economic dynamism is not uniformly distributed across the country, however.
Rural areas have experienced larger declines in economic dynamism than urban areas, and thus not surprisingly, have experienced lower rates of economic growth.
Yet, the existing literature on the geographic dispersion of economic growth largely ignores that this result is at least partly mechanical: areas that grow slower can not reach the threshold of demographic or economic indicators to be classified as nonrural.
At the same time, a significant proportion of rural areas gets reclassified into nonrural with every revision of the classification scheme.
In this light it is informative to identify transitioning areas that were rural at the beginning of the study period, but later became reclassified as nonrural.
These “formerly rural” areas not only grow faster than persistently rural areas (which is, again, almost by definition), but also outpace nonrural regions of the country.
We argue that the study of formerly rural regions is important in order to understand drivers of economic growth and dynamism in rural America.

We use the establishment microdata from the Census Bureau Longitudinal Business Database with location information from the Business Register, to compare rural, nonrural and formerly rural regions across an array of business dynamics measures, including:

- total number of firms and establishments;
- aggregate, average and median employment and payroll;
- age, size and wage dynamics and distribution;
- growth rates of employment, payroll and wages;
- establishment entry and exit rates;
- job and payroll creation and destruction rates;
- relationship between exit and growth rates on one side and age, size and industry on the other side.

This analysis allows us to identify not only the differences between business dynamics patterns in the three kinds of regions, but also the features that predict regions to transition from rural to nonrural status.

An important feature of our analysis, which is not possible with public data, is an investigation of the robustness of results to the definition of rural area.
The OMB definition of urban-rural typology is county-based, but researchers have long recognized that counties are often too large a geographic unit to accurately categorize places as urban or rural–many counties that are classified as metropolitan contain large areas of low population density.
As a form of measurement error, misclassifying rural areas can meaningfully affect interpretation of stylized facts.
Therefore, with the location information from the Business Register, we can apply the Rural-Urban Commuting Area codes developed by the USDA Economic Research Service that define rural areas at the census tract level, to produce the same set of statistics and compare them to county-based definitions.

+++ {"tags": []}

# Results

## Presentation outline

- multitude of definitions
  - importance: shared language, measurement error, policy
  - geographic unit
    - smaller geography more important in bigger county regions
  - measures: population, commute, others
  - non-binary: discreete or continuous scale of "rurality"
  - revisions over time
  - we show how definitions matter: geography and revisions
  
- definitions used here
  - OMB
  - RUCA
  
- sample
  - BDS
  - historical location
  - 2002 and 2012 (location data quality, tract revisions)

- time trends in public data
  - advantage of county based definitions
  - BDS figures with OMB def
    - growth
    - dynamism
    - summary of stylized facts
  - rural is lagging, but it depends on revision
    - it is lagging almost by definition, almost mechanical result
  - "formerly rural" of particular interest
    - it is not so big over 10 year revision, but increases with time span
    - grows faster than rural and (in some measures) faster than urban
  
- sub-county definition
  - how stylized facts change between OMB and RUCA
  
- LBD exclusive stylized facts
  - payroll creation and destruction
  - wage dynamics
  
- R vs FR in 2002

```{code-cell} ipython3
:tags: []

import pandas as pd
import numpy as np

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
from rurec.pubdata import geography
from rurec.pubdata import ers_rurality
from rurec.pubdata import naics
from rurec.pubdata import bds
from rurec import rurality
```

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

disc_file_path = nbd.root / 'data/rdc/20220816.xlsx'
df = pd.read_excel(disc_file_path, 'size', index_col=[0, 1, 2, 3, 4, 5])
df = df.reset_index('rural')
df['rural'] = df['rural'].astype(pd.CategoricalDtype(['R', 'FR', 'U'], ordered=True))
df = df.set_index('rural', append=True)

df = df.rename(columns={'nunique lbdfid': 'firms', 'size lbdnum': 'estabs', 'sum emp': 'emp', 'sum emp_d': 'emp_d', 'sum pay': 'pay', 'sum pay_d': 'pay_d'})
df0 = df
df.head()
```

## BDS

+++

### OMB revisions

```{code-cell} ipython3
:tags: []

df = bds.get_df('cty')\
    .rename(columns=str.upper)\
    .query('YEAR.between(2000, 2019)')
df['STCTY'] = df['ST'] + df['CTY']
df = df.merge(rural_cbsa(), 'left', 'STCTY')
```

```{code-cell} ipython3
:tags: []

t = df.groupby(['YEAR', 'RURAL_2000'])[['ESTABS', 'EMP']].sum()
t = t.stack().unstack([2, 1])
t = t.apply(lambda r: r / t.loc[2000, :], 1)

# t1 = t['ESTABS']
t1 = t['EMP']
t1.index = t1.index.astype(str)
t1.columns = ['Urban 2002', 'Rural 2002']
ax = t1.plot();

t = df.groupby(['YEAR', 'RURAL_2010'])[['ESTABS', 'EMP']].sum()
t = t.stack().unstack([2, 1])
t = t.apply(lambda r: r / t.loc[2000, :], 1)

# t1 = t['ESTABS']
t1 = t['EMP']
t1.index = t1.index.astype(str)
t1.columns = ['Urban 2012', 'Rural 2012']
t1.plot(ax=ax, ls=':', grid=True, title='Employment dynamics under different CBSA revisions');
```

## outcomes

```{code-cell} ipython3
:tags: []

df = []

# firms and estabs
d = df0[['firms', 'estabs']].copy()
d = d.groupby(['rurality', 'year', 'entry', 'exit', 'rural']).sum()
d = d.query('entry + exit < 2')
d.columns.name = 'measure'
d = d.stack().unstack(['entry', 'exit'])
# d.columns = d.columns.to_flat_index()
d.columns = d.columns.map({(1, 0): 'bir', (0, 0): 'cont', (0, 1): 'dea'})
# active_t: continuers_t + births_t
d['t'] = d['cont'] + d['bir']
# active_(t-1): continuers_t + deaths_t
d['l'] = d['cont'] + d['dea']
d = d[['t', 'l']]
d['d'] = d['t'] - d['l']
d.columns.name = 'suffix'
df.append(d)

# emp and pay
d = df0[['emp', 'emp_d', 'pay', 'pay_d']].copy()
d = d.rename(columns={'emp': 'emp_t', 'pay': 'pay_t'})
d = d.groupby(['rurality', 'year', 'rural']).sum()
d.columns = pd.MultiIndex.from_tuples([c.split('_') for c in d.columns])
d.columns.names = ['measure', 'suffix']
d = d.stack('measure')
d['l'] = d['t'] - d['d']
df.append(d)

df = pd.concat(df)

# wage
df = df.stack().unstack('measure')
df['wage'] = df['pay'] / df['emp']
df = df.stack().unstack('suffix')


df['a'] = (df['t'] + df['l']) / 2
df['g'] = df['d'] / df['a']
df = df.stack().unstack([3, 4])

df[('wage', 'd')] = df[('wage', 't')] - df[('wage', 'l')]
df[('wage', 'g')] = df[('wage', 'd')] / df[('wage', 'a')]

df_size = df
```

### relative size

- R and FR are bigger by RUCA than by OMB
- by OMB, 10% of economic activity in 2002 R becomes U in 2012
- by RUCA - 15%

```{code-cell} ipython3
:tags: []

d = df_size.copy()
d = d.loc[:, [('estabs', 'a'), ('emp', 'a'), ('pay', 'a')]].droplevel('suffix', 1)
d = d.stack().unstack('rural')
d = d.apply(lambda r: r / d.sum(1)) * 100
d['FR/(R+FR)'] = d.eval('FR / (R+FR) * 100')
d = d.round(1).stack().unstack(['rurality', 'rural'])
d.index.names = [None, None]
d.iloc[:, [0,1,3,2, 4,5,7,6]]
```

### 2002-2012 growth

- FR grows faster than R and even U under OMB
- but only much less so under RUCA
- fastest wage growth in R

```{code-cell} ipython3
:tags: []

d = df_size.copy()
d = d.loc[:, (slice(None), 't')].droplevel('suffix', 1)
# d = d.loc[:, (slice(None), 'a')].droplevel('suffix', 1)
d = d.stack().unstack('year')
d = d[2012] / d[2002] * 100 - 100
d = d.unstack(['rurality', 'rural']).round(1)
d.loc[['firms', 'estabs', 'emp', 'pay', 'wage'], :]
```

### 1-yr growth rates

- FR grows faster (falls slower) than others in 2002, but grows slower than others in 2012

```{code-cell} ipython3
:tags: []

d = df_size.copy()
d = d.loc[:, (slice(None), 'g')].droplevel('suffix', 1) * 100
d.round(1).stack().unstack(['rurality', 'rural'])
```

```{code-cell} ipython3
:tags: []

d = df_size.query('rurality == "ruca"').copy().droplevel('rurality')
d = d.loc[:, (slice(None), 'g')].droplevel('suffix', 1) * 100
d = d.round(1).stack().unstack(['rural', 'year']).sort_index(axis=1)
d.loc[['firms', 'estabs', 'emp', 'pay', 'wage'], :]
```

## activity and dynamism

+++

### estab size

- estabs per firm: increase in concentration everywhere from 2002 to 2012
    - FR always less estabs per firm
- emp, pay and wage: R < FR < U

```{code-cell} ipython3
:tags: []

d = df_size.copy()
d = d.loc[:, (slice(None), 'a')].droplevel('suffix', 1)
d['estab per firm'] = d['estabs'] / d['firms']
d['mean emp'] = d['emp'] / d['estabs']
d['mean pay'] = d['pay'] / d['estabs']
d = d[['estab per firm', 'mean emp', 'mean pay', 'wage']]
d.round(2).stack().unstack(['rurality', 'measure'])
```

### entry, exit and churn

- churn R < FR < U
- churn falls over time

```{code-cell} ipython3
:tags: []

d = df0[['estabs']].copy()
d = d.groupby(['rurality', 'year', 'entry', 'exit', 'rural']).sum()
d = d.query('entry + exit == 1')
d.columns.name = 'measure'
d = d.stack().unstack(['entry', 'exit'])
d.columns = d.columns.map({(1, 0): 'estabs_entry', (0, 1): 'estabs_exit'})
d = d.droplevel('measure')
d['estabs_a'] = df_size[('estabs', 'a')]
d['entry_rate'] = d['estabs_entry'] / d['estabs_a'] * 100
d['exit_rate'] = d['estabs_exit'] / d['estabs_a'] * 100
d['churn_rate'] = d['entry_rate'] + d['exit_rate']
d.iloc[:, -3:].stack().unstack(['rurality', 3]).round(1)
```

```{code-cell} ipython3
:tags: []

# smaller table for slides
d = df0[['estabs']].query('rurality == "ruca"').copy()
d = d.groupby(['rurality', 'year', 'entry', 'exit', 'rural']).sum().droplevel('rurality')
d = d.query('entry + exit == 1')
d.columns.name = 'measure'
d = d.stack().unstack(['entry', 'exit'])
d.columns = d.columns.map({(1, 0): 'estabs_entry', (0, 1): 'estabs_exit'})
d = d.droplevel('measure')
d['estabs_a'] = df_size.query('rurality == "ruca"')[('estabs', 'a')].droplevel('rurality')
d['entry_rate'] = d['estabs_entry'] / d['estabs_a'] * 100
d['exit_rate'] = d['estabs_exit'] / d['estabs_a'] * 100
d['churn_rate'] = d['entry_rate'] + d['exit_rate']
d.iloc[:, -3:].round(1).stack().unstack(['rural', 'year']).sort_index(axis=1)
```

### emp dynamism

- same pattern as with estab

```{code-cell} ipython3
:tags: []

d = df0[['emp_d']].query('emp_d_sign != "zero"').copy()
d = d.groupby(['rurality', 'year', 'rural', 'emp_d_sign']).sum()
d = d.unstack().droplevel(0, 1)
d.columns = ['emp_destr', 'emp_creat']
d['emp_destr'] *= -1
d['emp_a'] = df_size[('emp', 'a')]
d['emp_creat_rate'] = d['emp_creat'] / d['emp_a'] * 100
d['emp_destr_rate'] = d['emp_destr'] / d['emp_a'] * 100
d['emp_churn_rate'] = d['emp_creat_rate'] + d['emp_destr_rate']
d
```

### pay dynamism

- same pattern as with estab

```{code-cell} ipython3
:tags: []

d = df0[['pay_d']].query('emp_d_sign != "zero"').copy()
d = d.groupby(['rurality', 'year', 'rural', 'emp_d_sign']).sum()
d = d.unstack().droplevel(0, 1)
d.columns = ['pay_destr', 'pay_creat']
d['pay_destr'] *= -1
d['pay_a'] = df_size[('pay', 'a')]
d['pay_creat_rate'] = d['pay_creat'] / d['pay_a'] * 100
d['pay_destr_rate'] = d['pay_destr'] / d['pay_a'] * 100
d['pay_churn_rate'] = d['pay_creat_rate'] + d['pay_destr_rate']
d
```
