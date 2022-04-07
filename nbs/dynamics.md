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

from rurec import ers_codes, cbp, pop
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

df = ers_codes.get_ui_df().rename(columns=str.lower)
df['rural'] = ~df['ui_code'].isin(['1', '2'])
df = df[['fips', 'ui_year', 'rural']].rename(columns={'fips': 'stcty'})
rural = df
```

CODO: FIPS county code 999 probably means state-wide, and can not be classified as rural or non-rural.

```{code-cell} ipython3
:tags: []

@functools.cache
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
d = pop.pop()
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
agg = pd.concat(agg, axis=1, names=['rural_defn', 'rural'])
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

## Infogroup

```{code-cell} ipython3
#hide
df = rurality.get_df(cols=['YEAR', 'STATE', 'EMPLOYEES', 'RURAL_OUTSIDE_UA', 'UI_CODE', 'RUC_CODE', 'RUCA_CODE', 'RURAL_HRSA', 'FAR_LEVEL'], states=['WI', 'CT'])
df['STATE'].cat.remove_unused_categories(inplace=True)

df['RURAL_OUTSIDE_UA'].replace({'0': False, '1': True}, inplace=True)
df['RURAL_UI'] = df['UI_CODE'].isin(['6', '7', '8', '9', '10', '11', '12'])
df['RURAL_RUC'] = df['RUC_CODE'].isin(['5', '7', '9', '10'])
df['RURAL_RUCA'] = df['RUCA_CODE'].str[:1].isin(['7', '8', '9']) | (df['RUCA_CODE'].str[:2] == '10')
df['RURAL_FAR'] = (df['FAR_LEVEL'] > 0)
```

```{code-cell} ipython3
#hide
tab = {}
for rural_col in ['RURAL_OUTSIDE_UA', 'RURAL_HRSA', 'RURAL_UI', 'RURAL_RUC', 'RURAL_RUCA', 'RURAL_FAR']:
    x = df.groupby(['YEAR', 'STATE', rural_col])['EMPLOYEES'].agg(['size', 'sum']).stack().unstack(rural_col)
    x = x[True] / x.sum(1)
    x = x.unstack()
    x.rename(columns={'size': 'estab', 'sum': 'emp'}, inplace=True)
    tab[rural_col] = x
tab = pd.concat(tab, 1).fillna(0)
```

### Share of rural establishments

```{code-cell} ipython3
#collapse_output
idx = pd.IndexSlice
t = tab.loc[:, idx[:, 'estab']]
t.columns = t.columns.droplevel(1)
t = t.unstack()
t.style.format('{:.1%}')
```

```{code-cell} ipython3
#hide_input
fig, ax = plt.subplots(figsize=(12, 8))
for i, rur in enumerate(t.columns.levels[0]):
    for st in ['WI', 'CT']:
        l = 'solid' if st == 'WI' else 'dashed'
        y = t[(rur, st)]
        ax.plot(y, color=f'C{i}', linestyle=l, label=f'{rur}, {st}')
        
ax.set_xticks(t.index)
fig.legend(loc='lower center', ncol=len(t.columns.levels[0]));
```

### Share of rural employment

```{code-cell} ipython3
#collapse_output
idx = pd.IndexSlice
t = tab.loc[:, idx[:, 'emp']]
t.columns = t.columns.droplevel(1)
t = t.unstack()
t.style.format('{:.1%}')
```

```{code-cell} ipython3
#hide_input
fig, ax = plt.subplots(figsize=(12, 8))
for i, rur in enumerate(t.columns.levels[0]):
    for st in ['WI', 'CT']:
        l = 'solid' if st == 'WI' else 'dashed'
        y = t[(rur, st)]
        ax.plot(y, color=f'C{i}', linestyle=l, label=f'{rur}, {st}')
        
ax.set_xticks(t.index)
fig.legend(loc='lower center', ncol=len(t.columns.levels[0]));
```

> Warning: Below starts the old code that does not use nbdev and draws data from BigQuery. It has not been tested for a long time.

+++

# FAI

+++

### NAICS codes of FAI industries

```{code-cell} ipython3
%cd ..
```

```{code-cell} ipython3
with open('data/fai.json') as f:
    fai_naics = json.load(f)
with open('data/fai_subsectors.json') as f:
    fai_subsectors = json.load(f)

fai_subs_codes = []
for x in fai_subsectors.values():
    fai_subs_codes += x.keys()
    
# "fai.json" has 168 codes and includes farming and 12 other industries, "fai_subsectors.json" has 100 codes
fai_naics_narrow = fai_subs_codes

# Tom's 4-digit codes
fai_naics_broad = [str(x) for x in [2362,2382,3111,3112,3114,3115,3116,3117,3119,3222,3251,3253,3261,3272,3332,3333,
          3339,3352,3369,3371,4234,4238,4241,4244,4246,4249,4451,4452,4461,4471,4529,4543,
          5413,5417,6242,7223,7225,8113,9231,9261,1151,1152,2371,2379,3219,3254,3322,3323,
          3324,3326,3331,3399,4245,4442,4842,4931,5222,5324,5419,7121,8129,8134,8139]]
```

```{code-cell} ipython3
query = '''
SELECT DISTINCT
  substr(naics, 1, 6) as naics, 
  naics_desc
FROM
  `original.data`
WHERE
  year > 2002
ORDER BY
  naics
'''

naics_desc = pd.read_gbq(query, dialect='standard', project_id='info-group-162919').dropna()
```

```{code-cell} ipython3
df = naics_desc
df['naics4'] = df['naics'].str[:4]
df['fai_narrow'] = df['naics'].isin(fai_naics_narrow)
df['fai_broad'] = df['naics4'].isin(fai_naics_broad)
df = df[df['fai_narrow'] | df['fai_broad']]
df = df[['naics', 'naics4', 'naics_desc', 'fai_narrow', 'fai_broad']]
df.shape
```

```{code-cell} ipython3
# 6-digit codes
df1 = df.groupby(['fai_broad', 'fai_narrow']).size().unstack()
df1, df1.sum(), df1.sum(1)
```

### Load InfoGroup employment data

```{code-cell} ipython3
query = '''
SELECT
  year,
  naics,
  cbsa_level,
  count(*) as est,
  sum(employees) as emp
FROM
  (select employees, year, cbsa_level, substr(naics, 1, 6) as naics from `original.data`)
WHERE
  year > 2002
GROUP BY
  year,
  naics,
  cbsa_level
ORDER BY
  year,
  naics
'''

df = pd.read_gbq(query, dialect='standard', project_id='info-group-162919')
# df = df[df.naics.notnull()]
# df = df[df.naics.str.startswith('1151') | ~df.naics.str.startswith('11')]
df_by_year_naics = df
```

```{code-cell} ipython3
# missing NAICS share is negligible
df['naics_na'] = (df['naics'] == '')
df1 = df.groupby('naics_na')['emp'].sum()
df1 / df1.sum()
```

```{code-cell} ipython3
# farm employment share is negligible
df['naics'] = df['naics'].fillna('')
df['farm'] = df.naics.str.startswith('11') & ~df.naics.str.startswith('1151')
df1 = df.groupby('farm')['emp'].sum()
df1 / df1.sum()
```

### Industry classification: FAI and subsectors

```{code-cell} ipython3
df = df_by_year_naics
df['cbsa'] = df['cbsa_level'].replace([None, '1', '2'], ['rural', 'micro', 'metro'])
df['fai_narrow'] = df.naics.isin(fai_naics_narrow)
df['fai_broad'] = df.naics.str[:4].isin(fai_naics_broad)
df['subsector'] = None
for subsector, codes in fai_subsectors.items():
    df.loc[df.naics.isin(codes), 'subsector'] = subsector
df.subsector = df.subsector.astype('category')
```

```{code-cell} ipython3
df1 = {}
df1['total'] = df.groupby(['year', 'cbsa'])['est'].sum().unstack()
df1['fai_narrow'] = df[df['fai_narrow']].groupby(['year', 'cbsa'])['est'].sum().unstack() / df1['total']
df1['fai_broad'] = df[df['fai_broad']].groupby(['year', 'cbsa'])['est'].sum().unstack() / df1['total']
pd.options.display.precision = 3
df1 = pd.concat(df1, 1)
print(df1.to_string())
```

```{code-cell} ipython3
df1 = {}
df1['total'] = df.groupby(['year', 'cbsa'])['emp'].sum().unstack()
df1['fai_narrow'] = df[df['fai_narrow']].groupby(['year', 'cbsa'])['emp'].sum().unstack() / df1['total']
df1['fai_broad'] = df[df['fai_broad']].groupby(['year', 'cbsa'])['emp'].sum().unstack() / df1['total']
df1['total'] = (df1['total'] / 1000).astype('int')
pd.options.display.precision = 3
df1 = pd.concat(df1, 1)
print(df1.to_string())
```

### Space classification: rural, micropolitan, metropolitan

```{code-cell} ipython3
area_df = {
    'Rural': df[df.cbsa_level.isnull()],
    'Micropolitan': df[df.cbsa_level == '1'],
    'Metropolitan': df[df.cbsa_level == '2'] 
}
```

### Plot employment share in FAI

```{code-cell} ipython3
fig, ax = plt.subplots()
for area, adf in area_df.items():
    share = adf[adf['fai']].groupby('year')['emp'].sum() / adf.groupby('year')['emp'].sum()
    years = share.index
    ax.plot(years, share, label=area, alpha=0.7)
    ax.set_ylabel('Employment share')
    ax.set_xlim(years.min(), years.max())
lgd = ax.legend(loc='upper right');
#fig.savefig('fig/fai_dynamics.png', bbox_extra_artists=(lgd,), bbox_inches='tight')
```

```{code-cell} ipython3
fig, ax = plt.subplots()
for area, adf in area_df.items():
    share = adf[adf['fai_tom']].groupby('year')['emp'].sum() / adf.groupby('year')['emp'].sum()
    years = share.index
    ax.plot(years, share, label=area, alpha=0.7)
    ax.set_ylabel('Employment share')
    ax.set_xlim(years.min(), years.max())
lgd = ax.legend(loc='upper right');
#fig.savefig('fig/fai_dynamics.png', bbox_extra_artists=(lgd,), bbox_inches='tight')
```

### Plot breakdown of FAI employment

```{code-cell} ipython3
def fai_emp_breakdown(df):
    _df = df.groupby(['year', 'subsector'])['emp'].sum().unstack()
    _sum = _df.sum(axis=1)
    return _df.apply(lambda col: col / _sum)
```

```{code-cell} ipython3
colors = ['b','g','r','c','m','y','k']
fig, axes = plt.subplots(ncols=3, figsize=(17, 5))
axes[0].set_ylabel('Share of FAI')
lines = []
for (area, adf), ax in zip(area_df.items(), axes):
    shares = fai_emp_breakdown(adf)
    years = shares.index
    cum_share1 = np.zeros(len(shares))
    for subsector, color in zip(fai_subsectors, colors):
        cum_share0 = cum_share1.copy()
        share = shares[subsector]
        cum_share1 += share
        line = ax.plot(years, cum_share1, color, alpha=0.5, label=subsector)
        lines.append(line[0])
        ax.fill_between(years, cum_share0, cum_share1, facecolor=color, alpha=0.3)
    ax.set_title(area)
    ax.set_ylim(-0.0001,1)
    ax.set_xlim(years.min(), years.max())
lines = lines[:len(fai_subsectors)]
lgd = fig.legend(handles=lines, ncol=len(lines), loc='upper center', bbox_to_anchor=(0.44, 0.08))
fig.savefig('fig/fai_breakdown.png', bbox_extra_artists=(lgd,), bbox_inches='tight')
```

+++ {"tags": []}

# 1151 dynamics

Quick and dirty computation of simple stats.

```{code-cell} ipython3
from rurec import infogroup
```

```{code-cell} ipython3
df.columns
```

```{code-cell} ipython3
df0 = infogroup.get_df(cols=['ABI', 'YEAR', 'NAICS', 'EMPLOYEES', 'YEAR_EST'])#, states=['WI'])
```

```{code-cell} ipython3
df0.shape
```

```{code-cell} ipython3
(df0.EMPLOYEES==0).value_counts()
```

```{code-cell} ipython3
df = df0.query('NAICS.str.slice(0,4) == "1151"').copy()
```

```{code-cell} ipython3
df_1151 = df.copy()
```

```{code-cell} ipython3
df = df_1151.copy()
```

```{code-cell} ipython3
df['EMPLOYEES'] = df['EMPLOYEES'].fillna(0)
```

```{code-cell} ipython3
df['EMPLOYEES'] = (df['EMPLOYEES'].fillna(0) - 1).replace(-1, 0)
```

```{code-cell} ipython3
df.shape
```

```{code-cell} ipython3
df['FIRST_YEAR'] = df.groupby('ABI')['YEAR'].transform(np.min)
# df['YEAR_EST'] = df.groupby('ABI')['YEAR_EST'].transform(np.min)
# df.loc[df['YEAR_EST'] < df['FIRST_YEAR'], 'FIRST_YEAR'] = df['YEAR_EST']
df['LAST_YEAR'] = df.groupby('ABI')['YEAR'].transform(np.max)
df.loc[df['YEAR'] == 2017, 'LAST_YEAR'] = np.nan
df['AGE'] = df['YEAR'] - df['FIRST_YEAR']
```

```{code-cell} ipython3
df['ACTIVE_YEARS'] = df.groupby('ABI')['YEAR'].transform(lambda x: len(x))
```

```{code-cell} ipython3
df['YEAR_PREV'] = df['YEAR'] - 1
df = df.merge(df[['ABI', 'YEAR', 'EMPLOYEES']], 'left', left_on=['ABI', 'YEAR_PREV'], right_on=['ABI', 'YEAR'], suffixes=('', '_PREV'))
df.loc[(df['YEAR'] > df['FIRST_YEAR']), 'CONT_EMP_NC'] = df['EMPLOYEES'] - df['EMPLOYEES_PREV']
```

```{code-cell} ipython3
x = df.query('ACTIVE_YEARS > 5').sample()['ABI'].values[0]
df.query('ABI == @x').sort_values('YEAR')
```

```{code-cell} ipython3
dfa = df.query('EMPLOYEES > 0')

t = pd.DataFrame()
t['active estab'] = dfa.groupby('YEAR').size()
t['birth estab'] = dfa.query('YEAR == FIRST_YEAR').groupby('YEAR').size()
t['predeath estab'] = dfa.query('YEAR == LAST_YEAR').groupby('YEAR').size()
t['age mean'] = dfa.groupby('YEAR')['AGE'].mean()
t['age median'] = dfa.groupby('YEAR')['AGE'].median()
t['age std'] = dfa.groupby('YEAR')['AGE'].std()
t[['emp mean', 'emp std']] = dfa.groupby('YEAR')['EMPLOYEES'].agg([np.mean, np.std])
t['active emp'] = dfa.groupby('YEAR')['EMPLOYEES'].sum()
t['birth emp'] = dfa.query('YEAR == FIRST_YEAR').groupby('YEAR')['EMPLOYEES'].sum()
t['predeath emp'] = dfa.query('YEAR == LAST_YEAR').groupby('YEAR')['EMPLOYEES'].sum()
t['cont emp nc'] = df.groupby('YEAR')['CONT_EMP_NC'].sum()
t['cont emp gc'] = df.query('CONT_EMP_NC > 0').groupby('YEAR')['CONT_EMP_NC'].sum()
t['cont emp gd'] = -df.query('CONT_EMP_NC < 0').groupby('YEAR')['CONT_EMP_NC'].sum()
t['cont no emp change'] = df.query('CONT_EMP_NC == 0').groupby('YEAR').size()
```

```{code-cell} ipython3
t['emp prev'] = t['active emp'].shift()
t['death emp'] = t['predeath emp'].shift()
```

```{code-cell} ipython3
t
```

```{code-cell} ipython3
t0 = t
```

```{code-cell} ipython3
t1 = t
```

```{code-cell} ipython3
with pd.ExcelWriter('~/infogroup_1151.xlsx') as writer:
    t0.to_excel(writer, 'emp_as_is')
    t1.to_excel(writer, 'emp_minus_1')
```
