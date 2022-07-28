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

from rurec.pubdata import ers_rurality
```

```{code-cell} ipython3
:tags: []

def pop_shares(y0, y1):
    # y0, y1 = 2000, 2010

    df = ers_rurality.get_ruca_df()\
        .query(f'YEAR.isin([{y0}, {y1}])')\
        [['FIPS', 'YEAR', 'RUCA_CODE', 'POPULATION']]
    df['RUCA_CODE'] = df['RUCA_CODE'].astype(float).astype(int)
    df['RURAL'] = (df['RUCA_CODE'] > 3)

    d0 = df.query(f'YEAR == {y0}')
    d1 = df.query(f'YEAR == {y1}')
    d = d0.merge(d1, 'outer', 'FIPS', suffixes=('_0', '_1'))

    d['POPULATION_AVG'] = (d['POPULATION_0'] + d['POPULATION_1']) / 2
    d['POPULATION_DIF'] = d['POPULATION_1'] - d['POPULATION_0']
    t = d.groupby(['RURAL_0', 'RURAL_1'])[['POPULATION_0', 'POPULATION_1', 'POPULATION_AVG', 'POPULATION_DIF']].sum()
    t['POP_GR'] = t['POPULATION_DIF'] / t['POPULATION_AVG']
    display(t)
    t = t.apply(lambda row: row / t.sum(), 1)
    display(t)
```

FR is relatively small in population share, but has fastest growth and has disproportionaly larger share of pop creation.

```{code-cell} ipython3
:tags: []

pop_shares(2000, 2010)
```

Increasing time horizon from 10 to 20 years naturally increases the share of FR, and the contrast now is even more stark.

```{code-cell} ipython3
:tags: []

pop_shares(1990, 2010)
```
