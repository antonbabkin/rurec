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

# Farm sales by BEA industry detail

For the purpose of constructing measures of input-output connectedness, we need measures of county level output in the farming sector by IO "Detail" industry.

```{code-cell} ipython3
:tags: [nbd-module]

import shutil
from typing import Literal

import numpy as np
import pandas as pd

from rurec.pubdata import agcensus
from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
```

```{code-cell} ipython3
from rurec.pubdata import bea_io, naics
```

Relevant section of the BEA-NAICS crosswalk.
Sales by NAICS are not available in AgCensus by county level.
We use sales by commodity instead, manually mapping from AgCensus commodity classification to BEA industry.

```{code-cell} ipython3
:tags: []

d = bea_io.get_naics_df()
d = d.query('SUMMARY == "111CA"').rename(columns={'NAICS': 'NAICS_CODE'})
d = d.merge(naics.get_df(2017, 'code').add_prefix('NAICS_'), 'left', 'NAICS_CODE')
d[['SECTOR', 'SUMMARY', 'U_SUMMARY', 'DETAIL', 'DESCRIPTION', 'NAICS_CODE', 'NAICS_TITLE']].fillna('')
```

Farm sales in Ag Census are reported in the following classes.
The easiest way to see this is by looking at "Table 2: Market Value of Agricultural Products Sold" at the natinal level.
Exact field description can be than found in the QuickStats tables.

COMMODITY TOTALS = CROP TOTALS + ANIMAL TOTALS

CROP TOTALS = GRAIN + TOBACCO + COTTON + VEGETABLE + FRUIT & NUT + HORTICULTURE + XMAS TREES + OTHER

ANIMAL TOTALS = POULTRY + CATTLE + MILK + HOGS + SHEEP & GOAT + EQUINE + ACQUACULTURE + OTHER

GRAIN group is further broken down into corn, wheat, soybeans and others starting from 2007.
We need this separation, because BEA has "1111A0: Oilseed farming", which mainly consists of soybeans.

```{code-cell} ipython3
:tags: []

# for reference: relevant values of "SHORT_DESC" corresponding to rows in table 2: sales in dollars (2017)
['COMMODITY TOTALS - SALES, MEASURED IN $',
    'CROP TOTALS - SALES, MEASURED IN $',
        'GRAIN - SALES, MEASURED IN $', 
            'CORN - SALES, MEASURED IN $',
            'WHEAT - SALES, MEASURED IN $',
            'SOYBEANS - SALES, MEASURED IN $',
            'SORGHUM - SALES, MEASURED IN $',
            'BARLEY - SALES, MEASURED IN $',
            'RICE - SALES, MEASURED IN $',
            'GRAIN, OTHER - SALES, MEASURED IN $',
        'TOBACCO - SALES, MEASURED IN $',
        'COTTON, LINT & SEED - SALES, MEASURED IN $',
        'VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN - SALES, MEASURED IN $',
        'FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $',
            'FRUIT & TREE NUT TOTALS, (EXCL BERRIES) - SALES, MEASURED IN $',
            'BERRY TOTALS - SALES, MEASURED IN $',
        'HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS) - SALES, MEASURED IN $',
        'CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS - SALES, MEASURED IN $',
            'CUT CHRISTMAS TREES - SALES, MEASURED IN $',
            'SHORT TERM WOODY CROPS - SALES, MEASURED IN $',
        'FIELD CROPS, OTHER, INCL HAY - SALES, MEASURED IN $',
            'MAPLE SYRUP - SALES, MEASURED IN $',
    'ANIMAL TOTALS, INCL PRODUCTS - SALES, MEASURED IN $',
        'POULTRY TOTALS, INCL EGGS - SALES, MEASURED IN $',
        'CATTLE, INCL CALVES - SALES, MEASURED IN $',
        'MILK - SALES, MEASURED IN $', 
        'HOGS - SALES, MEASURED IN $',
        'SHEEP & GOATS TOTALS, INCL WOOL & MOHAIR & MILK - SALES, MEASURED IN $',
        'EQUINE, (HORSES & PONIES) & (MULES & BURROS & DONKEYS) - SALES, MEASURED IN $',
        'AQUACULTURE TOTALS - SALES & DISTRIBUTION, MEASURED IN $'
        'SPECIALTY ANIMAL TOTALS, (EXCL EQUINE) - SALES, MEASURED IN $',
]
```

Take QuickStats table with "STCTY", "SHORT_DESC" and "VALUE" columns, select needed commodity sales rows and compute BEA-detail industry aggregates, and return dataframe with STCTY as index and industry sales in columns.

Soybeans (and other grains) were not reported separately in 2002.
We take the simplest approach to impute it now by applying soy-to-grain share taken from 2007 sales.
This approach is very crude, as it ignores significant variation of soy production across counties.
A more nuanced imputation in the future can use county-level production data: `('SHORT_DESC', '==', 'SOYBEANS - PRODUCTION, MEASURED IN BU')`.

```{code-cell} ipython3
:tags: [nbd-module]

def get_farm_sales_by_bea_detail(year, geo_level):
    """Return dataframe of farm sales grouped by BEA detail industries.
    geo_level: "national", "state" or "county".
    year: 2002, 2007, 2012, 2017.
    """

    # load quickstats table
    df = agcensus.get_df([year], ['STATE_FIPS_CODE', 'COUNTY_CODE', 'SHORT_DESC', 'VALUE'],
                         [('DOMAIN_DESC', '==', 'TOTAL'),
                          ('AGG_LEVEL_DESC', '==', geo_level.upper()),
                          ('STATISTICCAT_DESC', '==', 'SALES'),
                          ('UNIT_DESC', '==', '$')])
    df['STCTY'] = df['STATE_FIPS_CODE'] + df['COUNTY_CODE'].fillna('000')

    # dictionary to rename and select needed rows
    select_sales_items = {
        'CROP TOTALS': 'CROP',
        'GRAIN': 'GRAIN',
        'SOYBEANS': 'SOYBEAN',
        'VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN': 'VEGETABLE',
        'FRUIT & TREE NUT TOTALS': 'FRUIT & NUT',
        'HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS)': 'HORTICULTURE',
        'CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS': 'TREE',
        'ANIMAL TOTALS, INCL PRODUCTS': 'ANIMAL',
        'POULTRY TOTALS, INCL EGGS': 'POULTRY',
        'CATTLE, INCL CALVES': 'CATTLE',
        # MILK: 2012, 2017
        'MILK': 'MILK',
        # MILK: 2002, 2007
        'MILK, INCL OTHER DAIRY PRODUCTS': 'MILK'
    }
    select_sales_items = {k + ' - SALES, MEASURED IN $': v for k, v in select_sales_items.items()}
    
    # irrelevant rows will be NA and drop
    df['SELECT_SALES'] = df['SHORT_DESC'].map(select_sales_items)
    df = df.dropna(subset=['VALUE', 'SELECT_SALES'])

    # keep only sales values and pivot to have STCTY in rows and commodities in columns
    df = df.set_index(['STCTY', 'SELECT_SALES'])['VALUE'].unstack('SELECT_SALES')

    # applies to 2002
    if 'SOYBEAN' not in df:
        soy_2007 = 20_283_986
        grain_2007 = 77_215_262
        soy_share_of_grain = soy_2007 / grain_2007
        df['SOYBEAN'] = df['GRAIN'] * soy_share_of_grain

    # BEA detail industries
    df['1111A0'] = df['SOYBEAN']
    df['1111B0'] = df['GRAIN'] - df['SOYBEAN'].fillna(0)
    df['111200'] = df['VEGETABLE']
    df['111300'] = df['FRUIT & NUT']
    df['111400'] = df['HORTICULTURE'].fillna(0) + df['TREE'].fillna(0)
    df['111900'] = df['CROP'] - df[['GRAIN', 'VEGETABLE', 'FRUIT & NUT', 'HORTICULTURE', 'TREE']].fillna(0).sum(1)
    df['1121A0'] = df['CATTLE']
    df['112120'] = df['MILK']
    df['112300'] = df['POULTRY']
    df['112A00'] = df['ANIMAL'] - df[['CATTLE', 'MILK', 'POULTRY']].fillna(0).sum(1)

    df = df[['1111A0', '1111B0', '111200', '111300', '111400', '111900', '1121A0', '112120', '112300', '112A00']]
    df.columns.name = 'BEA_INDUSTRY_DETAIL'
    df = df.fillna(0)
    
    # some cells can come negative due to subtraction of rounded values
    df[(df >= -1000) & (df <= 0)] = 0
    assert (df >= 0).all().all()
    
    return df
```

The resulting table looks like this.

```{code-cell} ipython3
:tags: []

get_farm_sales_by_bea_detail(2017, 'county').astype(int).head()
```

## Compare AgCensus and BEA values

To validate sales numbers computed from the NASS ag census, we compare national totals with what is reported by BEA Supply table.
The match is good enough.

These are some known disparities:

- County totals do not add up to natioal totals because of confidentiality suppression of small industry-county cells.
County aggregates are below nationals except for leftover categories "111900" and "112A00".
- Ag Census numbers are sales, and BEA is production. Change in inventories is one source of difference.
- Oilseed farming from NASS is only soybeans, while BEA "1111A0" further includes NAICS "11112: Oilseed (except Soybean) Farming".

```{code-cell} ipython3
:tags: []

df = bea_io.get_sup(2012, 'det').loc[['1111A0', '1111B0', '111200', '111300', '111400', '111900', '112120', '1121A0', '112300', '112A00'], ['T007']]
df.columns = ['BEA Suppply table, Total Commodity Output']
df['Ag Census, national'] = get_farm_sales_by_bea_detail(2012, 'national').T / 1000_000
df['Ag Census, county aggregate'] = get_farm_sales_by_bea_detail(2012, 'county').sum().T / 1000_000

df.plot(kind='bar', figsize=(12, 4), rot=0,
          title='Output by BEA detail industry within "111CA: Farms"');
```

# Build this module

```{code-cell} ipython3
:tags: []

nbd.nb2mod('ag_output.ipynb')
```
