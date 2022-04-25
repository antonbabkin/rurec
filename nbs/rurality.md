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

```{code-cell} ipython3
#default_exp rurality
```

# Adding rurality measures to InfoGroup

> Merge different definitions of rurality.

+++

# Rurality classifications

1. Non-urban counties (OMB)
2. Inverse of Census urbanity (Census Bureau)
3. Outside urban activity (ERS)
    - Urban Influence Codes
    - Rural-Urban Continuum
    - Rural-Urban Commuting Areas
4. Non-urban census tracts
    - HRSA/FORHP
    - Inverse of spatial overlap with urban areas
5. Zip codes
    - data challenges
    - FAR and Remote

## HRSA/FORHP

HRSA's Federal Office of Rural Health Policy (FORHP) accepts all non-metro counties as rural 
and uses an additional method of determining rural status called the Rural-Urban Commuting 
Area (RUCA) codes. Like the MSAs, these are based on Census data which is used to assign a 
code to each Census Tract. Tracts inside Metropolitan counties with the codes 4-10 are 
considered rural. While use of the RUCA codes has allowed identification of rural census 
tracts in Metropolitan counties, among the more than 60,000 tracts in the U.S. there are 
some that are extremely large and where use of RUCA codes alone fails to account for distance 
to services and sparse population. In response to these concerns, FORHP has designated 
132 large area census tracts with RUCA codes 2 or 3 as rural. These tracts are at least 
400 square miles in area with a population density of no more than 35 people. The FORHP 
definition includes about 1866 of the population and 8566 of the area of the USA. RUCA codes 
represent the current version of the Goldsmith Modification.


There are two major definitions which the Federal government uses to identify the rural 
status of an area: the Census Bureau's 'Urban Area' and the OMB's 'Core-Based Statistical 
Area'.

## Urban Area
The first is from the U.S. Census Bureau which identifies two types of 
urban areas, Urbanized Areas (UAs) of 50,000 or more people and Urban Clusters (UCs) of at 
Ieast2,S00 and less than 50,000 people. Since the U.S. Census Bureau does not explicitly 
classify areas as rural, rural is defined as “encompassing all population, housing, and 
territory not included within an urban area (those areas not identified as UC or UA)". 
In the 2010 Census, 19.366 of the population was rural while over 9596 of the land area 
is still classified as rural. 

## CBSA
The second is from the Office of Management and Budget (OMB) 
which designates counties as Metropolitan, Micropolitan, or Neither. All counties 
that are not part of a Metropolitan Statistical Area (MSA) are considered rural. 

There are measurement challenges with both the U.S. Census Bureau and OMB definitions. 
Some policy experts note that the U.S. Census Bureau definition classifies quite a bit of  
suburban area as rural. The OMB definition includes rural areas in Metropolitan bounties.
Consequently, one could argue that the Census Bureau standard includes an overcount of rural 
population whereas the OMB standard represents an undercount of the rural population.

## Frontier and Remote

```{code-cell} ipython3
#export

import os
import sys
import logging
import time
import shutil
import multiprocessing as mp

import numpy as np
import pandas as pd
import geopandas as gpd
from joblib import Memory
import fastparquet

from rurec import infogroup, ers_codes, resources
from rurec.resources import Resource

memory = Memory(resources.paths.cache)
```

```{code-cell} ipython3
#export
# for batch runs: log to a file
logging.basicConfig(filename=resources.paths.root / 'logs/build_rural.log', 
                    force=True, filemode='w', level=logging.INFO,
                    format='%(asctime)s %(levelname)s:\n%(message)s')
```

```{code-cell} ipython3
# for interactive use: log to stdout
logging.basicConfig(stream=sys.stdout, level=logging.INFO, format='%(asctime)s %(levelname)s:\n%(message)s', force=True)
```

```{code-cell} ipython3
resources.add(Resource('infogroup/rural', '/InfoGroup/data/processed/rural.pq', 'InfoGroup with rural columns', False))
```

## Outside of Urban Area

+++

The Census Bureau's concept of Urban Area includes two urban categories: the more densely
populated Urbanized Area and the Urban Cluster. See the gazetteer for the details of 
definition. Urban Areas are not defined in terms of any other standard spatial unit. The
borders of an urban area are defined by the density of commuting patterns in the orbit of
urban cores of various population size.

InfoGroup does not include the code for the Urbanized Area or Urban Cluster in which an
establishment may be located. The Bureau does distribute a shapefile for Urban Areas. It
would therefore be possible in theory to locate each establishment's locational coordinates
in an Urban Area or to determine that it is not included in any Urban Area. However, this 
would be 1) an incredibly CPU-intensive process; and 2) probably irrelevant since we are
concerned mostly with InfoGroup establishments in rural areas.

However, because we do have an establishment's census tract code on the InfoGroup record, 
we can determine with just barely imperfect accuracy whether an establishment is
located in a census tract that is itself centered in an Urban Area or non-urban territory.

We have created a geo-reference file starting with shapefiles for Urban Areas and census 
tracts. The centroid location of each census tract was computed and from that data point 
and the coordinate dimensions of each urban area, the 'parental' urban area, if any,
of each census tract was determined. This was an extrememly machine-intensive process itself.

### rural_outside_UA, UA Code, UA Type

The 'rural_outside_UA' variable identifies tracts that are not located within a Census Bureau 
Urban Area. More precisely, if the spatial centroid of the InfoGroup establishment's census 
tract is not located within the polygon of coordinates that defines an Urban Area, the 
establishment is considered 'rural' and coded '1'. In the 2017 file, 3,596,102 establishments,
24.5% of the total, were flagged 'rural' by this measure.

For the 'urban' establishments (coded '0' on 'rural_outside_UA') we also take the Census code 
for its 'parental' Urban Area ('UA Code') and the code for the parental urban area's type 
('UA Type'): 'U' = Urbanized Area, 'C' = Urban Cluster.

The accuracy of these three variables is 'just barely imperfect' because a census
tract can overlap multiple urban (or non-urban) areas. It is therefore not necessarily 
true that the urban area pinpointed by the centroid of the census tract is the one in which 
the InfoGroup establishment itself is actually located, although in nearly every case it 
would be.

Our locally processed geo-reference file 
('/InfoGroup/data/rurality/reference/geographical/points-in-polygons/data/all_tracts.csv')
consists of one record per 2010 census tract, with the following variables:
    'STATEFP', 'COUNTYFP', 'TRACTCE', 'GEOID', 'NAME', 'NAMELSAD', 'MTFCC',
    'FUNCSTAT', 'ALAND', 'AWATER', 'INTPTLAT', 'INTPTLON', 'geometry',
    'UA_GEOID10', 'UATYP10', 'rural_tract'
'STATEFP' through 'geometry' are simply taken from the Census Bureau's shapefile. 
'GEOID' is the file's 11-digit census tract identifier. 'UA-GEOID10' and 'UATYP10' are the 
Urban Area identifier and the Urban Area type code taken from the Urban Area shapefile, and 
'rural_tract' is the laboriously computed rurality flag for each census tract renamed to
'rural_outside_UA' in the InfoGroup record to distinguish it from other such indicator
variables to be created in step 3.

Having created this file at an earlier time, adding the last three variables to the InfoGroup
record was simply a matter of a pandas dataframe merge, where 'df' is the InfoGroup dataframe
and 'tract_df' is the dataframe created from 'all_tracts.csv'.

'all_tracts.csv' is a locally processed file starting with shapefiles for Urban Areas 
and census tracts. The centroid location of each census tract was computed and from that 
data point and the coordinate dimensions of each urban area, the 'parental' urban area, if 
any, of each census tract was determined. This was an extrememly machine-intensive process.

Urban Areas and census tracts are defined by entirely different criteria. Even though
census tracts are on average much smaller than urban areas, each can overlap several of the 
other. The purpose here is to identify 'rural' census tracts, defined as those whose centroid point does not fall within any urban area. A census tract has a 'parental' urban
area if its centroid point does fall within an urban area, either an urbanized area or a
smaller urban cluster.

The all_tracts.csv file contains one record per census tract and the identifying information 
for the single 'parental' urban area, if there is one. The records for rural tracts are 
coded '1' in the 'rural_tract' variable, which indicates a missing value for the Urban
Area identifier, 'UA_GEOID10'. 'UATYP10' identifies the type of the parental urban area: 
'U' = urbanized area, 'C'= urban cluster.

This file is the source data for the 'rural_outside_UA' variable added in this step to the
basic InfoGroup extract created in step 1. It is also the source for the 'UA Code' and
'UA Type' variables, understood to apply to the 'parental' urban area. Since a census
tract can overlap multiple urban areas, it is not necessarily true that the urban area
identified by the 'UA Code' and 'UA Type' variables is the one in which the InfoGroup
establishment itself is actually located, though in nearly every case it would be.

It would be possible to locate each InfoGroup record in an urban area by computing whether
the establishment's spatial coordinates lie within the polygon of coordinates specified in
the urban area shapefile. However, our focus is on the rural economy and that computation,
for all establishments over two decades, would consume an extraordinary amount of calendar 
time and computational resources.

```{code-cell} ipython3
#export

@memory.cache
def prepare_outside_ua_df():
    df = pd.read_csv('/InfoGroup/data/rurality/reference/geographical/points-in-polygons/data/all_tracts.csv',
                     usecols=['GEOID', 'UA_GEOID10', 'UATYP10', 'rural_tract'], dtype=object)
    df.rename(columns={'GEOID': 'CENSUS_TRACT_FULL', 'UA_GEOID10': 'UA_CODE', 
                       'UATYP10': 'UA_TYPE', 'rural_tract': 'RURAL_OUTSIDE_UA'}, inplace=True)
    return df
```

## ERS Measures of Urban Spatial Effect

The ERS's three measures of urban influence and spatial effect are the Urban Influence codes,
the Urban-Rural Continuum codes, and the Urban-Rural Commuting Area codes. These measures
are applied to the InfoGroup record by simple pandas merges as described below. The source
files are downloaded as Excel spreadsheets and processed into csv text files and finally
read into pandas dataframes.

The ERS has created files of all three codes for a variety of years. The unit of analysis
for the Urban Influence codes and the Rural-Urban Continuum codes is the county. For the
Rural-Urban Commuting Area codes the unit of analysis is the census tract.

+++

ERS classifications are created for a number of years. We choose the nearest classification
year when merging to a particular year of InfoGroup.

```{code-cell} ipython3
#export

def nearest_gridpoint(x, grid):
    """Return value from `grid` that is nearest to `x`."""
    grid = np.sort(grid)
    ids = np.arange(len(grid))
    i = np.interp(x, grid, ids)
    i = round(i)
    return grid[i]
```

```{code-cell} ipython3
# TEST
assert nearest_gridpoint(1999, [2000, 2003]) == 2000
assert nearest_gridpoint(2000, [2000, 2003]) == 2000
assert nearest_gridpoint(2001, [2000, 2003]) == 2000
assert nearest_gridpoint(2002, [2000, 2003]) == 2003
assert nearest_gridpoint(2010, [2000, 2003]) == 2003
# unsorted grid
assert nearest_gridpoint(2000, [2005, 1995, 2001]) == 2001
```

### UI_CODE

https://www.ers.usda.gov/data-products/:
"The 2013 Urban Influence Codes form a classification scheme that distinguishes metropolitan 
counties by population size of their metro area, and nonmetropolitan counties by size of the 
largest city or town and proximity to metro and micropolitan areas. The standard Office of 
Management and Budget (OMB) metro and nonmetro categories have been subdivided into two 
metro and 10 nonmetro categories, resulting in a 12-part county classification."

See https://www.ers.usda.gov/data-products/urban-influence-codes/.
Urban Influence codes "form a classification scheme that distinguishes metropolitan counties
by population size of their metro area, and nonmetropolitan counties by size of the largest 
city or town and proximity to metro and micropolitan areas. The standard Office of Management 
and Budget (OMB) metro and nonmetro categories have been subdivided into two metro and 10 
nonmetro categories, resulting in a 12-part county classification."

There are separate ERS collections of Urban Influence codes for 1974, 1983, 1993, 2001, 
and 2013. Having chosen a single year of data as appropriate for the particular year or 
years of InfoGroup data, the command to apply UI_CODE to the InfoGroup record, where
'df' is the dataframe of InfoGroup data and 'ui_df' is the dataframe of Urban Influence data,
is:
    merged = df.merge(ui_df,how='inner',left_on='FIPS Code',right_on='FIPS')

```{code-cell} ipython3
#export

@memory.cache
def prepare_ui_df(year):
    df = ers_codes.get_ui_df()[['FIPS', 'UI_YEAR', 'UI_CODE']]
    df.rename(columns={'FIPS': 'FIPS_CODE'}, inplace=True)
    ui_year = nearest_gridpoint(year, df['UI_YEAR'].unique())
    df = df[df['UI_YEAR'] == ui_year]
    return df[['FIPS_CODE', 'UI_CODE']]
```

### RUC_CODE

https://www.ers.usda.gov/data-products/:
"The 2013 Rural-Urban Continuum Codes form a classification scheme that distinguishes 
metropolitan counties by the population size of their metro area, and nonmetropolitan 
counties by degree of urbanization and adjacency to metro areas. The official Office of 
Management and Budget (OMB) metro and nonmetro categories have been subdivided into three
metro and six nonmetro categories. Each county in the U.S. and Puerto Rico is assigned one 
of the 9 codes."

See https://www.ers.usda.gov/data-products/rural-urban-continuum codes/. 
Rural-Urban Continuum codes "form a classification scheme that distinguishes metropolitan 
counties by the population size of their metro area, and nonmetropolitan counties by degree 
of urbanization and adjacency to a metro area. The official Office of Management and Budget 
(OMB) metro and nonmetro categories have been subdivided into three metro and six nonmetro 
categories. Each county in the U.S. is assigned one of the 9 codes."

There are separate ERS collections of Rural-Urban Continuum codes for 1993, 2003, and 2013.
Having first chosen a single year of RUC data, the command to apply RUC_CODE to the InfoGroup 
record, where is:
    merged = df.merge(ruc_df,how='inner',left_on='FIPS Code',right_on='FIPS')

```{code-cell} ipython3
#export

@memory.cache
def prepare_ruc_df(year):
    df = ers_codes.get_ruc_df()[['FIPS', 'RUC_YEAR', 'RUC_CODE']]
    df.rename(columns={'FIPS': 'FIPS_CODE'}, inplace=True)
    ruc_year = nearest_gridpoint(year, df['RUC_YEAR'].unique())
    df = df[df['RUC_YEAR'] == ruc_year]
    return df[['FIPS_CODE', 'RUC_CODE']]
```

### RUCA_CODE

See https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/.
Rural-Urban Commuting Area codes "classify U.S. census tracts using measures of population 
density, urbanization, and daily commuting....The classification contains two levels. Whole 
numbers (1-10) delineate metropolitan, micropolitan, small town, and rural commuting areas 
based on the size and direction of the primary (largest) commuting flows. These 10 codes are 
further subdivided based on secondary commuting flows, providing flexibility in combining 
levels to meet varying definitional needs and preferences."

There are separate ERS collections of Rural-Urban Commuting Area codes for 1990, 2000, 
and 2010. The three years of RUCA codes "are not directly comparable because many census 
tracts are reconfigured during each decade. Also, changes to census methodologies 
significantly affected the RUCA classifications."

```{code-cell} ipython3
#export

@memory.cache
def prepare_ruca_df(year):
    df = ers_codes.get_ruca_df()[['FIPS', 'YEAR', 'RUCA_CODE']]
    df.rename(columns={'FIPS': 'CENSUS_TRACT_FULL'}, inplace=True)
    ruca_year = nearest_gridpoint(year, df['YEAR'].unique())
    df = df[df['YEAR'] == ruca_year]
    return df[['CENSUS_TRACT_FULL', 'RUCA_CODE']]
```

First choose the appropriate year, then match as below:
    
1. Match 'FIPS' in /ers/ui/ui.csv to 'FIPS Code' (county level) in InfoGroup. 
'UI_YEAR' in ui.csv has the values [1974,1983,1993,2001,2013].

2. Match 'FIPS' in /ers/ruc/ruc.csv to 'FIPS Code' in InfoGroup.
'RUC_YEAR' in ruc.csv has the values [1993,2003,2013].

3. Match 'FIPS' in /ers/ruca/ruca.csv to 'Full Census Tract' in InfoGroup.
'YEAR' in ruca.csv has the values [1990,2000,2010].

For example:

+++

## rural_HRSA

Like the 'rural_outside_UA' variable created in step 2, this variable is an 1/0 flag 
indicating rurality at the census tract level.

HRSA refers to the Health Resources and Services Administration. It is particularly its
sub-unit, the Federal Office of Rural Health Policy (FORHP), that is responsible for this
definition of rurality. For its own administrative purposes it considers a census tract to
be rural if it is contained within a county that is not part of a CBSA. To these, they add
2,302 census tracts from CBSA counties that they have specially defined as rural by applying
the RUCA criteria, of which the FORHP was actually a developer in its early phase.

In the 2017 file, 1,277,342 establishments, 8.7% of the total, were flagged 'rural' by this 
measure, about 1/3 the incidence of rurality measured by the 'rural_outside_UA' variable.

```{code-cell} ipython3
#export

def gen_rural_hrsa(df):
    """Return bool column of rurality by HRSA definition for all establishments in `df`."""
    df_hrsa = pd.DataFrame(get_hrsa_rural_in_cbsa(), columns=['CENSUS_TRACT_FULL'])
    df = df[['CENSUS_TRACT_FULL', 'CBSA_LEVEL']].copy()
    df = df.merge(df_hrsa, 'left', 'CENSUS_TRACT_FULL', indicator=True)
    rural_in_cbsa = (df['_merge'] == 'both')
    rural_out_cbsa = df['CBSA_LEVEL'].isna()
    return rural_in_cbsa | rural_out_cbsa

def get_hrsa_rural_in_cbsa():
    """Return list of tracts that are in CBSA, but rural by HRSA definition."""
    tracts = []
    # FORHP list of 2300+ rural census tracts
    # This is a pre-processed text version of a former PDF file.
    with open('/InfoGroup/data/rurality/tract_data.txt', 'r') as fin:
        for line in fin:
            if line[0] != chr(32):
                continue
            else:
                line = line.strip()
                try:
                    if line[0].isnumeric(): 
                        tracts.append(line)
                except IndexError:
                    pass
    return tracts
```

## FAR Level

The USDA writes: “To assist in providing policy-relevant information about conditions in 
sparsely-settled, remote areas of the U.S. to public officials, researchers, and the general 
public, ERS has developed ZIP-code-level frontier and remote area (FAR) codes”.

FAR codes are applied to postal zip codes to identify different degrees and criteria of 
remoteness. It is not a code for any functional concept of rurality, but there is an obvious 
family resemblance between “remote” and “rural” which might find some analytical use.

The ERS created four FAR levels based on proximity (conceived of as travel time) to 
“urban” places of different sizes. Levels 1 through 4 measure increasing remoteness.
The ‘FAR Level’ variable captures the highest numbered positive FAR level for a location.

In 2017, 659,070 InfoGroup establishments, 4.56% of the total, were located in zip codes
designated far or remote.

```{code-cell} ipython3
#export

@memory.cache
def get_far_df():
    """Return dataframe with ERS FAR levels by Zip code."""
    far_file = '/InfoGroup/data/rurality/reference/FARcodesZIPdata2010WithAKandHI.xlsx'
    dtypes = {'ZIP': str, 'far1': int, 'far2': int, 'far3': int, 'far4': int}
    df = pd.read_excel(far_file, 'FAR ZIP Code Data', usecols=dtypes.keys(), dtype=dtypes)
    assert (df['ZIP'].str.len() == 5).all()
    assert df.isna().sum().sum() == 0
    # We can add up binary farX indicators to obtain single FAR level,
    # because classification is nested, e.g. far2 implies far1, and not-far1 implies not-far2.
    x = df['far1'] + df['far2'] + df['far3'] + df['far4']
    df['FAR_LEVEL'] = pd.Categorical(x, [0, 1, 2, 3, 4], True)
    df = df[['ZIP', 'FAR_LEVEL']].drop_duplicates()
    assert not df['ZIP'].duplicated().any()
    return df
```

## Merge and save new dataframe

```{code-cell} ipython3
#export

def add_rural_cols(year):
    """Load InfoGroup data for one `year`, add rurality columns and
    save as parquet partition.
    """
    
    logging.info(f'Start add_rural_cols({year})')

    df_outside_ua = prepare_outside_ua_df()
    df_ui = prepare_ui_df(year)
    df_ruc = prepare_ruc_df(year)
    df_ruca = prepare_ruca_df(year)
    df_far = get_far_df()

    cols = ['COMPANY', 'CITY', 'STATE', 'ZIP', 'COUNTY_CODE', 'SIC', 'NAICS', 
            'YEAR', 'EMPLOYEES', 'SALES', 'ABI', 'PARENT_NUMBER',
            'CENSUS_TRACT', 'LATITUDE', 'LONGITUDE', 'CBSA_CODE', 'CBSA_LEVEL', 'CSA_CODE', 'FIPS_CODE']

    df = infogroup.get_df([year], cols)
    df.drop(columns=['YEAR'], inplace=True)
    df['CENSUS_TRACT_FULL'] = df['FIPS_CODE'] + df['CENSUS_TRACT']

    df = df.merge(df_outside_ua, 'left', 'CENSUS_TRACT_FULL', indicator=True)
    counts = df['_merge'].value_counts()
    logging.debug(f'Merge result of "RURAL_OUTSIDE_UA" in {year}\n{counts}\n')
    df.drop(columns=['_merge'], inplace=True)

    df = df.merge(df_ui, 'left', 'FIPS_CODE', indicator=True)
    counts = df['_merge'].value_counts()
    logging.debug(f'Merge result of "UI_CODE" in {year}\n{counts}\n')
    df.drop(columns=['_merge'], inplace=True)

    df = df.merge(df_ruc, 'left', 'FIPS_CODE', indicator=True)
    counts = df['_merge'].value_counts()
    logging.debug(f'Merge result of "RUC_CODE" in {year}\n{counts}\n')
    df.drop(columns=['_merge'], inplace=True)

    df = df.merge(df_ruca, 'left', 'CENSUS_TRACT_FULL', indicator=True)
    counts = df['_merge'].value_counts()
    logging.debug(f'Merge result of "RUCA_CODE" in {year}\n{counts}\n')
    df.drop(columns=['_merge'], inplace=True)

    df['RURAL_HRSA'] = gen_rural_hrsa(df)
    counts = df['RURAL_HRSA'].value_counts()
    logging.debug(f'Rural by HRSA in {year}\n{counts}\n')

    df = df.merge(df_far, 'left', 'ZIP', indicator=True)
    counts = df['_merge'].value_counts()
    logging.debug(f'Merge result of "FAR_LEVEL" in {year}\n{counts}\n')
    df.drop(columns=['_merge'], inplace=True)
    
    partition_path = str(resources.get('infogroup/rural').path / f'YEAR={year}')
    fastparquet.write(partition_path, df, file_scheme='hive', write_index=False, partition_on=['STATE'])
    
    logging.info(f'Finish add_rural_cols({year})')
    return partition_path


def build_parquet_dataset(n_cpus=1):
    """Merge rural columns to all years, save and merge parquet partitions."""
    
    logging.info(f'Start build_parquet_dataset({n_cpus})')
    
    p = resources.get('infogroup/rural').path
    # Remove dataset files if they exist from before
    if p.exists():
        shutil.rmtree(p)
    p.mkdir()

    data_years = range(1997, 2018)
    with mp.Pool(n_cpus) as pool:
        partition_paths = pool.map(add_rural_cols, data_years)
    _ = fastparquet.writer.merge(partition_paths)
    
    logging.info(f'Finish build_parquet_dataset({n_cpus})')
```

## Interface for opening dataframe

```{code-cell} ipython3
#export

def get_df(years=None, cols=None, states=None, onlymeta=False):
    """Return InfoGroup with rurality columns as dataframe.
    If `onlymeta` is True, return ParquetFile instead, which can be used 
    to quickly inspect dataset schema without loading it ("dtypes" attribute).
    """
    path = resources.get('infogroup/rural').path
    if onlymeta:
        return fastparquet.ParquetFile(str(path))
    
    filters = []
    if years is not None:
        filters.append(('YEAR', 'in', years))
    if states is not None:
        filters.append(('STATE', 'in', states))

    df = pd.read_parquet(path, 'fastparquet', columns=cols, filters=filters)

    if cols is None or 'YEAR' in cols:
        df['YEAR'] = df['YEAR'].astype(int)
    else:
        df.drop(columns=['YEAR'], inplace=True)

    if not (cols is None or 'STATE' in cols):
        df.drop(columns=['STATE'], inplace=True)
        
    if cols is None or 'FAR_LEVEL' in cols:
        df['FAR_LEVEL'].cat.as_ordered(inplace=True)

    return df
```
