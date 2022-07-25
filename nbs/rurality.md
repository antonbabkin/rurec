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

```{raw-cell}

---
title: "Defining rurality"
bibliography: ../references.bib
format:
  html:
    code-fold: true
execute:
  echo: false
jupyter: python3
---
```

There are many different defitions of rurality, both within research community and in public policy. A recent systematic literature review [@nelson_definitions_2021] identified 65 research articles with different rurality definitions. A selection of definitions is presented here. In many cases, non-rural is defined first, and everything outside of it is considered rural. @fig-compare-defns-wisc visually compares what apprears to be rural in Wisconsin under different definitions.

Two major definitions which the Federal government uses to identify the rural status of an area are the Census Bureau's "Urban Area" and the OMB's "Core-Based Statistical Area".

```{code-cell} ipython3
:tags: [nbd-module]

import functools
import typing
import warnings

import pandas as pd
import geopandas

from rurec.pubdata import geography, ers_rurality
from rurec.reseng import util
from rurec.reseng.nbd import Nbd

nbd = Nbd('rurec')
PATH = {
    'root': nbd.root,
    'data': nbd.root / 'data/',
    'source': nbd.root / 'data/source/',
    'ers_far': nbd.root / 'data/ers_far/',
}
```

```{code-cell} ipython3
:tags: []

# notebook-only imports
import shapely
import folium
import folium.plugins
import matplotlib as mpl
import matplotlib.pyplot as plt
import ipywidgets
```

```{code-cell} ipython3
:tags: []

class RuralityMap:
    def __init__(self, categories, colors, tiles='CartoDB positron'):
        self.categories = categories
        self.colors = colors
        self.tiles = tiles
        self.map = None
    
    def add_layer(self, gdf, name, column, **kw):
        if self.map is None:
            self.map = gdf.explore(name=name, column=column, 
                                   categories=self.categories,
                                   cmap=self.colors,
                                   tiles=self.tiles,
                                   **kw)
        else:
            gdf.explore(m=self.map, name=name, column=column, 
                        categories=self.categories,
                        cmap=self.colors,
                        legend=False,
                        **kw)
            
    def show(self):
        tile_layer = [x for x in self.map._children.values() if isinstance(x, folium.raster_layers.TileLayer)][0]
        tile_layer.control = False
        folium.LayerControl(collapsed=False).add_to(self.map)
        return self.map
```

# Urban area

[Census Bureau](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html)

> The Census Bureau’s urban-rural classification is a delineation of geographic areas, identifying both individual urban areas and the rural areas of the nation. The Census Bureau’s urban areas represent densely developed territory, and encompass residential, commercial, and other non-residential urban land uses. The Census Bureau delineates urban areas after each decennial census by applying specified criteria to decennial census and other data. “Rural” encompasses all population, housing, and territory not included within an urban area.

The U.S. Census Bureau identifies two types of urban areas: *Urbanized Areas* (UAs) of 50,000 or more people and *Urban Clusters* (UCs) of at least 2,S00 and less than 50,000 people.
Urban Areas are not defined in terms of any other standard spatial unit.
The borders of an urban area are defined by the density of commuting patterns in the orbit of urban cores of various population size.

Data source page: [TIGER/Line® Shapefiles](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). Boundaries are defined after decennial census. Using latest national data files for every revision: 2009 file for 2000 boundaries, 2021 file for 2010 boundaries. *This may be worth examining more closely. OMB metro delineations are updated after decennials AND annually from ACS. Maybe urban area shapes change between decennials too.*

Processed geodataframe columns:

- `UACE`: Urban area code.
- `NAME`: Urban area name.
- `UATYP`: Urban area type. `"U"` - Urbanized Area, `"C"` - Urban Cluster.
- `ALAND`, `AWATER`: land and water area (square meters).
- `INTPTLAT`, `INTPTLON`: Latitude and longitude of the internal point.
- `geometry`: Geopandas (multi)polygons.

Urban area names are typically `"city_name, state_postal_abbreviation"` (`"Madison, WI"`, `"Hartford, CT"`). But bigger aglomeration names might include multiple cities (`"Los Angeles--Long Beach--Anaheim, CA"`) and lie in multiple states (`"Kansas City, MO--KS"`, `"Minneapolis--St. Paul, MN--WI"`, `"New York--Newark, NY--NJ--CT"`).

```{code-cell} ipython3
:tags: [nbd-module]

def get_source_ua(year: typing.Literal[2000, 2010] = 2010):
    """Download and return path to urban area boundary shapefile."""

    base = 'https://www2.census.gov/geo/tiger/'
    urls = {
        2000: f'{base}TIGER2009/tl_2009_us_uac.zip',
        2010: f'{base}TIGER2021/UAC/tl_2021_us_uac10.zip'
    }
    
    url = urls[year]
    local = PATH['source'] / 'urban_area' / url.split('/')[-1]
        
    if not local.exists():
        print(f'File "{local}" not found, attempting download.')
        util.download_file(url, local.parent, local.name)
    return local


def get_ua_df(year: typing.Literal[2000, 2010] = 2010,
              geometry: bool = True):
    """Load geodataframe with urban areas from `year` census.
    Download and process dataset if necessary, and cache as parquet for faster access.
    Pass `geometry=False` to load DataFrame without geometry column instead of GeoDataFrame.
    """
    columns = ['UACE', 'NAME', 'UATYP', 'ALAND', 'AWATER', 'INTPTLAT', 'INTPTLON']
    
    path = PATH['data']/f'urban_area/{year}.pq'
    if path.exists():
        print('Loading from parquet file.')
        return geopandas.read_parquet(path) if geometry else pd.read_parquet(path, 'pyarrow', columns)

    print('Parquet file not found, creating dataframe from source...')
    df = geopandas.read_file(get_source_ua(year))
    
    if year == 2010:
        df = df.rename(columns={f'{c}10': c for c in columns})
        
    df = df[columns + ['geometry']]
    df[['INTPTLAT','INTPTLON']] = df[['INTPTLAT','INTPTLON']].astype('float64')
    assert not df['UACE'].duplicated().any(), 'Duplicate UA code(s) found.'
    
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path)
    print('Dataframe saved to parquet.')
    
    if not geometry:
        df = pd.DataFrame(df).drop(columns='geometry')
        
    return df
```

```{code-cell} ipython3
:tags: []

#| tbl-cap: Sample from the urban area dataframe (no geometry).
get_ua_df(year=2010, geometry=False).sample(3)
```

@fig-urban-areas-dane-cty shows how urban areas expand in Dane county, Wisconsin between 2000 and 2010 censuses. Small 2000 urban clusters of Cross Plains and DeForest by 2010 merged with bigger Madion urbanized area.

```{code-cell} ipython3
:tags: []

#| label: fig-urban-areas-dane-cty
#| fig-cap: "Urban areas in Dane county, WI from 2000 and 2010 censuses."
cty_shp = geography.get_county_df(2010).query('CODE == "55025"').geometry.iloc[0]
m = RuralityMap(['2000 Uranized Area', '2000 Urban Cluster', '2010 Uranized Area', '2010 Urban Cluster'],
                [mpl.colors.to_hex(c) for c in plt.cm.tab20.colors[0:4]])

for year in [2000, 2010]:
    df = get_ua_df(year)
    df = df[df.intersects(cty_shp)]
    df['UATYP'] = df['UATYP'].map({'U': f'{year} Uranized Area', 'C': f'{year} Urban Cluster'})
    m.add_layer(df, f'Census {year}', 'UATYP')

m.show()
```

+++ {"tags": []}

# Metropolitan and micropolitan statistical areas

One of the most widely used definitions of rural is everything outside of metropolitan areas. Counties outside of metro/micro areas are sometimes called "noncore".

Metropolitan definition takes counties as base unit and classifies them using data on urban area residents and commuting flows.

[Census page](https://www.census.gov/programs-surveys/metro-micro.html)

> The 2010 standards provide that each CBSA must contain at least one urban area of 10,000 or more population. Each metropolitan statistical area must have at least one urbanized area of 50,000 or more inhabitants. Each micropolitan statistical area must have at least one urban cluster of at least 10,000 but less than 50,000 population.
>
> Under the standards, the county (or counties) in which at least 50 percent of the population resides within urban areas of 10,000 or more population, or that contain at least 5,000 people residing within a single urban area of 10,000 or more population, is identified as a "central county" (counties). Additional "outlying counties" are included in the CBSA if they meet specified requirements of commuting to or from the central counties.

[Delineation files](https://www.census.gov/programs-surveys/metro-micro/about/delineation-files.html)

> A metropolitan or micropolitan statistical area's geographic composition, or list of geographic components at a particular point in time, is referred to as its "delineation."

Multiple sets of delineation files exist:

- metropolitan or micropolitan statistical areas;
- New England city and town areas (NECTAs), which are conceptually similar to metropolitan and micropolitan statistical areas, but are delineated using cities and towns instead of counties;
- combined statistical areas, which are aggregates of adjacent metropolitan or micropolitan statistical areas that are linked by commuting ties;
- combined NECTAs;
- metropolitan divisions, which are a county or group of counties (or equivalent entities) delineated within a larger metropolitan statistical area, provided that the larger metropolitan statistical area contains a single core with a population of at least 2.5 million and other criteria are met;
- NECTA divisions.

## Revisions

Changes chiefly consist of inclusion of new counties as they reach required urban area population or commute flows to neighboring metro areas.
The first delineation was created from 1950 decennial census.
The standards for delineating the areas are reviewed and revised once every ten years, prior to each decennial census.
Commute flow updates happen every ten years.
Population driven updates happen more frequently, based on data from American Community Survey and Population Estimates Program.

2000 revision (first delineation published in 2003) introduced micropolitan areas.


| Revision | Census |
|----------|--------|
| Mar 2020 | 2010   |
| Sep 2018 | 2010   |
| Apr 2018 | 2010   |
| Aug 2017 | 2010   |
| Jul 2015 | 2010   |
| Feb 2013 | 2010   |
| Dec 2009 | 2000   |
| Nov 2008 | 2000   |
| Nov 2007 | 2000   |
| Dec 2006 | 2000   |
| Dec 2005 | 2000   |
| Nov 2004 | 2000   |
| Dec 2003 | 2000   |
| Jun 2003 | 2000   |
| Jun 1999 | 2000*  |
| Jun 1993 | 1990** |
| Jun 1990 | 1990*  |
| Jun 1983 | 1980** |
| Jun 1981 | 1980*  |
| Apr 1973 | 1980** |
| Feb 1971 | 1970*  |
| Oct 1963 | 1960** |
| Nov 1960 | 1960*  |
| Oct 1950 | 1950*  |

\* Delineations used for presenting metropolitan area statistics in upcoming Census publications.  
\** Delineations based on application of metropolitan area standards to preceding census data.

```{code-cell} ipython3
:tags: [nbd-module]

def get_cbsa_delin_src(year: int):
    """Download and return path to CBSA delineation file.
    When more than one revision exists in year (as in 2003 or 2018),
    the most recent one in that year is used.
    """

    base = 'https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/'
    urls = {
        2020: f'{base}2020/delineation-files/list1_2020.xls',
        2018: f'{base}2018/delineation-files/list1_Sep_2018.xls',
        # 2018-april: f'{base}2018/delineation-files/list1.xls',
        2017: f'{base}2017/delineation-files/list1.xls',
        2015: f'{base}2015/delineation-files/list1.xls',
        2013: f'{base}2013/delineation-files/list1.xls',
        2009: f'{base}2009/historical-delineation-files/list3.xls',
        2008: f'{base}2008/historical-delineation-files/list3.xls',
        2007: f'{base}2007/historical-delineation-files/list3.xls',
        2006: f'{base}2006/historical-delineation-files/list3.xls',
        2005: f'{base}2005/historical-delineation-files/list3.xls',
        2004: f'{base}2004/historical-delineation-files/list3.xls',
        2003: f'{base}2003/historical-delineation-files/0312cbsas-csas.xls',
        # 2003-june: f'{base}2003/historical-delineation-files/030606omb-cbsa-csa.xls',
        1993: f'{base}1993/historical-delineation-files/93mfips.txt'
    }
    
    assert year in urls, f'CBSA delineation not available for {year}.'
    
    url = urls[year]
    ext = url.split('.')[-1]
    local = PATH['source'] / f'cbsa/delin/{year}.{ext}'
        
    if not local.exists():
        print(f'File "{local}" not found, attempting download.')
        util.download_file(url, local.parent, local.name)
    return local

def get_cbsa_delin_df(year: int):
    f = get_cbsa_delin_src(year)
    
    if year == 1993:
        return _prep_cbsa_delin_df_1993(f)
    
    # number of rows to skip at top and bottom varies by year
    if year in [2003, 2013, 2015, 2017, 2018, 2020]:
        skip_head = 2
    elif year in [2005, 2006, 2007, 2008, 2009]:
        skip_head = 3
    elif year == 2004:
        skip_head = 7

    if year in [2003, 2004]:
        skip_foot = 0
    elif year == 2005:
        skip_foot = 6
    elif year in [2006, 2007, 2008, 2009, 2015, 2017, 2018, 2020]:
        skip_foot = 4
    elif year == 2013:
        skip_foot = 3

    df = pd.read_excel(f, dtype=str, skiprows=skip_head, skipfooter=skip_foot)

    # standardize column names
    if 2003 <= year <= 2009:
        del df['Status, 1=metro 2=micro']
        df['STATE_CODE'] = df['FIPS'].str[:2]
        df['COUNTY_CODE'] = df['FIPS'].str[2:]
        del df['FIPS']
        rename = {
            'CBSA Code': 'CBSA_CODE',
            'Metro Division Code': 'DIVISION_CODE',
            'CSA Code': 'CSA_CODE',
            'CBSA Title': 'CBSA_TITLE',
            'Level of CBSA': 'METRO_MICRO',
            'Metropolitan Division Title': 'DIVISION_TITLE',
            'CSA Title': 'CSA_TITLE',
            'Component Name': 'COUNTY',
            'State': 'STATE'
        }
        if year >= 2007:
            rename.update({'County Status': 'CENTRAL_OUTLYING'})
    elif 2013 <= year <= 2020:
        rename = {
            'CBSA Code': 'CBSA_CODE',
            'CBSA Title': 'CBSA_TITLE',
            'CSA Code': 'CSA_CODE',
            'CSA Title': 'CSA_TITLE',
            'Metropolitan Division Title': 'DIVISION_TITLE',
            'Metropolitan/Micropolitan Statistical Area': 'METRO_MICRO',
            'State Name': 'STATE',
            'County/County Equivalent': 'COUNTY',
            'FIPS State Code': 'STATE_CODE',
            'FIPS County Code': 'COUNTY_CODE',
            'Central/Outlying County': 'CENTRAL_OUTLYING'
        }
        if year == 2013:
            rename.update({'Metro Division Code': 'DIVISION_CODE'})
        else:
            rename.update({'Metropolitan Division Code': 'DIVISION_CODE'})
    
    df = df.rename(columns=rename)
    
    assert df[['STATE_CODE', 'COUNTY_CODE']].notna().all().all()
    assert not df.duplicated(['STATE_CODE', 'COUNTY_CODE']).any()
    assert df['METRO_MICRO'].notna().all()
    
    df['METRO_MICRO'] = df['METRO_MICRO'].map({
        'Metropolitan Statistical Area': 'metro',
        'Micropolitan Statistical Area': 'micro'
    })
    if 'CENTRAL_OUTLYING' in df:
        df['CENTRAL_OUTLYING'] = df['CENTRAL_OUTLYING'].str.lower()
    
    return df
```

## 1993 delineation

1993 delineation is hierarchical "staircase" table, CMSA -> MSA -> county -> city.
CMSA = Consolidated Metropolitan Statistical Area, a collection of MSA's.
First column is both for CMSA and MSA, and when it is CMSA, then MSA components are in the PRIMARY_MSA_CODE.

It seems that sometimes MSA boundary is going though the county, and then county NAME has "(pt.)" in it, and is followed by county towns that belong to the MSA. In that case, only towns - and not the county itself - are classified as central or outlying.

```{code-cell} ipython3
:tags: [nbd-module]

def _prep_cbsa_delin_df_1993(src_file):

    df = pd.read_fwf(src_file, skiprows=22, skipfooter=29, dtype=str, header=None,
                     colspecs=[(0, 4), (8, 12), (16, 18), (24, 26), (26, 29), (32, 33), (40, 45), (48, 106)],
                     names=['MSA_CMSA_CODE', 'PRIMARY_MSA_CODE', 'ALT_CMSA_CODE',
                            'STATE_CODE', 'COUNTY_CODE', 'CENTRAL_OUTLYING',
                            'TOWN_CODE', 'NAME'])

    assert not util.tag_invalid_values(df['MSA_CMSA_CODE'], notna=True, nchar=4, number=True).any()
    assert not util.tag_invalid_values(df['PRIMARY_MSA_CODE'], nchar=4, number=True).any()
    assert not util.tag_invalid_values(df['ALT_CMSA_CODE'], nchar=2, number=True).any()
    assert not util.tag_invalid_values(df['STATE_CODE'], nchar=2, number=True).any()
    assert not util.tag_invalid_values(df['COUNTY_CODE'], nchar=3, number=True).any()
    assert not util.tag_invalid_values(df['CENTRAL_OUTLYING'], cats=['1', '2']).any()
    assert not util.tag_invalid_values(df['TOWN_CODE'], nchar=5, number=True).any()
    assert not util.tag_invalid_values(df['NAME'], notna=True).any()

    df['CENTRAL_OUTLYING'] = df['CENTRAL_OUTLYING'].map({'1': 'central', '2': 'outlying'})

    return df
```

```{code-cell} ipython3
:tags: []

#| label: tbl-cbsa-counts
#| tbl-cap: "Number of counties in CBSA tables"
#| layout-nrow: 2
t0 = {}
t1 = {}
for year in [1993, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2013, 2015, 2017, 2018, 2020]:
    df = get_cbsa_delin_df(year)
    if year == 1993:
        df = df.dropna(subset='COUNTY_CODE').drop_duplicates(['STATE_CODE', 'COUNTY_CODE'])
        df['METRO_MICRO'] = 'metro'
    t0[year] = df['METRO_MICRO'].value_counts(dropna=False)
    if 'CENTRAL_OUTLYING' in df:
        t1[year] = df[['METRO_MICRO', 'CENTRAL_OUTLYING']].value_counts(dropna=False)
t0 = pd.concat(t0, axis=1)
t0 = t0.fillna(0).astype(int)
display(t0)
t1 = pd.concat(t1, axis=1).sort_index().fillna(0).astype(int)
t1
```

Boundary shapefiles, both TIGER (high res) and catrographic (low res) are available from Census Bureau [geography program](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html).

```{code-cell} ipython3
:tags: [nbd-module]

def get_cbsa_shape_src(year=2021, scale='20m'):
    """Download and return path to CBSA boundary shapefile."""

    base = 'https://www2.census.gov/geo/tiger/'
    urls = {
        (2010, '20m'):  f'{base}GENZ2010/gz_2010_us_310_m1_20m.zip',
        (2010, '500k'): f'{base}GENZ2010/gz_2010_us_310_m1_500k.zip',
        (2013, '20m'):  f'{base}GENZ2013/cb_2013_us_cbsa_20m.zip',
        (2013, '5m'):   f'{base}GENZ2013/cb_2013_us_cbsa_5m.zip',
        (2013, '500k'): f'{base}GENZ2013/cb_2013_us_cbsa_500k.zip',
    }
    urls.update({(y, s): f'{base}GENZ{y}/shp/cb_{y}_us_cbsa_{s}.zip'
                 for y in range(2014, 2022) for s in ['20m', '5m', '500k']})
    
    assert (year, scale) in urls, f'No CBSA shapes in {year}, {scale}.'
    
    local = PATH['source']/f'cbsa/shp/{year}_{scale}.zip'
        
    if not local.exists():
        print(f'File "{local}" not found, attempting download.')
        util.download_file(urls[(year, scale)], local.parent, local.name)
    return local
```

```{code-cell} ipython3
:tags: []

#| tbl-cap: "Shapefile columns over time"
df = {}
for y in [2010] + list(range(2013, 2022)):
    f = get_cbsa_shape_src(y)
    d = geopandas.read_file(f, rows=0)
    df[y] = pd.Series(True, index=d.columns)
df = pd.concat(df, axis=1).fillna(False).replace({False: '', True: 'X'})
df
```

Before 2010 there is a column `CENSUSAREA` - "Area of entity before generalization in square miles". After 2010 there are `ALAND` and `AWATER`. For most entries `CENSUSAREA` equals `ALAND` after conversion from square miles to square meters, but not always.

```{code-cell} ipython3
:tags: [nbd-module]

def get_cbsa_shape_df(year=2021, 
                    scale: typing.Literal['20m', '5m', '500k'] = '20m',
                    geometry=True):
    """Load CBSA shapefile as geodataframe."""
    f = get_cbsa_shape_src(year, scale)
    df = geopandas.read_file(f)

    if year == 2010:
        df = df.rename(columns={
            'CBSA': 'CBSA_CODE',
            'NAME': 'CBSA_TITLE',
            'LSAD': 'METRO_MICRO',
        })
        df['METRO_MICRO'] = df['METRO_MICRO'].str.lower()
        df = df[['CBSA_CODE', 'CBSA_TITLE', 'METRO_MICRO', 'CENSUSAREA', 'geometry']]
    elif 2013 <= year <= 2021:
        df = df.rename(columns={
            'CBSAFP': 'CBSA_CODE',
            'NAME': 'CBSA_TITLE',
            'LSAD': 'METRO_MICRO',
        })
        df['METRO_MICRO'] = df['METRO_MICRO'].map({'M1': 'metro', 'M2': 'micro'})
        df = df[['CBSA_CODE', 'CBSA_TITLE', 'METRO_MICRO', 'ALAND', 'AWATER', 'geometry']]
    else:
        raise NotImplementedError(f'Year {year}.')

    assert df['CBSA_CODE'].notna().all()
    assert not df['CBSA_CODE'].duplicated().any()

    if not geometry:
        df = pd.DataFrame(df).drop(columns='geometry')
    return df
```

```{code-cell} ipython3
:tags: []

#| tbl-cap: "Number of CBSAs in shapefiles"
t = {}
for year in [2010] + list(range(2013, 2022)):
    df = get_cbsa_shape_df(year, geometry=False)
    t[year] = df['METRO_MICRO'].value_counts(dropna=False)
t = pd.concat(t, axis=1)
t.loc['all'] = t.sum()
t
```

```{code-cell} ipython3
:tags: []

#| label: fig-cbsa-cty-wisc
#| fig-cap: "CBSA counties in Wisconsin, 2020."
year = 2020
state = '55'

cm = [mpl.colors.to_hex(c) for c in plt.cm.tab20.colors]
cm = cm[14:16] + cm[2:4] + cm[:2] + cm[5:6]
m = RuralityMap(['metro', 'micro', 'metro central', 'metro outlying', 'micro central', 'micro outlying', 'noncore'], cm)

df = geography.get_county_df(year).query('STATE_CODE == @state').drop(columns='CODE')
d = get_cbsa_delin_df(year)[['STATE_CODE', 'COUNTY_CODE', 'CBSA_CODE', 'CBSA_TITLE', 'METRO_MICRO', 'CENTRAL_OUTLYING']]
df = df.merge(d, 'left', ['STATE_CODE', 'COUNTY_CODE'])
df['CBSA'] = df['METRO_MICRO'] + ' ' + df['CENTRAL_OUTLYING']
df['CBSA'] = df['CBSA'].fillna('noncore')

d = get_cbsa_shape_df(year)
d = d.loc[d['CBSA_CODE'].isin(df['CBSA_CODE']), ['CBSA_CODE', 'CBSA_TITLE', 'METRO_MICRO', 'geometry']]

m.add_layer(d, 'CBSAs', 'METRO_MICRO', legend_kwds={'caption': ''})
m.add_layer(df, 'Counties', 'CBSA')
m.show()
```

# ERS measures of urban spatial effect

The ERS's three measures of urban influence and spatial effect are the Urban Influence codes, the Urban-Rural Continuum codes, and the Urban-Rural Commuting Area codes.

Each set of codes classifies corresponding spatial units into multiple groups, leaving decision to draw the line between rural and non-rural to the researcher.
The unit of analysis for the Urban Influence codes and the Rural-Urban Continuum codes is the county.
For the Rural-Urban Commuting Area codes the unit of analysis is the census tract.

Preparation of ERS codes is done in a [separate notebook](ers_codes.ipynb).

+++

## Rural-Urban Commuting Area (RUCA)

Sub-county division used by RUCA classification allows to identify parts of metro counties that are not strongly connected to respective urban areas. Likewise, there are parts of non-metro counties that are highly connected to an adjacent metro county, but are not big enough to make the entire county qualify for metro criteria.

Rural areas, derived from RUCA codes, often cross CBSA-based rural boundaries, as can be visually seen in @fig-compare-defns-wisc.

```{code-cell} ipython3
:tags: []

#| tbl-cap: "Primary RUCA codes, 2010 revision."
d = pd.read_fwf(ers_rurality.PATH['ruca_doc'], skiprows=144, header=None, widths=[4, 999]).head(11)
d.columns = ['RUCA_CODE', 'RUCA_DESC']
d = d.apply(lambda c: c.str.strip())
d['RUCA_SHORT'] = ['metro core', 'metro high comm', 'metro low comm',
                   'micro core', 'micro high comm', 'micro low comm',
                   'UC core', 'UC high comm', 'UC low comm', 'rural', 'NA']
ruca_code_desc = d = d[['RUCA_CODE', 'RUCA_SHORT', 'RUCA_DESC']]
d
```

```{code-cell} ipython3
:tags: []

#| label: fig-ruca-wisc
#| fig-cap: "Classification of tracts in central Wisconsin, 2010 RUCA revision."
st = '55'
cty = ['073', '097', '141', '019', '119', '099', '069', '085', '067']

# load geographic area
df = geography.get_tract_df([2010], [st])
df = df.loc[(df['STATE_CODE'] == st) & df['COUNTY_CODE'].isin(cty), ['STATE_CODE', 'COUNTY_CODE', 'TRACT_CODE', 'geometry']]

# add RUCA codes with short descriptions
d = ers_rurality.get_ruca_df().query('YEAR == 2010')[['FIPS', 'RUCA_CODE', 'POPULATION', 'AREA']]
d['STATE_CODE'] = d['FIPS'].str[:2]
d['COUNTY_CODE'] = d['FIPS'].str[2:5]
d['TRACT_CODE'] = d['FIPS'].str[5:]
del d['FIPS']
df = df.merge(d, 'left')
df['RUCA_CODE'] = df['RUCA_CODE'].astype(float).astype(int).astype(str)
df['RUCA_CODE'] = df['RUCA_CODE'].fillna('99')
df = df.merge(ruca_code_desc[['RUCA_CODE', 'RUCA_SHORT']], 'left')

# map
cats = ruca_code_desc['RUCA_SHORT'].tolist()
cm = [mpl.colors.to_hex(c) for c in plt.cm.tab20c.colors]
cm = cm[4:7] + cm[0:3] + cm[8:11] + cm[13:14] + cm[17:18]
df.explore(column='RUCA_SHORT', categories=cats, cmap=cm, tiles='CartoDB positron', legend_kwds={'caption': ''})
```

# FORHP

[HRSA](https://www.hrsa.gov/rural-health/about-us/definition/index.html)

This definition of rurality is based on tracts and uses OMB and RUCA definitions with additional refinements in large metro tracts.

HRSA refers to the Health Resources and Services Administration.
It is particularly its sub-unit, the Federal Office of Rural Health Policy (FORHP), that is responsible for this definition of rurality.
For its own administrative purposes it considers a census tract to be rural if it is contained within a county that is not part of a CBSA.

Quote from HRSA page.

> We define the following areas as rural:
> - All non-metro counties
> - All metro census tracts with RUCA codes 4-10 and
> - Large area Metro census tracts of at least 400 sq. miles in area with population density of 35 or less per sq. mile with RUCA codes 2-3. In larger tracts, you cannot use RUCA codes alone. The codes do not factor in distance to services and low numbers of people.
> - Beginning with Fiscal Year 2022 Rural Health Grants, we consider all outlying metro counties without a UA to be rural.

Lists of rural areas by county, census tract, and ZIP code are available on [HRSA Data Files page](https://www.hrsa.gov/rural-health/about-us/what-is-rural/data-files).

+++

# ERS Frontier and Remote (FAR)

There is an obvious family resemblance between "remote" and "rural" which might find some analytical use.

[USDA ERS](https://www.ers.usda.gov/data-products/frontier-and-remote-area-codes.aspx)

> To assist in providing policy-relevant information about conditions in sparsely-settled, remote areas of the U.S. to public officials, researchers, and the general public, ERS has developed ZIP-code-level frontier and remote area (FAR) codes.
>
> The term "frontier and remote" is used here to describe territory characterized by some combination of low population size and high geographic remoteness. FAR areas are defined in relation to the time it takes to travel by car to the edges of nearby Urban Areas (UAs). Four levels are necessary because rural areas experience degrees of remoteness at higher or lower population levels that affect access to different types of goods and services. A relatively large number of people live far from cities providing "high order" goods and services, such as advanced medical procedures, stores selling major household appliances, regional airport hubs, or professional sports franchises. Level one FAR codes are meant to approximate this degree of remoteness. A much smaller, but still significant, number of people find it hard to access "low order" goods and services, such as grocery stores, gas stations, and basic health-care services. Level four FAR codes more closely coincide with this much higher degree of remoteness. Other types of goods and services—clothing stores, car dealerships, movie theaters—fall somewhere in between. Users are able to choose the definition that bests suits their specific needs.

Two revisions of the codes currently exist, based on population data from 2000 and 2010 censuses. 2000 revision does not use road network travel time.

Although data is provided at ZIP code level, Census Bureau ZCTAs are not an exactly appropriate spatial unit. Explanation from 2010 data spreadsheet:

> ZIP Code areas used here come from ESRI mapping data, based on 2014 information from the U.S. Postal Service. These codes may or may not match exactly with other ZIP Code data sources due to frequent changes in ZIP Code configurations. They do not fully match with the Census Bureau's ZIP Code Tabulation Areas (ZCTAs). For more information, see Methodology Statement: 2013/2018 ESRI US Demographic Updates, August 2013: [link](http://downloads.esri.com/esri_content_doc/dbl/us/demographic-update-methodology-2013.pdf).

Dataframes contain all columns from the source spreadsheets and additional variable `FAR_LEVEL`.

| Variable  | Description                                                  |
|-----------|--------------------------------------------------------------|
| ZIP       | 5-digit ZIP Code                                             |
| STATE     | State postal abbreviation                                    |
| NAME      | ZIP Code area name (2010 only)                               |
| FAR1      | FAR classification, level one: 0=not FAR, 1=FAR              |
| FAR2      | FAR classification, level two: 0=not FAR, 1=FAR              |
| FAR3      | FAR classification, level three: 0=not FAR, 1=FAR            |
| FAR4      | FAR classification, level four: 0=not FAR, 1=FAR             |
| GRIDPOP   | ZIP code population estimate                                 |
| SQMI      | ZIP code land area in square miles                           |
| DENSITY   | ZIP code population per square mile                          |
| FR1POP    | ZIP code population classified as FAR level one              |
| FR2POP    | ZIP code population classified as FAR level two              |
| FR3POP    | ZIP code population classified as FAR level three            |
| FR4POP    | ZIP code population classified as FAR level four             |
| FR1PCT    | Percent of ZIP code population classified as FAR level one   |
| FR2PCT    | Percent of ZIP code population classified as FAR level two   |
| FR3PCT    | Percent of ZIP code population classified as FAR level three |
| FR4PCT    | Percent of ZIP code population classified as FAR level four  |
| FAR_LEVEL | Highest FAR level, between 0 and 4                           |

2010 tables are de-duplicated. There are five `ZIP` duplicates still remain that cross state boundaries: 57724 (MT, SD), 73949 (OK, TX), 63673 (MO, IL), 42223 (TN, KY), 72395 (AR, TN). These records are equal in every column except for `STATE`.

No ZIP duplicates exist in 2000 table.

```{code-cell} ipython3
:tags: [nbd-module]

def get_far_src(year: typing.Literal[2000, 2010] = 2010):
    assert year in [2000, 2010]
    
    url = 'https://www.ers.usda.gov/webdocs/DataFiles/51020/'
    if year == 2000:
        url += 'FARCodesZIPCodeData.xls?v=7835.9'
        local = PATH['source'] / 'ers_far/2000.xls'
    elif year == 2010:
        url += 'FARcodesZIPdata2010WithAKandHI.xlsx?v=7835.9'
        local = PATH['source'] / 'ers_far/2010.xlsx'
        
    if local.exists():
        return local
    return util.download_file(url, local.parent, local.name)

def get_far_df(year: typing.Literal[2000, 2010] = 2010):
    assert year in [2000, 2010]
    
    far_level_dt = pd.CategoricalDtype([0, 1, 2, 3, 4], True)
    
    path = PATH['ers_far'] / f'{year}.pq'
    if path.exists():
        df = pd.read_parquet(path, 'pyarrow')
        df['FAR_LEVEL'] = df['FAR_LEVEL'].astype(far_level_dt)
        return df
    
    f = get_far_src(year)
    df = pd.read_excel(f, 'FAR ZIP Code Data', dtype={'ZIP': str})
    df = df.rename(columns=str.upper)
    if year == 2000:
        df = df.rename(columns={f'PCTFR{x}': f'FR{x}PCT' for x in range(1, 5)})

    assert df.notna().all().all()
    assert (df['ZIP'].str.len() == 5).all()

    # We can add up binary FARX indicators to obtain single FAR level,
    # because classification is nested, e.g. far2 implies far1, and not-far1 implies not-far2.
    df['FAR_LEVEL'] = df[['FAR1', 'FAR2', 'FAR3', 'FAR4']].sum(1).astype(far_level_dt)

    # de-duplicate 2010
    if year == 2000:
        assert not df.duplicated('ZIP').any()
    elif year == 2010:
        df = df.drop_duplicates()

        # duplicate ZIPs that cross states:
        # 

        # drop duplicate ZIP in New Mexico with abnormally small area
        mask = (df['ZIP'] == '87320') & (df['SQMI'] == 0.34)
        df = df[~mask]

        # no duplicates at ZIP-STATE
        assert not df.duplicated(['ZIP', 'STATE']).any()
        # all duplicated ZIP only differ by STATE
        assert not df[[c for c in df.columns if c != 'STATE']].drop_duplicates()['ZIP'].duplicated().any()

    df = df.reset_index(drop=True)
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path, 'pyarrow', index=False)
    
    return df
```

```{code-cell} ipython3
:tags: []

#| tbl-cap: "Sample from 2010 dataframe with 5 different FAR levels."
get_far_df(2010).sample(1000).drop_duplicates('FAR_LEVEL').sort_values('FAR_LEVEL')
```

```{code-cell} ipython3
:tags: []

#| fig-cap: "FAR ZIP areas in Wisconsin part of Duluth metro area, WI. Note: Census Bureau ZCTAs are used as shapes instead of ESRI."

# Dane county
# map_shape = geography.get_county_df().query('CODE == "55025"').geometry.iloc[0]
# Wisconsin
# map_shape = geography.get_state_df().query('CODE == "55"').geometry.iloc[0]
# Duluth metro area in WI: Douglas and Bayfield
map_shape = geography.get_county_df().query('CODE.isin(["55007", "55031"])').dissolve().geometry.iloc[0]

df = geography.get_zcta_df(2013)
df = df[df.intersects(map_shape)]

d = get_far_df(2010).drop(columns=['STATE', 'FAR1', 'FAR2', 'FAR3', 'FAR4'])
df = df.merge(d, 'left', left_on='ZCTA', right_on='ZIP')

cm = [mpl.colors.to_hex(c) for c in plt.cm.tab20c.colors]
cm = cm[5:6] + cm[0:5]
df.explore(column='FAR_LEVEL', cmap=cm, tiles='CartoDB positron')
```

# Comparison of rural definitions

**UA**

Rural is everyting outside of urbanized areas and urban clusters. High resolution source shapes are simplified to reduce HTML size for online publication.

```{code-cell} ipython3
:tags: []

def rural_map_ua(state_code, simplify=True):
    state_shp = geography.get_state_df().query('CODE == @state_code').geometry.iloc[0]
    df = get_ua_df(2010)
    df = df[df.intersects(state_shp)]
    not_rural = df.geometry.unary_union
    if simplify: not_rural = not_rural.simplify(0.01)
    rural = state_shp - not_rural.buffer(0)
    if rural.is_empty: 
        warnings.warn(f'UA rural area is empty in state_code "{state_code}".')
    d = geopandas.GeoDataFrame({'definition': ['UA']}, geometry=[rural], crs=df.crs)
    return d
```

**CBSA**

Rural = everything outside of metropolitan counties. Micropolitan areas are by this definition also rural.

```{code-cell} ipython3
:tags: []

def rural_map_cbsa(state_code):
    year = 2013
    df = geography.get_county_df(year).query('STATE_CODE == @state_code')
    d = get_cbsa_delin_df(year)
    df = df.merge(d, 'left', indicator=True)
    rural = df[(df['_merge'] == 'left_only') | (df['METRO_MICRO'] == 'micro')]

    if len(rural) == 0:
        warnings.warn(f'CBSA rural area is empty in state_code "{state_code}".')
        rural = shapely.geometry.Point()
    else:
        rural = rural.geometry.unary_union

    d = geopandas.GeoDataFrame({'definition': ['CBSA']}, geometry=[rural], crs=df.crs)
    return d
```

**RUCA**

Rural = micro and non-core codes (4, 5, 6, 7, 8, 9, 10, 99). Tract shapes are simplified.

```{code-cell} ipython3
:tags: []

def rural_map_ruca(state_code, simplify=True):
    df = geography.get_tract_df([2010], [state_code])

    # add RUCA codes with short descriptions
    d = ers_rurality.get_ruca_df().query('YEAR == 2010')[['FIPS', 'RUCA_CODE']]
    d['STATE_CODE'] = d['FIPS'].str[:2]
    d['COUNTY_CODE'] = d['FIPS'].str[2:5]
    d['TRACT_CODE'] = d['FIPS'].str[5:]
    df = df.merge(d, 'left')
    df['RUCA_CODE'] = df['RUCA_CODE'].astype(float).astype(int).astype(str)
    df['RUCA_CODE'] = df['RUCA_CODE'].fillna('99')

    # select subset of codes as rural
    rural = df[df['RUCA_CODE'].isin(['4', '5', '6', '7', '8', '9', '10', '99'])]
    if len(rural) == 0:
        warnings.warn(f'RUCA rural area is empty in state_code "{state_code}".')
        rural = shapely.geometry.Point()
    else:
        rural = rural.geometry.unary_union
        if simplify: rural = rural.simplify(0.01)
        
    d = geopandas.GeoDataFrame({'definition': ['RUCA']}, geometry=[rural], crs=df.crs)
    return d
```

**FAR**

Compared to other definition, Frontier and Remote is rather conservative. Many areas that are not FAR (level 0) are classified as rural by other definitions. So we apply the most aggressive criterion and classify areas with any level of FAR (1, 2, 3 or 4) as rural.

```{code-cell} ipython3
:tags: []

def rural_map_far(state_code, simplify=True):

    df = geography.get_zcta_df(2013)

    state_abbr = dict(geography.get_state_df(False)[['CODE', 'ABBR']].values)[state_code]
    d = get_far_df(2010).query('STATE == @state_abbr')
    # innere merge: non-matches dropped
    df = df.merge(d, 'inner', left_on='ZCTA', right_on='ZIP')
    rural = df.query('FAR_LEVEL > 0')

    if len(rural) == 0:
        warnings.warn(f'FAR rural area is empty in state_code "{state_code}".')
        rural = shapely.geometry.Point()
    else:
        rural = rural.geometry.unary_union
        if simplify: rural = rural.simplify(0.01)

    d = geopandas.GeoDataFrame({'definition': ['FAR']}, geometry=[rural], crs=df.crs)
    return d
```

```{raw-cell}
:tags: []

# test all definitions in all states
for sc in geography.get_state_df(False)['CODE']:
    print(sc)
    try:
        d = rural_map_ua(sc)
        d = rural_map_cbsa(sc)
        d = rural_map_ruca(sc)
        d = rural_map_far(sc)
    except Exception as e:
        print('******************** ACHTUNG! ACHTUNG! ACHTUNG! ********************')
        print(e)
```

```{code-cell} ipython3
:tags: []

def rural_map_all(state_code):
    m = RuralityMap(['UA', 'CBSA', 'RUCA', 'FAR'], [mpl.colors.to_hex(c) for c in plt.cm.Dark2.colors])
    m.add_layer(rural_map_ua(state_code), 'UA', 'definition')
    m.add_layer(rural_map_cbsa(state_code), 'CBSA', 'definition')
    m.add_layer(rural_map_ruca(state_code), 'RUCA', 'definition')
    m.add_layer(rural_map_far(state_code), 'FAR', 'definition')
    return m.show()
```

```{code-cell} ipython3
:tags: []

#| label: fig-compare-defns-wisc
#| fig-cap: "Rural areas of Wisconsin by different definitions, based on 2010 data."
rural_map_all('55')
```

```{code-cell} ipython3
:tags: []

states = geography.get_state_df(False)[['NAME', 'CODE']].sort_values('NAME')
w_state = ipywidgets.Dropdown(options=states.values.tolist(), value='55')
w_out = ipywidgets.Output()
w_show = ipywidgets.Button(description='Show')
def show_selected_state(_):
    with w_out:
        w_out.clear_output(False)
        print('Working...')
        m = rural_map_all(w_state.value)
        w_out.clear_output(True)
        display(m)
w_show.on_click(show_selected_state)
ipywidgets.VBox([ipywidgets.HBox([w_state, w_show]), w_out])
```

# Build this module

```{raw-cell}
:tags: []

nbd.nb2mod('rurality.ipynb')
```
