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

There are many different defitions of rurality, both within research community and in public policy. A recent systematic literature review [@nelson_definitions_2021] identified 65 research articles with different rurality definitions. A selection of definitions is presented here. In many cases, non-rural is defined first, and everything outside of it is considered rural.

There are two major definitions which the Federal government uses to identify the rural status of an area: the Census Bureau's 'Urban Area' and the OMB's 'Core-Based Statistical Area'.

```{code-cell} ipython3
:tags: []

import functools
import typing

import pandas as pd
import geopandas
import folium
import folium.plugins
import matplotlib as mpl
import matplotlib.pyplot as plt
import ipywidgets

from rurec import geography, ers_codes
from rurec.reseng.util import download_file
from rurec.reseng.nbd import Nbd

nbd = Nbd('rurec')
PATH = {
    'root': nbd.root,
    'data': nbd.root/'data',
    'source': nbd.root/'data/source/'
}
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
:tags: []

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
        download_file(url, local.parent, local.name)
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

#| echo: true
#| tbl-cap: Sample from the urban area dataframe (no geometry).
get_ua_df(year=2010, geometry=False).sample(3).style.hide_index()
```

@fig-urban-areas-dane-cty shows how urban areas expand in Dane county, Wisconsin between 2000 and 2010 censuses. Small 2000 urban clusters of Cross Plains and DeForest by 2010 merged with bigger Madion urbanized area.

```{code-cell} ipython3
:tags: []

#| label: fig-urban-areas-dane-cty
#| fig-cap: "Urban areas in Dane county, WI from 2000 and 2010 censuses."

dane_cty = [slice(-90,-89.05), slice(42.9,43.3)]
with ipywidgets.Output(): # gobble stdout
    d0 = get_ua_df(2000).cx[dane_cty]
    d0['UATYP'] = d0['UATYP'].map({'U': '2000 Uranized Area', 'C': '2000 Urban Cluster'})
    d1 = get_ua_df(2010).cx[dane_cty]
    d1['UATYP'] = d1['UATYP'].map({'U': '2010 Uranized Area', 'C': '2010 Urban Cluster'})

cats = ['2000 Uranized Area', '2000 Urban Cluster', '2010 Uranized Area', '2010 Urban Cluster']
cm = [mpl.colors.to_hex(c) for c in plt.cm.tab20.colors[0:4]]
m = d0.explore(name='Census 2000', column='UATYP', categories=cats, cmap=cm, tiles='CartoDB positron')
d1.explore(m=m, name='Census 2010', column='UATYP', categories=cats, cmap=cm, legend=False)
tile_layer = [x for x in m._children.values() if isinstance(x, folium.raster_layers.TileLayer)][0]
tile_layer.control = False
folium.LayerControl(collapsed=False).add_to(m)
m
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

```{code-cell} ipython3
:tags: []

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
    }
    
    assert year in urls, f'CBSA delineation not available for {year}.'
    
    url = urls[year]
    ext = url.split('.')[-1]
    local = PATH['source'] / f'cbsa/delin/{year}.{ext}'
        
    if not local.exists():
        print(f'File "{local}" not found, attempting download.')
        download_file(url, local.parent, local.name)
    return local

def get_cbsa_delin_df(year: int):
    f = get_cbsa_delin_src(year)
    
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

```{code-cell} ipython3
:tags: []

#| label: tbl-cbsa-counts
#| tbl-cap: "Number of counties in CBSA tables"
#| layout-nrow: 2
t0 = {}
t1 = {}
for year in [2003, 2004, 2005, 2006, 2007, 2008, 2009, 2013, 2015, 2017, 2018, 2020]:
    df = get_cbsa_delin_df(year)
    t0[year] = df['METRO_MICRO'].value_counts(dropna=False)
    if 'CENTRAL_OUTLYING' in df:
        t1[year] = df[['METRO_MICRO', 'CENTRAL_OUTLYING']].value_counts(dropna=False)
t0 = pd.concat(t0, axis=1)
t0.loc['all'] = t0.sum()
display(t0)
t1 = pd.concat(t1, axis=1).sort_index()
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
        download_file(urls[(year, scale)], local.parent, local.name)
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
:tags: []

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

df = geography.get_county_df(year).query('STATE_CODE == @state').drop(columns='CODE')
d = get_cbsa_delin_df(year)[['STATE_CODE', 'COUNTY_CODE', 'CBSA_CODE', 'CBSA_TITLE', 'METRO_MICRO', 'CENTRAL_OUTLYING']]
df = df.merge(d, 'left', ['STATE_CODE', 'COUNTY_CODE'])
df['CBSA'] = df['METRO_MICRO'] + ' ' + df['CENTRAL_OUTLYING']
df['CBSA'] = df['CBSA'].fillna('noncore')

cats = ['metro', 'micro', 'metro central', 'metro outlying', 'micro central', 'micro outlying', 'noncore']
cm = [mpl.colors.to_hex(c) for c in plt.cm.tab20.colors]
cm = cm[14:16] + cm[2:4] + cm[:2] + cm[5:6]

d = get_cbsa_shape_df(year)
d = d.loc[d['CBSA_CODE'].isin(df['CBSA_CODE']), ['CBSA_CODE', 'CBSA_TITLE', 'METRO_MICRO', 'geometry']]
m = d.explore(name='CBSAs', column='METRO_MICRO', categories=cats, cmap=cm, 
              tiles='CartoDB positron', legend_kwds={'caption': ''})

df.explore(m=m, name='Counties', column='CBSA', categories=cats, cmap=cm, legend=False)

tile_layer = [x for x in m._children.values() if isinstance(x, folium.raster_layers.TileLayer)][0]
tile_layer.control = False
folium.LayerControl(collapsed=False).add_to(m)

m
```

# ERS measures of urban spatial effect

The ERS's three measures of urban influence and spatial effect are the Urban Influence codes, the Urban-Rural Continuum codes, and the Urban-Rural Commuting Area codes.

Each set of codes classifies corresponding spatial units into multiple groups, leaving decision to draw the line between rural and non-rural to the researcher.
The unit of analysis for the Urban Influence codes and the Rural-Urban Continuum codes is the county.
For the Rural-Urban Commuting Area codes the unit of analysis is the census tract.

Preparation of ERS codes is done in a [separate notebook](ers_codes.ipynb).

+++

# FORHP

[HRSA](https://www.hrsa.gov/rural-health/about-us/definition/index.html)

This definition of rurality is based on tracts and can be though of as a special case of RUCA codes.

HRSA refers to the Health Resources and Services Administration.
It is particularly its sub-unit, the Federal Office of Rural Health Policy (FORHP), that is responsible for this definition of rurality.
For its own administrative purposes it considers a census tract to be rural if it is contained within a county that is not part of a CBSA.
To these, they add 2,302 census tracts from CBSA counties that they have specially defined as rural by applying the RUCA criteria, of which the FORHP was actually a developer in its early phase.
Tracts inside Metropolitan counties with the codes 4-10 are considered rural.
While use of the RUCA codes has allowed identification of rural census tracts in Metropolitan counties, among the more than 60,000 tracts in the U.S. there are some that are extremely large and where use of RUCA codes alone fails to account for distance to services and sparse population.
In response to these concerns, FORHP has designated 132 large area census tracts with RUCA codes 2 or 3 as rural.
These tracts are at least 400 square miles in area with a population density of no more than 35 people.
The FORHP definition includes about 18% of the population and 85% of the area of the USA.
RUCA codes represent the current version of the Goldsmith Modification.

+++

# ERS Frontier and Remote (FAR)

[USDA](https://www.ers.usda.gov/data-products/frontier-and-remote-area-codes.aspx)

> To assist in providing policy-relevant information about conditions in sparsely-settled, remote areas of the U.S. to public officials, researchers, and the general public, ERS has developed ZIP-code-level frontier and remote area (FAR) codes.

FAR codes are applied to postal ZIP codes to identify different degrees and criteria of remoteness.
It is not a code for any functional concept of rurality, but there is an obvious family resemblance between “remote” and “rural” which might find some analytical use.

The ERS created four FAR levels based on proximity (conceived of as travel time) to “urban” places of different sizes.
Levels 1 through 4 measure increasing remoteness.
The ‘FAR Level’ variable captures the highest numbered positive FAR level for a location.

+++

# Map

```{code-cell} ipython3
:tags: []

@functools.cache
def ui_df():
    df = geography.get_county_df()\
        .query('STATE_CODE == "55"')\
        [['CODE', 'NAME', 'geometry']]\
        .rename(columns={'CODE': 'FIPS'})

    d = ers_codes.get_ui_df()\
        .query('UI_YEAR == 2013 and STATE == "WI"')\
        [['FIPS', 'UI_CODE']]
    assert set(df['FIPS']) == set(d['FIPS'])
    df = df.merge(d)

    df['RURAL'] = (df['UI_CODE'].astype(int) > 9)
    return df
```

```{code-cell} ipython3
:tags: []

@functools.cache
def ruca_df():
    df = geography.get_tract_df([2010], ['55'])\
        [['CODE', 'geometry']]\
        .rename(columns={'CODE': 'FIPS'})

    d = ers_codes.get_ruca_df()\
        .query('YEAR == 2010 and STATE == "WI"')\
        [['FIPS', 'RUCA_CODE']]

    # some tracts are in RUCA only, maybe mismatch of years
    df = df.merge(d, 'left')

    df['RURAL'] = (df['RUCA_CODE'].astype(float).astype(int) > 9)
    return df
```

```{code-cell} ipython3
:tags: []

def state_bbox(state_code):
    df = geography.get_state_df()
    shape = df.loc[df['CODE'] == state_code, 'geometry'].values[0]
    x = shape.bounds
    return [(x[1], x[0]), (x[3], x[2])]
```

```{code-cell} ipython3
:tags: []

m = folium.Map()
tile_layer = next(iter(m._children.values()))
tile_layer.control = False

def style_fn_factory(color):
    def style_fn(feature):
        style = dict(stroke=False, color=color)
        if feature['properties']['RURAL']:
            style['fillPattern'] = folium.plugins.StripePattern(color=color, angle=-45)
        return style
    return style_fn

folium.GeoJson(ui_df().to_json(), name='UI', style_function=style_fn_factory('blue')).add_to(m)
folium.GeoJson(ruca_df().to_json(), name='RUCA', style_function=style_fn_factory('red')).add_to(m)
folium.plugins.MousePosition().add_to(m)
folium.LayerControl(collapsed=False).add_to(m)
m.fit_bounds(state_bbox('55'))
m
```
