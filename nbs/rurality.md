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
    'source': nbd.root/'data/source/rurality/',
    'urban_area': nbd.root/'data/rurality/urban_area/'
}
```

# Urban area

[Census Bureau](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html)

> The Census Bureau’s urban-rural classification is a delineation of geographic areas, identifying both individual urban areas and the rural areas of the nation. The Census Bureau’s urban areas represent densely developed territory, and encompass residential, commercial, and other non-residential urban land uses. The Census Bureau delineates urban areas after each decennial census by applying specified criteria to decennial census and other data. “Rural” encompasses all population, housing, and territory not included within an urban area.

The U.S. Census Bureau identifies two types of urban areas: *Urbanized Areas* (UAs) of 50,000 or more people and *Urban Clusters* (UCs) of at least 2,S00 and less than 50,000 people.
Urban Areas are not defined in terms of any other standard spatial unit.
The borders of an urban area are defined by the density of commuting patterns in the orbit of urban cores of various population size.

Data source page: [TIGER/Line® Shapefiles](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). Boundaries are defined after decennial census. Using latest national data files for every revision: 2009 file for 2000 boundaries, 2021 file for 2010 boundaries.

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
    local = PATH['source'] / url.split('/')[-1]
        
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
    
    path = PATH['urban_area']/f'{year}.pq'
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

+++ {"tags": [], "jp-MarkdownHeadingCollapsed": true}

# Core based statistical area (CBSA)

[Census page](https://www.census.gov/programs-surveys/metro-micro.html)

The Office of Management and Budget (OMB) designates counties as Metropolitan, Micropolitan, or Neither.
All counties that are not part of a Metropolitan Statistical Area (MSA) are considered rural. 

There are measurement challenges with both the U.S. Census Bureau and OMB definitions. 
Some policy experts note that the U.S. Census Bureau definition classifies quite a bit of suburban area as rural.
The OMB definition includes rural areas in Metropolitan bounties.
Consequently, one could argue that the Census Bureau standard includes an overcount of rural population whereas the OMB standard represents an undercount of the rural population.

+++

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
