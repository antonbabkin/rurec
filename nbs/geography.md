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

# Geographic gazetteer

The main purpose of this notebook is to retrieve and prepare dataframes with geographic boundaries of various geographic units of the USA.

```{code-cell} ipython3
:tags: [nbd-module]

import functools
import warnings
import shutil

import pandas as pd
import geopandas as gpd
import pyarrow

from rurec.reseng.config import Paths
from rurec import util

PATH = Paths(
    source='data/geo/source',
    state='data/geo/state.pq',
    county='data/geo/county.pq',
    tract='data/geo/tract.pq',
    cbsa='data/geo/cbsa.json'
)
```

```{code-cell} ipython3
:tags: [nbd-module]

# in geopandas 0.8, parquet support is still experimental
# https://geopandas.org/docs/user_guide/io.html#apache-parquet-and-feather-file-formats
import warnings
warnings.filterwarnings('ignore', message='.*initial implementation of Parquet.*')
```

```{code-cell} ipython3
:tags: []

import ipywidgets as widgets
import matplotlib as mpl
import matplotlib.pyplot as plt
```

+++ {"tags": []}

# Source files

Source data files are downloaded from web and cached locally.

Census Bureau:
- [TIGER Data Products Guide](https://www.census.gov/programs-surveys/geography/guidance/tiger-data-products-guide.html): Which Product Should I Use?
- Cartographic Boundary Files. [2018 and before](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html), [2019 and after](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html). Simplified representations of selected geographic areas from the Census Bureau’s MAF/TIGER geographic database. Small scale (limited detail) spatial files clipped to shoreline.
- [TIGER/Line shapefiles](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html). Most comprehensive geographic dataset in full detail.
- [Relationship files](https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html). These text files describe geographic relationships. There are two types of relationship files; those that show the relationship between the same type of geography over time (comparability) and those that show the relationship between two types of geography for the same time period.
- [LSAD codes](https://www.census.gov/library/reference/code-lists/legal-status-codes.html). Legal/Statistical Area Description Codes and Definitions
- [FIPS codes](https://www.census.gov/geographies/reference-files/2019/demo/popest/2019-fips.html)
- [Gazeteer reference files](https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html)
- [Character encoding](https://www.census.gov/programs-surveys/geography/technical-documentation/user-note/special-characters.html). Files from 2014 and earlier use "ISO-8859-1", 2015 and after use "UTF-8".

Shapefiles contain area columns, but we do not include them in our dataframes because in their raw form these columns are not consistent across years. 1990 and 2000 have `AREA`, 2010 has `CENSUSAREA`, 2013+ have `ALAND` and `AWATER`.

+++

## Scale

Shapefiles are available in different scale. TIGER is the most precise, then follows 1:500,000, then 1:5,000,000, and 1:20,000,000 is the lowest resolution.

Shapefile revisions change from year to year. Between year differences are clearly visible in all scales except TIGER.

```{code-cell} ipython3
:tags: []

df = []

f = util.download_file('https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/2010/tl_2010_09_county10.zip', PATH.root/'tmp')
d = gpd.read_file(f).query('COUNTYFP10 == "013"')[['geometry']]
d['scale'] = 'tiger 2010'
df.append(d)

f = util.download_file('https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/tl_2020_us_county.zip', PATH.root/'tmp')
d = gpd.read_file(f).query('STATEFP == "09" and COUNTYFP == "013"')[['geometry']]
d['scale'] = 'tiger 2020' 
df.append(d)


f = util.download_file('https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_500k.zip', PATH.root/'tmp')
d = gpd.read_file(f).query('STATE == "09" and COUNTY == "013"')[['geometry']]
d['scale'] = '500k 2010'
df.append(d)

f = util.download_file('https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_500k.zip', PATH.root/'tmp')
d = gpd.read_file(f).query('STATEFP == "09" and COUNTYFP == "013"')[['geometry']]
d['scale'] = '500k 2020'
df.append(d)

f = util.download_file('https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_5m.zip', PATH.root/'tmp')
d = gpd.read_file(f).query('STATE == "09" and COUNTY == "013"')[['geometry']]
d['scale'] = '5m 2010'
df.append(d)

f = util.download_file('https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip', PATH.root/'tmp')
d = gpd.read_file(f).query('STATE == "09" and COUNTY == "013"')[['geometry']]
d['scale'] = '20m 2010'
df.append(d)

df = pd.concat(df, ignore_index=True)
```

Table below compares boundaries of Tolland County, Connecticut, taken from shapefiles in different years and scales. "Length" column is is boundary length in shape units (degrees), and "points" is the total number of points in the polygon.

```{code-cell} ipython3
:tags: []

with warnings.catch_warnings():
    warnings.filterwarnings('ignore', message='.*Geometry is in a geographic CRS.*')
    d = df.copy()
    d.geometry = d.boundary
    d['length'] = d.geometry.length
    d['points'] = [len(x.coords) for x in d.geometry]
    d = d.drop(columns='geometry')
    display(d.style.hide_index())
```

Map below visualizes boundary differences.

```{code-cell} ipython3
:tags: []

d = df.copy()
d.geometry = d.boundary
d.explore(column='scale', tiles='CartoDB positron')
```

```{code-cell} ipython3
:tags: [nbd-module]

def get_source(src):
    """Return path to file specified by `src` key, downloading if missing."""
    if src == 'state-boundary':
        url = 'https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip'
        local = PATH.source/'cb_2018_us_state_20m.zip'
    elif src.startswith('tract-boundary-'):
        # tract-boundary-YYYY-SS, YYYY = decennial census year, SS = state FIPS code
        y = int(src[15:19])
        s = src[-2:]
        if y == 1990:
            url = f'https://www2.census.gov/geo/tiger/PREVGENZ/tr/tr90shp/tr{s}_d90_shp.zip'
        elif y == 2000:
            url = f'https://www2.census.gov/geo/tiger/PREVGENZ/tr/tr00shp/tr{s}_d00_shp.zip'
        elif y == 2010:
            url = f'https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_{s}_140_00_500k.zip'
        elif y == 2020:
            url = f'https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_{s}_tract_500k.zip'
        else:
            raise Exception(f'No tract revisions in {y}.')
        local = PATH.source/f'tract/{y}/{s}.zip'
    else:
        raise Exception(f'Unknown source: {src}')
        
    if not local.exists():
        print(f'File "{local}" not found, attempting download.')
        util.download_file(url, local.parent, local.name)
    return local
```

+++ {"tags": []}

# State

```{code-cell} ipython3
:tags: [nbd-module]

def get_state_df(geometry=True):
    path = PATH.state
    if path.exists():
        if geometry:
            return gpd.read_parquet(path)
        else:
            return pd.read_parquet(path, 'pyarrow', ['CODE', 'ABBR', 'NAME', 'ALAND', 'AWATER'])

    p = get_source('state-boundary')
    df = gpd.read_file(p)
    df = df.rename(columns={'STATEFP': 'CODE', 'STUSPS': 'ABBR'})
    df = df[['CODE', 'ABBR', 'NAME', 'ALAND', 'AWATER', 'geometry']]
    assert not df.duplicated('CODE').any()
    df.to_parquet(path)
    if not geometry:
        df = pd.DataFrame(df).drop(columns='geometry')
    return df
```

```{code-cell} ipython3
def show_state_map():
    import matplotlib.pyplot as plt

    df = get_state_df()
    df = df[~df['ABBR'].isin(['AK', 'HI', 'PR'])]
    fig, ax = plt.subplots(figsize=(24, 8))
    ax.set_aspect('equal')
    df.boundary.plot(ax=ax)
    for abbr, shape in df[['ABBR', 'geometry']].itertuples(False):
        ax.annotate(abbr, shape.centroid.coords[0], horizontalalignment='center')

show_state_map()
```

# County


**Source data**

[Cartographic Boundary Files](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html) are available for 1990, 2000, 2010, 2013 and every year after that.

[TIGER/Line Shapefiles](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) in shapefile format are available for 2000, 2007 and every year after that. 1992 and 2006 are available in legacy format.


**Changes**

County changes happen whenever decided by local authoritities. Annually released boundary files reflect boundaries effective January 1 of the reference year. List of changes [here](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html).

> Substantial county boundary changes are those affecting an estimated population of 200 or more; changes of at least one square mile where an estimated population number was not available, but research indicated that 200 or more people may have been affected; and annexations of unpopulated territory of at least 10 square miles.

```{code-cell} ipython3
:tags: [nbd-module]

def get_source_county(year=2020, scale='20m'):
    """Download and return path to county boundary shapefile."""

    base = 'https://www2.census.gov/geo/tiger/'
    urls = {
        # 1990 and 2000 files exist only in one scale, probably 20m
        (1990, '20m'):  f'{base}PREVGENZ/co/co90shp/co99_d90_shp.zip',
        (2000, '20m'):  f'{base}PREVGENZ/co/co00shp/co99_d00_shp.zip',
        (2010, '20m'):  f'{base}GENZ2010/gz_2010_us_050_00_20m.zip',
        (2010, '5m'):   f'{base}GENZ2010/gz_2010_us_050_00_5m.zip',
        (2010, '500k'): f'{base}GENZ2010/gz_2010_us_050_00_500k.zip',
        (2013, '20m'):  f'{base}GENZ2013/cb_2013_us_county_20m.zip',
        (2013, '5m'):   f'{base}GENZ2013/cb_2013_us_county_5m.zip',
        (2013, '500k'): f'{base}GENZ2013/cb_2013_us_county_500k.zip',
    }
    urls.update({(y, s): f'{base}GENZ{y}/shp/cb_{y}_us_county_{s}.zip'
                 for y in range(2014, 2021) for s in ['20m', '5m', '500k']})
    
    assert (year, scale) in urls, f'No county shapes in {year}, {scale}.'
    
    local = PATH.source/f'county/{year}_{scale}.zip'
        
    if not local.exists():
        print(f'File "{local}" not found, attempting download.')
        util.download_file(urls[(year, scale)], local.parent, local.name)
    return local
```

CRS in 1990 and 2000 is unknown, created dataframes have "naive geometries".

```{code-cell} ipython3
:tags: [nbd-module]

def get_county_df(year=2020, geometry=True, scale='20m'):

    path = PATH.county/f'{year}/{scale}.pq'
    if path.exists():
        if geometry:
            return gpd.read_parquet(path)
        else:
            return pd.read_parquet(path, 'pyarrow', ['CODE', 'NAME', 'STATE_CODE', 'COUNTY_CODE'])

    p = get_source_county(year, scale)
    df = gpd.read_file(p)
    if year == 1990:
        df = df.rename(columns={'ST': 'STATE_CODE', 'CO': 'COUNTY_CODE'})
    elif year in [2000, 2010]:
        df = df.rename(columns={'STATE': 'STATE_CODE', 'COUNTY': 'COUNTY_CODE'})
    else:
        df = df.rename(columns={'STATEFP': 'STATE_CODE', 'COUNTYFP': 'COUNTY_CODE'})
    df['CODE'] = df['STATE_CODE'] + df['COUNTY_CODE']
    df = df[['CODE', 'NAME', 'STATE_CODE', 'COUNTY_CODE', 'geometry']]
    
    # 1990 and 2000 shapefiles have multiple polygon records per non-contiguous county
    if year in [1990, 2000]:
        df = df.dissolve('CODE', as_index=False, sort=False)
    
    assert not df.duplicated('CODE').any()
    
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path)
    if not geometry:
        df = pd.DataFrame(df).drop(columns='geometry')
    return df
```

```{code-cell} ipython3
:tags: []

def show_county_map():
    county_years = [1990, 2000, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020]
    w_year = widgets.Dropdown(description='Year', value=2020, options=county_years)
    w_state = widgets.Dropdown(description='State', value='55',
                               options=sorted(get_state_df(False)[['NAME', 'CODE']].values.tolist()))
    w_out = widgets.Output()
    w_show = widgets.Button(description='Show map')
    def _show(_):
        df = get_county_df(w_year.value)
        df = df[df['STATE_CODE'] == w_state.value]
        with w_out:
            w_out.clear_output(True)
            fig, ax = plt.subplots(figsize=(12, 12))
            ax.set_aspect('equal')
            df.boundary.plot(ax=ax)
            for name, code, shape in df[['NAME', 'CODE', 'geometry']].itertuples(False):
                ax.annotate(f'{name}\n{code}', shape.centroid.coords[0], horizontalalignment='center')
            plt.close()
            display(fig)    
    w_show.on_click(_show)

    display(widgets.VBox([widgets.HBox([w_year, w_state, w_show]), w_out]))
    
show_county_map()
```

# Census Tract

Code is 11 digits: 2 state, 5 county, 4+2 tract.

[Reference](https://www2.census.gov/geo/pdfs/education/CensusTracts.pdf)

```{code-cell} ipython3
:tags: [nbd-module]

def get_tract_df(years=None, state_codes=None, geometry=True):
    _years = years or [1990, 2000, 2010, 2020]
    _state_codes = state_codes or get_state_df(geometry=False)['CODE'].tolist()
    for y in _years:
        for sc in _state_codes:
            _prep_tract_df(y, sc)
    
    p = pyarrow.dataset.partitioning(flavor='hive',
        schema=pyarrow.schema([('YEAR', pyarrow.int16()), ('STATE_CODE', pyarrow.string())]))
    f = [] if (years or state_codes) else None
    if years:
        f.append(('YEAR', 'in', years))
    if state_codes:
        f.append(('STATE_CODE', 'in', state_codes))
    if geometry:
        return gpd.read_parquet(PATH.tract, partitioning=p, filters=f)
    else:
        c = ['YEAR', 'CODE', 'NAME', 'STATE_CODE', 'COUNTY_CODE', 'TRACT_CODE']
        return pyarrow.parquet.read_table(PATH.tract, columns=c, partitioning=p, filters=f,
                                          use_pandas_metadata=True).to_pandas()

def _prep_tract_df(year, state_code):
    """Download shapefiles for one year and one state, normalize column names and save as parquet partition."""
    path = PATH.tract/f'YEAR={year}/STATE_CODE={state_code}/part.pq'
    if path.exists(): return

    p = get_source(f'tract-boundary-{year}-{state_code}')
    df = gpd.read_file(p)
    if year == 1990:
        if state_code == '34':
            # 2 records have NA tracts, don't know what it means
            df = df[df['TRACTBASE'].notna()] 
        df = df.rename(columns={'ST': 'STATE_CODE', 'CO': 'COUNTY_CODE'})
        df['TRACT_CODE'] = df['TRACTBASE'] + df['TRACTSUF'].fillna('00')
    elif year == 2000:
        df = df.rename(columns={'STATE': 'STATE_CODE', 'COUNTY': 'COUNTY_CODE'})
        df['TRACT_CODE'] = df['TRACT'].str.pad(6, 'right', '0')
    elif year == 2010:
        df = df.rename(columns={'STATE': 'STATE_CODE', 'COUNTY': 'COUNTY_CODE', 'TRACT': 'TRACT_CODE'})
    elif year == 2020:
        df = df.rename(columns={'STATEFP': 'STATE_CODE', 'COUNTYFP': 'COUNTY_CODE', 'TRACTCE': 'TRACT_CODE'})
    df['CODE'] = df['STATE_CODE'] + df['COUNTY_CODE'] + df['TRACT_CODE']
    assert (df['CODE'].str.len() == 11).all(), f'Tract {year} {state_code}: wrong code length.'
    df['NAME'] = df['TRACT_CODE'].astype('int64').astype('str')
    df['NAME'] = df['NAME'].str[:-2] + '.' + df['NAME'].str[-2:]
    df = df[['CODE', 'NAME', 'geometry', 'COUNTY_CODE', 'TRACT_CODE']]
    
    # 1990 and 2000 shapefiles have multiple polygon records per non-contiguous tract
    if year in [1990, 2000]:
        df = df.dissolve('CODE', as_index=False, sort=False)

    assert not df.duplicated('CODE').any()
        
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path)
```

```{code-cell} ipython3
def show_tract_map():
    w_year = widgets.Dropdown(description='Year', value=2020, options=[1990, 2000, 2010, 2020])
    w_state = widgets.Dropdown(description='State', value='55',
                               options=sorted(get_state_df(False)[['NAME', 'CODE']].values.tolist()))
    w_county = widgets.Dropdown(description='County')
    def upd_county_list(*args):
        df = get_county_df(w_year.value, False)
        df = df.loc[df['STATE_CODE'] == w_state.value, ['NAME', 'COUNTY_CODE']]
        w_county.options = sorted(df.values.tolist())
    upd_county_list()
    w_year.observe(upd_county_list, 'value')
    w_state.observe(upd_county_list, 'value')

    w_out = widgets.Output()
    w_show = widgets.Button(description='Show map')
    def _show(_):
        df = get_tract_df([w_year.value], [w_state.value])
        
        df = df[df['COUNTY_CODE'] == w_county.value]
        with w_out:
            w_out.clear_output()
            fig, ax = plt.subplots(figsize=(12, 12))
            ax.set_aspect('equal')
            df.boundary.plot(ax=ax)
            for name, shape in df[['NAME', 'geometry']].itertuples(False):
                ax.annotate(name, shape.centroid.coords[0], horizontalalignment='center')
            plt.close()
            display(fig)
    w_show.on_click(_show)

    display(widgets.VBox([widgets.HBox([w_year, w_state, w_county, w_show]), w_out]))

show_tract_map()
```

## Changes over time

The first four digits of the tract code are "permanent." 
When tracks get large (+8000 residents), tracts are split and 2 digit tag is used (same with the split of splits):

|1990|2000|2010| 
|----|----|----|
|1000|1000.01|1000.03|
|1000|1000.01|1000.04|
|1000|1000.02|1000.05| 
|1000|1000.02|1000.06|
        
The naming convention for merges (population falls below 1,200) and boundary revisions are less clear-cut.

When changes (splits, merges, redefinitions) occur, the relationship of new tracts to old tracts is crosswalked.

There is a master file, as well as two files that provided the identifiers of tracts that were "substantially changed" between decennials. The two files of significantly changed census tracts consist only of a list of census tracts that exhibited a change of 2.5-percent or greater. Tract relationships may be one-to-one, many-to-one, one-to-many, or many-to-many.


Relationship files are currently available for 2010 (relative to 2000) and 2000 (relative to 1990).
- 2010: [data](https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html), [metadata](https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-census-tract-record-layout.html).
- 2000: [data](https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2000.html), [metadata](https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2000-tract-relationship-record-layout.html).

```{code-cell} ipython3
@functools.cache
def get_tract_changes(y1):
    """Return relationship table for tracts in `y1` and `y1-10`."""
    y0 = y1 - 10
    if y1 == 2000:
        meta = pd.read_html('https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2000-tract-relationship-record-layout.html')
        df = pd.read_fwf('https://www2.census.gov/geo/relfiles/tract/us/us2kpop.txt',
                         widths=meta[0]['Field Length'].tolist(),
                         encoding='ISO-8859-1', dtype='str', header=None)
        df.columns = meta[0]['Field Description']

        df['ALAND'] = df['Land area of the record (1000 sq.meters)'].astype(int)
        df['POP_2000'] = df['2000 population of the area covered by the record'].astype(int)    
        for y in [y0, y1]:
            df[f'TRACT_{y}'] = df[f'{y} state FIPS code'] + df[f'{y} county FIPS code'] + df[f'{y} census tract base'] + df[f'{y} census tract suffix']
            df[f'PART_{y}'] = (df[f'{y} census tract part flag'] == 'P')
            df[f'POP_PCT_{y}'] = df[f'Percentage of {y} census tract population*'].astype(int)/10
    elif y1 == 2010:
        meta = pd.read_html('https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-census-tract-record-layout.html')
        df = pd.read_csv('https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/us2010trf.txt',
                         encoding='ISO-8859-1', dtype='str', header=None)
        df.columns = meta[0]['Column Name']

        df['ALAND'] = df['AREALANDPT'].astype(int)
        df['POP_2010'] = df['POP10PT'].astype(int)    
        for y in [y0, y1]:
            yy = str(y)[-2:]
            df[f'TRACT_{y}'] = df[f'GEOID{yy}']
            df[f'PART_{y}'] = (df[f'PART{yy}'] == 'P')
            df[f'POP_PCT_{y}'] = df[f'POPPCT{yy}'].astype(float)

    df.columns.name = None
    df = df[['ALAND', f'POP_{y1}',
             f'TRACT_{y0}', f'PART_{y0}', f'POP_PCT_{y0}',
             f'TRACT_{y1}', f'PART_{y1}', f'POP_PCT_{y1}']]
    return df

def plot_tract_change(y0, t0, y1, t1):
    state_code = t0[0][:2]
    ts0 = get_tract_df([y0], [state_code]).query('CODE.isin(@t0)')
    ts1 = get_tract_df([y1], [state_code]).query('CODE.isin(@t1)')

    fig, ax = plt.subplots(figsize=(8, 8))
    ax.set_aspect('equal')
    ts0.boundary.plot(ax=ax, color='red', linewidth=3, alpha=0.4)
    for name, shape in ts0[['NAME', 'geometry']].itertuples(False):
        ax.annotate(name+'\n', shape.centroid.coords[0], horizontalalignment='center', color='red')
    ts1.boundary.plot(ax=ax, color='blue', linewidth=1)
    for name, shape in ts1[['NAME', 'geometry']].itertuples(False):
        ax.annotate('\n'+name, shape.centroid.coords[0], horizontalalignment='center', color='blue')

    ax.legend([mpl.lines.Line2D([0], [0], linewidth=3, alpha=0.4, color='red'),
                mpl.lines.Line2D([0], [0], linewidth=1, alpha=1, color='blue')],
               [y0, y1])

    state = get_state_df(False).query('CODE == @state_code')['NAME'].values[0]
    county_code = t0[0][:5]
    county = get_county_df(y1, False).query('CODE == @county_code')['NAME'].values[0]
    ax.set_title(f'{state}, {county} county ({county_code})')
    plt.close()
    return fig
```

### Split

All tracts in $y_1$ are a partition of a single tract in $y_0$.

```{code-cell} ipython3
def show_random_tract_split():
    w_y1 = widgets.Dropdown(description='Years', value=2010,
                            options=[('1990-2000', 2000), ('2000-2010', 2010)])
    w_state = widgets.Dropdown(description='State', value='00',
                               options=[['United States', '00']] + sorted(get_state_df(False)[['NAME', 'CODE']].values.tolist()))
    w_out = widgets.Output()
    w_show = widgets.Button(description='Show random split')
    def _prep():
        y1 = w_y1.value
        y0 = y1 - 10
        df = get_tract_changes(y1)
        if w_state.value != '00':
            df = df[df[f'TRACT_{y0}'].str[:2] == w_state.value]
        d = df.groupby(f'TRACT_{y0}')[f'PART_{y1}'].agg(['size', 'sum'])
        d = d.query('size > 1 and sum == 0')
        t0 = d.sample(1).index[0]
        d = df.query(f'TRACT_{y0} == @t0').reset_index(drop=True)
        t1 = d[f'TRACT_{y1}'].values.tolist()
        fig = plot_tract_change(y0, [t0], y1, t1)
        return d, fig
    def _show(_):
        with w_out:
            w_out.clear_output()
            print('Working...')
            d, f = _prep()
            w_out.clear_output()
            display(d)
            display(f)
    w_show.on_click(_show)
    display(widgets.VBox([widgets.HBox([w_y1, w_state, w_show]), w_out]))
    
show_random_tract_split()
```

### Join

All tracts in $y_0$ are a partition of a single tract in $y_1$.

```{code-cell} ipython3
def show_random_tract_join():
    w_y1 = widgets.Dropdown(description='Years', value=2010,
                            options=[('1990-2000', 2000), ('2000-2010', 2010)])
    w_state = widgets.Dropdown(description='State', value='00',
                               options=[['United States', '00']] + sorted(get_state_df(False)[['NAME', 'CODE']].values.tolist()))
    w_out = widgets.Output()
    w_show = widgets.Button(description='Show random join')
    def _prep():
        y1 = w_y1.value
        y0 = y1 - 10
        df = get_tract_changes(y1)
        if w_state.value != '00':
            df = df[df[f'TRACT_{y0}'].str[:2] == w_state.value]
        d = df.groupby(f'TRACT_{y1}')[f'PART_{y0}'].agg(['size', 'sum'])
        d = d.query('size > 1 and sum == 0')
        t1 = d.sample(1).index[0]
        d = df.query(f'TRACT_{y1} == @t1').reset_index(drop=True)
        t0 = d[f'TRACT_{y0}'].values.tolist()
        fig = plot_tract_change(y0, t0, y1, [t1])
        return d, fig
    def _show(_):
        with w_out:
            w_out.clear_output()
            print('Working...')
            d, f = _prep()
            w_out.clear_output()
            display(d)
            display(f)
    w_show.on_click(_show)
    display(widgets.VBox([widgets.HBox([w_y1, w_state, w_show]), w_out]))
    
show_random_tract_join()
```

### Other reshape

Arbitrary change in boundaries between two or more adjacent tracts. May include multiple splits, joins and boundary shifts.

```{code-cell} ipython3
def show_random_tract_reshape():
    w_y1 = widgets.Dropdown(description='Years', value=2010,
                            options=[('1990-2000', 2000), ('2000-2010', 2010)])
    w_state = widgets.Dropdown(description='State', value='00',
                               options=[['United States', '00']] + sorted(get_state_df(False)[['NAME', 'CODE']].values.tolist()))
    w_out = widgets.Output()
    w_show = widgets.Button(description='Show random reshape',
                            layout=widgets.Layout(width='auto'))
    def _prep():
        y1 = w_y1.value
        y0 = y1 - 10
        df = get_tract_changes(y1)
        if w_state.value != '00':
            df = df[df[f'TRACT_{y0}'].str[:2] == w_state.value]
        # Identification of reshaping cluster:
        # Start by picking an area that had it's tract number changed.
        # Than keep adding tracts that were affected by this change. 
        # Alternatively use package that finds connected components in bipartite graph,
        # where nodes on two sides are tracts, and edges are records in the change table.
        d = df.query(f'(TRACT_{y0} != TRACT_{y1}) and PART_{y0} and PART_{y1}')
        t0, t1 = d.sample(1)[[f'TRACT_{y0}', f'TRACT_{y1}']].values[0]
        t0, t1 = [t0], [t1]
        n = 1
        while True:
            d = df.query(f'TRACT_{y0}.isin(@t0) or TRACT_{y1}.isin(@t1)')
            if len(d) == n:
                break
            t0 = d[f'TRACT_{y0}'].unique().tolist()
            t1 = d[f'TRACT_{y1}'].unique().tolist()
            n = len(d)
        fig = plot_tract_change(y0, t0, y1, t1)
        return d.reset_index(drop=True), fig
    def _show(_):
        with w_out:
            w_out.clear_output()
            print('Working...')
            d, f = _prep()
            w_out.clear_output()
            display(d)
            display(f)
    w_show.on_click(_show)
    display(widgets.VBox([widgets.HBox([w_y1, w_state, w_show]), w_out]))
    
show_random_tract_reshape()
```

### Changes between decennial years

Quick look at year to year changes. This section is work in progress.

```{code-cell} ipython3
# tracts = {}
# for state in get_state_df(False)['CODE']:
#     year_url = {
#         2013: f'https://www2.census.gov/geo/tiger/GENZ2013/cb_2013_{state}_tract_500k.zip',
#         2014: f'https://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_{state}_tract_500k.zip',
#         2015: f'https://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_{state}_tract_500k.zip',
#         2016: f'https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_{state}_tract_500k.zip',
#         2017: f'https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_{state}_tract_500k.zip',
#         2018: f'https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_{state}_tract_500k.zip',
#         2019: f'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_{state}_tract_500k.zip'
#     }
#     tracts[state] = {}
#     for y, u in year_url.items():
#         try:
#             tracts[state][y] = gpd.read_file(u)
#         except:
#             print('error', state, y, u)
```

```{code-cell} ipython3
# tracts_us = {}
# for y in year_url.keys():
#     tracts_us[y] = []
#     for state in tracts.keys():
#         try:
#             tracts_us[y].append(tracts[state][y])
#         except:
#             pass
#     tracts_us[y] = pd.concat(tracts_us[y])
```

```{code-cell} ipython3
# for y0 in list(tracts_us.keys())[:-1]:
#     df0 = tracts_us[y0][['GEOID', 'ALAND']]
#     y1 = y0 + 1
#     df1 = tracts_us[y1][['GEOID', 'ALAND']]
#     m = df0.merge(df1, on='GEOID', suffixes=(y0, y1))
#     m['RATIO'] = m[f'ALAND{y1}'] / m[f'ALAND{y0}']
# #     print(y, m['RATIO'].nlargest().tolist())
#     d = m.query('RATIO > 1.15')
#     for r in d[['GEOID', 'RATIO']].itertuples(False):
#         print(y1, list(r))
```

One very visible change in Kansas.

```{code-cell} ipython3
# state = '02'
# tig13 = gpd.read_file(f'https://www2.census.gov/geo/tiger/TIGER2013/TRACT/tl_2013_{state}_tract.zip')
# tig18 = gpd.read_file(f'https://www2.census.gov/geo/tiger/TIGER2018/TRACT/tl_2018_{state}_tract.zip')
# cbf13 = gpd.read_file(f'https://www2.census.gov/geo/tiger/GENZ2013/cb_2013_{state}_tract_500k.zip')
# cbf18 = gpd.read_file(f'https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_{state}_tract_500k.zip')

# tract = '20055960508'
# fig, ax = plt.subplots(figsize=(8, 8))
# # tig13.query('GEOID == @tract').boundary.plot(ax=ax, label='TIGER 2013', color='red')
# # tig18.query('GEOID == @tract').boundary.plot(ax=ax, label='TIGER 2018', color='cyan')
# cbf13.query('GEOID == @tract').boundary.plot(ax=ax, label='CBF 2013', color='red', linewidth=3, alpha=0.4)
# cbf18.query('GEOID == @tract').boundary.plot(ax=ax, label='CBF 2018', color='blue')

# plt.legend()
```

# Postal Zip Code

+++

# Zip Code Tabulation Area (ZCTA)

+++

# Area phone code

+++

# Congressional District

A geographical and political division in which voters elect representatives to the U.S. House of Representatives. Each state establishes its congressional districts based on population counts, with the goal of having districts as equal in population as possible. ([ESRI dictionary](https://support.esri.com/en/other-resources/gis-dictionary/term/ae341e9d-cc60-4f96-93ee-c95c739df5df))

**About Congressional Districts** ([Census](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/congressional-dist.html))
- All congressional districts population are supposed to be equal throughout the state to equally be able to elect the representative
- They don’t cross state lines, but may cross all other classifications such as Census tracts.
- They DO cross county boundaries 
- [Map of CT](https://www2.census.gov/geo/maps/cong_dist/cd115/st_based/CD115_CT.pdf) for reference
- Closer breakdown of [District 1 in CT](https://www2.census.gov/geo/maps/cong_dist/cd115/cd_based/ST09/CD115_CT01.pdf)
- States are required to redraw the district lines every 10 years after the Census is released (except single district states)

In 33 states, state legislatures play the dominant role in congressional redistricting. In eight states, commissions draw congressional district lines. In two states, hybrid systems are used, in which the legislatures share redistricting authority with commissions. The remaining states comprise one congressional district each, rendering redistricting unnecessary (AK, DE, DC, MT, ND, SD, VT, WY). [Link](https://ballotpedia.org/State-by-state_redistricting_procedures)

Gerrymandering can and often does occur with congressional districts lines to help whomever the in power party is to make them stay in power. [Examples](https://www.theguardian.com/us-news/2020/dec/15/gerrymandering-republicans-map-charts-states)

+++

# School Districts

The U.S. has more than 13,000 geographically defined public school districts. These include districts that are administratively and fiscally independent of any other government, as well as public school systems that lack sufficient autonomy to be counted as separate governments and are classified as a dependent agency of some other government—a county, municipal, township, or state. Most public school systems are Unified districts that operate regular, special, and/or vocational programs for children in Prekindergarten through 12th grade.

- School districts are complex and have almost no consistency from state to state because they are formulated by the local town government in most public school cases.
- [Boundary files](https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries)
- Since they vary by local government then changes happen every year in many places throughout the US.

+++

# Native American Reservations

Link to download the data: https://catalog.data.gov/dataset/tiger-line-shapefile-2018-nation-u-s-current-american-indian-alaska-native-native-hawaiian-area

Link to info about American Indian and Alaska Native boundaries: https://www.census.gov/programs-surveys/geography/about/partnerships/aian.html

Link to a definitions of the types of American Indian and Alaska Native geographic areas: https://www.census.gov/programs-surveys/geography/about/glossary/aian-definitions.html

From the above link it is visable that there are many diffrent breakdowns avalible with the data avalable. These breakdowns go as small as tracts and block groups. They are updated each year by the federal government. 

Link to better understand the breakdown: https://www.census.gov/newsroom/blogs/random-samplings/2014/08/understanding-geographic-relationships-american-indian-areas.html

+++

## Build this module

```{code-cell} ipython3
:tags: []

from rurec.reseng.nbd import Nbd
nbd = Nbd('rurec')
nbd.nb2mod('geography.ipynb')
```
