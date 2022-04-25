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

# Maps

> Spatial distribution of economic indicators.

```{code-cell} ipython3
#hide
import json
from pathlib import Path
from IPython.display import Image

import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import matplotlib as mpl
import matplotlib.pyplot as plt

from rurec import rurality, resources
```

# Rurality by state

+++

### Download state shapefiles

```{code-cell} ipython3
#hide
pwd = !pwd
dr = resources.paths.root / 'data/geo'
url = 'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_state_20m.zip'
!mkdir --parents {dr}
%cd {dr}
!wget --no-clobber {url}
file = Path(url).name
!7z x {file}
%cd {pwd[0]}
```

### Create dataframe with shares and shapes

```{code-cell} ipython3
#hide
year = 2017
df = rurality.get_df(years=[year], cols=['YEAR', 'STATE', 'EMPLOYEES', 'RURAL_OUTSIDE_UA', 'UI_CODE', 'RUC_CODE', 'RUCA_CODE', 'RURAL_HRSA', 'FAR_LEVEL'])
rural_col = 'RURAL_HRSA'
x = df.groupby(['STATE', rural_col])['EMPLOYEES'].agg(['size', 'sum']).stack().unstack(rural_col)
x = x[True] / x.sum(1)
x = x.unstack()
x.rename(columns={'size': 'ESTAB', 'sum': 'EMP'}, inplace=True)
x = x.fillna(0)
df_rur = x

df = (gpd.read_file(dr / 'cb_2019_us_state_20m.shp')
       .rename(columns={'STUSPS': 'STATE', 'NAME': 'STATE_NAME'})
       .to_crs({'init': 'epsg:6579'}))
df = df.merge(df_rur, 'right', 'STATE')
df = df[~df['STATE'].isin(['AK', 'PR', 'HI', 'VI'])]
```

```{code-cell} ipython3
#collapse_output
(df[['STATE', 'STATE_NAME', 'ESTAB', 'EMP']]
 .sort_values('EMP', ascending=False)
 .reset_index(drop=True)
 .style.format({'ESTAB': '{:.1%}', 'EMP': '{:.1%}'}))
```

```{code-cell} ipython3
#hide_input
fig, ax = plt.subplots(figsize=(12, 6))
df.plot(ax=ax, column='EMP', legend=True)
ax.axis('off')
fig.suptitle(f'Share of employment in {rural_col} areas in {year}', size=20);
```

> Warning: Below starts the old code that does not use nbdev and draws data from BigQuery. It has not been tested for a long time.

+++

# FAI maps

```{code-cell} ipython3
%cd ..
```

```{code-cell} ipython3
excluded_states = {'AK', 'PR', 'HI', 'VI'}
frac_fai_bins = [-0.01, 0.1, 0.3, 1]
frac_fai_labels = ['0 - 10%', '10 - 30%', '30 - 100%']
frac_fai_colors = ['#FF0808', '#F7FF00', '#29E2FF']
area_style = {'facecolor': 'black', 'alpha': 0.5, 'edgecolor': 'black', 'linestyle': '-', 'linewidth': 0.1}
```

# Download geographic boundaries data
Census [Cartographic Boundary Shapefiles](https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html)

```{code-cell} ipython3
%%bash
OPWD=`pwd`
for ENTITY in csa cbsa; do
    FILE=cb_2017_us_${ENTITY}_500k.zip
    URL=https://www2.census.gov/geo/tiger/GENZ2017/shp/$FILE
    DIR=data/$ENTITY
    mkdir --parents $DIR
    cd $DIR
    wget --no-clobber --quiet $URL
    7z x $FILE > /dev/null
    cd $OPWD
    echo $FILE done.
done
```

# Employment data - InfoGroup

+++

## Query and download raw data

```{code-cell} ipython3
query = '''
SELECT
  state,
  substr(naics, 1, 6) as naics,
  employees,
  latitude,
  longitude
FROM
  `original.data`
where year = 2015;
'''
df = pd.read_gbq(query, dialect='standard', project_id='info-group-162919')
df.to_pickle('data/ig_2015.gz')
```

```{code-cell} ipython3
df = pd.read_pickle('data/ig_2015.gz')
df = df[~df.state.isin(excluded_states)]
df = df.drop('state', axis=1)
df.employees = pd.to_numeric(df.employees)
df = df.dropna(subset=['employees'])
```

## Identify non-farming FAI establishments

```{code-cell} ipython3
with open('data/fai.json') as f:
    fai_codes = json.load(f)
fainf_codes = {c: d for c, d in fai_codes.items() if c[:3] not in {'111', '112', '113', '114'}}
df['fai'] = df.naics.isin(fainf_codes)
```

```{code-cell} ipython3
pt = df.pivot_table('employees', 'fai', aggfunc=[pd.np.sum, pd.np.count_nonzero])
pt
```

```{code-cell} ipython3
pt / pt.sum()
```

## Aggregate establishments into geographic bins for plotting

```{code-cell} ipython3
def expand_interval(df, col):
    for n in ['left', 'mid', 'right']:
        nn = col + '_' + n
        df[nn] = df[col]
        df[nn].cat.categories = df[col].cat.categories.__getattribute__(n)

n_lat = 400
df['blat'] = pd.cut(df.latitude, n_lat)
n_lon = 800
df['blon'] = pd.cut(df.longitude, n_lon)

dfb = df.drop(['latitude', 'longitude'], axis=1)
dfb = dfb.groupby(['blon', 'blat', 'fai'])
dfb = dfb.sum()
dfb = dfb.unstack('fai')

dfb = dfb.dropna(how='all')
dfb = dfb.fillna(0)
dfb.columns = ['emp_nonfai', 'emp_fai']

dfb = dfb.reset_index()
expand_interval(dfb, 'blat')
expand_interval(dfb, 'blon')

dfb['frac_fai'] = dfb.emp_fai / (dfb.emp_fai + dfb.emp_nonfai)
dfb['frac_fai_cat'] = pd.cut(dfb.frac_fai, frac_fai_bins)
dfb.frac_fai_cat.cat.categories = frac_fai_labels

dfb.frac_fai_cat.value_counts()
```

## Demonstration of aggregation

This section demonstrates how multiple establishments are aggregated into a single bin.

In the figure below, 4 adjacent bins are shown. Establishments are drawn as circles, size show employment and color shows FAI or non-FAI.
A "plus" marker is in the middle of each bin, and is colored according to the total fraction of FAI employment in that bin.

```{code-cell} ipython3
show_lat_codes = [312, 313]
show_lon_codes = [447, 448]

_df = df.loc[df.blat.cat.codes.isin(show_lat_codes) & df.blon.cat.codes.isin(show_lon_codes)].copy()
expand_interval(_df, 'blat')
expand_interval(_df, 'blon')
_, ax = plt.subplots(figsize=(16, 8))
c = _df.fai.map({False: frac_fai_colors[0], True: frac_fai_colors[-1]})
ax.scatter(_df.longitude, _df.latitude, s=_df.employees, c=c)

_dfb = dfb[dfb.blon_mid.isin(_df.blon_mid.unique()) & dfb.blat_mid.isin(_df.blat_mid.unique())]
c = _dfb.frac_fai_cat.cat.codes.map({k: v for k, v in enumerate(frac_fai_colors)})
ax.scatter(_dfb.blon_mid, _dfb.blat_mid, s=1000, marker='+', c=c)

for _, row in _dfb.iterrows():
    ax.annotate(row.frac_fai_cat, (row.blon_mid, row.blat_mid))
    r = mpl.patches.Rectangle(row[['blon_left', 'blat_left']], row.blon.length, row.blat.length, zorder=0, fill=False)
    ax.add_patch(r)

plt.plot()
_dfb
```

# Mapping

+++

## Convert aggregated points to geodataframe and reproject

```{code-cell} ipython3
dfb = gpd.GeoDataFrame(dfb)
dfb.geometry = dfb.apply(lambda row: Point(row.blon_mid, row.blat_mid), axis=1)
dfb.crs = {'init': 'epsg:4326'}
dfb = dfb.to_crs({'init': 'epsg:6579'})
dfb['color'] = points.frac_fai_cat.cat.rename_categories(frac_fai_colors)
```

## Plotting function
To be used with CBSA and CSA shapes.

```{code-cell} ipython3
def make_map(areas, points, path_fig, area_label):
    '''Plot map, save to file and return IPython image for display.'''
    fig, ax = plt.subplots()
    ax.set_aspect('equal')
    ax.axis('off')
    ax.set_title('%s boundaries and FAI employment shares' % area_label)

    areas.plot(ax=ax, **area_style)
    points.plot(ax=ax, facecolor=points['color'], edgecolor='none', markersize=0.085, marker='s')

    area_legend = mpl.patches.Patch(**area_style, label=area_label)
    points_legend = []
    points_legend_style = dict(markeredgecolor='none', marker='s', linestyle='')
    for c, l in zip(frac_fai_colors, frac_fai_labels):
        points_legend += [mpl.lines.Line2D([], [], markerfacecolor=c, label=l, **points_legend_style)]
    ax.legend(handles=[area_legend, *points_legend], loc='lower left', frameon=False, fontsize=6, markerscale=0.5)

    plt.savefig(path_fig, dpi=1080)
    plt.clf()
    return Image(path_fig)
```

## CBSA
[Definition](https://www.census.gov/geo/reference/gtc/gtc_cbsa.html):
> **Core Based Statistical Areas (CBSAs)** consist of the county or counties or equivalent entities associated with at least one core (urbanized area or urban cluster) of at least 10,000 population, plus adjacent counties having a high degree of social and economic integration with the core as measured through commuting ties with the counties associated with the core.  The general concept of a CBSA is that of a core area containing a substantial population nucleus, together with adjacent communities having a high degree of economic and social integration with that core.  The term "core based statistical area" became effective in 2003 and refers collectively to metropolitan statistical areas and micropolitan statistical areas.

```{code-cell} ipython3
path_shp = './data/cbsa/cb_2017_us_cbsa_500k.shp'
cbsa = gpd.read_file(path_shp)
cbsa['state'] = cbsa.NAME.str.extract(', (.+)$')
cbsa = cbsa[~cbsa.state.isin(excluded_states)]
cbsa = cbsa.to_crs({'init': 'epsg:6579'})

make_map(cbsa, dfb, 'fig/cbsa_fai.png', 'CBSA')
```

## CSA
[Definition](https://www.census.gov/geo/reference/gtc/gtc_cbsa.html):
> **Combined Statistical Areas (CSAs)** consist of two or more adjacent CBSAs that have substantial employment interchange.  The CBSAs that combine to create a CSA retain separate identities within the larger CSA.  Because CSAs represent groupings of metropolitan and/or micropolitan statistical areas, they should not be ranked or compared with individual metropolitan and micropolitan statistical areas.

```{code-cell} ipython3
path_shp = './data/csa/cb_2017_us_csa_500k.shp'
csa = gpd.read_file(path_shp)
csa['state'] = csa.NAME.str.extract(', (.+)$')
csa = csa[~csa.state.isin(excluded_states)]
csa = csa.to_crs({'init': 'epsg:6579'})

make_map(csa, dfb, 'fig/csa_fai.png', 'CSA')
```
