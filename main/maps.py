import json
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import matplotlib as mpl
import matplotlib.pyplot as plt
import requests
import zipfile
import os.path



excluded_states = {'AK', 'PR', 'HI', 'VI'}
frac_fai_bins = [-0.01, 0.1, 0.3, 1]
frac_fai_labels = ['0 - 10%', '10 - 30%', '30 - 100%']
frac_fai_colors = ['#FF0808', '#F7FF00', '#29E2FF']
area_style = {'facecolor': 'black', 'alpha': 0.5, 'edgecolor': 'black', 'linestyle': '-', 'linewidth': 0.1}


# # Download geographic boundaries data
# Census [Cartographic Boundary Shapefiles](https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html)

# In[86]:


get_ipython().run_cell_magic('bash', '', 'OPWD=`pwd`\nfor ENTITY in csa cbsa; do\n    FILE=cb_2017_us_${ENTITY}_500k.zip\n    URL=https://www2.census.gov/geo/tiger/GENZ2017/shp/$FILE\n    DIR=data/$ENTITY\n    mkdir --parents $DIR\n    cd $DIR\n    wget --no-clobber --quiet $URL\n    7z x $FILE > /dev/null\n    cd $OPWD\n    echo $FILE done.\ndone')


# # Employment data - InfoGroup

# ## Query and download raw data

# In[ ]:


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


# In[8]:


df = pd.read_pickle('data/ig_2015.gz')
df = df[~df.state.isin(excluded_states)]
df = df.drop('state', axis=1)
df.employees = pd.to_numeric(df.employees)
df = df.dropna(subset=['employees'])


# ## Identify non-farming FAI establishments

# In[9]:


with open('data/fai.json') as f:
    fai_codes = json.load(f)
fainf_codes = {c: d for c, d in fai_codes.items() if c[:3] not in {'111', '112', '113', '114'}}
df['fai'] = df.naics.isin(fainf_codes)


# In[6]:


pt = df.pivot_table('employees', 'fai', aggfunc=[pd.np.sum, pd.np.count_nonzero])
pt


# In[7]:


pt / pt.sum()


# ## Aggregate establishments into geographic bins for plotting

# In[11]:


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


# ## Demonstration of aggregation
# 
# This section demonstrates how multiple establishments are aggregated into a single bin.
# 
# In the figure below, 4 adjacent bins are shown. Establishments are drawn as circles, size show employment and color shows FAI or non-FAI.
# A "plus" marker is in the middle of each bin, and is colored according to the total fraction of FAI employment in that bin.

# In[87]:


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


# # Mapping

# ## Convert aggregated points to geodataframe and reproject

# In[19]:


dfb = gpd.GeoDataFrame(dfb)
dfb.geometry = dfb.apply(lambda row: Point(row.blon_mid, row.blat_mid), axis=1)
dfb.crs = {'init': 'epsg:4326'}
dfb = dfb.to_crs({'init': 'epsg:6579'})
dfb['color'] = points.frac_fai_cat.cat.rename_categories(frac_fai_colors)


# ## Plotting function
# To be used with CBSA and CSA shapes.

# In[82]:


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


# ## CBSA
# [Definition](https://www.census.gov/geo/reference/gtc/gtc_cbsa.html):
# > **Core Based Statistical Areas (CBSAs)** consist of the county or counties or equivalent entities associated with at least one core (urbanized area or urban cluster) of at least 10,000 population, plus adjacent counties having a high degree of social and economic integration with the core as measured through commuting ties with the counties associated with the core.  The general concept of a CBSA is that of a core area containing a substantial population nucleus, together with adjacent communities having a high degree of economic and social integration with that core.  The term "core based statistical area" became effective in 2003 and refers collectively to metropolitan statistical areas and micropolitan statistical areas.

# In[85]:


path_shp = './data/cbsa/cb_2017_us_cbsa_500k.shp'
cbsa = gpd.read_file(path_shp)
cbsa['state'] = cbsa.NAME.str.extract(', (.+)$')
cbsa = cbsa[~cbsa.state.isin(excluded_states)]
cbsa = cbsa.to_crs({'init': 'epsg:6579'})

make_map(cbsa, dfb, 'fig/cbsa_fai.png', 'CBSA')


# ## CSA
# [Definition](https://www.census.gov/geo/reference/gtc/gtc_cbsa.html):
# > **Combined Statistical Areas (CSAs)** consist of two or more adjacent CBSAs that have substantial employment interchange.  The CBSAs that combine to create a CSA retain separate identities within the larger CSA.  Because CSAs represent groupings of metropolitan and/or micropolitan statistical areas, they should not be ranked or compared with individual metropolitan and micropolitan statistical areas.

# In[84]:


path_shp = './data/csa/cb_2017_us_csa_500k.shp'
csa = gpd.read_file(path_shp)
csa['state'] = csa.NAME.str.extract(', (.+)$')
csa = csa[~csa.state.isin(excluded_states)]
csa = csa.to_crs({'init': 'epsg:6579'})

make_map(csa, dfb, 'fig/csa_fai.png', 'CSA')

