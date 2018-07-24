import json
import pandas as pd
import geopandas as gpd
import matplotlib as mpl
import matplotlib.pyplot as plt
import requests
import zipfile
import os.path

excluded_states = {'AK', 'PR', 'HI', 'VI'}
frac_fai_bins = [-0.01, 0.1, 0.3, 1]
frac_fai_labels = ['0 - 10%', '10 - 30%', '30 - 100%']

areas = ['csa', 'cbsa']
for area in areas:
    if not os.path.isfile('data/' + area + '.zip'): 
        f = 'cb_2017_us_' + area + '_500k.zip'
        url = 'https://www2.census.gov/geo/tiger/GENZ2017/shp/' + f
        r = requests.get(url)
        with open('data/' + area + '.zip', 'wb') as f:
            f.write(r.content)
        z = zipfile.ZipFile('data/' + area + '.zip', 'r')
        z.extractall('data/' + area)

#query = '''
#SELECT
#  state,
#  substr(naics, 1, 6) as naics,
#  employees,
#  latitude,
#  longitude
#FROM
#  `original.data`
#where year = 2015;
#'''
#
#df = pd.read_gbq(query, dialect='standard', project_id='info-group-162919')
#df.to_pickle('data/ig_2015.gz')
#
#df = pd.read_pickle('data/ig_2015.gz')
#df = df[~df.state.isin(excluded_states)]
#df = df.drop('state', axis=1)
#df.employees = pd.to_numeric(df.employees)
#df = df.dropna(subset=['employees'])
#
#df = df.sample(100000)
#
#df.to_pickle('tmp/ig_100k.gz')
#
#df = pd.read_pickle('tmp/ig_100k.gz')
#
#with open('data/fai.json') as f:
#    fai_codes = json.load(f)
#fainf_codes = {c: d for c, d in fai_codes.items() if c[:3] not in {'111', '112', '113', '114'}}
#df['fai'] = df.naics.isin(fainf_codes)
#
#pt = df.pivot_table('employees', 'fai', aggfunc=[pd.np.sum, pd.np.count_nonzero])
#pt / pt.sum()
#
#
#def expand_interval(df, col):
#    for n in ['left', 'mid', 'right']:
#        nn = col + '_' + n
#        df[nn] = df[col]
#        df[nn].cat.categories = df[col].cat.categories.__getattribute__(n)
#
#n_lat = 400
#df['blat'] = pd.cut(df.latitude, n_lat)
#n_lon = 800
#df['blon'] = pd.cut(df.longitude, n_lon)
#
#dfb = df.drop(['latitude', 'longitude'], axis=1)
#dfb = dfb.groupby(['blon', 'blat', 'fai'])
#dfb = dfb.sum()
#dfb = dfb.unstack('fai')
#
#dfb = dfb.dropna(how='all')
#dfb = dfb.fillna(0)
#dfb.columns = ['emp_nonfai', 'emp_fai']
#
#dfb = dfb.reset_index()
#expand_interval(dfb, 'blat')
#expand_interval(dfb, 'blon')
#
#dfb['frac_fai'] = dfb.emp_fai / (dfb.emp_fai + dfb.emp_nonfai)
#dfb['frac_fai_cat'] = pd.cut(dfb.frac_fai, frac_fai_bins)
#dfb.frac_fai_cat.cat.categories = frac_fai_labels
#
#dfb.frac_fai_cat.value_counts()
#
## ## Demonstration of aggregation
## 
## This section demonstrates how multiple establishments are aggregated into a single bin.
## 
## In the figure below, 4 adjacent bins are shown. Establishments are drawn as circles, size show employment and color shows FAI or non-FAI.
## A "plus" marker is in the middle of each bin, and is colored according to the total fraction of FAI employment in that bin.
#
#show_lat_codes = [312, 313]
#show_lon_codes = [447, 448]
#
#_df = df.loc[df.blat.cat.codes.isin(show_lat_codes) & df.blon.cat.codes.isin(show_lon_codes)].copy()
#expand_interval(_df, 'blat')
#expand_interval(_df, 'blon')
#_, ax = plt.subplots(figsize=(16, 8))
#ax.scatter(_df.longitude, _df.latitude, s=_df.employees, c=_df.fai)
#
#_dfb = dfb[dfb.blon_mid.isin(_df.blon_mid.unique()) & dfb.blat_mid.isin(_df.blat_mid.unique())]
#ax.scatter(_dfb.blon_mid, _dfb.blat_mid, s=1000, marker='+', c=_dfb.frac_fai_cat.cat.codes)
#
#plt.plot()
#
#for _, row in _dfb.iterrows():
#    ax.annotate(row.frac_fai_cat, (row.blon_mid, row.blat_mid))
#    r = mpl.patches.Rectangle(row[['blon_left', 'blat_left']], row.blon.length, row.blat.length, zorder=0, fill=False)
#    ax.add_patch(r)
#_dfb
#
#
## # CBSA
## [Definition](https://www.census.gov/geo/reference/gtc/gtc_cbsa.html):
## > **Core Based Statistical Areas (CBSAs)** consist of the county or counties or equivalent entities associated with at least one core (urbanized area or urban cluster) of at least 10,000 population, plus adjacent counties having a high degree of social and economic integration with the core as measured through commuting ties with the counties associated with the core.  The general concept of a CBSA is that of a core area containing a substantial population nucleus, together with adjacent communities having a high degree of economic and social integration with that core.  The term "core based statistical area" became effective in 2003 and refers collectively to metropolitan statistical areas and micropolitan statistical areas.
#
#
#path_shp = './data/cbsa/cb_2017_us_cbsa_500k.shp'
#gdf = gpd.read_file(path_shp)
#gdf['state'] = gdf.NAME.str.extract(', (.+)$')[0]
#gdf = gdf[~gdf.state.isin(excluded_states)]
#
#ax = gdf.plot(figsize=(32, 16))
#ax.scatter(dfb.blon_mid, dfb.blat_mid, c=dfb.frac_fai_cat.cat.codes)
#
#
## # CSA
## [Definition](https://www.census.gov/geo/reference/gtc/gtc_cbsa.html):
## > **Combined Statistical Areas (CSAs)** consist of two or more adjacent CBSAs that have substantial employment interchange.  The CBSAs that combine to create a CSA retain separate identities within the larger CSA.  Because CSAs represent groupings of metropolitan and/or micropolitan statistical areas, they should not be ranked or compared with individual metropolitan and micropolitan statistical areas.
#
## In[219]:
#
#path_shp = './data/csa/cb_2017_us_csa_500k.shp'
#gdf = gpd.read_file(path_shp)
#gdf['state'] = gdf.NAME.str.extract(', (.+)$')[0]
#gdf = gdf[~gdf.state.isin(excluded_states)]
#
#ax = gdf.plot(figsize=(32, 16))
#ax.scatter(dfb.blon_mid, dfb.blat_mid, c=dfb.frac_fai_cat.cat.codes)
