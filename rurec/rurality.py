#!/usr/bin/env python
# coding: utf-8

import functools
import typing
import warnings

import pandas as pd
import geopandas

from .pubdata import geography, ers_rurality
from .reseng.util import download_file
from .reseng.nbd import Nbd

nbd = Nbd('rurec')
PATH = {
    'root': nbd.root,
    'data': nbd.root / 'data/',
    'source': nbd.root / 'data/source/',
    'ers_far': nbd.root / 'data/ers_far/',
}


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
    return download_file(url, local.parent, local.name)

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

