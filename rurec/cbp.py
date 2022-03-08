#!/usr/bin/env python
# coding: utf-8

import pandas as pd

from .reseng.util import download_file
from .reseng.nbd import Nbd


nbd = Nbd('rurec')
PATH = {
    'root': nbd.root,
    'source': nbd.root/'data/cbp/source/',
    'county': nbd.root/'data/cbp/county.pq/'
}


def get_source_file(year):
    path = PATH['source']/f'co/{year}.zip'
    if path.exists(): return path
    yr = str(year)[2:]
    url = f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}co.zip'
    download_file(url, path.parent, path.name)
    return path


employee_size_classes = ['1_4', '5_9', '10_19', '20_49', '50_99', '100_249', '250_499', '500_999', '1000', '1000_1', '1000_2', '1000_3', '1000_4']

def get_county_df(year):
    path = PATH['county']/f'{year}.pq'
    if path.exists():
        return pd.read_parquet(path, 'pyarrow')

    dt = {
        'fipstate': str,
        'fipscty': str,
        'naics': str,
        'emp': float,
        'qp1': float,
        'ap': float,
        'est': float
    }
    suf = (['1_4'] if year < 2017 else ['<5']) + employee_size_classes[1:]
    dt.update({f'n{s}': float for s in suf})
    if year < 1998:
        del dt['naics']
        dt['sic'] = str
    if year == 2015:
        dt = {k.upper(): v for k, v in dt.items()}

    df = pd.read_csv(get_source_file(year), usecols=dt.keys(), dtype=dt, na_values='N')

    if year == 2015:
        df.columns = df.columns.str.lower()
    if year >= 2017:
        df = df.rename(columns={'n<5': 'n1_4'})
    
    df['ind_digits'] = -1
    if year < 1998:
        s = df['sic']
        assert s.notna().all() and (s != '').all()
        df.loc[s == '----', 'ind_digits'] = 0
        df.loc[s.str[:2].str.isdigit() & (s.str[2:] == '--'), 'ind_digits'] = 2
        df.loc[s.str[:3].str.isdigit() & (s.str[3] == '/'), 'ind_digits'] = 3
        df.loc[s.str.isdigit(), 'ind_digits'] = 4
    else:
        s = df['naics']
        assert s.notna().all() and (s != '').all()
        df.loc[s == '------', 'ind_digits'] = 0
        df.loc[s.str[:2].str.isdigit() & (s.str[2:] == '----'), 'ind_digits'] = 2
        df.loc[s.str[:3].str.isdigit() & (s.str[3:] == '///'), 'ind_digits'] = 3
        df.loc[s.str[:4].str.isdigit() & (s.str[4:] == '//'), 'ind_digits'] = 4
        df.loc[s.str[:5].str.isdigit() & (s.str[5] == '/'), 'ind_digits'] = 5
        df.loc[s.str.isdigit(), 'ind_digits'] = 6

    df['ind_digits'] = df['ind_digits'].astype('int8')
    assert (df['ind_digits'] != -1).all()
    
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(path, 'pyarrow', index=False)
    return df

