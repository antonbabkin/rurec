#!/usr/bin/env python
# coding: utf-8

import pandas as pd

from .reseng.util import download_file
from .reseng.nbd import Nbd

nbd = Nbd('rurec')
PATH = {
    'source': nbd.root/'data/naics/source/',
    'naics_codes': nbd.root/'data/bea_io/naics_codes.csv'
}


def download_source_files():
    base = 'https://www.census.gov/naics/'
    
    url = f'{base}2012NAICS/2012_NAICS_Index_File.xls'
    download_file(url, PATH['source'])

    url = f'{base}2012NAICS/2-digit_2012_Codes.xls'
    download_file(url, PATH['source'])


def get_codes_df():
    df = pd.read_excel(PATH['source']/'2-digit_2012_Codes.xls', skiprows=2, header=None, usecols=[1, 2], dtype=str)
    df.columns = ['code', 'desc']

    df['digits'] = df['code'].str.len()
    df.loc[df['code'] == '31-33', 'digits'] = 2
    df.loc[df['code'] == '44-45', 'digits'] = 2
    df.loc[df['code'] == '48-49', 'digits'] = 2

    df.loc[df['digits'] == 2, 'code_2'] = df['code']
    df['code_2'] = df['code_2'].fillna(method='ffill')
    df.loc[df['digits'] == 3, 'code_3'] = df['code']
    df['code_3'] = df.groupby('code_2')['code_3'].fillna(method='ffill')
    df.loc[df['digits'] == 4, 'code_4'] = df['code']
    df['code_4'] = df.groupby('code_3')['code_4'].fillna(method='ffill')
    df.loc[df['digits'] == 5, 'code_5'] = df['code']
    df['code_5'] = df.groupby('code_4')['code_5'].fillna(method='ffill')
    df.loc[df['digits'] == 6, 'code_6'] = df['code']
    
    return df

