#!/usr/bin/env python
# coding: utf-8

import shutil
from typing import Literal

import numpy as np
import pandas as pd

from .pubdata import agcensus
from .reseng.nbd import Nbd
nbd = Nbd('rurec')


def get_farm_sales_by_bea_detail(year, geo_level):
    """Return dataframe of farm sales grouped by BEA detail industries.
    geo_level: "national", "state" or "county".
    year: 2002, 2007, 2012, 2017.
    """

    # load quickstats table
    df = agcensus.get_df([year], ['STATE_FIPS_CODE', 'COUNTY_CODE', 'SHORT_DESC', 'VALUE'],
                         [('DOMAIN_DESC', '==', 'TOTAL'),
                          ('AGG_LEVEL_DESC', '==', geo_level.upper()),
                          ('STATISTICCAT_DESC', '==', 'SALES'),
                          ('UNIT_DESC', '==', '$')])
    df['STCTY'] = df['STATE_FIPS_CODE'] + df['COUNTY_CODE'].fillna('000')

    # dictionary to rename and select needed rows
    select_sales_items = {
        'CROP TOTALS': 'CROP',
        'GRAIN': 'GRAIN',
        'SOYBEANS': 'SOYBEAN',
        'VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN': 'VEGETABLE',
        'FRUIT & TREE NUT TOTALS': 'FRUIT & NUT',
        'HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS)': 'HORTICULTURE',
        'CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS': 'TREE',
        'ANIMAL TOTALS, INCL PRODUCTS': 'ANIMAL',
        'POULTRY TOTALS, INCL EGGS': 'POULTRY',
        'CATTLE, INCL CALVES': 'CATTLE',
        # MILK: 2012, 2017
        'MILK': 'MILK',
        # MILK: 2002, 2007
        'MILK, INCL OTHER DAIRY PRODUCTS': 'MILK'
    }
    select_sales_items = {k + ' - SALES, MEASURED IN $': v for k, v in select_sales_items.items()}
    
    # irrelevant rows will be NA and drop
    df['SELECT_SALES'] = df['SHORT_DESC'].map(select_sales_items)
    df = df.dropna(subset=['VALUE', 'SELECT_SALES'])

    # keep only sales values and pivot to have STCTY in rows and commodities in columns
    df = df.set_index(['STCTY', 'SELECT_SALES'])['VALUE'].unstack('SELECT_SALES')

    # applies to 2002
    if 'SOYBEAN' not in df:
        soy_2007 = 20_283_986
        grain_2007 = 77_215_262
        soy_share_of_grain = soy_2007 / grain_2007
        df['SOYBEAN'] = df['GRAIN'] * soy_share_of_grain

    # BEA detail industries
    df['1111A0'] = df['SOYBEAN']
    df['1111B0'] = df['GRAIN'] - df['SOYBEAN'].fillna(0)
    df['111200'] = df['VEGETABLE']
    df['111300'] = df['FRUIT & NUT']
    df['111400'] = df['HORTICULTURE'].fillna(0) + df['TREE'].fillna(0)
    df['111900'] = df['CROP'] - df[['GRAIN', 'VEGETABLE', 'FRUIT & NUT', 'HORTICULTURE', 'TREE']].fillna(0).sum(1)
    df['1121A0'] = df['CATTLE']
    df['112120'] = df['MILK']
    df['112300'] = df['POULTRY']
    df['112A00'] = df['ANIMAL'] - df[['CATTLE', 'MILK', 'POULTRY']].fillna(0).sum(1)

    df = df[['1111A0', '1111B0', '111200', '111300', '111400', '111900', '1121A0', '112120', '112300', '112A00']]
    df.columns.name = 'BEA_INDUSTRY_DETAIL'
    df = df.fillna(0)
    
    # some cells can come negative due to subtraction of rounded values
    df[(df >= -1000) & (df <= 0)] = 0
    assert (df >= 0).all().all()
    
    return df

