#!/usr/bin/env python
# coding: utf-8

import pandas as pd

from .reseng.util import download_file
from .reseng.nbd import Nbd
nbd = Nbd('rurec')


def get_df(year):
    yr = str(year)[2:]
    f = download_file(f'https://www2.census.gov/programs-surveys/cbp/datasets/{year}/cbp{yr}co.zip', nbd.root / 'data')
    df = pd.read_csv(f)
    return df

