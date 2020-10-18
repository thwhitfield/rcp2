"""Utilities for working with interim data.

These functions read in interim data in the correct format. 

"""

import pandas as pd

from src import utils

def read_cleaned_nfirs(year):
    """Read in cleaned and de-duped nfirs data (but not geocoded)
    
    Args:
        year: int, year of nfirs data to read
        
    Returns:
        df: pandas dataframe containing nfirs data for that year
    """
    
    dtypes = {
        'state':'str',
        'fdid':'str',
        'st_fdid':'str',
        'dept_sta':'str',
        'inc_no':'str',
        'exp_no':'str',
        'inc_type':'str',
        'zip5':'str'
    }
    
    path = utils.DATA['interim'] / 'nfirs' / f'nfirs_cleaned_{year}.csv'
    
    df = pd.read_csv(path,
                    dtype = dtypes,
                    parse_dates = ['inc_date'])
    
    return(df)