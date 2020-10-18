"""
Functions and script to geocode nfirs data.
"""


import pandas as pd
import numpy as np
import os
from src import utils
from src.data.interim import read_cleaned_nfirs

import pathlib
from datetime import datetime, timedelta
from time import sleep

import censusgeocode as cg

def create_nfirs_temp(df, year, nrows = 1000):
    """Create the temp directory and the input files to be passed to the 
    geocoder. If the temp directory already exists, it will raise an exception, telling you
    to delete the temp folder. NOTE: The temp folder isn't deleted automatically to reduce
    risk of user error. It must be manually deleted.
    
    Args:
        df: pandas dataframe containing nfirs data to be geocoded
        nrows: int number of rows per input file to use (must be <= 1000)
        
    Returns:
        None
    """
    
    assert nrows <= 1000, "Max number of rows is 1000 (larger values tend to result in timeout errors with Census API)"
    
    # Directory paths
    nfirs_interim = utils.DATA['interim'] / 'nfirs'
    temp_input = nfirs_interim / f'temp_{year}' / 'input'
    temp_output = nfirs_interim / f'temp_{year}' / 'output'
    
    if os.path.exists(temp_input):
        raise Exception(f'Error, temp_{year} folder already exists. Delete folder before running this function.')
    else:
        os.makedirs(temp_input)
        os.makedirs(temp_output)
        
    # Subset nfirs columns to use
    usecols = ['address','city','state_id','zip5']
    df = df[usecols]
    df = df.reset_index()

    # split into blocks of length defined by nrows argument
    nrow = len(df.index)
    file_num = 1
    cur_row = 0

    while cur_row <= nrow -1:
        temp_df = df.iloc[cur_row:cur_row + nrows - 1, :].copy()
        filename = os.path.join(temp_input, f'nfirs_{year}_part_{file_num:04}.csv')
        temp_df.to_csv(filename, index=False, header=False)
        file_num += 1
        cur_row += nrows
    max_file = file_num - 1
    print('files 1 to {0} created'.format(max_file))
    
    return

def geocode_nfirs(year):
    """Geocode files within temp directory (created with create_nfirs_temp function).
    Writes output geocoded files to temp_output folder.
    Uses censusgeocode library to access the census geocoder api, described at
    https://geocoding.geo.census.gov/ 

    Args:
        year: int, year to consolidate
        
    Returns:
        None
    """
    
    # Directory paths
    nfirs_interim = utils.DATA['interim'] / 'nfirs'
    temp_input = nfirs_interim / f'temp_{year}' / 'input'
    temp_output = nfirs_interim / f'temp_{year}' / 'output'

    # Start and current time
    start_time = datetime.now()
    cur_time = datetime.now()

    # Geocode the files
    for filename in os.listdir(temp_input):
        
        input_path = os.path.join(temp_input, filename)
        output_path = os.path.join(temp_output, f'{filename[:-4]}_output.csv')
                
        if os.path.exists(output_path):
            print(f'{filename} already geocoded.')
            continue

        #Try up to 10 attempts to geocode file. Sometimes the connection to Census API will
        #time out and cause the particular file to fail to be geocoded on that attempt.
        for attempt in range(10):
            try:
                # Sleep random amount to keep from being banned from geocoder api
                sleep(np.random.randint(1,5))

                # Print time info
                cur_time = datetime.now()
                print('\nCurrent time:',cur_time.strftime('%H:%M:%S'))
                print(f'Geocoding: {filename}')

                # Geocode file
                results = pd.DataFrame(cg.addressbatch(input_path)).sort_values('id')
                results.to_csv(output_path, index = False)

                # Print time info
                step_time = (datetime.now() - cur_time).total_seconds() / 60
                total_time = (datetime.now() - start_time).total_seconds() / 60
                print(f'Step time elapsed: {step_time:.02f} minutes')
                print(f'Total time elapsed: {total_time:.02f} minutes')

                break
            except:
                print(f'Failed on try {attempt}')
                continue
    
    print('\n\nFinished geocoding.\n')
    return

def consolidate_geocodes(year):
    """Consolidate geocoded records.
    
    Args:
        year: int, year to consolidate
        
    Returns:
        geocodes: pandas dataframe, contains geocoded nfirs addresses
    """
    
    # Directory paths
    nfirs_interim = utils.DATA['interim'] / 'nfirs'
    temp_input = nfirs_interim / f'temp_{year}' / 'input'
    temp_output = nfirs_interim / f'temp_{year}' / 'output'

    # Data types to read in
    dtypes = {'id':'str','address':'str','match':'bool','matchtype': 'str','parsed': 'str',
             'tigerlineid': 'str','side': 'str','statefp': 'str','countyfp': 'str','tract': 'str','block': 'str',
             'lat': 'float64','lon': 'float64'}
    
    geocodes = pd.DataFrame()

    # Read in all individual geocoded files and join together
    for filename in os.listdir(temp_output):
        input_path = os.path.join(temp_output,filename)
        df_temp = pd.read_csv(input_path, dtype = dtypes)
        geocodes = geocodes.append(df_temp)
        
    return(geocodes)

def geocode_all_uncoded_nfirs():
    """Geocode all nfirs data which hasn't already been geocoded, and write those
    files to the interim folder.
    
    Args:
        None
        
    Returns:
        None
    """
    
    nfirs_interim = utils.DATA['interim'] / 'nfirs'
    
    cleaned_years = [filename[-8:-4] for filename in os.listdir(nfirs_interim) if filename[:-9] == 'nfirs_cleaned']
    geocoded_years = [filename[-8:-4] for filename in os.listdir(nfirs_interim) if filename[:-9] == 'nfirs_geocoded_addresses']
    
    for year in cleaned_years:
        filename = f'nfirs_geocoded_addresses_{year}.csv'
        filepath = nfirs_interim / filename
        if os.path.exists(filepath):
            print(f'{filename} already geocoded.')
            continue
        else:
            df = read_cleaned_nfirs(year)
            create_nfirs_temp(df, year, nrows = 1000)
            geocode_nfirs(year)
            geocodes = consolidate_geocodes(year)
            geocodes.to_csv(filepath, index=False)
            
    return

if __name__ == "__main__":
    geocode_all_uncoded_nfirs()