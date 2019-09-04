from datetime import datetime, timedelta
import pygrib
import pandas as pd
import xarray as xr

import sys
sys.path.append('/home/wdownin/pyBKB_v3')
from BB_HRRR.HRRR_Pando import *


import sys
sys.path.append('/home/wdownin/pyBKB_v3')
from BB_HRRR.HRRR_Pando import *

# Note: You're getting grid relative u and v wind components, this will need to be converted later more than likely.
variables = ['VIS:surface', 'GUST:surface']
#variables = ['VIS:surface', 'GUST:surface', 'TMP:surface', 'CNWAT:surface', 'WEASD:surface', 'SNOWC:surface',
#       'SNOD:surface', 'TMP:2 m', 'POT:2 m', 'SPFH:2 m', 'DPT:2 m', 'RH:2 m', 'UGRD:10 m', 'VGRD:10 m',
#       'WIND:10 m', 'MAXUW:10 m', 'MAXVW:10 m', 'CPOFP:surface', 'PRATE:surface', 'APCP:surface',
#       'WEASD:surface', 'FROZR:surface', 'FRZR:surface', 'SSRUN:surface', 'CSNOW:surface', 'CICEP:surface'
#       'CRAIN:surface', 'SFCR:surface', 'FRICV:surface', 'GFLUX:surface', 'CAPE:surface', 'CIN:surface',
#       'DSWRF:surface']

dates = pd.date_range(start=datetime(2019,6,1,0,0), end=datetime(2019,6,1,23,0), freq='H')

# !!!!! Feature to add - It may be worthwile to pull some of the attributes during get_variable step and insert them into the data arrays. 

datasets = []
timeSliceArrays = []

# get variables for each date
for date in dates:
       print(date)
       ds = xr.Dataset() # Keeps the datasets small to avoid growing datasets too large in a loop.
       for var in variables:
              data = get_hrrr_variable(date, var, fxx=0, model='hrrr',
                                   field='sfc', removeFile=True,
                                   value_only=False, verbose=True,
                                   outDIR='/tmp/'); #/tmp/ is a cluster directory
              
              # lat/lon values have been chosen to center on Indiana
              data = hrrr_subset(data, half_box=85, lat=39.7684, 
                            lon=-86.1581, verbose=True)

              ds[var] = xr.DataArray(data['value'], dims=['y','x'],
                                                 coords = {'lon': (('y','x'), data['lon']),
                                                        'lat': (('y','x'), data['lat']),
                                                        'time': ((), date)},
                                                 name = var)
       datasets.append(ds)

# Combine all datasets into a single dataset
# !!!!! You may need to do this in chunks as you write to a file if the data is beg enough.
# This can be done with dask
ds = xr.concat(datasets,dim='time')  

ds.to_netcdf(path='/depot/wwtung/data/LoganD/wxData/hrrr_lowerLevs.nc', mode='w')
