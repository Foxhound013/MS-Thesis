# MS-Thesis
Work related to my masters thesis at Purdue University. Research is centered on traffic and weather interactions.

## Useful Resources
- Bryan Blaylock's HRRR Archive Repository: A wealth of useful scripts and examples for getting data out of the HRRR archive efficiently. https://github.com/blaylockbk/pyBKB_v3

- Wgrib2 Official Docs: This is the tool for interacting directly with the grib data. The following links are all connected.
  - All Information: https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/
  - All Commands: https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/long_cmd_list.html
  - Common Commands: https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/short_cmd_list.html

## HRRR Projection Info
Using wgrib2 on a grib2 file from the HRRR archive reveals the following.
- Grid Template: 30
- Mode: 8 
- Projection: Lambert Conformal
- Lat1 (Latitude of 1st Grid Point): 37.376803
- Lon1 (Longitude of 1st Grid Point): 270.992022
- LoV (Longitude of meridian parallel to y-axis along which latitude increases as the y-coordinate increases): 262.500000
- LatD (Latitude of meridian parallel to y-axis along which latitude increases as the y-coordinate increases): 38.500000
- Latin1 (1st Latitude from the Pole at which the secant cone cuts the sphere): 38.500000
- Latin2 (2nd Latitude from the Pole at which the secant cone cuts the sphere): 38.500000
- LatSP (Latitude of the Southern Pole of Projection): 0.00000
- LonSP (Latitude of the Southern Pole of Projection): 0.00000
- Dx (X Direction Grid Length in meters): 3000.00000
- Dy (Y Direction Grid Length in meters): 3000.00000
- Grid Template Tables/Info: Information sourced from NOAA pertaining to the projection info of HRRR data. https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml
  - The table that describes most of the wgrib2 output. https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp3-30.shtml
