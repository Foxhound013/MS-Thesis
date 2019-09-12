########################################################
# This bash script is designed to leverage the Iowa Environmental Mesonet Archive of the Multi Radar Multi Sensor (MRMS) system.
# This script will function as follows . . . 
# 1. wget will be used to download the archived grib file.
# 2. wgrib2 will be used to subset the MRMS data file to the Indiana region.
# 3. wgrib2 will be used to convert the MRMS data to a netcdf format.
# 4. Once all files have been downloaded and converted, cdo will be used to merge the netcdf files into
#   a single netcdf with multiple time slices.
#
# NOTE: This script will not work if wgrib2 and cdo are not installed.
# wgrib2 documenation can be found at https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/
# cdo documentation can be found at https://www.unidata.ucar.edu/software/netcdf/workshops/most-recent/third_party/CDO.html
#
########################################################

# Example of wget url 
# -> http://mtarchive.geol.iastate.edu/2018/04/01/mrms/ncep/PrecipRate/PrecipRate_00.00_20180401-000000.grib2.gz	
# -> http://mtarchive.geol.iastate.edu/2018/04/01/mrms/ncep/PrecipRate/PrecipRate_00.00_20180401-000200.grib2.gz

# Following variables never change in the download of the data
BASEURL=http://mtarchive.geol.iastate.edu/
IEM_DIR=/mrms/ncep/PrecipRate/
FSTART=PrecipRate_00.00_

# Output directory on Purdue's Depot
DEPOT=/depot/wwtung/data/LoganD/wxData/mrms/

# Set start and end to be the same if you don't want to change year, month, day, etc.
startYear=2018
endYear=2018
startMonth=04
endMonth=04
startDay=01
endDay=01
# Time will increment by 200 (i.e. 2 minutes)
startTime=000000
#endTime=235800
endTime=000200
# Domain values determined by tinkering and using the Weather Climate Toolkit.
wgribDomainW=4150
wgribDomainE=4560
wgribDomainN=2200
wgribDomainS=1750

# NOTE: The loop system you have in place isn't working right. It literally appends {start..end}
# Much sad, many wow...
for year in {startYear..endYear}
do
    for month in {startMonth..endMonth}
    do
        for day in {startDay..endDay}
        do
            for time in {startTime..endTime..200}
            do
                # Good to wrap everything into functions
                fileDownload=${BASEURL}${year}/${month}/$day/${IEM_DIR}${FSTART}${year}${month}${day}-${time}.grib2.gz
                wget ${fileDownload}
                gunzip ${fileDownload}

                # Now work on it in wgrib2
                file=${FSTART}${year}${month}${day}-${time}
                wgrib2 ${file} -ijsmall_grib 4150 ${wgribDomainW}:${wgribDomainE} ${wgribDomainS}:${wgribDomainN} small.${file}.grib2
                wgrib2 small.${file}.grib2 -netcdf ${file}.nc

            done
        done
    done
done
