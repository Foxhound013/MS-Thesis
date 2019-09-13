#!/bin/bash
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

# Required modules
module load netcdf

# Example of wget url 
# -> http://mtarchive.geol.iastate.edu/2018/04/01/mrms/ncep/PrecipRate/PrecipRate_00.00_20180401-000000.grib2.gz	
# -> http://mtarchive.geol.iastate.edu/2018/04/01/mrms/ncep/PrecipRate/PrecipRate_00.00_20180401-000200.grib2.gz

# Following variables never change in the download of the data
BASEURL=http://mtarchive.geol.iastate.edu/
IEM_DIR=/mrms/ncep/PrecipRate/
FSTART=PrecipRate_00.00_

# Output directory on Purdue's Depot
DEPOT=/depot/wwtung/data/LoganD/wxData/mrms/

# Output file for missing data
missingLog=missing_mrms.txt

# Set start and end to be the same if you don't want to change year, month, day, etc.
startYear=2018
endYear=2018
startMonth=04
endMonth=04
startDay=01
endDay=01
# Time will increment by 200 (i.e. 2 minutes)
# Note: MRMS is in a 6 digit minute format. printf will handle that formatting later
# but ensure that your entering the minute value below. For example, 0 should be 0 for startTime despite it
# being listed as 000000 in the MRMS file name. Hour 23, minute 58, will be entered as 235800.
# The way you enter it in this variable affects the way the loop works for time.
startTime=0
#endTime=235800
endTime=1000
timeStep=200
# Domain values determined by tinkering and using the Weather Climate Toolkit.
wgribDomainW=4150
wgribDomainE=4560
wgribDomainN=2200
wgribDomainS=1750
# fileCount is a variable to count variables downloaded. After a certain range, merge all netcdf files.
# This is necessary because cdo opens each of the netcdf files for combining them. There is a limit to
# the number of files that can be opened.
fileCount=0 
savedFile=out.nc

# Remove log file in order to ensure a clean missing data log on each run of this script.
rm ${missingLog}


# NOTE: The loop system you have in place isn't working right. It literally appends {start..end}
# Much sad, many wow...
for ((year=${startYear};year<=${endYear};year++))
do
    for ((month=${startMonth};month<=${endMonth};month++)) # {${startMonth}..${endMonth}}
    do
        for ((day=${startDay};day<=${endDay};day++)) # {${startDay}..${endDay}}
        do
            for ((time=${startTime};time<=${endTime};time+=${timeStep})) # {${startTime}..${endTime}..${timeStep}}
            do
                # Format the date data
                monthf=$(printf "%02d" ${month})
                dayf=$(printf "%02d" ${day})
                timef=$(printf "%06d" ${time})
                
                file=${FSTART}${year}${monthf}${dayf}-${timef}
                fileDownload=${BASEURL}${year}/${monthf}/${dayf}${IEM_DIR}${file}.grib2.gz
                echo ${file}

                # wget else if that fails echo this and save the datetime that is missing to missing.txt
                wget ${fileDownload} --directory-prefix /tmp/ || echo "${year}/${monthf}/${dayf} at ${timef}" >> ${missingLog}            
                gunzip -f /tmp/${file}.grib2.gz

                # Now work on it in wgrib2
                wgrib2 /tmp/${file}.grib2 -ijsmall_grib ${wgribDomainW}:${wgribDomainE} ${wgribDomainS}:${wgribDomainN} /tmp/small.${file}.grib2
                wgrib2 /tmp/small.${file}.grib2 -netcdf ${DEPOT}${file}.nc

            done
            # only merge all nc files every day rather than each hour. Should be faster.
            # This may be problematic when ran for a long period of time. You may have to merge in batches.
            cdo mergetime ${DEPOT}*.nc ${DEPOT}${savedFile}
        done
    done
done
