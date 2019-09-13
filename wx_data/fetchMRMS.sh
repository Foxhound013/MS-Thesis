#!/bin/bash
########################################################
# NOTE: Unfortunately this script, in the environment I'm running on, is bound to doing only 1 month at a time.
# This is because the cdo mergetime command used to merge all netcdf files requires opening all of the netcdf files
# at the same time. My estimates put the data download at roughly 75 Gb per month.
# The best fix to this would be migrating this script over to Python using Dask to long write to a netcdf file.
# A feature that I unfortunately don't have the time to prototype and polish at the moment.
#
# USER NOTE: When you run this, make sure you change 
#
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
endYear=startYear
startMonth=04
endMonth=startMonth
startDay=01
endDay=31
# Note: the hour and minutes are established in the loops below. If you're using the MRMS data, I find it fairly
# unlikely that you want 4 slices of it in the middle of an hour. Toy with those parameters in the loop constructs
# below if you really need to or for debugging purposes.

# Domain values determined by tinkering and using the Weather Climate Toolkit.
wgribDomainW=4150
wgribDomainE=4560
wgribDomainN=2200
wgribDomainS=1750

# count and saved file work together to always keep the output file unique
# this is to ensure cdo can merge the files.
count=0

# Remove log file in order to ensure a clean missing data log on each run of this script.
rm ${missingLog}

for ((year=${startYear};year<=${endYear};year++))
do
    for ((month=${startMonth};month<=${endMonth};month++)) # {${startMonth}..${endMonth}}
    do
        for ((day=${startDay};day<=${endDay};day++)) # {${startDay}..${endDay}}
        do
            for hour in {00..23} # in case you change this, the default range is 00 to 23
            do
                for minute in {00..58..02} # in case you change this, the default range is 00 to 58
                do
                    # Format the date data
                    monthf=$(printf "%02d" ${month})
                    dayf=$(printf "%02d" ${day})
                    
                    file=${FSTART}${year}${monthf}${dayf}-${hour}${minute}00
                    fileDownload=${BASEURL}${year}/${monthf}/${dayf}${IEM_DIR}${file}.grib2.gz

                    # wget else if that fails echo this and save the datetime that is missing to missing.txt
                    wget ${fileDownload} --directory-prefix /tmp/ || echo "${year}/${monthf}/${dayf} at ${timef}" >> ${missingLog}            
                    gunzip -f /tmp/${file}.grib2.gz

                    # Now work on it in wgrib2
                    wgrib2 /tmp/${file}.grib2 -ijsmall_grib ${wgribDomainW}:${wgribDomainE} ${wgribDomainS}:${wgribDomainN} /tmp/small.${file}.grib2
                    wgrib2 /tmp/small.${file}.grib2 -netcdf ${DEPOT}${month}_${file}.nc

                done
            # merge netcdf files every hour. 
            cdo mergetime ${DEPOT}${month}*.nc ${DEPOT}${month}_out${count}.nc

            # should empty the directory of all files except for the out.nc
            find ${DEPOT} -type f -name "${month}_PrecipRate*" -delete 

            # delete the out file that is no longer relevant (i.e. the previous one)
            if [ ${count} -gt 0 ];
            then
                find ${DEPOT} -type f -name "${month}_out$((${count}-1)).nc" -delete
            fi

            count=$((${count}+1))
            done
        done
    done
done
