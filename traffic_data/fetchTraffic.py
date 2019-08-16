import pandas as pd
import pymssql
import sys
from secrets import *


def getDB(server, database, user, password):
    conn = pymssql.connect(server=server,
                        database=database,
                        user=user,
                        password=password,
                        login_timeout=300,
                        port=1433)
    cursor = conn.cursor()
    return cursor

def getOutDirectory(versionDate):
    if versionDate == '2017-04-02':
        return('m1_2017-04-02')
    elif versionDate == '2017-10-24':
        return('m2_2017-10-24')
    elif versionDate == '2018-04-25':
        return('m3_2018-04-25')
    elif versionDate == '2018-12-03':
        return('m4_2018-12-03')

def get_sql(versionDate):
    # Sort out the location to place the resulting file
    fpath = '/depot/wwtung/data/LoganD/trafficData/'
    mapVersion = getOutDirectory(versionDate)
    fname = '/trafficSpeeds.csv'
    
    cursor = getDB(server, db, uid, pwd)
    query = """
        DECLARE @versionDate date
        SET @versionDate = '%s'

        SELECT
            xdInfo.version,
            arterials.xdid,
            xdSpeed.tstamp,
            xdSpeed.speed,
            arterials.roadname,
            xdInfo.RoadName as roadname2,
            xdInfo.RoadNumber,
            arterials.direction,
            xdInfo.Bearing,
            xdInfo.County,
            xdInfo.District,
            xdInfo.StartLat,
            xdInfo.StartLong,
            xdInfo.Miles
        FROM udf_xdpos_details(@versionDate) arterials

        INNER JOIN inrix_xd.dbo.__xd xdInfo 
            ON xdInfo.XDSegID = arterials.xdid

        INNER JOIN inrix_xd.dbo.xdspeeds xdSpeed
            ON xdSpeed.xdid = arterials.xdid

        WHERE xdInfo.version = @versionDate and
            xdSpeed.tstamp > '%s' and xdSpeed.tstamp <= '2019-04-16T00:00:00'
            and xdSpeed.score = 30
    """ % (versionDate, versionDate)
    cursor.execute(query)
    
    # Get the data in batches
    count = 0 # 0 = create file and write first batch, 1 = append to existing file.
    while True:
        # Read in data
        column_names = [item[0] for item in cursor.description]
        df = pd.DataFrame(cursor.fetchmany(250000), columns=column_names)
        
        # Check to see if there's any data, end if not
        if len(df) == 0:
            break
        else:
            if count == 0:                
                df.to_csv(fpath+mapVersion+fname, header=True, index=False, mode='w')
                count += 1
            else:
                df.to_csv(fpath+mapVersion+fname, header=False, index=False, mode='a')


try:
    versionDate = sys.argv[1]
    get_sql(versionDate) # Begin data download
except:
    print("No date argument provided...")
    print("The script should be executed as follows 'python fetchTraffic.py YYYY-MM-DD")
    exit()