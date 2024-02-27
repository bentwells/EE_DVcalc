## Retrieve AQS data not available in API for Exceptional Event Design Value Tool
source("R/get_monitors.r")
curr.year <- as.numeric(substr(as.character(Sys.Date()),1,4)) - 
  ifelse(as.numeric(substr(as.character(Sys.Date()),6,7)) > 1,1,2)
o3.methods <- get.methods(par=44201)
o3.monitors <- get.monitors(par=44201,yr1=curr.year-4,yr2=curr.year,all=TRUE)
o3.seasons <- get.seasons(yr1=curr.year-4,yr2=curr.year)
pm.monitors <- get.monitors(par=88101,yr1=curr.year-4,yr2=curr.year,all=TRUE)
pm.schedules <- get.schedules(yr1=curr.year-4,yr2=curr.year)

## Remove sites that are NAAQS excluded for the entire 5 years
o3.monitors <- subset(o3.monitors,
  as.Date(gsub(" ",paste(curr.year,"01-01",sep="-"),nonreg_begin_date)) >= as.Date(paste(curr.year-4,"01-01",sep="-")) &
  as.Date(gsub(" ",paste(curr.year,"01-01",sep="-"),nonreg_end_date)) <= as.Date(paste(curr.year,"12-31",sep="-")) &
  nonreg_concur != "Y")
pm.monitors <- subset(pm.monitors,
  as.Date(gsub(" ",paste(curr.year,"01-01",sep="-"),nonreg_begin_date)) >= as.Date(paste(curr.year-4,"01-01",sep="-")) &
  as.Date(gsub(" ",paste(curr.year,"01-01",sep="-"),nonreg_end_date)) <= as.Date(paste(curr.year,"12-31",sep="-")) &
  nonreg_concur != "Y")

## Retrieve ozone hourly data regional concurrence flags
dt.begin <- paste(curr.year-4,"01-01 00:00:00",sep="-")
dt.end <- paste(curr.year+1,"01-01 06:00:00",sep="-")
o3.concurrences <- get.aqs.data(paste("SELECT DISTINCT
          rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
          TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
          GREATEST(rd.standard_sample_value*1000,0) AS conc,
          rd.method_code AS method,
          COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
          COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
     FROM raw_data_concurrences rd
    WHERE rd.duration_code = '1'
      AND (rd.event_code IS NOT NULL OR rd.null_data_code IS NOT NULL)
      AND rd.parameter_code = '44201'
      AND rd.sampling_begin_datetime >= TO_DATE('",dt.begin,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.sampling_begin_datetime <= TO_DATE('",dt.end,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.state_code NOT IN ('80','CC')
      AND (rd.event_concurence_indicator = 'Y' OR rd.null_code_concurrence = 'Y')
 ORDER BY 1,2",sep=""))

dt.end <- paste(curr.year,"12-31 23:00:00",sep="-")
pm.concurrences.hourly <- get.aqs.data(paste("SELECT DISTINCT
          rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
          TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
          GREATEST(rd.standard_sample_value,0) AS conc,
          rd.method_code AS method,
          COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
          COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
     FROM raw_data_concurrences rd
    WHERE rd.duration_code = '1'
      AND (rd.event_code IS NOT NULL OR rd.null_data_code IS NOT NULL)
      AND rd.parameter_code = '88101'
      AND rd.sampling_begin_datetime >= TO_DATE('",dt.begin,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.sampling_begin_datetime <= TO_DATE('",dt.end,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.state_code NOT IN ('80','CC')
      AND (rd.event_concurence_indicator = 'Y' OR rd.null_code_concurrence = 'Y')
 ORDER BY 1,2",sep=""))

pm.concurrences.daily <- get.aqs.data(paste("SELECT DISTINCT
          rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
          TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
          GREATEST(rd.standard_sample_value,0) AS conc,
          rd.method_code AS method,
          COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
          COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
     FROM raw_data_concurrences rd
    WHERE rd.duration_code = '7'
      AND (rd.event_code IS NOT NULL OR rd.null_data_code IS NOT NULL)
      AND rd.parameter_code = '88101'
      AND rd.sampling_begin_datetime >= TO_DATE('",dt.begin,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.sampling_begin_datetime <= TO_DATE('",dt.end,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.state_code NOT IN ('80','CC')
      AND (rd.event_concurence_indicator = 'Y' OR rd.null_code_concurrence = 'Y')
 ORDER BY 1,2",sep=""))

save(list=c("o3.methods","o3.monitors","o3.seasons","pm.monitors","pm.schedules",
  "o3.concurrences","pm.concurrences.hourly","pm.concurrences.daily"),
    file="EE_DVcalc/data/aqsdata.Rdata")
