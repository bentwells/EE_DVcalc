## Create monitor metadata files for O3 and PM2.5
source("R/get_monitors.r")
curr.year <- as.numeric(substr(as.character(Sys.Date()),1,4)) - 
  ifelse(as.numeric(substr(as.character(Sys.Date()),6,7)) > 4,1,2)
o3.methods <- get.methods(par=44201)
o3.monitors <- get.monitors(par=44201,yr1=curr.year-4,yr2=curr.year)
o3.seasons <- get.seasons(yr1=curr.year-4,yr2=curr.year)
pm.monitors <- get.monitors(par=88101,yr1=curr.year-4,yr2=curr.year)
pm.schedules <- get.schedules(yr1=curr.year-4,yr2=curr.year)
save(list=c("o3.monitors","pm.monitors"),file="EE_DVcalc/data/monitors.Rdata")
save(o3.seasons,file="EE_DVcalc/data/o3seasons.Rdata")
save(o3.methods,file="EE_DVcalc/data/o3methods.Rdata")
save(pm.schedules,file="EE_DVcalc/data/pmschedules.Rdata")