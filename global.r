## Set options, load required packages
setwd("C:/Your/Local/Directory/")
require(RAQSAPI,quietly=TRUE,warn.conflicts=FALSE)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(xlsx,quietly=TRUE,warn.conflicts=FALSE)
aqs_credentials(username="Your.Email@epa.gov",key="Your_AQS_API_key")
options(stringsAsFactors=FALSE)
curr.year <- as.numeric(substr(as.character(Sys.Date()),1,4)) - 
  ifelse(as.numeric(substr(as.character(Sys.Date()),6,7)) > 4,1,2)

## Custom functions called within the main function
count <- function(x) { return(sum(!is.na(x))) }
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
mean.na <- function(x) { return(ifelse(all(is.na(x)),NA,mean(x,na.rm=TRUE))) }
min.na <- function(x) { return(ifelse(all(is.na(x)),NA,min(x,na.rm=TRUE))) }

## Function to calculate moving 8-hour averages (O3)
avg8 <- function(x,sub,lvl) {
  n <- count(x)
  if (n == 0) { return(NA) }
  if (n >= 6) { return(mean(x,na.rm=TRUE)) }
  if (n > 0 & n < 6) {
    x.sub <- mean(replace(x,which(is.na(x)),sub))
    return(ifelse(floor(x.sub) > lvl,x.sub,NA))
  }
}

## Function to calculate 24-hour averages (PM2.5 FEMs) 
avg24 <- function(x,sub,lvl) {
  n <- count(x)
  if (n == 0) { return(NA) }
  if (n >= 18) { return(floor(10*mean(x,na.rm=TRUE))/10) }
  if (n > 0 & n < 18) {
    x.sub <- floor(10*mean(replace(x,which(is.na(x)),sub)))/10
    return(ifelse(round(x.sub) > lvl,x.sub,NA))
  }
}

## Load monitor metadata, method codes, o3 seasons, pm2.5 sampling schedules
load("data/aqsdata.Rdata")
