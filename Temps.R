#################################################### 
# Script to bring in wunderground data             #
# Created by Piper                                 #
# created on 5/1/2017                              #
# updated on 10/9/2017                             #
####################################################


library(lubridate)
library(stringr)
library(rwunderground) #weather underground
library(rnoaa) #noaa data

# API Key: fd95f5a719ff2d47
# alt APIs?: fdd3f0ac72161ac8    0def10027afaebb7   b6e3b1d7c1e3c28f



# clear workspace
rm(list=ls())

# set working directory
setwd(getwd())
 
# Site by Site

# Starfish Point
 Tdata <- history_range(set_location(lat_long = "47.936,-124.570"),"20150816", "20160816", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/SP.csv"))

# Ecola
 Tdata <- history_range(set_location(lat_long = "45.892,-123.961"),"20160914", "20170914", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/Survey/EC.csv"))
 
# Cape Meares
 Tdata <- history_range(set_location(lat_long = "45.468,-123.974"),"20160914", "20170914", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CMYear.csv")) 
 
# Fogarty
 Tdata <- history_range(set_location(lat_long = "44.722,-124.058"),"20150805", "20160805", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/FC.csv")) 
 
# Seal Rock
 Tdata <- history_range(set_location(lat_long = "44.534,-124.074"),"20160821", "20170821", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/SR.csv"))  
 
# Bob Creek
 Tdata <- history_range(set_location(lat_long = "44.291,-124.111"),"20160916", "20170916", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/BC.csv"))   
 
# Cape Arago
 Tdata <- history_range(set_location(lat_long = "43.420,-124.246"),"20150819", "20160819", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CA.csv"))  

# Pyramid Point
 Tdata <- history_range(set_location(lat_long = "41.943,-124.170"),"20160918", "20170918", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/PP.csv")) 

# Point St. George
 Tdata <- history_range(set_location(lat_long = "41.780,-124.234"),"20150822", "20160822", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/PG.csv"))  
 
# Kibesillah Hill
 Tdata <- history_range(set_location(lat_long = "39.503,-123.743"),"20160919", "20170919", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/KH.csv"))   
 
# Bodega
 Tdata <- history_range(set_location(lat_long = "38.306,-123.014"),"20150731", "20160731", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/BB.csv"))  
 
# Monterey
 Tdata <- history_range(set_location(lat_long = "36.623,-121.919"),"20150706", "20160706", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/MB.csv")) 
 
# Cambria
 Tdata <- history_range(set_location(lat_long = "35.534,-121.083"),"20151019", "20161019", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CB.csv")) 
 
# Hazards
 Tdata <- history_range(set_location(lat_long = "35.316,-120.845"),"20151020", "20161020", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/HZ.csv"))  
 
# Shell Beach
 Tdata <- history_range(set_location(lat_long = "35.156,-120.674"),"20151104", "20161104", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/SB.csv"))   
 
# Carpenteria
 Tdata <- history_range(set_location(lat_long = "34.390,-119.511"),"20151031", "20161031", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CT.csv"))   
 
# Little Corona
 Tdata <- history_range(set_location(lat_long = "33.603,-117.879"),"20151014", "20161014", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/LC.csv"))
 
# Crystal Cove
 Tdata <- history_range(set_location(lat_long = "33.548,-117.805"),"20151028", "20161028", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CC.csv"))
 
# Dana Point
 Tdata <- history_range(set_location(lat_long = "33.460,-117.671"),"20151026", "20161026", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/DP.csv"))
   
# Scripps
 Tdata <- history_range(set_location(lat_long = "32.863,-117.240"),"20151114", "20161114", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/SC.csv"))
 
 
# Fogarty 2017
 Tdata <- history_range(set_location(lat_long = 44.722,-124.058),"201601020", "20160805", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/FC17.csv"))  
 
 # Cambria
 Tdata <- history_range(set_location(lat_long = 35.534,-121.083),"20161021", "20161021", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CB17.csv"))  
 
 # Crystal Cove
 Tdata <- history_range(set_location(lat_long = 33.548,-117.805),"20161023", "20161023", key = get_api_key(), use_metric = TRUE)
 write.csv(Tdata, file = paste0("Data/Temps/CCYear.csv"))
 
 
   
## For Loops 
# Extract Temp Data from Wunderground
Stations <- c("47.936,-124.570", "44.722,-124.058", "43.420,-124.246", "41.780,-124.234", "38.306,-123.014", "36.623,-121.919", "35.534,-121.083", "35.330,-120.844", "35.160,-120.675", "34.390,-119.511", "33.603,-117.879", "33.548,-117.805", "33.460,-117.671", "32.863,-117.240") #wunderground station lat_longs

SiteNames<-c('SP','FC','CA','PG','BB','MB','CB','HZ','SB','CT','LC','CC','DP','SC')

for (i in 1:length(Stations)){ #loop over sites
  Tdata <- history_range(set_location(lat_long = Stations[i]),"20150701", "20160701", key = get_api_key(), use_metric = TRUE)
  write.csv(Tdata, file = paste0("Data/Temps/",SiteNames[i],".csv"))
}

# 2017 sites

# Extract Temp Data from Wunderground
#Stations <- c("45.892,-123.961", "45.468,-123.974", "44.534,-124.074", "44.291,-124.111", "41.943,-124.170", "39.444,-123.802") #wunderground station lat_longs

#SiteNames<-c('EC','CM','SR','BC','PP','KH')

Stations <- c("44.291,-124.111", "41.943,-124.170", "39.444,-123.802") #wunderground station lat_longs

SiteNames<-c('BC','PP','KH')

for (i in 1:length(Stations)){ #loop over sites
  Tdata <- history_range(set_location(lat_long = Stations[i]),"20160701", "20170701", key = get_api_key(), use_metric = TRUE)
  write.csv(Tdata, file = paste0("Data/Temps/",SiteNames[i],".csv"))
}





########################################################### 
# Script to calculate emersion time and detide data       #
# Created by Piper                                        #
# Created on 2/21/2017                                    #
# Most recent update 10/30/2017                           #
# Added line by line comments 10/20/2018                  #
###########################################################

# clear workspace
rm(list=ls())

# read in packages
library(readr)
library(plyr)

# Data----
# Load Data
VerticalMin <- read.csv('Data/VerticalMin.csv')  #foundation species min
VerticalMax <- read.csv('Data/VerticalMax.csv')  #foundation species max
Stars <- read.csv('Data/Stars.csv')  #sea stars
#Survey <- read.csv('Data/SurveyData.csv') #temp metrics
SiteData <- read.csv('Data/SiteData.csv') #tide heights for d-etiding

# Order sites by latitude for foundation species min
order(VerticalMin$Latitude)
VerticalMin$Site[order(VerticalMin$Latitude)]
VerticalMin$Site <- ordered(VerticalMin$Site, levels = unique(VerticalMin$Site[order(VerticalMin$Latitude)]))
unique(VerticalMin$Site)

# Order sites by latitude for foundation species max
order(VerticalMax$Latitude)
VerticalMax$Site[order(VerticalMax$Latitude)]
VerticalMax$Site <- ordered(VerticalMax$Site, levels = unique(VerticalMax$Site[order(VerticalMax$Latitude)]))
unique(VerticalMax$Site)

# Calculate TH
VerticalMin$MinTH <- VerticalMin$Ocean + VerticalMin$TH - VerticalMin$Min
VerticalMax$MaxTH <- VerticalMax$Ocean + VerticalMax$TH - VerticalMax$Max


# Order sites by latitude for Stars
order(Stars$Latitude)
Stars$Site[order(Stars$Latitude)]
Stars$Site <- ordered(Stars$Site, levels = unique(Stars$Site[order(Stars$Latitude)]))
unique(Stars$Site)

# Calculate TH
Stars$StarTH <- Stars$Ocean + Stars$TH - Stars$Location


# Order sites by latitude 
order(SiteData$Latitude)
SiteData$Site[order(SiteData$Latitude)]
SiteData$Site <- ordered(SiteData$Site, levels = unique(SiteData$Site[order(SiteData$Latitude)]))
unique(SiteData$Site)


# Calculate iButton TH
SiteData$LowTH <- SiteData$Ocean + SiteData$TH - SiteData$Low
SiteData$MidTH <- SiteData$Ocean + SiteData$TH - SiteData$Mid
SiteData$HighTH <- SiteData$Ocean + SiteData$TH - SiteData$High


# Order sites for Survey Data
#order(Survey$Latitude)
#Survey$Site[order(Survey$Latitude)]
#Survey$Site <- ordered(Survey$Site, levels = unique(Survey$Site[order(Survey$Latitude)]))
#unique(Survey$Site)

# Calculate temperature metrics----
# extract the file names for temperature data
file.names <- list.files('Data/Temps/Survey/')

# split the name by the "."
TempSites<-as.data.frame(strsplit(file.names, "[.]"))
TempSites<-t(TempSites[1,]) # tranposed and pulled out just the site name


# Pre-allocate space 
params <- c('Site', 'Mean', 'P90','Max', 'MonAvg', 'MMMax', 'DMean', 'MDMax', 'Range', 'DRange', 'MRange')
#params <- c('Site', 'Mean', 'P90', 'P05','Max', 'Min', 'MonAvg', 'MMMax', 'MMMin', 'DMean', 'MDMax','MDMin', 'Range', 'DRange', 'MRange')
# P90 = 90th perccentile, MonAvg = monthly average, MDM = mean daily max, MMM = mean monthly max, daily range, monthly range, P05 = 5th percentile

TempMetrics <- data.frame(matrix(NA, nrow = length(file.names), ncol = length(params))) #allocate space

# Name Columns in TempMetrics
colnames(TempMetrics) <- params


# For loop for temp data

for (i in 1:length(file.names)){
  # Load Temp Data
  Temps <- read.csv(paste0('Data/Temps/Survey/',file.names[i]))
  Temps$DateTime <- as.POSIXlt(x = as.character(Temps$DateTime), format = "%m/%d/%y %H:%M") #format
  
  #take hourly averages
  Temps <- aggregate(list(Temp = Temps$Temp), 
                        list(DateTime = cut(Temps$DateTime, "1 hour")), 
                        mean)
  
  # Load Tide Data
  Tides <- read.csv(paste0('Data/Tides/Survey/',file.names[i]))
  Tides$DateTime <- as.POSIXlt(x = Tides$DateTime, format = "%m/%d/%y %H:%M") #format
  
  # Merge Data
  TempTide <- merge(Temps, Tides, by="DateTime")
  TempTide$DateTime <- as.POSIXlt(x = TempTide$DateTime, format = "%Y-%m-%d %H:%M") #format
  
    
# Detide all variables 
#TempTide$Min10 <- ifelse(SiteData$MidTH[i] > TempTide$TH, 1, 0) # actually max tide height 
    
TempTide$Min10 <- ifelse(SiteData$MidTH[i] > TempTide$TH, 1, 0) 
   
  
  # temp metric errything 
  TempMetrics$Site[i] <- TempSites[i] # name the site (site i)
  TempMetrics$Mean[i] <- mean(TempTide$Temp[TempTide$Min10 == 1], na.rm = TRUE)
  TempMetrics$P90[i] <- quantile(TempTide$Temp[TempTide$Min10 == 1], 0.9, na.rm = TRUE)  
  #TempMetrics$P05[i] <- quantile(TempTide$Temp[TempTide$Min10 == 1], 0.5, na.rm = TRUE)  
  TempMetrics$Max[i] <- max(TempTide$Temp[TempTide$Min10 == 1], na.rm = TRUE)
  #TempMetrics$Min[i] <- min(TempTide$Temp[TempTide$Min10 == 1], na.rm = TRUE)
  
  
  # Get monthly averages
  TMonth <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                      list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 month")), function(x) mean(x,na.rm = TRUE))
  
  #get Inf... convert to NA
  TMonth$Temp[is.infinite(TMonth$Temp)] <- NA
  
  # Take average of monthly average
  TempMetrics$MonAvg[i] <- mean(TMonth$Temp, na.rm = TRUE)
  
  # Get daily mean 
  TDMean <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                         list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 day")), function(x) mean(x,na.rm = TRUE))
  
  #get Inf... convert to NA
  TDMean$Temp[is.infinite(TDMean$Temp)] <- NA
  
  # Take average of daily max
  TempMetrics$DMean[i] <- mean(TDMean$Temp, na.rm = TRUE)
  
  
  # Get daily max 
  TMaxDaily <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                       list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 day")), function(x) max(x,na.rm = TRUE))
  
  #get Inf... convert to NA
  TMaxDaily$Temp[is.infinite(TMaxDaily$Temp)] <- NA
  
  # Take average of daily max
  TempMetrics$MDMax[i] <- mean(TMaxDaily$Temp, na.rm = TRUE)
  
  
  # Get daily min 
  #TMinDaily <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                       #list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 day")), function(x) min(x,na.rm = TRUE))
  
  #get Inf... convert to NA
  #TMinDaily$Temp[is.infinite(TMinDaily$Temp)] <- NA
  
  # Take average of daily min
  #TempMetrics$MDMin[i] <- mean(TMinDaily$Temp, na.rm = TRUE)
  
  
  # Get monthly max
  TMMax <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                       list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 month")), function(x) max(x,na.rm = TRUE))
  
  #get Inf... convert to NA
  TMMax$Temp[is.infinite(TMMax$Temp)] <- NA
  
  # Take average of monthly max
  TempMetrics$MMMax[i] <- mean(TMMax$Temp, na.rm = TRUE)
  
  
  # Get monthly min
  #TMMin <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                     #list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 month")), function(x) min(x,na.rm = TRUE))
  
  #get Inf... convert to NA
  #TMMin$Temp[is.infinite(TMMin$Temp)] <- NA
  
  # Take average of monthly max
  #TempMetrics$MMMin[i] <- mean(TMMin$Temp, na.rm = TRUE)

  
  # Calculate Range 
  TempMetrics$Range[i] <- max(TempTide$Temp[TempTide$Min10 == 1], na.rm = TRUE) - min(TempTide$Temp[TempTide$Min10 == 1], na.rm = TRUE)
  
  
  # Get daily average range 
  TDRangeMax <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                          list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 day")), function(x) max(x,na.rm = TRUE))
  
  TDRangeMin <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                          list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 day")), function(x) min(x,na.rm = TRUE))
  
  #Daily average range
  TDRange <- TDRangeMax #saved max as range to save dates
  TDRange$Temp <- TDRangeMax$Temp - TDRangeMin$Temp #subtract min from max

  #get Inf... convert to NA
  TDRange$Temp[is.infinite(TDRange$Temp)] <- NA
  
  # Take average of daily range
  TempMetrics$DRange[i] <- mean(TDRange$Temp, na.rm = TRUE)
  
  
  # Get monthly average range 
  TMRangeMax <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                          list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 month")), function(x) max(x,na.rm = TRUE))
  
  TMRangeMin <- aggregate(list(Temp = TempTide$Temp[TempTide$Min10 == 1]), 
                          list(DateTime = cut(TempTide$DateTime[TempTide$Min10 == 1], "1 month")), function(x) min(x,na.rm = TRUE))
  
  #Daily average range
  TMRange <- TMRangeMax
  TMRange$Temp <- TMRangeMax$Temp - TMRangeMin$Temp

  #get Inf... convert to NA
  TMRange$Temp[is.infinite(TMRange$Temp)] <- NA  
  
  # Take average of daily range
  TempMetrics$MRange[i] <- mean(TMRange$Temp, na.rm = TRUE)
}



# merge and order site and temp metrics
SiteMetrics <- merge(SiteData, TempMetrics, by="Site")


#remove all unnecessary variables
rm(TMonth, TDRange, TDRangeMax, TDRangeMin, TMRange, TMRangeMax, TMRangeMin, Tides, Temps, i, file.names, TempSites, SiteData, TempTide, TempMetrics, params, TDMean, TMaxDaily, TMMax)

# Emersion time----

# extract the file names for tide data
file.names<-list.files('Data/Tides/Survey/')

# split the name by the "."
site.names<-as.data.frame(strsplit(file.names, "[.]"))
site.names<-t(site.names[1,]) # tranposed and pulled out just the site name

VerticalMin$ET<-NA # pre-allocate space for emersion time
VerticalMax$ET<-NA # pre-allocate space for emersion time


for (j in 1:length(site.names)){ # loop over sites
  TData<-read.csv(paste('Data/Tides/Survey/',file.names[j],sep="")) # read in data for site j 
  Site<-site.names[j] # pull out site name j
  
  rownums<-which(VerticalMin$Site==Site) # pull out the row numbers in Data dataframe from site j to calculate emersion time
  
  for (i in 1:length(rownums)){ # loop over transects in site j 
    ## min emersion time
    tempmin<-ifelse(VerticalMin$MinTH[rownums[i]] > TData$TH,1,0) # give 1/0 for exposed/not
    n<-rownums[i] # index for where to save output
    VerticalMin$ET[n] <-100*sum(tempmin)/length(!is.na(tempmin)) #sum the 1 and divide by total number of data points an multiply by 100 to get % emersed
    
    ## max emersion time
    tempmax<-ifelse(VerticalMax$MaxTH[rownums[i]] > TData$TH,1,0) # give 1/0 for exposed/not
    VerticalMax$ET[n] <-100*sum(tempmax)/length(!is.na(tempmax)) #sum the 1
  }
}

head(VerticalMin)
head(VerticalMax)


# Quadrats----

# Quadrats$QET<-NA # pre-allocate space for emersion time

# for (j in 1:length(site.names)){ # loop over sites
# TData<-read.csv(paste('Data/Tides/Annual/',file.names[j],sep="")) # read in data for site j 
# Site<-site.names[j] # pull out site name j

# rownums<-which(Quadrats$Site==Site) # pull out the row numbers in Data dataframe from site J to calculate emersion time
# for (i in 1:length(rownums)){ # loop over transects in site j 

# tempquad<-ifelse(Quadrats$QTH[rownums[i]] > TData$TH,1,0) # give 1/0 for exposed/not
# n<-rownums[i] # index for where to save output
# Quadrats$QET[n] <-100*sum(tempquad)/length(tempquad) #sum the 1 and divide by total number of data points an multiply by 100 to get % emersed
# }
# }

# head(Quadrats)



# All Stars----
Stars$ET<-NA # pre-allocate space for emersion time

for (j in 1:length(site.names)){ # loop over sites
  TData<-read.csv(paste('Data/Tides/Survey/',file.names[j],sep="")) # read in data for site j 
  Site<-site.names[j] # pull out site name j
  
  rownums<-which(Stars$Site==Site) # pull out the row numbers in Data dataframe from site J to calculate emersion time
  for (i in 1:length(rownums)){ # loop over transects in site j 
    
    tempstars<-ifelse(Stars$StarTH[rownums[i]] > TData$TH,1,0) # give 1/0 for exposed/not
    n<-rownums[i] # index for where to save output
    Stars$ET[n] <-100*sum(tempstars)/length(tempstars) #sum the 1 and divide by total number of data points an multiply by 100 to get % emeresed
  }
}

head(Stars)

#remove all unnecessary variables
rm(site.names, TData, file.names, i, j, n, rownums, Site, tempmax, tempmin, tempstars)


# Short term temp data----
# extract the file names for st data
file.names <- list.files('Data/Temps/ST/')

# split the name by the "."
TempSites<-as.data.frame(strsplit(file.names, "[.]"))
TempSites<-t(TempSites[1,]) # tranposed and pulled out just the site name


# Pre-allocate space 
params <- c('Site', 'iMean', 'iP90','iMax','iRange')
TempMetrics <- data.frame(matrix(NA, nrow = length(file.names), ncol = length(params))) #allocate space

# Name Columns in TempMetrics
colnames(TempMetrics) <- params

i = 1:20
# For loop for temp data

for (i in 1:length(file.names)){
  # Load Temp Data
  Temps <- read.csv(paste0('Data/Temps/ST/',file.names[i]))

  # temp metric errything 
  TempMetrics$Site[i] <- TempSites[i] # name the site (site i)
  TempMetrics$iMean[i] <- mean(c(Temps$Temp1, Temps$Temp2), na.rm = TRUE)
  TempMetrics$iP90[i] <- mean(c(quantile(Temps$Temp1, 0.9, na.rm = TRUE)), quantile (Temps$Temp2, 0.9, na.rm = TRUE))
  TempMetrics$iMax[i] <- mean(c(max(Temps$Temp1, na.rm = TRUE), max(Temps$Temp2, na.rm = TRUE)))
  TempMetrics$iRange[i] <- mean(c(max(Temps$Temp1, na.rm = TRUE), max(Temps$Temp2, na.rm = TRUE))) - mean(c(min(Temps$Temp1, na.rm = TRUE), min(Temps$Temp2, na.rm = TRUE)))
}

# merge and order site and temp metrics
iTempMetrics <- TempMetrics


#remove all unnecessary variables
rm(Temps, i, file.names, TempSites, TempMetrics, params)



