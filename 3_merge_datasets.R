###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 3 of x 
# The purpose of this script is to merge the data into approperiate groupings for  
# data checking and processing
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: 


# Check str of data
# using data tables is better than data frames
# better able to deal with larger data files.
# convert all HH data frames to data tables
HH_EVHOE<-as.data.table(HH_EVHOE)
HH_FRCGFS<-as.data.table(HH_FRCGFS)
HH_IGFS<-as.data.table(HH_IGFS)
HH_NSIBTS<-as.data.table(HH_NSIBTS)
HH_ROCK<-as.data.table(HH_ROCK)
HH_SWC<-as.data.table(HH_SWC)
HH_ARSA<-as.data.table(HH_ARSA)
HH_SPNGFS<-as.data.table(HH_SPNGFS)
HH_SPPORC<-as.data.table(HH_SPPORC)
HH_NIGFS<-as.data.table(HH_NIGFS)
HH_BTS<-as.data.table(HH_BTS)
HH_BTS7a<-as.data.table(HH_BTS7a)
HH_PT<-as.data.table(HH_PT)

# merge DATRAS HH data files by primary gear type
HH_gov<-rbind(HH_EVHOE, HH_FRCGFS, 
          HH_IGFS, HH_NSIBTS, 
          HH_ROCK, HH_SWC, fill=TRUE)

HH_rot<-rbind(HH_NIGFS, fill=TRUE)
names(HH_SPPORC)
HH_baka<-rbind(HH_SPPORC, HH_SPNGFS, HH_ARSA, fill=TRUE)
names(HH_SPNGFS)
HH_nct<-rbind(HH_PT, fill=TRUE)

HH_beams<-rbind(HH_BTS, HH_BTS7a, fill=TRUE)
# Quick Check that haul numbers match
# HH_gov has 41817 obs
3565+577+30394+2230+2406+2645 # match
# HH_rot has 901, HH_NIGFS has 901 - match
# HH_nct has 1036 HH_PT has 1036 - match
# HH_beam has 10764
8088+2676 # match
# HH_baka has a 5424 
1481+2834+1109 # match

# convert HL data trames to data tables
HL_EVHOE<-as.data.table(HL_EVHOE)
HL_FRCGFS<-as.data.table(HL_FRCGFS)
HL_IGFS<-as.data.table(HL_IGFS)
HL_NSIBTS<-as.data.table(HL_NSIBTS)
HL_ROCK<-as.data.table(HL_ROCK)
HL_SWC<-as.data.table(HL_SWC)
HL_ARSA<-as.data.table(HL_ARSA)
HL_SPNGFS<-as.data.table(HL_SPNGFS)
HL_SPPORC<-as.data.table(HL_SPPORC)
HL_NIGFS<-as.data.table(HL_NIGFS)
HL_BTS<-as.data.table(HL_BTS)
HL_BTS7a<-as.data.table(HL_BTS7a)
HL_PT<-as.data.table(HL_PT)

HL_gov<-rbind(HL_EVHOE, HL_FRCGFS, 
              HL_IGFS, HL_NSIBTS, 
              HL_ROCK, HL_SWC, fill=TRUE)

HL_rot<-rbind(HL_NIGFS, fill=TRUE)

HL_baka<-rbind(HL_SPPORC, HL_SPNGFS, HL_ARSA, fill=TRUE)

HL_nct<-rbind(HL_PT, fill=TRUE)

HL_beams<-rbind(HL_BTS, HL_BTS7a, fill=TRUE)

# HL_gov has 4411954 obs
401741+138771+416005+2851635+67386+536416 # match
# HL_rot has 159294, HL_NIGFS has 159294 - match
# HL_nct has 72668 HL_PT has 72668 - match
# HL_beam has 857370
608163+249207 # match
# HL_baka has 799682
205818+361715+232149 #match
# HH_baka has two extra cols - misspellings
HH_baka$Buoyancy<-NULL
HH_baka$Buoyancy<-HH_baka$Bouyancy
HH_baka$Bouyancy<-NULL
HH_baka$Month[is.na(HH_baka$Month)]<-HH_baka$month[is.na(HH_baka$Month)]
HH_baka$month<-NULL
HH_baka$month<-HH_baka$Month
HH_baka$Month<-NULL
# Remove the intermediate files to free up space
rm(HH_EVHOE,HH_FRCGFS,HH_IGFS,HH_NSIBTS,HH_ROCK,HH_SWC,
   HH_ARSA,HH_SPNGFS,HH_SPPORC,HH_NIGFS,HH_BTS,HH_BTS7a,HH_PT,
   HL_EVHOE,HL_FRCGFS,HL_IGFS,HL_NSIBTS,HL_ROCK,HL_SWC,
   HL_ARSA,HL_SPNGFS,HL_SPPORC,HL_NIGFS,HL_BTS,HL_BTS7a,HL_PT)

# Add unique Hauls ID fields to each of the datasets to allow combination and changing
# This takes the structure Survey/YEAR/Quarter/Ship/HaulNo/Gear
#----------------------------------------------------------------------
# gov
HH_gov$UniqueID<-paste(HH_gov$Survey,HH_gov$Year,HH_gov$Quarter, 
                       HH_gov$Ship, HH_gov$HaulNo, HH_gov$Gear, sep="/")
HL_gov$UniqueID<-paste(HL_gov$Survey,HL_gov$Year,HL_gov$Quarter, 
                       HL_gov$Ship, HL_gov$HaulNo, HL_gov$Gear, sep="/")
# nct
HH_nct$UniqueID<-paste(HH_nct$Survey,HH_nct$Year,HH_nct$Quarter, 
                       HH_nct$Ship, HH_nct$HaulNo, HH_nct$Gear, sep="/")
HL_nct$UniqueID<-paste(HL_nct$Survey,HL_nct$Year,HL_nct$Quarter, 
                       HL_nct$Ship, HL_nct$HaulNo, HL_nct$Gear, sep="/")
# baka
HH_baka$UniqueID<-paste(HH_baka$Survey,HH_baka$Year,HH_baka$Quarter, 
                        HH_baka$Ship, HH_baka$HaulNo, HH_baka$Gear, sep="/")
HL_baka$UniqueID<-paste(HL_baka$Survey,HL_baka$Year,HL_baka$Quarter, 
                        HL_baka$Ship, HL_baka$HaulNo, HL_baka$Gear, sep="/")
# beams
HH_beams$UniqueID<-paste(HH_beams$Survey,HH_beams$Year,HH_beams$Quarter, 
                         HH_beams$Ship, HH_beams$HaulNo, HH_beams$Gear, sep="/")
HL_beams$UniqueID<-paste(HL_beams$Survey,HL_beams$Year,HL_beams$Quarter, 
                         HL_beams$Ship, HL_beams$HaulNo, HL_beams$Gear, sep="/")
# rot
HH_rot$UniqueID<-paste(HH_rot$Survey,HH_rot$Year,HH_rot$Quarter, 
                       HH_rot$Ship, HH_rot$HaulNo, HH_rot$Gear, sep="/")
HL_rot$UniqueID<-paste(HL_rot$Survey,HL_rot$Year,HL_rot$Quarter, 
                       HL_rot$Ship, HL_rot$HaulNo, HL_rot$Gear, sep="/")


####################################
## Sort out data frame structure HH#
####################################

cnames <- colnames(HH_gov)
for(cname in cnames) {
  set(HH_gov, j = cname, value = gsub("[[:space:]]", "", HH_gov[[cname]]))
}
str(HH_gov)

# Change appropriate cols to numeric and have data tables set up properlly
numCols <- c("Quarter", "SweepLngt", "HaulNo", "Year", "month",
             "Day", "TimeShot", "HaulDur", "ShootLat",
             "ShootLong", "HaulLat", "HaulLong", "Depth", 
             "Netopening",  "Distance", "Warplngt", "Warpdia", "WarpDen",
             "DoorSurface", "DoorWgt","DoorSpread", "WingSpread",
             "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", 
             "GroundSpeed" , "SpeedWater", "SurCurDir", "SurCurSpeed",
             "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
             "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal",
             "ThermoCline", "ThClineDepth", "DateofCalculation")
HH_beams[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HH_gov[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HH_nct[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HH_rot[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
numCols <- c("Quarter", "SweepLngt", "HaulNo", "Year", "Month",
             "Day", "TimeShot", "HaulDur", "ShootLat",
             "ShootLong", "HaulLat", "HaulLong", "Depth", 
             "Netopening",  "Distance", "Warplngt", "Warpdia", "WarpDen",
             "DoorSurface", "DoorWgt","DoorSpread", "WingSpread",
             "Bouyancy", "KiteDim", "WgtGroundRope", "TowDir", 
             "GroundSpeed" , "SpeedWater", "SurCurDir", "SurCurSpeed",
             "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
             "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal",
             "ThermoCline", "ThClineDepth", "DateofCalculation")
HH_baka[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
#####################
# Change -9's to NA #
####################
# All -9 should be NA as -9 represents a missing value in DATRAS
# Funtion to replace multiple -9 across lots of data tables
replace_function=function(DT){
  cnames <- colnames(DT)
  for(cname in cnames) {
    set(DT, j = cname, value = gsub("[[:space:]]", "", DT[[cname]]))
  }
  for(cname in cnames){
    set(DT, i = which(DT[[cname]] == -9), j = cname, value = NA)
  }
}

system.time(replace_function(HH_baka))
system.time(replace_function(HH_beams))
system.time(replace_function(HH_gov))
system.time(replace_function(HH_nct))
system.time(replace_function(HH_rot))

####################################
## Sort out data frame structure HL#
####################################
# Change appropriate cols to numeric and have data tables set up properlly
cnames <- colnames(HL_gov)
for(cname in cnames) {
  set(HL_gov, j = cname, value = gsub("[[:space:]]", "", HL_gov[[cname]]))
}
str(HL_gov)

numCols <- c("Quarter","SweepLngt","HaulNo","Year", "SpecCode","SpecVal",
             "TotalNo","CatIdentifier","NoMeas","SubFactor", "SubWgt",
             "CatCatchWgt","LngtClass", "HLNoAtLngt","Valid_Aphia")
HL_baka[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HL_beams[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HL_nct[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HL_rot[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
HL_gov[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]

#####################
# Change -9's to NA #
####################
# All -9 should be NA as -9 represents a missing value in DATRAS
# Change -9 values to NA

temp = tables()
list<-temp$NAME

# Funtion to replace multiple -9 across lots of data tables
replace_function=function(DT){
  cnames <- colnames(DT)
  for(cname in cnames) {
    set(DT, j = cname, value = gsub("[[:space:]]", "", DT[[cname]]))
  }
  for(cname in cnames){
    set(DT, i = which(DT[[cname]] == -9), j = cname, value = NA)
  }
}

system.time(replace_function(HL_baka))
system.time(replace_function(HL_beams))
system.time(replace_function(HL_gov))
system.time(replace_function(HL_nct))
system.time(replace_function(HL_rot))


# all offending -9 should now be gone from all my data tables
# check summarys and insure the data sturcture is sound
str(HL_gov)
summary(HL_gov)

# if all is good- move on, if not rerun numeric col stuff again after applying the
# NA replace_function as this might mess with the structure.

