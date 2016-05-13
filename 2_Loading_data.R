###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 2 of X 
# The purpose of this script is to demonstrate the download processes directly from 
# DATRAS and show users how to access the data used for the product.
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: 

#############
# Load data #
#############
#
# If FALSE, mostly suppress CI computation
need.CI <- FALSE
#
# Number of bootstrap replicates
if(need.CI){
  nb <- 1000
}else{
  nb <- 3 ## just to make code run through
}
#
# Confidence interval range
CV <- .95
#
# Set seed for reproducable results
setSeed <- set.seed(627)
# load getDATRAS function - this is available in script 1. Housekeeping...
# Note: ICES data centre may have a newer version of the "getDATRAS" function
# get all relevant data sets
# North Sea: NS-IBTS,1983-2016, quaters 1-4:
#            FR-CGFS, 1988-2016, quaters 1-4: 
#            BTS, 1997-2016, quaters 1-4: 
# Celtic Seas: SCW-IBTS,1985-2016, quaters 1-4: 
#              EVHOE; 1997-2016, quaters 1-4:
#              IE_IGFS, 2003-2016, quaters 1-4:
#              ROCKHALL, 1999-2016, quaters 1-4:
#              NIGFS, 2008-2016, quaters 1-4:
#              BTS_VIIa,
# Other        PT_IGFS, 2004-2016, quaters 1-4:

# Data Loaded and time stamped: 09/05/2016 15:00
# This code will bring data directly from DATRAS portal. 
# Warning: If you need to reproduce my data set excatly then you need to use
# the download provided here as the DATRAS may have changed.
# sample code to use DATRAS function to get HH file.

#HH_NS<-getDATRAS(record="HH", 
#                 survey="NS-IBTS",
#                 startyear=1983,
#                 endyear=2016,
#                 quarters=c(1:4),
#                 parallel=TRUE,
#                 cores = 4)


# Check str of data
# names(HH_NS)

# save a copy of data
# write.table(HH, "DATRAS_NS-IBTS_download_09_05_2016.csv")

###################
# Load Saved Data #
###################
# As this product must be totally reproducable I have saved a copy of the 
# DATRAS file on a given date (09/05/2016)- provided with the file in the sharepoint 
# site associated with this product
# Read in haul data
HH_SWC<-read.csv("./Data/DATRAS_09-05-2016/SWC-IBTS/Exchange Data_2016-05-09 16_25_54.csv")
HH_EVHOE<-read.csv("./Data/DATRAS_09-05-2016/EVHOE/Exchange Data_2016-05-09 12_42_13.csv")
HH_FRCGFS<-read.csv("./Data/DATRAS_09-05-2016/FR-CGFS/Exchange Data_2016-05-09 12_40_51.csv")
HH_IGFS<-read.csv("./Data/DATRAS_09-05-2016/IE-IGFS/Exchange Data_2016-05-09 12_22_03.csv")
HH_NSIBTS<-read.csv("./Data/DATRAS_09-05-2016/NS-IBTS/Exchange Data_2016-05-09 12_42_42.csv")
HH_ROCK<-read.csv("./Data/DATRAS_09-05-2016/ROCKALL/Exchange Data_2016-05-09 12_39_37.csv")
HH_PT<-read.csv("./Data/DATRAS_09-05-2016/PT-IBTS/Exchange Data_2016-05-09 12_39_55.csv")
HH_NIGFS<-read.csv("./Data/DATRAS_09-05-2016/NIGFS/Exchange Data_2016-05-09 12_40_31.csv")
HH_BTS<-read.csv("./Data/DATRAS_09-05-2016/BTS/Exchange Data_2016-05-09 14_47_40.csv")
HH_BTS7a<-read.csv("./Data/DATRAS_09-05-2016/BTS-VIIa/Exchange Data_2016-05-09 14_49_57.csv")

# Read biological data
HL_SWC<-read.csv("./Data/DATRAS_09-05-2016/SWC-IBTS/Exchange Data_2016-05-09 12_38_01.csv")
HL_EVHOE<-read.csv("./Data/DATRAS_09-05-2016/EVHOE/Exchange Data_2016-05-09 12_41_46.csv")
HL_FRCGFS<-read.csv("./Data/DATRAS_09-05-2016/FR-CGFS/Exchange Data_2016-05-09 12_41_24.csv")
HL_IGFS<-read.csv("./Data/DATRAS_09-05-2016/IE-IGFS/Exchange Data_2016-05-09 12_22_15.csv")
HL_NSIBTS<-read.csv("./Data/DATRAS_09-05-2016/NS-IBTS/Exchange Data_2016-05-09 12_48_52.csv")
HL_ROCK<-read.csv("./Data/DATRAS_09-05-2016/ROCKALL/Exchange Data_2016-05-09 12_39_25.csv")
HL_PT<-read.csv("./Data/DATRAS_09-05-2016/PT-IBTS/Exchange Data_2016-05-09 12_40_00.csv")
HL_NIGFS<-read.csv("./Data/DATRAS_09-05-2016/NIGFS/Exchange Data_2016-05-09 12_56_51.csv")
HL_BTS<-read.csv("./Data/DATRAS_09-05-2016/BTS/Exchange Data_2016-05-09 14_46_55.csv")
HL_BTS7a<-read.csv("./Data/DATRAS_09-05-2016/BTS-VIIa/Exchange Data_2016-05-09 14_50_03.csv")
# Add national submitted data

# Add corrections of data from National Data providers
# Denmark earliest years of survey were missing species
NS_DEN_sp_1983<-read.csv("./Data/Corrections/DNK_IBTS1_1983_GOV.CSV", header=F)
NS_DEN_sp_1984<-read.csv("./Data/Corrections/DNK_IBTS1_1984_GOV.CSV", header=F)
NS_DEN_sp_1985<-read.csv("./Data/Corrections/DNK_IBTS1_1985_GOV.CSV", header=F)
NS_DEN_sp_1986<-read.csv("./Data/Corrections/DNK_IBTS1_1986_GOV.CSV", header=F)
# France, didn't have time to reupload data
FRA_HH_corrections<-read.csv("./Data/Corrections/Exchange Data_HH-FranceCorrige.csv")
FRA_HL_corrections1<-read.csv("./Data/Corrections/Exchange Data_HL-FranceCorrige1.csv")
FRA_HL_Corrections2<-read.csv("./Data/Corrections/Exchange Data_HL-FranceCorrige2.csv", header=F)
# After extensive review the french data added more problems than it solved!
# Northern Ireland early data not available on Datras
NI_extra<-read.csv("./Data/National Submissions/Datras_MSFD_NI/Datras_MSFD.csv", header=F)
##############
#SPAIN DATA ##
##############

# Add in Spanish Data not on DATRAS
# All should now be updated and good to go
# Add in National Submissions - be aware that Fran needs to update these and
# send corrected data
setwd("~/MSFD-QA-GFSM-A-DP/Data/National Submissions/Spain_18-03-2016/HH_ARSA") # set path here
files <- list.files()

for (i in 1:length(files)){
  
  # if the merged dataset doesn't exist, create it
  if (i==1){
    dataset <- read.table(files[i], header=FALSE, sep=",")
  } else {
    tmp<-read.table(files[i], header=FALSE, sep=",")
    dataset<-rbind(dataset, tmp)
  }
}  
HH_ARSA<-dataset
  # then remove dataset and start again with next files
rm(dataset)
rm(tmp)
setwd("~/MSFD-QA-GFSM-A-DP/Data/National Submissions/Spain_18-03-2016/HH_SPNGFS")
files <- list.files()

for (i in 1:length(files)){
  
  # if the merged dataset doesn't exist, create it
  if (i==1){
    dataset <- read.table(files[i], header=FALSE, sep=",")
  } else {
    tmp<-read.table(files[i], header=FALSE, sep=",")
    dataset<-rbind(dataset, tmp)
  }
}  
HH_SPNGFS<-dataset
# then remove dataset and start again with next files
rm(dataset)
rm(tmp)

setwd("~/MSFD-QA-GFSM-A-DP/Data/National Submissions/Spain_18-03-2016/HL_ARSAsComplete")
files <- list.files()

for (i in 1:length(files)){
  
  # if the merged dataset doesn't exist, create it
  if (i==1){
    dataset <- read.table(files[i], header=FALSE, sep=",")
  } else {
    tmp<-read.table(files[i], header=FALSE, sep=",")
    dataset<-rbind(dataset, tmp)
  }
}  
HL_ARSA<-dataset
# then remove dataset and start again with next files
rm(dataset)
rm(tmp)

setwd("~/MSFD-QA-GFSM-A-DP/Data/National Submissions/Spain_18-03-2016/HL_SPNGFS")
files <- list.files()

for (i in 1:length(files)){
  
  # if the merged dataset doesn't exist, create it
  if (i==1){
    dataset <- read.table(files[i], header=FALSE, sep=",")
  } else {
    tmp<-read.table(files[i], header=FALSE, sep=",")
    dataset<-rbind(dataset, tmp)
  }
}  
HL_SPNGFS<-dataset
# then remove dataset and start again with next files
rm(dataset)
rm(tmp)

setwd("~/MSFD-QA-GFSM-A-DP/Data/National Submissions/Spain_18-03-2016/HL_SPPORC")
files <- list.files()

for (i in 1:length(files)){
  
  # if the merged dataset doesn't exist, create it
  if (i==1){
    dataset <- read.table(files[i], header=FALSE, sep=",")
  } else {
    tmp<-read.table(files[i], header=FALSE, sep=",")
    dataset<-rbind(dataset, tmp)
  }
}  
HL_SPPORC<-dataset
# then remove dataset and start again with next files
rm(dataset)
rm(tmp)


setwd("~/MSFD-QA-GFSM-A-DP/Data/National Submissions/Spain_18-03-2016/HH_SPPORC")
files <- list.files()

for (i in 1:length(files)){
  
  # if the merged dataset doesn't exist, create it
  if (i==1){
    dataset <- read.table(files[i], header=TRUE, sep=",")
  } else {
    tmp<-read.table(files[i], header=TRUE, sep=",")
    dataset<-rbind(dataset, tmp)
  }
}  
HH_SPPORC<-dataset
# then remove dataset and start again with next files
rm(dataset)
rm(tmp)

# Reset Working Directory
setwd("~/MSFD-QA-GFSM-A-DP")

# Sort out spanish data frames so they match the others
names(HH_SPNGFS)<-c("RecordType", "Quarter", "Country" , "Ship" , "Gear" , 
                    "SweepLngt", "GearExp",  "DoorType", "StNo" , "HaulNo" , "Year",
                    "month","Day", "TimeShot", "Stratum", "HaulDur", "DayNight", "ShootLat","ShootLong",
                    "HaulLat", "HaulLong", "StatRec" , "Depth", "HaulVal", "HydroStNo",    "StdSpecRecCode",   "BycSpecRecCode",
                    "DataType", "Netopening","Rigging" ,"Tickler" ,"Distance",    "Warplngt","Warpdia"  ,   "WarpDen" ,
                    "DoorSurface",   "DoorWgt",    "DoorSpread",   "WingSpread",
                    "Buoyancy",  "KiteDim",    "WgtGroundRope",   "TowDir",
                    "GroundSpeed", "SpeedWater",    "SurCurDir",   "SurCurSpeed",
                    "BotCurDir","BotCurSpeed",  "WindDir",   "WindSpeed",
                    "SwellDir","SwellHeight",   "SurTemp",   "BotTemp",
                    "SurSal","BotSal" , "ThermoCline",   "ThClineDepth")
HH_SPNGFS$Survey<-"SPNGFS"
HH_SPNGFS$DateofCalculation<-"NA"
names(HH_ARSA)<-c("RecordType", "Quarter", "Country" , "Ship" , "Gear" , 
                  "SweepLngt", "GearExp",  "DoorType", "StNo" , "HaulNo" , "Year",
                  "month"    ,         "Day"           ,    "TimeShot"     ,     "Stratum"          ,
                  "HaulDur"   ,        "DayNight"      ,    "ShootLat"      ,    "ShootLong"        ,
                  "HaulLat"    ,       "HaulLong"      ,    "StatRec"        ,   "Depth"            ,
                  "HaulVal"     ,      "HydroStNo"     ,    "StdSpecRecCode" ,   "BycSpecRecCode"   ,
                  "DataType"     ,     "Netopening"    ,    "Rigging"         ,  "Tickler"          ,
                  "Distance"      ,    "Warplngt"      ,    "Warpdia"        ,   "WarpDen"          ,
                  "DoorSurface"    ,   "DoorWgt"       ,    "DoorSpread"     ,   "WingSpread"       ,
                  "Buoyancy"        ,  "KiteDim"       ,    "WgtGroundRope"  ,   "TowDir"           ,
                  "GroundSpeed"      , "SpeedWater"    ,    "SurCurDir"      ,   "SurCurSpeed"      ,
                  "BotCurDir"         ,"BotCurSpeed"     ,  "WindDir"        ,   "WindSpeed"        ,
                  "SwellDir" ,         "SwellHeight"    ,   "SurTemp"        ,   "BotTemp"          ,
                  "SurSal"    ,        "BotSal"        ,    "ThermoCline"    ,   "ThClineDepth"     )

HH_ARSA$Survey<-"SP_ARSA"
HH_ARSA$DateofCalculation<-"NA"
names(HL_ARSA)

summary(HL_ARSA)
names(HL_ARSA)<-c("RecordType", "Quarter","Country",          
                  "Ship", "Gear","SweepLngt", "GearExp",         
                  "DoorType", "StNo","HaulNo","Year",        
                  "SpecCodeType","SpecCode","SpecVal","Sex",              
                  "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                  "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                  "HLNoAtLngt")
HL_ARSA$Survey<-"SP_ARSA"
HL_ARSA$DateofCalculation<-"NA"
HL_ARSA$Valid_Aphia<-HL_ARSA$SpecCode 
names(HL_SPNGFS)<-c("RecordType", "Quarter","Country",          
                    "Ship", "Gear","SweepLngt", "GearExp",         
                    "DoorType", "StNo","HaulNo","Year",        
                    "SpecCodeType","SpecCode","SpecVal","Sex",              
                    "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                    "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                    "HLNoAtLngt")
HL_SPNGFS$Survey<-"SPNGFS"
HL_SPNGFS$DateofCalculation<-"NA"
HL_SPNGFS$Valid_Aphia<-HL_SPNGFS$SpecCode 

names(HL_SPPORC)<-c("RecordType", "Quarter","Country",          
                  "Ship", "Gear","SweepLngt", "GearExp",         
                  "DoorType", "StNo","HaulNo","Year",        
                  "SpecCodeType","SpecCode","SpecVal","Sex",              
                  "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                  "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                  "HLNoAtLngt")
HL_SPPORC$Survey<-"SP_PORC"
HL_SPPORC$DateofCalculation<-"NA"
HL_SPPORC$Valid_Aphia<-HL_SPPORC$SpecCode 

HH_SPPORC$Survey<-"SP_PORC"
HH_SPPORC$DateofCalculation<-"NA"
