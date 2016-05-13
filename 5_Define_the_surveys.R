###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 5 of X
# The next step is to define the surveys - after the genuine errors have been 
# checked/changes e.g. a haul recorded as 90mins but was actually 30mins would be
# deleted if this step occurred befor the checks are done.

#################
# Select Columns#
#################

# manually remove the individual countries data sets and work on data combined by gear, 
# see script 3_merge_datasets for details on how this is done 
# check data table names

# 10 data tables 5 HH and 5 HL, 
# we will focus on HH for the moment
# "HH_baka"  "HH_beams" "HH_gov"   "HH_nct"   "HH_rot" 
# check the structure of these data tables
str(HH_baka) # note numeric cols are correct

# Remove extra columns
# if(any(colnames(HH) == "V1")) HH[, ":=" (V1 = NULL)]

# Remove extra spaces from character strings
# cnames <- colnames(HH)
# for(cname in cnames) {
# set(HH, j = cname, value = gsub("[[:space:]]", "", HH[[cname]]))
# }

#######################
## Define the surveys##
#######################

# In section 1.2 "Overview od the Monitoring Product" (Moriarty & Greenstreet, 2016)
# We show the processes undertaken to standardise/exclude hauls from the data set
# Is the haul valid? yes retain, no remove

invalidhauls<-subset(HH_gov, HaulVal=="I", ) # 985 obs
write.csv(invalidhauls, "invalid_removed_gov.csv")
invalidhauls<-subset(HH_nct, HaulVal=="I", ) # 11 obs
write.csv(invalidhauls, "invalid_removed_nct.csv")
invalidhauls<-subset(HH_rot1, HaulVal=="I", ) # 18 obs
write.csv(invalidhauls, "invalid_removed_rot.csv")
invalidhauls<-subset(HH_baka, HaulVal=="I", ) # 0 obs
write.csv(invalidhauls, "invalid_removed_baka.csv")
invalidhauls<-subset(HH_beams, HaulVal=="I", ) # 430 obs
write.csv(invalidhauls, "invalid_removed_beams.csv")

# Select only valid hauls 
# Select important columns
# I'm making a copy of the HH files here so that I retain the original file
# This means if I make a mistake I can easily go back to this and redo without
# needing to go back to scripts 1-3.
HH<-rbind(HH_gov, HH_baka, HH_nct, HH_rot1, HH_beams, fill=T)
names (HH)
# For all Surveys 
# Delete invaild hauls (this step is included above)
invalidhauls <- HH[HH_gov$HaulVal=='I']
write.csv(invalidhauls, "all_invalid_hauls_10-05-2016.csv")
hauls <- HH[HH_gov$HaulVal=='V' ]
# we can uneversially drop cols that are not of use to us going forward
# RecordType is HH for all - delete
# other cols deleted: Warpdia, WarpDen, DoorSurface, DoorWgt, Buoyancy,
# KiteDim, WgtGroundRope, TowDir, SurCurDir, SurCurSpeed, BotCurDir, BotCurSpeed,
# WindDir, WindSpeed, SwellDir, SwellHeight, SurTemp, BotTemp, SurSal, BotSal,
# ThermoCline, ThClineDepth, DateofCalculation

keepers<-c("Survey","Quarter","Country", "Ship","Gear", "SweepLngt","GearExp",          
           "DoorType","StNo","HaulNo","Year","month", "Day","TimeShot","Stratum",
           "HaulDur","DayNight","ShootLat","ShootLong", "HaulLat","HaulLong",
           "StatRec","Depth","HaulVal","StdSpecRecCode","BycSpecRecCode",  
           "DataType","Netopening","Rigging","Tickler", "Distance","Warplngt",             
           "DoorSpread","WingSpread","GroundSpeed","SpeedWater","UniqueID")
others <- colnames(hauls)[!colnames(hauls) %in% keepers]
hauls[, c(others):= NULL]

str(hauls)

# for all surveys we will not look at any data prior to 1983
# delete all hauls prior to 1983
pre1983<-subset(hauls, Year<1983) # this removes 4623 hauls from the IBTS Q1
write.csv(pre1983, "deleted_older_hauls_10-05-2016.csv")
hauls<-subset(hauls, Year>1982, )
# Gears differ between regions and Surveys we retain several gear types 
# GOV 35383 records 
# PORB 1109 records
# BAK 4309 records
# NCT 862 records
# ROT 2325 records
# BT8  3808 records 
# BT4A 5078 records 
# BT7 651records 
# BT4P 443 records
# BT4S  488 records
# Delete:DHT   H18   VIN    HT   BOT   SOV   FOT   HOB   KAB   H12   GRT   ABD  CAR   
# No :    90    63     0     0     0     0     0     9     0     0    87   520  157            
list<-c('GOV','PORB','BAK','NCT','ROT','BT8','BT4A','BT7','BT4P','BT4S')
non_standard_gear<-subset(hauls, !Gear%in%list,)
hauls<-subset(hauls, Gear%in%list, )
summary(hauls$Gear)
write.csv(non_standard_gear, "deleted_non_standard_gear_hauls_10-05-2016.csv")
# What is the standard tow duration
# Delete hauls that are outside of the standard time 
smallhauls<- subset(hauls, HaulDur<12 , )
hauls<-subset(hauls, HaulDur>12 , )
write.csv(smallhauls, "deleted_too_short_hauls_10-05-2016.csv")
largehauls<- subset(hauls, HaulDur>67 ,)
hauls<- subset(hauls, HaulDur<67 , )
write.csv(largehauls, "deleted_too_long_gear_hauls_10-05-2016.csv")

# Is the Survey Coordinated eg Q1 NS-IBTS
levels(as.factor(hauls$Survey))
# Delete NS-IBTS Quarter 2 and 4 surveys - related to the years of the stomach
quater2or4<-subset(hauls, Survey=="NS-IBTS"&Quarter==2&Year>1997|Quarter==4)

hauls<-subset(hauls,Survey=="NS-IBTS"&Quarter==3&Year>1997|Quarter==1
              |Survey=="EVHOE"|Survey=="FR-CGFS"
              |Survey=="IE-IGFS"|Survey=="SWC-IBTS"
              |Survey=="BTS"|Survey=="BTS-VIIa"|Survey=="NIGFS"
              |Survey=="PT-IBTS"|Survey=="ROCKALL"|Survey=="SPNGFS"
              |Survey=="SP_ARSA"|Survey=="SP_PORC")
# check quaters by survey
cols<-rainbow(13)
plot(hauls$Quarter, col=cols[as.factor(hauls$Survey)], pch=20)
quaterscheck<-ddply(hauls, c("Survey", "Quarter"),
                    summarise,
                    count=length(StNo))

hauls$QuarterCheck[hauls$Survey=="NS-IBTS" &
                     hauls$Quarter=="2"& hauls$Year>="1998"|hauls$Survey=="NS-IBTS" & hauls$Quarter=="4"
                   & hauls$Year>="1998"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="NS-IBTS" &
                hauls$Quarter=="2"& hauls$Year>="1998"|hauls$Survey=="NS-IBTS" & hauls$Quarter=="4"
              & hauls$Year>="1998"] <-3
hauls$QuarterCheck[hauls$Survey=="BTS" &
                     hauls$Quarter=="4"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="BTS" &
                hauls$Quarter=="4"] <-3
hauls$QuarterCheck[hauls$Survey=="BTS-VIIa" &
                     hauls$Quarter=="4"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="BTS-VIIa" &
                hauls$Quarter=="4"] <-3
hauls$QuarterCheck[hauls$Survey=="IE-IGFS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="IE-IGFS" &
                hauls$Quarter=="3"] <- 4
hauls$QuarterCheck[hauls$Survey=="PT-IBTS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="PT-IBTS" &
                hauls$Quarter=="3"] <- 4
hauls$QuarterCheck[hauls$Survey=="SWC-IBTS" &
                     hauls$Quarter=="2"] <-"changed to 1"
hauls$Quarter[hauls$Survey=="SWC-IBTS" &
                hauls$Quarter=="2"] <-1
hauls$QuarterCheck[hauls$Survey=="SP_PORC" &
                     hauls$Quarter=="4"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="SP_PORC" &
                hauls$Quarter=="4"] <-3
# additional mistake in SP_PORC/2002/1/EZA/49/PORB
# month wrong and quater wrong
hauls$month[hauls$UniqueID=="SP_PORC/2002/1/EZA/49/PORB"] <-9
hauls$Quarter[hauls$UniqueID=="SP_PORC/2002/1/EZA/49/PORB"] <-3
hauls$QuarterCheck[hauls$UniqueID=="SP_PORC/2002/1/EZA/49/PORB"] <-"changed to 4/month corrected"
hauls$QuarterCheck[hauls$Survey=="NIGFS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="NIGFS" &
                hauls$Quarter=="3"] <-4

######################
# Additional Tweaks ##
######################
# Ship CAR which is used on in the BTS isn't suitable as the biodiversity of fish 
# not adequately sampled - delete from product
summary(hauls$Ship)
Car_ship<-subset(hauls, Ship=="CAR",)
hauls<-subset(hauls, !Ship=="CAR", )
write.csv(Car_ship, "Samples_removed_unsuitable_boat_10-05-2016.csv")
# Ship SOL when used on in the IBTS isn't suitable as GOV was not standard 
SOL_ship<-subset(hauls, Ship=="SOL"&Year==1992,)
list<-SOL_ship$UniqueID
hauls<-subset(hauls, !UniqueID%in%list, )
write.csv(SOL_ship, "Samples_removed_unsuitable_boat1_10-05-2016.csv")

#####################################
## Define the standard survey area ##
#####################################
# Last step is two standardise the survey area 
# i.e. area sampled in > 50% of years 
# we will clean all 48640 hauls prior to ommiting hauls from 
# the MSFD standardised area 
# as the hauls are useful in some cases.

