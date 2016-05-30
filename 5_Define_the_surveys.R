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
invalidhauls <- HH[!HH$HaulVal=='V']
write.csv(invalidhauls, "all_invalid_hauls_26-05-2016.csv")
hauls <- HH[HH$HaulVal=='V' ]
# check this has deleted all the Invalid hauls
summary(as.factor(HH$HaulVal))
summary(as.factor(hauls$HaulVal))
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
           "DoorSpread","WingSpread","GroundSpeed","SpeedWater","UniqueID", "Shipchanged")
others <- colnames(hauls)[!colnames(hauls) %in% keepers]
hauls[, c(others):= NULL]

str(hauls)

# for all surveys we will not look at any data prior to 1983
# delete all hauls prior to 1983
pre1983<-subset(hauls, Year<1983) # this removes 4623 hauls from the IBTS Q1
write.csv(pre1983, "deleted_older_hauls_26-05-2016.csv")
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
write.csv(non_standard_gear, "deleted_non_standard_gear_hauls_26-05-2016.csv")
# What is the standard tow duration
# Delete hauls that are outside of the standard time 
str(hauls)
hauls$HaulDur<-(as.numeric(hauls$HaulDur))
smallhauls<- subset(hauls, HaulDur<12 , )
hauls<-subset(hauls, HaulDur>12 , )
write.csv(smallhauls, "deleted_too_short_hauls_26-05-2016.csv")
largehauls<- subset(hauls, HaulDur>67 ,)
hauls<- subset(hauls, HaulDur<67 , )
write.csv(largehauls, "deleted_too_long_gear_hauls_26-05-2016.csv")
summary(as.numeric(hauls$HaulDur))
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
summary(as.factor(hauls$Quarter[hauls$Survey=="SP_PORC"]))
hauls$New_UniqueID[hauls$Survey=="SP_PORC"]<-paste(hauls$Survey[hauls$Survey=="SP_PORC"],
                                                  hauls$Year[hauls$Survey=="SP_PORC"],
                                                  hauls$Quarter[hauls$Survey=="SP_PORC"], 
                                                  hauls$Ship[hauls$Survey=="SP_PORC"], 
                                                  hauls$HaulNo[hauls$Survey=="SP_PORC"],
                                                  hauls$Gear[hauls$Survey=="SP_PORC"], sep="/")

sp_hauls<-(hauls$New_UniqueID[hauls$Survey=="SP_PORC"])
# Note: the HL files are set in Q 3 while the HH files had Q 4 this causes an 
# issue in linking the HL and HH file directly
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
write.csv(Car_ship, "Samples_removed_unsuitable_boat_26-05-2016.csv")
# Ship SOL when used on in the IBTS isn't suitable as GOV was not standard 
SOL_ship<-subset(hauls, Ship=="SOL"&Year==1992,)
list<-SOL_ship$UniqueID
hauls<-subset(hauls, !UniqueID%in%list, )
write.csv(SOL_ship, "Samples_removed_unsuitable_boat1_26-05-2016.csv")
# Remove Belgium Data - not a long enough time series (only one year of data)
belgium<-subset(hauls, Country=="BEL",)
list<-belgium$UniqueID
hauls<-subset(hauls, !UniqueID%in%list,)
write.csv(belgium, "samples_removed_time_series_not_sufficent_26-05-2016.csv")
#####################################
## Define the standard survey area ##
#####################################
# Last step is two standardise the survey area 
# i.e. area sampled in > 50% of years 
# we will clean all 48640 hauls prior to ommiting hauls from 
# the MSFD standardised area 
# as the hauls are useful in some cases.

write.csv(hauls, "Working_HH_file_26_05_2016.csv")
## Check Unique IDS are unique
check<-unique(hauls$UniqueID)
findduplicates<-hauls[duplicated(hauls$UniqueID),]
length(unique(hauls$UniqueID))
list<-findduplicates$UniqueID
bad_data<-subset(hauls, UniqueID%in%list,)

length(unique(HL$UniqueID))
bad_data_fish<-subset(HL, UniqueID%in%list,)
# okay so 46 values aren't unique - solve this by so sort of trickery
list1<-bad_data$StNo
list2<-bad_data$UniqueID
list3<-bad_data$TimeShot

# okies lets solve this shitty problem for all the hauls and HL files together
# merge the HL files

HL$Quarter[HL$Survey=="SP_PORC" & HL$Quarter=="4"] <-3
HL$Quarter[HL$UniqueID=="SP_PORC/2002/1/EZA/49/PORB"] <-3
HL$New_UniqueID[HL$Survey=="SP_PORC"]<-paste(HL$Survey[HL$Survey=="SP_PORC"],
                                             HL$Year[HL$Survey=="SP_PORC"],
                                             HL$Quarter[HL$Survey=="SP_PORC"], 
                                             HL$Ship[HL$Survey=="SP_PORC"], 
                                             HL$HaulNo[HL$Survey=="SP_PORC"],
                                             HL$Gear[HL$Survey=="SP_PORC"], sep="/")


hauls$New_UniqueID<-hauls$UniqueID
hauls$New_UniqueID[hauls$UniqueID=="SPNGFS/1993/4/CDS/35/BAK"&hauls$TimeShot=="1332"]<-"SPNGFS/1993/4/CDS/35/BAK/a"
hauls$New_UniqueID[hauls$UniqueID=="SPNGFS/1993/4/CDS/35/BAK"&hauls$TimeShot=="1559"]<-"SPNGFS/1993/4/CDS/35/BAK/b"
hauls$New_UniqueID[hauls$UniqueID=="SPNGFS/1995/4/CDS/94/BAK"&hauls$TimeShot=="1613"]<-"SPNGFS/1995/4/CDS/94/BAK/a"
hauls$New_UniqueID[hauls$UniqueID=="SPNGFS/1995/4/CDS/94/BAK"&hauls$TimeShot=="1546"]<-"SPNGFS/1995/4/CDS/94/BAK/b"
hauls$New_UniqueID[hauls$UniqueID=="SPNGFS/1998/4/CDS/17/BAK"&hauls$TimeShot=="1452"]<-"SPNGFS/1998/4/CDS/17/BAK/a"
hauls$New_UniqueID[hauls$UniqueID=="SPNGFS/1998/4/CDS/17/BAK"&hauls$TimeShot=="650"]<-"SPNGFS/1998/4/CDS/17/BAK/b"
        
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="17"]<-"NIGFS/1999/3/LF/1/ROT/17"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="35"]<-"NIGFS/1999/3/LF/1/ROT/35"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="46"]<-"NIGFS/1999/3/LF/1/ROT/46"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="48"]<-"NIGFS/1999/3/LF/1/ROT/48"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="50"]<-"NIGFS/1999/3/LF/1/ROT/50"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="51"]<-"NIGFS/1999/3/LF/1/ROT/51"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="56"]<-"NIGFS/1999/4/LF/1/ROT/56"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="61"]<-"NIGFS/1999/4/LF/1/ROT/61"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="63"]<-"NIGFS/1999/4/LF/1/ROT/63"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="64"]<-"NIGFS/1999/4/LF/1/ROT/64"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="70"]<-"NIGFS/1999/3/LF/1/ROT/70"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="71"]<-"NIGFS/1999/3/LF/1/ROT/71"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="73"]<-"NIGFS/1999/3/LF/1/ROT/73"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="75"]<-"NIGFS/1999/3/LF/1/ROT/75"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="76"]<-"NIGFS/1999/4/LF/1/ROT/76"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="77"]<-"NIGFS/1999/4/LF/1/ROT/77"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="79"]<-"NIGFS/1999/4/LF/1/ROT/79"
hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="81"]<-"NIGFS/1999/3/LF/1/ROT/81"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="83"]<-"NIGFS/1999/3/LF/1/ROT/83"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="88"]<-"NIGFS/1999/3/LF/1/ROT/88"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="90"]<-"NIGFS/1999/4/LF/1/ROT/90"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="92"]<-"NIGFS/1999/4/LF/1/ROT/92"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="93"]<-"NIGFS/1999/4/LF/1/ROT/93"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="94"]<-"NIGFS/1999/4/LF/1/ROT/94"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="96"]<-"NIGFS/1999/3/LF/1/ROT/96"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="97"]<-"NIGFS/1999/3/LF/1/ROT/97"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="99"]<-"NIGFS/1999/3/LF/1/ROT/99"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo== "100"]<-"NIGFS/1999/3/LF/1/ROT/100"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="101"]<-"NIGFS/1999/3/LF/1/ROT/101"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="102"]<-"NIGFS/1999/4/LF/1/ROT/102"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="103"]<-"NIGFS/1999/3/LF/1/ROT/103"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="105"]<-"NIGFS/1999/4/LF/1/ROT/105"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo=="208"]<-"NIGFS/1999/3/LF/1/ROT/208"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/3/LF/1/ROT"&hauls$StNo== "216"]<-"NIGFS/1999/3/LF/1/ROT/216"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="242"]<-"NIGFS/1999/4/LF/1/ROT/242"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="243"]<-"NIGFS/1999/4/LF/1/ROT/243"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="245"]<-"NIGFS/1999/4/LF/1/ROT/245"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="246"]<-"NIGFS/1999/4/LF/1/ROT/246"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo== "249"]<-"NIGFS/1999/4/LF/1/ROT/249"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo== "250"]<-"NIGFS/1999/4/LF/1/ROT/250"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="256"]<-"NIGFS/1999/4/LF/1/ROT/256"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/1999/4/LF/1/ROT"&hauls$StNo=="259"]<-"NIGFS/1999/4/LF/1/ROT/259"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/2001/4/LF/37/ROT"&hauls$StNo=="94"]<-"NIGFS/2001/4/LF/37/ROT/94"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/2001/4/LF/37/ROT"&hauls$StNo=="106"]<-"NIGFS/2001/4/LF/37/ROT/106"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/2002/1/LF/9/ROT"&hauls$StNo=="93"]<-"NIGFS/2002/1/LF/9/ROT/93"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/2002/1/LF/9/ROT"&hauls$StNo=="243"]<-"NIGFS/2002/1/LF/9/ROT/243"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/2002/1/LF/15/ROT"&hauls$StNo=="17"]<-"NIGFS/2002/1/LF/15/ROT/17"
  hauls$New_UniqueID[hauls$UniqueID=="NIGFS/2002/1/LF/15/ROT"&hauls$StNo=="250"]<-"NIGFS/2002/1/LF/15/ROT/205"
  HL$New_UniqueID<-HL$UniqueID
  HL$New_UniqueID[HL$UniqueID=="SPNGFS/1993/4/CDS/35/BAK"&HL$TimeShot=="1332"]<-"SPNGFS/1993/4/CDS/35/BAK/a"
  HL$New_UniqueID[HL$UniqueID=="SPNGFS/1993/4/CDS/35/BAK"&HL$TimeShot=="1559"]<-"SPNGFS/1993/4/CDS/35/BAK/b"
  HL$New_UniqueID[HL$UniqueID=="SPNGFS/1995/4/CDS/94/BAK"&HL$TimeShot=="1613"]<-"SPNGFS/1995/4/CDS/94/BAK/a"
  HL$New_UniqueID[HL$UniqueID=="SPNGFS/1995/4/CDS/94/BAK"&HL$TimeShot=="1546"]<-"SPNGFS/1995/4/CDS/94/BAK/b"
  HL$New_UniqueID[HL$UniqueID=="SPNGFS/1998/4/CDS/17/BAK"&HL$TimeShot=="1452"]<-"SPNGFS/1998/4/CDS/17/BAK/a"
  HL$New_UniqueID[HL$UniqueID=="SPNGFS/1998/4/CDS/17/BAK"&HL$TimeShot=="650"]<-"SPNGFS/1998/4/CDS/17/BAK/b"
  
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="17"]<-"NIGFS/1999/3/LF/1/ROT/17"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="35"]<-"NIGFS/1999/3/LF/1/ROT/35"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="46"]<-"NIGFS/1999/3/LF/1/ROT/46"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="48"]<-"NIGFS/1999/3/LF/1/ROT/48"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="50"]<-"NIGFS/1999/3/LF/1/ROT/50"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="51"]<-"NIGFS/1999/3/LF/1/ROT/51"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="56"]<-"NIGFS/1999/4/LF/1/ROT/56"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="61"]<-"NIGFS/1999/4/LF/1/ROT/61"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="63"]<-"NIGFS/1999/4/LF/1/ROT/63"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="64"]<-"NIGFS/1999/4/LF/1/ROT/64"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="70"]<-"NIGFS/1999/3/LF/1/ROT/70"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="71"]<-"NIGFS/1999/3/LF/1/ROT/71"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="73"]<-"NIGFS/1999/3/LF/1/ROT/73"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="75"]<-"NIGFS/1999/3/LF/1/ROT/75"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="76"]<-"NIGFS/1999/4/LF/1/ROT/76"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="77"]<-"NIGFS/1999/4/LF/1/ROT/77"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="79"]<-"NIGFS/1999/4/LF/1/ROT/79"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="81"]<-"NIGFS/1999/3/LF/1/ROT/81"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="83"]<-"NIGFS/1999/3/LF/1/ROT/83"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="88"]<-"NIGFS/1999/3/LF/1/ROT/88"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="90"]<-"NIGFS/1999/4/LF/1/ROT/90"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="92"]<-"NIGFS/1999/4/LF/1/ROT/92"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="93"]<-"NIGFS/1999/4/LF/1/ROT/93"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="94"]<-"NIGFS/1999/4/LF/1/ROT/94"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="96"]<-"NIGFS/1999/3/LF/1/ROT/96"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="97"]<-"NIGFS/1999/3/LF/1/ROT/97"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="99"]<-"NIGFS/1999/3/LF/1/ROT/99"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo== "100"]<-"NIGFS/1999/3/LF/1/ROT/100"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="101"]<-"NIGFS/1999/3/LF/1/ROT/101"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="102"]<-"NIGFS/1999/4/LF/1/ROT/102"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="103"]<-"NIGFS/1999/3/LF/1/ROT/103"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="105"]<-"NIGFS/1999/4/LF/1/ROT/105"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo=="208"]<-"NIGFS/1999/3/LF/1/ROT/208"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/3/LF/1/ROT"&HL$StNo== "216"]<-"NIGFS/1999/3/LF/1/ROT/216"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="242"]<-"NIGFS/1999/4/LF/1/ROT/242"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="243"]<-"NIGFS/1999/4/LF/1/ROT/243"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="245"]<-"NIGFS/1999/4/LF/1/ROT/245"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="246"]<-"NIGFS/1999/4/LF/1/ROT/246"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo== "249"]<-"NIGFS/1999/4/LF/1/ROT/249"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo== "250"]<-"NIGFS/1999/4/LF/1/ROT/250"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="256"]<-"NIGFS/1999/4/LF/1/ROT/256"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/1999/4/LF/1/ROT"&HL$StNo=="259"]<-"NIGFS/1999/4/LF/1/ROT/259"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/2001/4/LF/37/ROT"&HL$StNo=="94"]<-"NIGFS/2001/4/LF/37/ROT/94"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/2001/4/LF/37/ROT"&HL$StNo=="106"]<-"NIGFS/2001/4/LF/37/ROT/106"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/2002/1/LF/9/ROT"&HL$StNo=="93"]<-"NIGFS/2002/1/LF/9/ROT/93"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/2002/1/LF/9/ROT"&HL$StNo=="243"]<-"NIGFS/2002/1/LF/9/ROT/243"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/2002/1/LF/15/ROT"&HL$StNo=="17"]<-"NIGFS/2002/1/LF/15/ROT/17"
  HL$New_UniqueID[HL$UniqueID=="NIGFS/2002/1/LF/15/ROT"&HL$StNo=="250"]<-"NIGFS/2002/1/LF/15/ROT/205"
  
  ## Check Unique IDS are unique
  check<-unique(hauls$New_UniqueID)
  findduplicates<-hauls[duplicated(hauls$New_UniqueID),]
  length(unique(HL$New_UniqueID))
  # hopefully this solves some problems of the not so unique ids :)
  
  write.csv(hauls, "Working_HH_file_26_05_2016.csv")
  write.csv(HL, "Working_HL_file_26_05_2016.csv")
  
