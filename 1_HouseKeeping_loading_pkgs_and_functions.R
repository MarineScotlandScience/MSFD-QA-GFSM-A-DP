###################
# Version Control #
###################

# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
citation()
###############
# Script Info #
###############
# This is Script 1 of X 
# The purpose of this script is to set up the working directory for this project,
# do general house keeping, insure all functions and packages are uploaded.
# In general the goals of this scriptin series are outline below:
# Goal 1. Check survey data for outliers and query with data providers
#             This code has been re-run to insure all errors that have been 
#             reported have been removed from DATRAS, otherwise a "fix" is used
# Goal 2. Derive Surveys and fix errors not updated in DATRAS
# Goal 3. Estimate missing/incorrect gear parameters
# Goal 4. Calculate Swept Area and Swepth Volume
# Goal 5. Estimate missing/incorrect biological parameters
# Goal 6. Calculate Swept Area densities
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: 
# DATA SOURCE: DATRAS DOWNLOAD 22-02-2016/18-03-2016
# Updated with more recent download Feb 2016
# Final Download from DATRAS 09-05-2016 - this is the version 
# that is provided in the associated sharepoint file.

#################
## Housekeeping## 
#################
# Remove files from R Global Environment 
rm(list = ls())
##Check proxy settings##
Sys.getenv("http_proxy")
## Fix proxy settings (; should be :)
Sys.setenv(http_proxy="http://192.168.41.8:80")
## this proxy is required for my system if no proxy is required use:
# Sys.setenv(http_proxy="")
############
# PACKAGES #
############
# library(devtools) - only required to download the rICES package from github
#citation("devtools")
#devtools::install_github("ices-dk/rICES")
# library(rICES) - only required if downloading directly from DATRAS
#citation("rICES")
library(ggplot2) #for pretty plots
#citation("ggplot2")
library(data.table) #for dealing with big data
#citation("data.table")
library(reshape2)
#citation("reshape2")
library(arm)
#citation("arm")
library(car)
#citation("car")
library(DMwR) #for lofactor(..)
#citation("DMwR")
# library(DATRAS) only required if downloading from DATRAS directly - see getDATRAS
# function if this isn't loading as required.
#citation("DATRAS")
library(lme4) #for mixed models
#citation("lme4")
library(plyr) #for summariesing data
#citation("plyr")
library(marmap) #for depth estimates
#citation("marmap")
library(colorspace)
library(plot3D)
library(rgl)
library(Hmisc)
library(MuMIn)
library(mapplots)
library(class)
##################
# Load functions #
##################

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371 # Mean radius (km) of the earth
  d <- R * c
  return(d)
}
# DATRAS function iscurrently being updated - worth checking in if it is ready
#' getDATRASfun.R

#' Extracts age data files from DATRAS
#' @param record, survey, startyear, endyear, startquarter, endquarter, parallel = FALSE, cores = NULL
#' @return none
#' @seealso none
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords download, DATRAS, survey, age, length
#' @examples \dontrun{
#'  getDATRAS()
#' }
#' @export
#
getDATRAS <- function(record, survey, startyear, endyear, quarters, parallel = FALSE, cores = NULL) {
  library(XML)
  library(doParallel)
  library(parallel)
  library(foreach)
  library(data.table)
  strt <- Sys.time()
  #
  seqYear <- startyear:endyear
  #
  if(!record %in% c("HL", "HH", "CA") ) stop("Please specify record type: HH (haul meta-data),
                                             HL (Species length-based data),
                                             CA (species age-based data)")
  getURL <- apply(expand.grid(record, survey, seqYear, quarters),
                  1,
                  function(x) paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/get",
                                     x[1],
                                     "data?survey=", x[2],
                                     "&year=", x[3],
                                     "&quarter=", x[4]))
  #
  if(parallel == TRUE) {
    if(missing("cores")) stop("Please specify how many cores you wish to devote to this task.")
    #
    unregister <- function() {
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    } # close unregister
    #
    cl <- makeCluster(cores)
    registerDoParallel(cores = cl)
    #
    getDATA <- foreach(temp = getURL,
                       .combine = function(...) rbindlist(list(...), fill = TRUE),
                       .multicombine = T,
                       .inorder = F,
                       .maxcombine = 1000,
                       .packages = c("XML", "data.table")) %dopar% {
                         data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                                function(x) xmlSApply(x, xmlValue))))
                       } # close foreach %dopar%
    stopCluster(cl)
    unregister()
  } # close parallel == TRUE
  #
  if(parallel == FALSE) {
    getDATA <- foreach(temp = getURL,
                       .combine = function(...) rbindlist(list(...), fill = TRUE),
                       .multicombine=T,
                       .inorder=F,
                       .maxcombine=1000,
                       .packages = c("XML", "data.table")) %do% {
                         data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                                function(x) xmlSApply(x, xmlValue))))
                       } # close foreach %do%
  } # close parallel == FALSE
  print(Sys.time()-strt)
  return(getDATA)
} # close function
#
na.false <- function(x) {return(replace(x, which(is.na(x)), FALSE))}

# Function to compare if two dataset contain the same information
compare_function <- function(x.1,x.2,...){
  x.1p <- do.call("paste", x.1)
  x.2p <- do.call("paste", x.2)
  x.1[! x.1p %in% x.2p, ]
}

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
#########################
# Set Working Directory #
#########################
setwd("~/MSFD-QA-GFSM-A-DP/Data_Cleaning_Process")
