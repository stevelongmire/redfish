#
#Start date 7/6/2021
#
#This is the r script for exploratory analysis of the output FACT data
install.packages('readr')
install.packages('tibble')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('plotly')
install.packages('ggplot2')
install.packages('remotes')
install.packages('ggmap')
install.packages('dplyr')
install.packages('geosphere')
install.packages('rgdal')
install.packages('raster')
install.packages('mapdata')
install.packages('maptools')
install.packages('geosphere')
install.packages('igraph')
install.packages('viridis')
install.packages("data.table")
install.packages("sf")
install.packages('gganimate')
install.packages("rnaturalearth")
install.packages("beepr")
install.packages("gifski")
install.packages("devtools")
install.packages('lunar')
install.packages("httr", dependencies = TRUE)
install.packages("ncdf4",dependencies = TRUE)
install.packages("sp", dependencies = TRUE)
install.packages("devtools")
devtools::install_github("rmendels/xtractomatic")
devtools::install_github("rossdwyer/VTrack")


#install_url("https://gitlab.oceantrack.org/GreatLakes/glatos/-/archive/master
         #   /glatos-master.zip",
          #  build_opts = c("--no-resave-data", "--no-manual"))


library(glatos)
library(readr)
library(tibble)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(remotes)
library(ggmap)
library(dplyr)
library(geosphere)
library(rgdal)
library(raster)
library(mapdata)
library(maptools)
library(geosphere)
library(igraph)
library(viridis)
library(data.table)
library(sf)
library(gganimate)
library(rnaturalearth)
library(beepr)
library(gifski)
library(devtools)
library(lunar)
library(httr)
library(ncdf4)
library(sp)






##################################################################################
#upload data sets from the FACT output using the GLATOS package and their 
#FACT Network command for reading csv


dets2020 <- read_otn_detections("data/whitt_matched_detections_2020.csv")
dets2019 <- read_otn_detections("data/whitt_matched_detections_2019.csv")

reds <- read.csv("data/Red_Drum_Inventory.csv", stringsAsFactors = FALSE)

  
  
dets <- rbind(dets2019, dets2020)
###################################################################################
####################################################################################

#make sure the data is clean and correct

###########################################

#reds data
#make a new dataset for post-cleaning
reds2 <- reds 

#check for spelling errors or typos
unique(reds2$Location)

#adjust the dates
reds2$Date <- as.Date(reds2$Date)

#set the time using asPOSIXCT
str(reds2$Time)

#concatenate Date and Time into one column before using asPOSIXct
reds2$DateTime <- paste(reds2$Date, reds2$Time)


  reds2$DateTime <- as.POSIXct(reds2$DateTime, tz = 'EST')

#nice work

#check for spelling errors or typos
unique(reds2$Location)

##########################################
#dets data
#make a new dataset for post-cleaning

dets2 <- dets

#create a new column for EST
  dets2$detection_timestamp_est <- dets2$detection_timestamp_utc

#use attr to convert new column fro UTC to EST
  attr(dets2$detection_timestamp_est,'tzone') <-'EST'

#check to make sure the time actually shifted
class(dets2$detection_timestamp_est)
  head(dets2$detection_timestamp_est)
#######
#make a new column to designate fish by tag number


as.character(dets2$transmitter_id)

dets2$Fish_ID <- dets2$transmitter_id

  dets2$Fish_ID <- gsub("A69-1602-23320" , "1" , dets2$Fish_ID)
  dets2$Fish_ID <- gsub("A69-1602-23321" , "2" , dets2$Fish_ID)
  dets2$Fish_ID <- gsub("A69-1602-23317" , "3" , dets2$Fish_ID)
  dets2$Fish_ID <- gsub("A69-1602-23318" , "4" , dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23319" , "5" , dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23322" , "6" , dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23325" , "7" , dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23324" , "8" , dets2$Fish_ID)
  dets2$Fish_ID <- gsub("A69-1602-23326" , "9" , dets2$Fish_ID)
  dets2$Fish_ID <- gsub("A69-1602-23323" , "10", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23327" , "11", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23328" , "12", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23329" , "13", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23330" , "14", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23331" , "15", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23332" , "16", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23333" , "17", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23334" , "18", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23335" , "19", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-23336" , "20", dets2$Fish_ID)       
  #dets2$Fish_ID <- gsub("A69-1602-59219" , "21", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59220" , "22", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59221" , "23", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59222" , "24", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59223" , "25", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59224" , "26", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59225" , "27", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59226" , "28", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59227" , "29", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-59228" , "30", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53424" , "31", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53425" , "32", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53426" , "33", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53428" , "34", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53427" , "35", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53429" , "36", dets2$Fish_ID)       
  dets2$Fish_ID <- gsub("A69-1602-53430" , "37", dets2$Fish_ID)    
  dets2$Fish_ID <- gsub("A69-1602-53431" , "38", dets2$Fish_ID)
  #has to be done after next step
  #dets2$Fish_ID <- gsub("A69-1602-59219" , "39", dets2$Fish_ID)

  #We have to make sure fish 39 is properly labeled in the dataset because 
  #that tag was re-released
 
  
  #use a dplyr mutate case_when function
dets2 <- dets2 %>%  
  mutate(transmitter_id = 
  case_when(detection_timestamp_est >= as.POSIXct("2020-11-06 14:05:00") 
                      & transmitter_id == "A69-1602-59219"~ 
                      "A69-1602-59219R", TRUE ~ transmitter_id))

unique(dets2$transmitter_id)

#now designate fish 39 and fish 21
dets2$Fish_ID <- gsub("A69-1602-59219R" , "39", dets2$Fish_ID)
dets2$Fish_ID <- gsub("A69-1602-59219" , "21", dets2$Fish_ID) 

  unique(dets2$Fish_ID)
  
  
  
unique(dets2$transmitter_id)
#check to make sure there are no missed tag conversions  
unique(dets2$Fish_ID)

#####################################################
#now we should be good, let's start seeing what our data looks like

  count(dets2, Fish_ID)

#lots of fish have only been detected once. Let's try graphing to visualize
  
  #switch to as numeric to organize x-axis in sequential order
   dets2$Fish_ID <- as.numeric(dets2$Fish_ID)  
  
#quick plot to visualize
   ggplot(dets2, aes(y = Fish_ID )) +
  geom_histogram(stat = "count")

#let's try to plot by number of occurences at transceiver site
   ggplot(dets2, aes(y = station)) +
     geom_histogram(stat = "count")

   #how many folks have receivers out there?
   unique(dets2$detectedby)
  #6
   
   #lets plot to compare detections
   ggplot(dets2, aes(y= detectedby)) +
     geom_histogram (stat = "count")


