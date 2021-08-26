#
#Start date 7/6/2021
#
#This is the r script for exploratory analysis of the output FACT data
install.packages('arcgisbinding')

install.packages('leaflet')
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

install.packages('VTrack')
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
3

#install_url("https://gitlab.oceantrack.org/GreatLakes/glatos/-/archive/master
         #   /glatos-master.zip",
          #  build_opts = c("--no-resave-data", "--no-manual"))


library(plyr)
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
library(maps)
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
library(leaflet)
library(Vtrack)





##################################################################################
#upload data sets from the FACT output using the GLATOS package and their 
#FACT Network command for reading csv

dets2021 <- read_otn_detections("data/whitt_matched_detections_2021.csv")
dets2020 <- read_otn_detections("data/whitt_matched_detections_2020.csv")
dets2019 <- read_otn_detections("data/whitt_matched_detections_2019.csv")

reds <- read.csv("data/Red_Drum_Inventory.csv", stringsAsFactors = FALSE)

receivers <- read.csv("data/receivers.csv")
  
dets <- rbind(dets2019, dets2020, dets2021)
unique(dets$station)
###################################################################################
####################################################################################

#make sure the data is clean and correct

###########################################

#reds data
#make a new dataset for post-cleaning
reds2 <- reds 

#check for spelling errors or typos
unique(reds2$Location)

#adjust the dates MAKE SURE THE EXCEL DATES ARE SET AS Y-m-d in the raw data
reds2$Date <- as.Date(reds2$Date)

#check
str(reds2$Date)

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
#create a case_when to deal with the fact that a tag was redeployed after 11/6/2020 
dets2 <- dets2 %>%  
    mutate(transmitter_id = 
             case_when(detection_timestamp_est >= as.POSIXct("2020-11-06 14:05:00") 
                       & transmitter_id == "A69-1602-59219"~ 
                         "A69-1602-59219R", TRUE ~ transmitter_id))  

#check
unique(dets2$transmitter_id)  
  
#make a new column to designate fish by tag number
dets2$Fish_ID <- dets2$transmitter_id
  

#check
unique(dets2$Fish_ID)

#now use a simple Base R function to replace all tags in the new column with 
#a simple Fish ID


  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23320"] <- 1
  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23321"] <- 2  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23317"] <- 3  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23318"] <- 4  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23319"] <- 5  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23322"] <- 6  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23325"] <- 7  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23324"] <- 8  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23326"] <- 9  
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23323"] <- 10 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23327"] <- 11 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23328"] <- 12 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23329"] <- 13 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23330"] <- 14 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23331"] <- 15 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23332"] <- 16 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23333"] <- 17 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23334"] <- 18 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23335"] <- 19 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-23336"] <- 20 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59219"] <- 21 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59220"] <- 22 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59221"] <- 23 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59222"] <- 24 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59223"] <- 25 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59224"] <- 26 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59225"] <- 27 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59226"] <- 28 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59227"] <- 29 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59228"] <- 30 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53424"] <- 31 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53425"] <- 32 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53426"] <- 33 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53428"] <- 34 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53427"] <- 35 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53429"] <- 36 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53430"] <- 37 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53431"] <- 38 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-59219R"] <- 39 
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53432"] <- 40
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-53433"] <- 41
  dets2$Fish_ID[dets2$Fish_ID == "A69-1602-63390"] <- 42

  
  
  #check to make sure there are no missed tag conversions  
unique(dets2$Fish_ID)

count(dets2, Fish_ID)
count(dets2, transmitter_id)
###
##let's create a new column for release location in the dets2
# create a new column copying Fish_ID so that we can change the values

dets2$fish_release_loc <- dets2$Fish_ID 

########################### NOTE THAT G-SUB DOES NOT WORK THIS TIME LIKE LAST TIME
###########################IT WILL REPLACE ALL TIMES A SINGLE DIGIT EXISTS IN A DOUBLE DIGIT 
###########################wE MUST USE A SIMPLE R function

dets2$fish_release_loc[dets2$fish_release_loc == 1]  <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 2]  <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 3]  <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 4]  <- "St. Augustine Inlet"    
dets2$fish_release_loc[dets2$fish_release_loc == 5]  <- "Vilano Jetty"       
dets2$fish_release_loc[dets2$fish_release_loc == 6]  <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 7]  <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 8]  <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 9]  <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 10] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 11] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 12] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 13] <- "St. Augustine Inlet"     
dets2$fish_release_loc[dets2$fish_release_loc == 14] <- "Matanzas Inlet"   
dets2$fish_release_loc[dets2$fish_release_loc == 15] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 16] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 17] <- "Vilano Bridge"      
dets2$fish_release_loc[dets2$fish_release_loc == 18] <- "Crescent Beach flats"       
dets2$fish_release_loc[dets2$fish_release_loc == 19] <- "Crescent Beach flats"       
dets2$fish_release_loc[dets2$fish_release_loc == 20] <- "Crescent Beach flats"       
dets2$fish_release_loc[dets2$fish_release_loc == 21] <- "Crescent Beach flats"       
dets2$fish_release_loc[dets2$fish_release_loc == 22] <- "Crescent Beach flats"       
dets2$fish_release_loc[dets2$fish_release_loc == 23] <- "Crescent Beach flats"       
dets2$fish_release_loc[dets2$fish_release_loc == 24] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 25] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 26] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 27] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 28] <- "St. Augustine Inlet"      
dets2$fish_release_loc[dets2$fish_release_loc == 29] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 30] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 31] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 32] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 33] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 34] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 35] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 36] <- "St. Augustine Beach"      
dets2$fish_release_loc[dets2$fish_release_loc == 37] <- "St. Augustine Inlet"   
dets2$fish_release_loc[dets2$fish_release_loc == 38] <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 39] <- "St. Augustine Inlet"
dets2$fish_release_loc[dets2$fish_release_loc == 40] <- "Matanzas Inlet"  
dets2$fish_release_loc[dets2$fish_release_loc == 41] <- "Matanzas Inlet"  
dets2$fish_release_loc[dets2$fish_release_loc == 42] <- "Matanzas Inlet"  


#now check to make sure there were no hiccups or unintended consequences
unique(dets2$fish_release_loc)
count(dets2, fish_release_loc)
############################################fish_release_loc##
#now we should be good, let's start seeing what our data looks like
count(dets2, Fish_ID)

#lots of fish have only been detected once. Let's try graphing to visualize
  
  #switch to as factor to organize x-axis in sequential order
   dets2$Fish_ID <- as.factor(dets2$Fish_ID)  
  
#quick plot to visualize
   ggplot(dets2, aes(y = Fish_ID , fill = fish_release_loc)) +
  geom_histogram(stat = "count")

#let's try to plot by number of occurences at transceiver site
   ggplot(dets2, aes(y = station, fill = fish_release_loc)) +
     geom_histogram(stat = "count")
   
   ################################################################
   ################################################################
   ###############################################################
   #weird stuff going on, so
   #create a subset for when detections occur in WHITR or WHITT
   dets3 <- dets2 %>% 
     filter(detectedby !="WHITT")
   
   WHITR <- dets2 %>% 
     filter(detectedby == "WHITR")
   WHITT <- dets2 %>% 
     filter(detectedby == "WHITT")
   
   count(dets3, Fish_ID)
   
   
   
    #export csv for gis
   write.csv(dets3, "dets_corrected.csv", row.names = F)

   ##################################################################
   ##################################################################
   ##################################################################
  
   #let's also get rid of the OTN ping in quebec
   dets3 <- dets3 %>% 
      filter(detectedby != "OTN.V2LENGLB")
   
   count(dets3, station)
   
   
   #data should be clean now
   #let's designate offshore and inshore sites and hide the ST40 station
   dets3$inshore_offshore <- dets3$deploy_long
   
   dets3 <- dets3 %>%
              mutate(
                 inshore_offshore = case_when(
                     inshore_offshore < -81.2081 ~ "inshore", 
                     inshore_offshore >= -81.2081 ~ "offshore"))

  
   
   #Fish_ID count by station
   ggplot(WHITR, aes(y = Fish_ID, fill = station)) +
     geom_histogram(stat = "count")

   #how many folks have receivers out there?
   unique(dets3$detectedby)
  #5
   
   count(dets, detectedby)
   
   #lets plot to compare total detections at each station
   ggplot(dets3, aes(y= station)) +
     geom_histogram (stat = "count", fill = "tan")
   
   ggplot(dets3, aes(y = detectedby)) +
      geom_histogram (stat = "count", fill = "orange")
   
   ###################################################   
######################################################################################
   #what is station ST40?
   
#it is in Quebec.... Fish_ID 12 has been reported once. 
   #it was detected on 10/16/2020
   #so let's see if Fish 12 was ever detected after that.....
   
 Fish_12 <-  dets3 %>% 
      filter(Fish_ID == "12") %>% 
      case_when(detection_timestamp_est >= POSIXct("2020-08-01"))
   
   #abacus plot
   
   abacus_plot(dets3, id)
   
   abacus_plot(dets3[dets3$Fish_ID == 1], location_col = 'station',
   main = 'Fish 1')
   
   abacus_plot(dets3[dets3$Fish_ID == 2,], location_col = 'station',
               main = 'Fish 2')
   abacus_plot(dets3[dets3$Fish_ID == 3,], location_col = 'station',
               main = 'Fish 3')
   abacus_plot(dets3[dets3$Fish_ID == 4,], location_col = 'station',
               main = 'Fish 4')
   abacus_plot(dets3[dets3$Fish_ID == 5,], location_col = 'station',
               main = 'Fish 5')
   abacus_plot(dets3[dets3$Fish_ID == 6,], location_col = 'station',
               main = 'Fish 6')
   abacus_plot(dets3[dets3$Fish_ID == 7,], location_col = 'station',
               main = 'Fish 7')
   abacus_plot(dets3[dets3$Fish_ID == 8,], location_col = 'station',
               main = 'Fish 8')
   abacus_plot(dets3[dets3$Fish_ID == 9,], location_col = 'station',
               main = 'Fish 9')
   abacus_plot(dets3[dets3$Fish_ID == 10,], location_col = 'station',
               main = 'Fish 10')
   abacus_plot(dets3[dets3$Fish_ID ==11,], location_col = 'station',
               main = 'Fish 11')
   abacus_plot(dets3[dets3$Fish_ID == 12,], location_col = 'station',
               main = 'Fish 12')
   abacus_plot(dets3[dets3$Fish_ID == 13], location_col = 'station',
               main = 'Fish 13')
   abacus_plot(dets3[dets3$Fish_ID == 14,], location_col = 'station',
               main = 'Fish 14')
   abacus_plot(dets3[dets3$Fish_ID == 15,], location_col = 'station',
               main = 'Fish 15')
   abacus_plot(dets3[dets3$Fish_ID == 16,], location_col = 'station',
               main = 'Fish 16')
   abacus_plot(dets3[dets3$Fish_ID == 17,], location_col = 'station',
               main = 'Fish 17')
   abacus_plot(dets3[dets3$Fish_ID == 18,], location_col = 'station',
               main = 'Fish 18')
   abacus_plot(dets3[dets3$Fish_ID == 19], location_col = 'station',
               main = 'Fish 19')
   abacus_plot(dets3[dets3$Fish_ID == 20,], location_col = 'station',
               main = 'Fish 20')
   abacus_plot(dets3[dets3$Fish_ID == 21,], location_col = 'station',
               main = 'Fish 21')
   abacus_plot(dets3[dets3$Fish_ID == 22,], location_col = 'station',
               main = 'Fish 22')
   abacus_plot(dets3[dets3$Fish_ID == 23,], location_col = 'station',
               main = 'Fish 23')
   abacus_plot(dets3[dets3$Fish_ID == 24,], location_col = 'station',
               main = 'Fish 24')
   abacus_plot(dets3[dets3$Fish_ID == 25,], location_col = 'station',
               main = 'Fish 25')
   abacus_plot(dets3[dets3$Fish_ID == 26,], location_col = 'station',
               main = 'Fish 26')
   abacus_plot(dets3[dets3$Fish_ID == 27,], location_col = 'station',
               main = 'Fish 27')
   abacus_plot(dets3[dets3$Fish_ID == 28,], location_col = 'station',
               main = 'Fish 28')
   
   abacus_plot(dets3[dets3$Fish_ID == 29,], location_col = 'station',
               main = 'Fish 29')
   abacus_plot(dets3[dets3$Fish_ID == 30,], location_col = 'station',
               main = 'Fish 30')
   abacus_plot(dets3[dets3$Fish_ID == 31,], location_col = 'station',
               main = 'Fish 31')
   abacus_plot(dets3[dets3$Fish_ID == 32,], location_col = 'station',
               main = 'Fish 32')
   abacus_plot(dets3[dets3$Fish_ID == 33,], location_col = 'station',
               main = 'Fish 33')
   abacus_plot(dets3[dets3$Fish_ID == 34,], location_col = 'station',
               main = 'Fish 34')
   abacus_plot(dets3[dets3$Fish_ID == 35,], location_col = 'station',
               main = 'Fish 35')
   abacus_plot(dets3[dets3$Fish_ID == 36,], location_col = 'station',
               main = 'Fish 36')
   abacus_plot(dets3[dets3$Fish_ID == 37,], location_col = 'station',
               main = 'Fish 37')
   abacus_plot(dets3[dets3$Fish_ID == 38,], location_col = 'station',
               main = 'Fish 38')
   abacus_plot(dets3[dets3$Fish_ID == 39,], location_col = 'station',
               main = 'Fish 39')

   ggplot(dets3, aes(Fish_ID, station)) +
      geom_tile(stat = "count")
   
   ggplot(dets3, aes(x = deploy_lat, y = deploy_long , color = station) ) +
      geom_point()
   
      unique(dets3$station)
   
      count(dets3, station)
      

   count(dets3, station == "ST40")
   
   
   
   
   
   count(dets3, detectedby)
   
   
   
   
###########################################################################
   #let's try making a map of all station locations and generally start learning
   #how to map
   
      
      
      usa <- map_data("usa") 
   
   ggplot() +
   geom_polygon(data = usa, aes(x=long, y = lat, group = group))
   + coord_fixed(1.3)
   
   gg1 <- ggplot() +
   geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "violet",
                color = "blue") + coord_fixed(1.3)
   
   gg1 + geom_point(data = receivers, aes(x = long, y = lat), color = "black", size = 2)+
    geom_point(data = receivers, aes(x = long, y = lat), color = "yellow", size = 1)
     
   #Now let's try again but zoomed in with Florida
   states <- map_data("state") 
   florida <- subset(states, region %in% "florida")
   
   gg2 <- ggplot()+
     geom_polygon(data= florida, aes(x = long, y = lat, group = group), 
                  fill = "light green", color = "tan") + coord_fixed(1.3)
   
   map_data <- get_map(location = c(lon = 80.000, lat = 31.000), 
                       color= "color", #or b/w
                       source = "",
                       maptype = "satellite",
                       zoom = 17)

   
   
#######################################
   
   
   