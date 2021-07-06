#
#Start date 7/6/2021
#
#This is the r script for exploratory analysis of the output FACT data

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


install_url("https://gitlab.oceantrack.org/GreatLakes/glatos/-/archive/
            master/glatos-master.zip",
            build_opts = c("--no-resave-data", "--no-manual"))


##################################################################################
#upload data sets from the FACT output


dets2019 <- read.csv("data/whitt_matched_detections_2019.csv")
dets2020 <- read.csv("data/whitt_matched_detections_2020.csv")
reds <- read.csv("data/Red_Drum_Sample_Inventory.csv")

dets <- rbind(dets2019, dets2020)

####################################################################################

#make sure the data is clean and correct

#adjust the dates using lubridate
reds <- reds %>% 

  mutate(Date = mdy(Date))

#check for spelling errors or typos
unique(reds$Location)























