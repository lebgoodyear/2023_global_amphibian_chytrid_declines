############################################################################
################## Bd distribution maps across time ########################
############################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Aug 2023
# Last edited: Sep 2023

# clear workspace
rm(list=ls())


############################################################################
################################# Set up ###################################


# date corresponding to dataset
dater <- "230724"
# date corresponding to today (day of running)
datt <- "230905"

# create base maps for plotting? Yes=1/No=0
create_base <- 1

# set paths
path_data <- "~/Documents/scripts/global_amphibian_chytrid_declines_2023/data/"
path_outputs <- "~/Documents/scripts/global_amphibian_chytrid_declines_2023/outputs/"


#############################################################################
############################# Load required packages ########################


# load packages
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
# do the following to install naturalearthhires package
#install.packages("remotes")
#remotes::install_github("ropenscilabs/rnaturalearthhires")
#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
library("rnaturalearthhires")
#library("rgdal")
library("ggplot2")
theme_set(theme_bw())
theme_update(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             panel.background=element_blank(),
             panel.border=element_blank(),
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             plot.background=element_blank())
library("dplyr")
library("tidyr")


##############################################################################
################################# Load data ##################################


# bd detections pre 1980
t1980 <- read.csv(paste0(path_data, "olson1980_all_fields_", dater, ".csv"))
# bd detections pre 2004
t2004 <- read.csv(paste0(path_data, "olson2004_all_fields_", dater, ".csv"))
# bd detections pre 2020
t2020 <- read.csv(paste0(path_data, "olson2020_all_fields_", dater, ".csv"))

# remove correct anomalous coordinate
t2020$decimalLongitude[which(t2020$decimalLongitude == "-176.52729 No ")] <- "-176.52729"

# check if results directory exists and if not, create it
ifelse(!dir.exists(file.path(paste0(path_outputs, "/", datt, "/"))), 
       dir.create(file.path(paste0(path_outputs, "/", datt, "/"))), 
       FALSE)
# set new output path by date of running
path_out <- paste0(path_outputs, "/", datt, "/")


##############################################################################
########################## Set up base maps ##################################


# load world map data
world <- ne_countries(scale = 'large', returnclass = 'sf')

# set Mollweide projection coordinates
mollweide_crs <- '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs'
# create Mollweide projection
area_projected <- st_transform(world, crs=mollweide_crs)
# fix error by making sure object is valid
area_projected_fix <- st_make_valid(area_projected)


##############################################################################
################# Function to create Bd distribution maps ####################


create_map <- function(dat) {
  
  # remove na in coordinates
  df <- dat[which(!is.na(dat[["decimalLongitude"]])),]
  df <- df[which(!is.na(df[["decimalLatitude"]])),]
  # set coordinates as numeric
  df[["decimalLongitude"]] <- as.numeric(df[["decimalLongitude"]])
  df[["decimalLatitude"]] <- as.numeric(df[["decimalLatitude"]])
  # subset by Bd-detected and Bd-not detected
  dft <- df[df$diseaseDetected == TRUE,]
  dff <- df[df$diseaseDetected == FALSE,]

  # create Mollweide projection for Bd-detected
  geot <- as.data.frame(cbind(dft[["decimalLatitude"]], dft[["decimalLongitude"]])) # subset by coordinates
  names(geot) <- c("Longitude", "Latitude") # note latitude and longitude are incorrectly labeled in dataset
  pointst <- st_as_sf(geot, coords=c("Latitude", "Longitude"), crs = 4326) # set standard coordinate system
  geoprojt <- st_transform(pointst, crs=mollweide_crs) # create mollweide projection

  # create Mollweide projection for Bd-not detected
  geof <- as.data.frame(cbind(dff[["decimalLatitude"]], dff[["decimalLongitude"]]))
  names(geof) <- c("Longitude", "Latitude")
  pointsf <- st_as_sf(geof, coords=c("Latitude", "Longitude"), crs = 4326)
  geoprojf <- st_transform(pointsf, crs=mollweide_crs)

  # plot map
  mapp <- ggplot() +
          geom_sf(data=area_projected_fix, colour = "white", size = 0.1, fill = "white") +
          geom_sf(data=geoprojf, colour="#0037a6", size =0.3, alpha=0.2) +
          geom_sf(data=geoprojt, colour="#FF5608", size =0.3, alpha=0.2) +
    theme(panel.background = element_rect(fill = '#e6e6e6', color = '#e6e6e6'))
  return(mapp)
}


##############################################################################
############################# Plot maps ######################################


# generate map for each time period
map1980 <- create_map(t1980)
map2004 <- create_map(t2004)
map2020 <- create_map(t2020)

# save outputs
ggsave(paste0(path_out, "bd_map_1980.png"), map1980)
ggsave(paste0(path_out, "bd_map_2004.png"), map2004)
ggsave(paste0(path_out, "bd_map_2020.png"), map2020)


print("Script completed.")


## end of script
