# Map processing for tree-shrub type conversion manuscript


# --------------------------------------------------------------------------
# Load libraries
# --------------------------------------------------------------------------

x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","sp","maps","grid","mapdata","raster")
lapply(x, library, character.only = TRUE) # load the required packages

library(RColorBrewer)
library(lubridate)
library(manipulate)
library(devtools)


# --------------------------------------------------------------------------
# Big Creek Detailed Map (Data Prep)
# --------------------------------------------------------------------------

load("data/ssczo/bc_aspect.RData")
load("data/ssczo/bc_basin.RData")
load("data/ssczo/bc_dem30m.RData")
load("data/ssczo/bc_slope.RData")
load("data/ssczo/bc_stream.RData")
load("data/ssczo/bc_basin_v.RData")
load("data/ssczo/bc_stream_v.RData")

# summary of imported data
#summary(bc_aspect)
#summary(bc_basin)
#summary(bc_dem30m)
#summary(bc_slope)
#summary(bc_stream)
#summary(bc_basin_v)
#summary(bc_stream_v)

# Rasterize and subset and reproject from utm to longlat
subset_utm_to_longlat = function(x){
  x = raster(x)
  x <- crop(x, extent(x,2550,3000,2280,2700))    # (for Raster x, row 5 to 10, column 7 to 12)
  new_proj <- "+proj=longlat"
  x <- projectRaster(x, crs = new_proj)
}
bc_aspect_r = subset_utm_to_longlat(bc_aspect)
bc_aspect_r = bc_aspect_r*pi/180          # Convert aspect from degrees (GRASS) to radians (R)
bc_basin_r = subset_utm_to_longlat(bc_basin)
bc_dem30m_r = subset_utm_to_longlat(bc_dem30m)
bc_slope_r = subset_utm_to_longlat(bc_slope)
bc_slope_r = bc_slope_r*pi/180          # Convert aspect from degrees (GRASS) to radians (R)
bc_stream_r = subset_utm_to_longlat(bc_stream)

# Big Creek rasters to point data
bc_aspect_p <- as.data.frame(rasterToPoints(bc_aspect_r))
bc_basin_p <- as.data.frame(rasterToPoints(bc_basin_r))
bc_dem30m_p <- as.data.frame(rasterToPoints(bc_dem30m_r))
bc_slope_p <- as.data.frame(rasterToPoints(bc_slope_r))
bc_stream_p <- as.data.frame(rasterToPoints(bc_stream_r))

# Vector Processing
# Convert vector from UTM to longlat
bc_basin_v = spTransform(bc_basin_v,CRS("+proj=longlat"))
bc_stream_v = spTransform(bc_stream_v,CRS("+proj=longlat"))

# Subset vector data
bc_stream_subset_basin = crop(bc_stream_v, bc_basin_v)    # Only includes stream inside watershed
bc_stream_subset_sq = crop(bc_stream_v, bbox(bc_aspect_r))              # Includes all streams in tile



# --------------------------------------------------------------------------
# P301 Detailed Map (Data Prep)
# --------------------------------------------------------------------------

load("data/ssczo/bc_aspect.RData")
load("data/ssczo/bc_dem30m.RData")
load("data/ssczo/bc_slope.RData")
load("data/ssczo/p301_basin_v.RData")
load("data/ssczo/p301_stream_v.RData")

# summary of imported data
#summary(bc_aspect)
#summary(bc_dem30m)
#summary(bc_slope)
#summary(p301_basin_v)
#summary(p301_stream_v)

# Rasterize and subset and reproject from utm to longlat
subset_utm_to_longlat = function(x){
  x = raster(x)
  x <- crop(x, extent(x,2720,2790,2550,2620))    # (for Raster x, row 5 to 10, column 7 to 12)
  new_proj <- "+proj=longlat"
  x <- projectRaster(x, crs = new_proj)
}
p301_aspect_r = subset_utm_to_longlat(bc_aspect)
p301_aspect_r = p301_aspect_r*pi/180          # Convert aspect from degrees (GRASS) to radians (R)
p301_dem30m_r = subset_utm_to_longlat(bc_dem30m)
p301_slope_r = subset_utm_to_longlat(bc_slope)
p301_slope_r = p301_slope_r*pi/180          # Convert aspect from degrees (GRASS) to radians (R)

# P301 rasters to point data
p301_aspect_p <- as.data.frame(rasterToPoints(p301_aspect))
p301_dem30m_p <- as.data.frame(rasterToPoints(p301_dem30m))
p301_slope_p <- as.data.frame(rasterToPoints(p301_slope))


# Vector Processing
# Correct zone of p301 files from 10 to 11
proj4string(p301_basin_v) <- CRS("+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs +ellps=clrk66")
proj4string(p301_stream_v) <- CRS("+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs +ellps=clrk66")

# Convert vector from UTM to longlat
p301_basin_v = spTransform(p301_basin_v,CRS("+proj=longlat"))
p301_stream_v = spTransform(p301_stream_v,CRS("+proj=longlat"))   # subset this???

# Subset vector data
p301_stream_subset_basin = crop(p301_stream_v, p301_basin_v)    # Only includes stream inside watershed
p301_stream_subset_sq = crop(p301_stream_v, bbox(p301_aspect_r))              # Includes all streams in tile


# --------------------------------------------------------------------------
# P301 Meteorological Station Data
# --------------------------------------------------------------------------

p301_met <- readOGR(dsn = "data/ssczo/", layer = "KREW_MetStations")
p301_met = spTransform(p301_met,CRS("+proj=longlat"))
p301_met_l = p301_met[c(4),]    # Lower P301 station
p301_met_u = p301_met[c(3),]    # Upper P301 station


# --------------------------------------------------------------------------
# Higher Resolution P301 Perimeter
# --------------------------------------------------------------------------

prov_sheds <- readOGR(dsn = "data/ssczo/", layer = "Providence_Watersheds")
prov_sheds = spTransform(prov_sheds,CRS("+proj=longlat"))
p301_shed = prov_sheds[c(1),]    # P301






