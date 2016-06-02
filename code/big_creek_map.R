# Map for tree-shrub type conversion manuscript

# Load libraries
x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","sp","maps","grid","mapdata","raster")
lapply(x, library, character.only = TRUE) # load the required packages

library(RColorBrewer)
library(lubridate)
library(manipulate)
library(devtools)


# -------------
# Big Creek Detailed Map

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
  x <- crop(x, extent(x,2550,3000,2300,2700))    # (for Raster x, row 5 to 10, column 7 to 12)
  new_proj <- "+proj=longlat"
  x <- projectRaster(x, crs = new_proj)
}
bc_aspect = subset_utm_to_longlat(bc_aspect)
bc_aspect = bc_aspect*pi/180          # Convert aspect from degrees (GRASS) to radians (R)
bc_basin = subset_utm_to_longlat(bc_basin)
bc_dem30m = subset_utm_to_longlat(bc_dem30m)
bc_slope = subset_utm_to_longlat(bc_slope)
bc_slope = bc_slope*pi/180          # Convert aspect from degrees (GRASS) to radians (R)
bc_stream = subset_utm_to_longlat(bc_stream)

bc_aspect_p <- as.data.frame(rasterToPoints(bc_aspect))
bc_basin_p <- as.data.frame(rasterToPoints(bc_basin))
bc_dem30m_p <- as.data.frame(rasterToPoints(bc_dem30m))
bc_slope_p <- as.data.frame(rasterToPoints(bc_slope))
bc_stream_p <- as.data.frame(rasterToPoints(bc_stream))


# Convert vector from UTM to longlat
bc_basin_v = spTransform(bc_basin_v,CRS("+proj=longlat"))
bc_stream_v = spTransform(bc_stream_v,CRS("+proj=longlat"))   # subset this???

b <- bbox(bc_basin)
#bc_stream_subset = crop(bc_stream_v, bc_basin_v)    # Only includes stream inside watershed
bc_stream_subset = crop(bc_stream_v, b)              # Includes all streams in tile




# ----------
# Make Big Creek Map

# SunPosition Gist
source_gist("e2dbdc8e9c45c1c2b7fc")

# Source http://mikebirdgeneau.com/blog/light_elevation_mountains/
hillslope_map<-function(mon,dy,hr){
  sunpos<-sunPosition(year = 2014,month = mon,day = dy,hour = hr+7,min = 0,sec = 0,lat = mean(bc_dem30m_p$y),long=mean(bc_dem30m_p$x))
  hs<-hillShade(bc_slope,bc_aspect,angle = sunpos$elevation,direction = sunpos$azimuth,normalize = TRUE)/255.0
  #plot(hs,col=grey(1:100/100),legend=F)
  #plot(bc_dem30m,col=terrain.colors(100),alpha=0.0,add=T,legend=F)
  HS<-data.frame(rasterToPoints(hs))
  DEM<-data.frame(rasterToPoints(bc_dem30m))
  b.dem <- seq(min(DEM$dem30m),max(DEM$dem30m),length.out=100)
  map = ggplot(HS,aes(x=x,y=y))+
                 geom_raster(data=DEM,aes(fill=dem30m),alpha=0.75)+
                 geom_raster(aes(alpha=1-layer),fill="gray10")+
                 scale_alpha(guide=FALSE,range = c(0,1.00))+
                 scale_fill_gradientn(name="Altitude",colours = terrain.colors(100))+
                 theme_bw()+coord_equal()+xlab("Longitude")+ylab("Latitude")+ggtitle("Big Creek, CA")
  return(map)
}

bc_map = hillslope_map(6,21,7)  # month, day, hour
#bc_map

bc_map + geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'yellow', fill = 'black', alpha = .05, size = .3) +
  #geom_raster(data=bc_stream_p, aes(fill=str.t1000),alpha=0.75)
  geom_path(aes(x = long, y = lat, group=group), data = bc_stream_subset, colour = 'white', size = 0.3)




# ----

# Derive basemap
#?get_stamenmap

bc_center = data.frame(lon = -119.233455, lat = 37.04815)
bc_box = c(-119.3, 37, -119.17, 37.11)   # left, bottom, right, top




#bc_base <- get_map(location = bc_center,  maptype = "terrain", source = "stamen", zoom = 12)
bc_base <- get_map(location = bc_box,  maptype = "terrain", source = "stamen")

# Plot map
bc_map = ggmap(bc_base) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'black', fill = 'black', alpha = .05, size = .3) +
  #geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'black', fill = NA, alpha = .4, size = .3) +
  # geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), angle = 0, vjust= 1, hjust = 1.1, color = "black") +
  #geom_point(data = bc_box, color = "black", size = 3.5) +
  labs(x="Longitude",y="Latitude")
bc_map

bc_map + geom_raster(data = bc_dem30m, aes(x=x, y=y, fill=dem30m)) + coord_cartesian()

bc_map + geom_raster(data = bc_aspect, aes(x=x, y=y, fill=aspect)) + coord_cartesian()
bc_map + geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'black', fill = 'black', alpha = .05, size = .3) +
  


# ---

# Correct zone of p301 files from 10 to 11
proj4string(p301_vect) <- CRS("+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs +ellps=clrk66")

# Convert from UTM to longlat
bc_vect_t = spTransform(bc_vect,CRS("+proj=longlat"))
p301_vect_t = spTransform(p301_vect,CRS("+proj=longlat"))
#p301_dem_t = spTransform(p301_dem,CRS("+proj=longlat"))

# Fortify (for vectors)
bc_vect_f = fortify(bc_vect_t)
p301_vect_f = fortify(p301_vect_t)



# -------------
# For R (raster processing)

load("data/ssczo/p301_dem.RData")
load("data/ssczo/p301_vect.RData")
summary(p301_dem)

# Correct zone of p301 files from 10 to 11
proj4string(p301_dem) <- CRS("+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs +ellps=clrk66")

# Turn to raster
p301_dem_r <- raster(p301_dem)

# Reproject to lat/long
new_proj <- "+proj=longlat"
p301_dem_t <- projectRaster(p301_dem_r, crs = new_proj)

# Change raster to points
p301_dem_p <- rasterToPoints(p301_dem_t)
p301_dem_p = as.data.frame(p301_dem_p)


head(p301_dem_p)


# ----------
# ----------

# Derive basemap
#?get_stamenmap

#bc_center = data.frame(lon = -119.233455, lat = 37.04815)
bc_box = c(-119.3, 37, -119.17, 37.11)   # left, bottom, right, top

#bc_base <- get_map(location = bc_center,  maptype = "terrain", source = "stamen", zoom = 12)
bc_base <- get_map(location = bc_box,  maptype = "terrain", source = "stamen")

# Plot map
bc_map = ggmap(bc_base) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = bc_vect_f, colour = 'black', fill = 'black', alpha = .05, size = .3) +
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_vect_f, colour = 'black', fill = NA, alpha = .4, size = .3) +
  # geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), angle = 0, vjust= 1, hjust = 1.1, color = "black") +
  geom_point(data = bc_center, color = "black", size = 3.5) +
  labs(x="Longitude",y="Latitude")
bc_map

# ----------
# P301 Map
# Derive basemap

p301_center = data.frame(lon = -119.2, lat = 37.068)
#p301_box = c(-119.3, 37, -119.17, 37.11)   # left, bottom, right, top

p301_base <- get_map(location = p301_center,  maptype = "terrain-background", source = "stamen", zoom = 15)
#p301_base <- get_map(location = p301_box,  maptype = "terrain-background", source = "stamen")

# Plot map
p301_map = ggmap(p301_base) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_vect_f, colour = 'black', fill = NA, alpha = .4, size = .3) +
  # geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), angle = 0, vjust= 1, hjust = 1.1, color = "black") +
  geom_point(data = p301_center, color = "black", size = 3.5) +
  labs(x="Longitude",y="Latitude")
p301_map

p301_map + geom_raster(data = p301_dem_p, aes(x=x, y=y, fill=p301.dem)) + coord_cartesian()


ggplot(data=p301_dem_p, aes(x=x, y=y)) + geom_raster(aes(fill=p301.dem))

      
# ------
# Met stations
p301_met <- readOGR(dsn = "../data/ssczo/", layer = "KREW_MetStations")
p301_met_t = spTransform(p301_met,CRS("+proj=longlat"))
bc_map + geom_point(aes(x = coords.x1, y = coords.x2), data = as.data.frame(coordinates(p301_met_t)),
                    alpha = .5, color="darkred", size = 3)

# ------
# Stream network
p301_stream <- readOGR(dsn = "../data/ssczo/", layer = "Streams")
p301_stream_t = spTransform(p301_stream,CRS("+proj=longlat"))
#bc_map + geom_line(aes(x = coords.x1, y = coords.x2), data = as.data.frame(coordinates(p301_stream_t)),
                    alpha = .5, color="darkred", size = 3)
bc_map + geom_line(aes(x = coords.x1, y = coords.x2), data = as.data.frame(coordinates(p301_stream_t)),
                   alpha = .5, color="darkred", size = 3)


