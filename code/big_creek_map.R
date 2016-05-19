# Map for tree-shrub type conversion manuscript

# Load libraries
x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","sp","maps","grid","mapdata","raster")
lapply(x, library, character.only = TRUE) # load the required packages


# -------------
# For running R in Grass 


# P301 GRASS

p301_vect = readVECT("p301_basin_vect")
save(p301_vect, file = "p301_vect.RData")

# -------------
# For R (vector processing)

load("../data/ssczo/bc_vect.RData")
load("../data/ssczo/p301_vect.RData")


# summary of imported data
bc_vect
p301_vect

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

load("../data/ssczo/p301_dem.RData")
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


