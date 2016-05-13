# Map for tree-shrub type conversion manuscript

# ------------
# Get GRASS data instructions
# https://grasswiki.osgeo.org/wiki/R_statistics/spgrass6

# Open grass
# In command line, start R.
# Load spgrass6 and raster libraries 
library(spgrass6)
library(raster)

# Read a raster or vector file into R
file_rast = readRAST("file.rast")
file_vect = readVECT("file.vect")

# Convert files to rasters (probably a comprable step for vectors)
file_rast = raster(file_rast)
save(fire_rast, file = "fire_rast.RData")

# In R
load("fire_rast.RData")
plot(fire_rast)
# For ggplot - Fortify?
fortify(fire_rast)  # error

# -------------
# Implementing Grass conversion

load("big_creek.RData")
summary(bc_basin_r)
plot(bc_basin_r)

# Fortify?
fortify(bc_basin_r)



# ----------

# Derive basemap
#?get_stamenmap

phen_means <- rev(sapply(phen_site[2:3], mean))
phen_means[1] = -119.205455     # Adjust long
phen_means[2] = 37.066158     # Adjust lat
#phen_base <- get_map(location = phen_means,  maptype = "terrain-background", source = "osm", zoom = 9)
phen_base <- get_map(location = phen_means,  maptype = "terrain", source = "stamen", zoom = 12)
#phen_base <- get_map(location = phen_means,  maptype = "terrain-background", source = "stamen", zoom = 8)

# Plot map
phen_map = ggmap(phen_base) + 
  geom_point(data = phen_site, color = "black", size = 3) +
  geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), angle = 0, vjust= 1, hjust = 1.1, color = "black") +
  labs(x="Longitude",y="Latitude")
phen_map
