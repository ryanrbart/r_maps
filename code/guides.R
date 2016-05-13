# Guides for various map related processes


# ------------
# Get GRASS to R instructions
# See https://grasswiki.osgeo.org/wiki/R_statistics/spgrass6

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
