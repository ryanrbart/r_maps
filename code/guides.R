# Guides for various map related processes


# ------------
# Get GRASS to R instructions
# See https://grasswiki.osgeo.org/wiki/R_statistics/spgrass6

# Open GRASS

# First create vector files in grass.
# Example GRASS code for basin raster file to basin vector file
# r.to.vect input=basin_rast output=basin_vect feature=area

# In GRASS command line, start R.
# Load spgrass6 and raster libraries 
library(spgrass6)

# Read a raster or vector file into R
file_rast = readRAST("file.rast")
file_vect = readVECT("file.vect")

# Save vector and raster files to be opened in stand-alone (non-grass) R
save(file_rast, file = "file_rast.RData")
save(file_vect, file = "file_vect.RData")


# ---
# In stand-alone R

load("file_rast.RData")
load("file_vect.RData")

# Convert from UTM to longlat (if needed)
file_rast_t = spTransform(file_rast,CRS("+proj=longlat"))
file_vect_t = spTransform(file_vect,CRS("+proj=longlat"))

# For vector input into ggmap
fortify(file_vect)

# -------------


