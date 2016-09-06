# Map for fire effects paper
# Ryan Bart September 2016

# Load libraries
x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","sp","maps","grid","mapdata")
lapply(x, library, character.only = TRUE) # load the required packages

# --------------------------------------------------------------------------
# Read in location data

# Read in phenology site data
sites <- read.table("data/fire_effects/fire_effects_sites.txt", sep = ",", header = TRUE)
sites


# --------------------------------------------------------------------------
# Detailed map

# Derive basemap
#?get_stamenmap
loc_means <- rev(sapply(sites[2:3], mean))
loc_means[1] = loc_means[1] + 0.00     # Adjust centering of longitude
loc_means[2] = loc_means[2] + 0.00     # Adjust centering of latitute 
loc_base <- get_map(location = loc_means,  maptype = "terrain-background", source = "stamen", zoom = 5)
# error in get_stamenmap. "terrain", "terrain-background", "watercolor" assigned jpg instead of png
loc_base <- get_map(location = loc_means,  maptype = "terrain-lines", source = "stamen", zoom = 5)

# Plot map
loc_map = ggmap(loc_base) + 
  geom_point(data = sites, color = "black", size = 3.5) +
  geom_text(data = sites, aes(label = paste("  ", as.character(name), sep="")), size=4.2, angle = 0, vjust= 1.1, hjust = 1.05, color = "black") +
  labs(x="Longitude",y="Latitude")
loc_map




