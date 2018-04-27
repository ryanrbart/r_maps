# Map for fire effects paper
# Ryan Bart April 2018

# Load libraries
x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","sp",
       "maps","grid","mapdata","sf","broom")
lapply(x, library, character.only = TRUE) # load the required packages

# --------------------------------------------------------------------------
# Read in location data

# Read in phenology site data
sites <- read.table("data/fire_effects/fire_effects_sites.txt", sep = ",", header = TRUE)
sites

# Extents plus adjustment
north <- max(sites$lat) + 5.30
south <- min(sites$lat) - 3.50
east <- max(sites$lon) + 3.50
west <- min(sites$lon) - 3.00
e <- extent(c(west, east, south, north))
e

# --------------------------------------------------------------------------
# Read in spatial data

proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Natural Earth 
# http://www.naturalearthdata.com/downloads

rivers <- st_read("data/natural_earth/ne_50m_rivers_lake_centerlines")
rivers <- st_crop(rivers, e) # Subset

lakes <- st_read("data/natural_earth/ne_50m_lakes")
lakes <- st_crop(lakes, e) # Subset

# Hillshade data
hill <- raster("data/natural_earth/HYP_50M_SR_W/HYP_50M_SR_W.tif")
hill <- crop(hill, e)
hill <- rasterToPoints(hill)
hill_tib <- as_tibble(hill)

states <- st_read("data/natural_earth/ne_50m_admin_1_states_provinces_lines")
states <- st_crop(states, e) # Subset

country <- st_read("data/natural_earth/ne_50m_admin_0_boundary_lines_land")
country <- st_crop(country, e) # Subset


# --------------------------------------------------------------------------
# Detailed map

fire_map <- ggplot() + 
  geom_raster(data=hill_tib, aes(x=x,y=y, fill=HYP_50M_SR_W)) +
  scale_fill_gradient(low="gray20", high="gray99") +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
#  geom_sf(data=rivers, color="blue") +
#  geom_sf(data=lakes, color="blue") +
  geom_sf(data=states, color="black") +
  geom_sf(data=country, color="black") +
  geom_point(data = sites, aes(x = lon, y = lat), shape = 19, color = "black", fill = "grey50", size = 2) +
  geom_text(data = dplyr::filter(sites, name == 'Santa Barbara'), aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), size=3.0, angle = 0, vjust= -0.75, hjust = 0.98, color = "black") +
  geom_text(data = dplyr::filter(sites, name == 'Santa Fe'), aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), size=3.0, angle = 0, vjust= 0.45, hjust = 1.13, color = "black") +
  geom_text(data = dplyr::filter(sites, name == 'Southern Sierra'), aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), size=3.0, angle = 0, vjust= -0.75, hjust = 0.98, color = "black") +
  geom_text(data = dplyr::filter(sites, name == 'HJ Andrews'), aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), size=3.0, angle = 0, vjust= 0.45, hjust = 0, color = "black") +
  labs(x="Longitude",y="Latitude") +
  theme_classic() +
  theme(legend.position="none")
fire_map


ggsave(filename = "images/fire_effects_map.tiff", width=4,height=4)




