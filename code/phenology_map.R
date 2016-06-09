# Map for grassland phenology manuscript


# --------------------------------------------------------------------------
# Load libraries
# --------------------------------------------------------------------------
x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","sp","maps","grid","mapdata")
lapply(x, library, character.only = TRUE) # load the required packages


# --------------------------------------------------------------------------
# Read in data
# --------------------------------------------------------------------------
# Read in phenology site data
phen_site <- read.table("data/phenology/grass_phenology_sites.txt", sep = ",", header = TRUE)
phen_site


# --------------------------------------------------------------------------
# Detailed map
# --------------------------------------------------------------------------

# Derive basemap
#?get_stamenmap
phen_means <- rev(sapply(phen_site[2:3], mean))
phen_means[1] = phen_means[1] + 0.07     # Adjust centering of longitude
phen_means[2] = phen_means[2] + 0.1     # Adjust centering of latitute 
phen_base <- get_map(location = phen_means,  maptype = "terrain-background", source = "stamen", zoom = 9)

# Plot map
phen_map = ggmap(phen_base) + 
  geom_point(data = phen_site, color = "black", size = 3.5) +
  geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), size=4.2, angle = 0, vjust= 1.1, hjust = 1.05, color = "black") +
  labs(x="Longitude",y="Latitude")
phen_map


# --------------------------------------------------------------------------
# Inset map
# --------------------------------------------------------------------------

states <- map_data("state")
ca_df <- subset(states, region == "california")
head(ca_df)

ca_inset <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "lightgray") +
  theme_nothing() +
  geom_rect(aes(xmin=-121.1,xmax=-119.4,ymin=34.2,ymax=35.7),colour="red",fill=NA,size=1)
ca_inset

# --------------------------------------------------------------------------
# Put it all together
# --------------------------------------------------------------------------

#png(file="phenology_map.png",w=1800,h=1800, res=300)
pdf("phenology_map.pdf", width = 5, height = 5)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = .4, height = .4, x = 0.8, y = 0.75) #plot area for the inset map
print(phen_map,vp=v1) 
print(ca_inset,vp=v2)
dev.off()









# --------------------------------------------------------------------------
# Other option for detailed map
# --------------------------------------------------------------------------

phen_map = ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  #  geom_point(data = phen_site, color = "black", size = 4) +
  #  geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), angle = 0, hjust = 1.1, color = "black") +
  labs(x="Longitude",y="Latitude") +
  coord_fixed(1.3)
phen_map


states <- map_data("state")
ca_df <- subset(states, region == "california")
rivers <- map_data("rivers")

phen_map <- ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = ca_df, aes(x = long, y = lat, group = group), color = "black", fill = "gray") +
  coord_fixed(xlim = c(-121.2, -119.5),  ylim = c(34.2, 35.65), ratio = 1.3) +
  geom_point(data = phen_site, aes(x = lon, y = lat), color = "black", size = 4) +
  #geom_text(data = phen_site, aes(label = paste("  ", as.character(name), sep="")), angle = 0, hjust = 1.1, color = "black") +
  labs(x="Longitude",y="Latitude") +
  theme_bw()
phen_map


