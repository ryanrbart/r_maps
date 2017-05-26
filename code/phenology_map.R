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

coastline <- map_data("worldHires")     # This map (from 'mapdata') has a more detailed coastline map than states (from 'maps')
usa_coast <- dplyr::filter(coastline, region=="USA")
#head(usa_coast)

states <- map_data("state")
ca_df <- subset(states, region == "california")
#head(ca_df)

# --------------------------------------------------------------------------
# Detailed map
# --------------------------------------------------------------------------

phen_site2 <- phen_site
phen_site2[1,2] <- phen_site[1,2] + 0.02     # Adjust latitude of Santa Barbara
phen_site2[1,3] <- phen_site[1,3] + 0.01     # Adjust latitude of Santa Barbara
phen_site2[2,2] <- phen_site[2,2] + 0.01     # Adjust latitute of Morro Bay
phen_site2[2,3] <- phen_site[2,3] + 0.014     # Adjust longitute of Morro Bay

sb = dplyr::filter(phen_site2, name == 'Santa Barbara')
mor = dplyr::filter(phen_site2, name == 'Morro Bay')

phen_map <- ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = usa_coast, aes(x = long, y = lat, group = group), color = "black", fill = "lightgrey") +
  coord_fixed(xlim = c(-121.2, -119.5),  ylim = c(34.34, 35.65), ratio = 1.3) +
  geom_point(data = phen_site2, aes(x = lon, y = lat), shape = 21, color = "black", fill = "grey50", size = 3) +
  geom_text(data = sb, aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), size=4.0, angle = 0, vjust= -0.7, hjust = 0.98, color = "black") +
  geom_text(data = mor, aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), size=4.0, angle = 0, vjust= 0.5, hjust = 1.13, color = "black") +
  labs(x="Longitude",y="Latitude") +
  theme_classic() +
  theme(panel.background = element_rect(fill='lightblue'))
phen_map

# --------------------------------------------------------------------------
# Inset map
# --------------------------------------------------------------------------

ca_inset <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "navajowhite2") +
  theme_nothing() +
  geom_rect(aes(xmin=-121.1,xmax=-119.4,ymin=34.2,ymax=35.7),colour="red",fill=NA,size=1)
ca_inset

# --------------------------------------------------------------------------
# Put it all together
# --------------------------------------------------------------------------

#png(file="phenology_map.png",w=1800,h=1800, res=300)
pdf("phenology_map.pdf", width = 4.2, height = 4.2)
grid.newpage()
#par(mar=c(2.3,2.1,1.5,0.7)+0.1)
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = .45, height = .45, x = 0.78, y = 0.73) #plot area for the inset map
print(phen_map,vp=v1) 
print(ca_inset,vp=v2)
dev.off()




