# Map for tree-shrub type conversion manuscript


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
