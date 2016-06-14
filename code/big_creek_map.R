

source("code/big_creek_map_processing.R")


# --------------------------------------------------------------------------
# Big Creek Contour Map
# --------------------------------------------------------------------------

bc_map = ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_shed, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  stat_contour(aes(x = x, y = y, z=dem30m), data = bc_dem30m_p, bins=13, colour="tan", size=0.4) +
  geom_path(aes(x = long, y = lat, group=group), data = bc_stream_subset_basin, colour = 'blue', size = .6, alpha=1) +
  geom_point(aes(x = coords.x1, y = coords.x2), data = as.data.frame(coordinates(p301_met_u)), shape = 22, alpha = 1, color="black", fill="yellow", size = 1.5, stroke=1) +
  geom_text(aes(x=coords.x1, y=coords.x2, label="Met Station"), data = as.data.frame(coordinates(p301_met_u)), size=2, angle = 0, vjust= -1.5, hjust = 0.3, color = "black") +
  coord_fixed(1.3, xlim=c(-119.290,-119.174), ylim=c(37.005,37.104)) +
  labs(x="Longitude",y="Latitude", title="Big Creek") +
  theme_bw() +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7))
#  scalebar(data = bc_basin_v, location = "bottomright", dist=.5, dd2km=TRUE, model="WGS84")
bc_map


# --------------------------------------------------------------------------
# P301 Contour Map
# --------------------------------------------------------------------------

p301_map = ggplot() +
 # geom_polygon(aes(x = long, y = lat, group = group), data = p301_basin_v, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  geom_polygon(aes(x = x1, y = x2), data = p301_basin_v_3, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  #  geom_polygon(aes(x = long, y = lat, group = group), data = p301_shed, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  stat_contour(aes(x = x, y = y, z=dem30m), data = p301_dem30m_p, bins=13, colour="tan", size=0.4) +
  geom_path(aes(x = long, y = lat, group=group), data = p301_stream_subset_basin, colour = 'blue', size = .6, alpha=1) +
  geom_point(aes(x=lon, y=lat), data = flux_gauge, shape = 22, alpha = 1, color="black", fill="yellow", size = 1.5, stroke=1) +
  geom_text(aes(x=lon, y=lat, label=name), data = flux_gauge, size=2, angle = 0, vjust= 1.3, hjust = 1.08, color = "black") +
  coord_fixed(1.3, xlim=c(-119.208,-119.189), ylim=c(37.0605,37.0745)) +
  labs(x="Longitude",y="Latitude", title="P301") +
  theme_map() +
# theme(axis.text = element_text(size = 7)) +
# theme(axis.title = element_text(size = 8)) +
  theme(plot.title = element_text(size = 7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA)) 
#  theme(plot.background = element_rect(colour = "grey50"))
p301_map


# --------------------------------------------------------------------------
# Inset map
# --------------------------------------------------------------------------

states <- map_data("state")
ca_df <- subset(states, region == "california")

# Option 1
#ca_inset <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
#  coord_fixed(1.3) + 
#  geom_polygon(color = "black", fill = "khaki") +
#  theme_nothing() +
#  geom_rect(aes(xmin=-119.5,xmax=-118.95,ymin=36.75,ymax=37.3),colour="red",fill=NA,size=.6)
#ca_inset

# Option 2
inset_ca_box = c(-124.8, 32.2, -113.9, 42.4)
inset_base <- get_map(location = inset_ca_box,  maptype = "terrain-background", source = "stamen")

# Plot map
ca_inset = ggmap(inset_base) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group), data=ca_df, color = "gray10", fill = NA, size=.3) +
  geom_rect(aes(xmin=-119.5,xmax=-118.95,ymin=36.75,ymax=37.3),colour="black",fill=NA,size=.6) +
  labs(x="Longitude",y="Latitude") +
  theme_map()
ca_inset



# --------------------------------------------------------------------------
# Arrows
# --------------------------------------------------------------------------

arrow1_df <- data.frame(x1 = 20, x2 = 10, y1 = 20.0, y2 = 19.0)
arrow2_df <- data.frame(x1 = 10, x2 = 20, y1 = 20.0, y2 = 12.0)

arrow1 = ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = arrow1_df,arrow = arrow(angle=20, length = unit(0.2, "cm")), color="black", size=0.5) +
  theme_nothing()
  #coord_fixed()
arrow1

arrow2 = ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = arrow2_df,arrow = arrow(angle=20, length = unit(0.2, "cm")), color="black", size=0.5) +
  theme_nothing()
  #coord_fixed()
arrow2

# --------------------------------------------------------------------------
# Combining Maps for Display
# --------------------------------------------------------------------------



#pdf("images/big_creek_map.pdf", width = 6, height = 4)
setEPS(width = 6, height = 4)
postscript("images/big_creek_map.eps")
grid.newpage()
v1<-viewport(width = .98, height = .98, x = 0.31, y = 0.5) #plot area for the big creek map
v2<-viewport(width = .47, height = .47, x = 0.8, y = 0.32) #plot area for the p301 map
v3<-viewport(width = .38, height = .38, x = 0.8, y = 0.74) #plot area for the inset map
v4<-viewport(width = .1, height = .05, x = 0.65, y = 0.67) # Arrow1
v5<-viewport(width = .2, height = .2, x = 0.565, y = 0.49) # Arrow2
print(bc_map,vp=v1)
print(p301_map,vp=v2)
print(ca_inset,vp=v3)
print(arrow1,vp=v4)
print(arrow2,vp=v5)
dev.off()



# Pointer lines
# Smooth polygon line
# Map labels
# Inset Map > ggmap terrain map?
# Map arrangement










# --------------------------------------------------------------------------
# --------------------------------------------------------------------------




# --------------------------------------------------------------------------
# Big Creek Hillslope Shade Map
# --------------------------------------------------------------------------

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
                 scale_fill_gradientn(name="Altitude",colours = topo.colors(100))+
                 theme_bw()+coord_equal()+xlab("Longitude")+ylab("Latitude")+ggtitle("Big Creek")
  return(map)
}

bc_map_h = hillslope_map(6,21,7) + # month, day, hour
  geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'yellow', fill = 'black', alpha = .05, size = .3) +
  geom_path(aes(x = long, y = lat, group=group), data = bc_stream_subset, colour = 'white', size = 0.3)
bc_map_h


# --------------------------------------------------------------------------
# Big Creek DEM Map
# --------------------------------------------------------------------------

bc_map_d = ggplot(bc_dem30m_p,aes(x=x,y=y))+
  geom_raster(data=bc_dem30m_p,aes(fill=dem30m),alpha=1)+
  scale_fill_gradientn(name="Altitude",colours = topo.colors(100))+
  geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'black', fill = 'black', alpha = .05, size = .3) +
  geom_path(aes(x = long, y = lat, group=group), data = bc_stream_subset_sq, colour = 'blue', size = 1)
bc_map_d

bc_map_d + stat_contour(aes(x = x, y = y, z=dem30m), data = bc_dem30m_p, bins=10, colour="grey", size=0.3)


# --------------------------------------------------------------------------
# P301 Hillslope Shade Map
# --------------------------------------------------------------------------

# SunPosition Gist
source_gist("e2dbdc8e9c45c1c2b7fc")

# Source http://mikebirdgeneau.com/blog/light_elevation_mountains/
hillslope_map<-function(mon,dy,hr){
  sunpos<-sunPosition(year = 2014,month = mon,day = dy,hour = hr+7,min = 0,sec = 0,lat = mean(p301_dem30m_p$y),long=mean(p301_dem30m_p$x))
  hs<-hillShade(p301_slope,p301_aspect,angle = sunpos$elevation,direction = sunpos$azimuth,normalize = TRUE)/255.0
  #plot(hs,col=grey(1:100/100),legend=F)
  #plot(bc_dem30m,col=terrain.colors(100),alpha=0.0,add=T,legend=F)
  HS<-data.frame(rasterToPoints(hs))
  DEM<-data.frame(rasterToPoints(p301_dem30m))
  b.dem <- seq(min(DEM$dem30m),max(DEM$dem30m),length.out=100)
  map = ggplot(HS,aes(x=x,y=y))+
    geom_raster(data=DEM,aes(fill=dem30m),alpha=0.75)+
    geom_raster(aes(alpha=1-layer),fill="gray20")+
    scale_alpha(guide=FALSE,range = c(0,1.00))+
    scale_fill_gradientn(name="Altitude",colours = topo.colors(100))+
    theme_bw()+coord_equal()+xlab("Longitude")+ylab("Latitude")+ggtitle("P301")
  return(map)
}

p301_map = hillslope_map(6,21,7) + # month, day, hour
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_basin_v, colour = 'yellow', fill = 'black', alpha = .05, size = .3) +
  geom_path(aes(x = long, y = lat, group=group), data = p301_stream_v, colour = 'blue', size = 0.3) +
  stat_contour(aes(x = x, y = y, z=dem30m), data = p301_dem30m_p, bins=30)
  #stat_contour(geom="polygon",aes(x = x, y = y, z=dem30m, fill=dem30m), data = p301_dem30m_p, bins=10)
  #geom_tile(aes(x = x, y = y, z=dem30m, fill=dem30m), data = p301_dem30m_p)
p301_map_h


# --------------------------------------------------------------------------
# P301 DEM Map
# --------------------------------------------------------------------------

p301_map_d = ggplot(p301_dem30m_p,aes(x=x,y=y))+
  geom_raster(data=p301_dem30m_p,aes(fill=dem30m),alpha=1)+
  scale_fill_gradientn(name="Altitude",colours = topo.colors(100))+
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_basin_v, colour = 'blue', fill = 'black', alpha = .01, size = .3) +
  geom_path(aes(x = long, y = lat, group=group), data = p301_stream_subset, colour = 'white', size = 0.3)
p301_map_d

