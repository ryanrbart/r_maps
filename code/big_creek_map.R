

source("code/big_creek_map_processing.R")


# --------------------------------------------------------------------------
# Big Creek Contour Map
# --------------------------------------------------------------------------

bc_map = ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), data = bc_basin_v, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_basin_v, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
#  geom_polygon(aes(x = long, y = lat, group = group), data = p301_shed, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  stat_contour(aes(x = x, y = y, z=dem30m), data = bc_dem30m_p, bins=13, colour="tan", size=0.4) +
  geom_path(aes(x = long, y = lat, group=group), data = bc_stream_subset_basin, colour = 'blue', size = 1, alpha=0.6) +
  geom_point(aes(x = coords.x1, y = coords.x2), data = as.data.frame(coordinates(p301_met_u)), shape = 24, alpha = 1, color="black", size = 3, stroke=2) +
  coord_fixed(1.3, xlim=c(-119.290,-119.174), ylim=c(37.005,37.104)) +
  labs(x="Longitude",y="Latitude") +
  theme(axis.text = element_text(size = 8)) +
  theme(axis.title = element_text(size = 9)) +
  theme(plot.background = element_rect(colour = "grey50")) +
  theme_bw()
bc_map


# --------------------------------------------------------------------------
# P301 Contour Map
# --------------------------------------------------------------------------

p301_map = ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), data = p301_basin_v, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
#  geom_polygon(aes(x = long, y = lat, group = group), data = p301_shed, colour = 'gray30', fill = 'navajowhite2', alpha = 1, size = .5) +
  stat_contour(aes(x = x, y = y, z=dem30m), data = p301_dem30m_p, bins=17, colour="tan", size=0.4) +
  geom_path(aes(x = long, y = lat, group=group), data = p301_stream_subset_basin, colour = 'blue', size = 1) +
  #geom_point(aes(x = coords.x1, y = coords.x2), data = as.data.frame(coordinates(p301_met_u)), shape = 24, alpha = 1, color="black", size = 3, stroke=2) + 
  coord_fixed(1.3, xlim=c(-119.208,-119.189), ylim=c(37.0605,37.0745)) +
  labs(x="Longitude",y="Latitude") +
#  theme(axis.text = element_text(size = 7)) +
#  theme(axis.title = element_text(size = 8)) +
  theme(plot.background = element_rect(colour = "grey50")) +
  theme_bw()
p301_map


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
  geom_rect(aes(xmin=-119.3,xmax=-119.15,ymin=37.00,ymax=37.11),colour="red",fill=NA,size=1)
ca_inset




# --------------------------------------------------------------------------
# Combining Maps for Display
# --------------------------------------------------------------------------


#png(file="images/big_creek_map.png",w=1800,h=1800, res=300)
pdf("images/big_creek_map.pdf", width = 7, height = 4.8)
grid.newpage()
v1<-viewport(width = .87, height = .87, x = 0.32, y = 0.47) #plot area for the big creek map
v2<-viewport(width = .45, height = .45, x = 0.8, y = 0.28) #plot area for the p301 map
v3<-viewport(width = .3, height = .3, x = 0.75, y = 0.7) #plot area for the inset map
print(bc_map,vp=v1)
print(p301_map,vp=v2)
print(ca_inset,vp=v3)
dev.off()



# Zoom in
# Smooth polygon line
# Map labels
# Inset Map > ggmap?
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

