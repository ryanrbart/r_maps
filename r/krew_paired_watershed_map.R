# Map for KREW water yield manuscript
# Ryan Bart - May 2019



# --------------------------------------------------------------------------
# Load libraries and projections

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(sf)
library(cowplot)
library(sp)
library(ggspatial)
library(gghighlight)
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(raster)
library(mapview)
library(patchwork)
library(tmap)
library(metR)


proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj_utm <- "+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs"

# --------------------------------------------------------------------------
# Read in spatial data

# Providence watersheds
#prov_i <- st_read("data/krew/KREW_Watersheds/Providence_Integrated.shp")
prov <- st_read("data/krew/KREW_Watersheds/Providence_Watersheds.shp") %>% 
  st_transform(proj_utm) %>% 
  mutate(Watershed = if_else(POLY_ID == "P304", "Control", "Treated"))
box_prov <- st_bbox(st_buffer(prov, 0))       # Establish extent of prov. Buffer can be used to add extent


# Bull watersheds
#bull_i <- st_read("data/krew/KREW_Watersheds/Bull_Integrated.shp")
bull <- st_read("data/krew/KREW_Watersheds/Bull_Watersheds.shp") %>% 
  st_transform(proj_utm) %>% 
  mutate(Watershed = if_else(POLY_ID == "TK1", "Control", "Treated"))
box_bull <- st_bbox(st_buffer(bull, 0))      # Establish extent of bull. Buffer can be used to add extent


# Stream data
streams_prov <- st_read("data/krew/KREW_Watersheds/Prov_Streams.shp") %>% 
  st_transform(proj_utm)  # Read in stream data and transform
streams_crs <- st_crs(streams_prov)  # Note crs for use later
streams_prov_new_geom <- st_geometry(streams_prov) + c(80,0) # Modify geometry
streams_prov = st_set_geometry(streams_prov, streams_prov_new_geom) # Put geometry back in
st_crs(streams_prov) = streams_crs # Reestabish CRS, which got erased on previous step
streams_prov <- streams_prov %>% 
  st_zm() %>%
  st_crop(box_prov) %>%  # Crop the stream data
  st_intersection(prov)

streams_bull <- st_read("data/krew/KREW_Watersheds/Bull_Streams.shp")
streams_bull <- st_read("data/krew/KREW_Watersheds/Bull_Streams.shp") %>% 
  st_transform(proj_utm)  # Read in stream data and transform
streams_crs <- st_crs(streams_bull)  # Note crs for use later
streams_prov_new_geom <- st_geometry(streams_bull) + c(80,0) # Modify geometry
streams_bull = st_set_geometry(streams_bull, streams_prov_new_geom) # Put geometry back in
st_crs(streams_bull) = streams_crs # Reestabish CRS, which got erased on previous step
streams_bull <- streams_bull %>% 
  st_zm() %>%
  st_crop(box_bull) %>% # Crop the stream data
  st_intersection(bull)


# Sites
sites <- st_read("data/krew/KREW_Watersheds/Sites.shp")
sites_prov <- sites %>% 
  st_transform(proj_utm) %>% 
  st_crop(box_prov) %>% 
  dplyr::filter(Name %in% c("UP", "LP","P301", "P303","P304","D102")) %>% 
  mutate(Instrument = case_when(
    Name == "UP" ~ "Meteorological\nStation",
    Name == "LP" ~ "Meteorological\nStation",
    Name == "P301" ~ "Stream\nGauge",
    Name == "P303" ~ "Stream\nGauge",
    Name == "P304" ~ "Stream\nGauge",
    Name == "D102" ~ "Stream\nGauge"))
sites_bull <- sites %>% 
  st_transform(proj_utm) %>% 
  st_crop(box_bull) %>% 
  dplyr::filter(Name %in% c("UB", "LB","B201", "B203","B204","T003")) %>% 
  mutate(Instrument = case_when(
    Name == "UB" ~ "Meteorological\nStation",
    Name == "LB" ~ "Meteorological\nStation",
    Name == "B201" ~ "Stream\nGauge",
    Name == "B203" ~ "Stream\nGauge",
    Name == "B204" ~ "Stream\nGauge",
    Name == "T003" ~ "Stream\nGauge"))

# Make adjustments to P303 site position
sites_prov$geometry[4] <- sites_prov$geometry[4] + c(-80,-50)
sites_prov$Easting[4] <- sites_prov$Easting[4] - 80
sites_prov$Northing[4] <- sites_prov$Northing[4] - 50

# California map
west <- -125
east <- -113.7
south <- 32
north <- 42.5

box_cal <- st_bbox(c(xmin = west, xmax = east, ymax = north, ymin = south))
e <- extent(c(west, east, south, north))

states_full <- st_read("data/States/detailed/cb_2018_us_state_20m.shp")
states_region <- states_full %>% 
  st_crop(box_cal)
'%!in%' <- function(x,y)!('%in%'(x,y))     # Function for the opposite of %in%
country <- states_full %>% 
  dplyr::filter(NAME %!in% c("Hawaii", "Puerto Rico", "Alaska"))
california <- states_full %>% 
  dplyr::filter(NAME == c("California"))

location_dot <- tibble(site=c("prov","bull"),lat=c(37.055,36.97),lon=c(-119.19,-119.05))

# Old states
# states <- st_read("data/natural_earth/ne_50m_admin_1_states_provinces_lines") %>% 
#   st_crop(box_cal)
# hill <- raster("data/natural_earth/HYP_50M_SR_W/HYP_50M_SR_W.tif") %>% 
#   crop(e) %>% 
#   rasterToPoints() %>% 
#   as_tibble()
# country <- st_read("data/natural_earth/ne_50m_admin_0_boundary_lines_land") %>% 
#   st_crop(box_cal)


# National Forest Map
# Source: https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries
nf <- st_read("data/national_forests/S_USA.NFSLandUnit") %>% 
  st_transform(proj_longlat) %>% 
  dplyr::filter(NFSLANDU_2 == "Sierra National Forest")

sn <- st_read("data/ecoregions_ca/ca_eco_l3.shp") %>% 
  st_transform(proj_longlat) %>% 
  dplyr::filter(US_L3NAME == "Sierra Nevada")


# DEM for countour lines
load("data/dem/kings_sj_tile/bc_dem30m.RData")
gridded(bc_dem30m)  # Test to show that product is gridded, not a raster. What's the difference? Not sure.
dem_full <- raster(bc_dem30m) %>%   # Coerce to a raster
  projectRaster(crs = proj_utm)

e_prov <- extent(c(box_prov[1],box_prov[3],box_prov[2],box_prov[4])) + c(200,150)
e_bull <- extent(c(box_bull[1],box_bull[3],box_bull[2],box_bull[4])) + c(800,150)

dem_bull <- dem_full %>% 
  raster::crop(e_bull) %>% 
  rasterToPoints() %>% 
  as_tibble()

dem_prov <- dem_full %>% 
  raster::crop(e_prov) %>% 
  rasterToPoints() %>% 
  as_tibble()



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Prov map

map_prov <- ggplot() +
  geom_sf(data=prov, aes(fill=Watershed)) + 
  stat_contour(aes(x = x, y = y, z=dem30m), data = dem_prov, 
               breaks = c(1450,1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050,2100),
               colour="tan", size=0.4) +
  geom_text_contour(aes(x = x, y = y, z=dem30m), data = dem_prov,
                    breaks = c(1450,1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050,2100),
                    min.size=10,skip = 1,
                    stroke = 0.3, size=2, color = "tan",
                    check_overlap = TRUE) +
  geom_sf(data=streams_prov, color="blue") +
  geom_sf(data=prov, color="black", fill=NA, size=0.2) +
  geom_point(data=sites_prov, aes(x=Easting, y=Northing, shape=Instrument),
          alpha = 1, color="black",
          fill="yellow", size = 3, stroke=1) +
  geom_text(data=sites_prov, aes(x=Easting, y=Northing ,label=Name), fontface = "bold",
               size = 3, nudge_x = c(120,0,0,-140,-200,-120),
               nudge_y = c(110,-110,-110,-110,0,-110)) +
  scale_shape_manual(values=c(22, 24)) +
  scale_fill_manual(values=c("navajowhite4","navajowhite")) +
  scale_x_continuous(breaks=c(-119.2,-119.19,-119.18), expand = c(0,0)) +
  scale_y_continuous(breaks=c(37.04,37.05,37.06,37.07), expand = c(0,0)) +
  labs(title = "Providence", size=1.0) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 315, hjust=0, vjust=0.6),
        axis.text.y = element_text(angle = 315, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="right") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  NULL
#plot(map_prov)

map_prov_no_legend <- map_prov + theme(legend.position="none") 

# --------------------------------------------------------------------------
# Bull map

map_bull <- ggplot() +
  geom_sf(data=bull, aes(fill=Watershed)) + 
  stat_contour(aes(x = x, y = y, z=dem30m), data = dem_bull, 
               breaks = c(1800,1850,1900,1950,2000,2050,2100,2150,2200,2250,
                          2300,2350,2400,2450,2500,2550),
               colour="tan", size=0.4) +
  geom_text_contour(aes(x = x, y = y, z=dem30m), data = dem_bull,
                    breaks = c(1800,1850,1900,1950,2000,2050,2100,2150,2200,2250,
                               2300,2350,2400,2450,2500,2550),
                    min.size=10,skip = 1,
                    stroke = 0.3, size=2, color = "tan",
                    check_overlap = TRUE) +  geom_sf(data=streams_bull, color="blue") +
  geom_sf(data=bull, color="black", fill=NA, size=0.2) + 
  geom_point(data=sites_bull, aes(x=Easting, y=Northing, shape=Instrument),
             alpha = 1, color="black",
             fill="yellow", size = 3, stroke=1) +
  geom_text(data=sites_bull, aes(x=Easting, y=Northing, label=Name), fontface = "bold",
               size = 3, nudge_x = c(200,0,-240,-250,-250,240),
               nudge_y = c(0,160,0,-50,-100, 0)) +
  scale_shape_manual(values=c(22, 24)) +
  scale_fill_manual(values=c("navajowhite4","navajowhite")) +
  scale_x_continuous(breaks=c(-119.08,-119.07,-119.06,-119.05,-119.04,-119.03), expand = c(0,0)) +
  scale_y_continuous(breaks=c(36.96,36.97,36.98), expand = c(0,0)) +
  labs(title = "Bull", size=1.0) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 315, hjust=0, vjust=0.6),
        axis.text.y = element_text(angle = 315, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="right") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  NULL
#plot(map_bull)

map_bull_no_legend <- map_bull + theme(legend.position="none") 


# --------------------------------------------------------------------------
# Regional map

map_region <- ggplot() +
  geom_sf(data=states_region, color="black", fill="white", size=0.5) +
  geom_sf(data = sn, fill="gray70", color=NA) +
  geom_point(data = location_dot, aes(x=lon, y=lat), size=0.7) +
  geom_text(data=location_dot, aes(x=lon, y=lat), 
            label=c("Providence", "Bull"),
            size = 3, nudge_x = c(-1.18, 1.1), nudge_y = c(-0.6, -1.0)) +
  geom_text(data=location_dot, aes(x=-120.5, y=38.1), 
            label=c("Sierra\nNevada"), size = 4) +
  geom_segment(aes(x=-119.4,y=36.65,xend=-119.24,yend=36.95), size=0.7) +   # Providence
  geom_segment(aes(x=-118.4,y=36.2,xend=-118.95,yend=36.85), size=0.7) +   # Bull
  #scale_color_manual(values=c("black","blue")) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  theme_classic(base_size = 7) +
  theme(legend.position="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  NULL
#plot(map_region)

# --------------------------------------------------------------------------
# Country plot

map_country <- ggplot() +
  geom_sf(data=country, color="black", fill="white", size=0.4) +
  geom_sf(data=california, color="gray20", fill="gray30", size=0.4) +
  scale_x_continuous(expand=c(0.07,0.07)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0.07,0.07)) +   # This eliminates margin buffer around plot
  theme_classic(base_size = 3) +
  theme(legend.position="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  NULL
#plot(map_country)

# --------------------------------------------------------------------------
# Patchwork

legend_prov <- cowplot::get_legend(map_prov)


layout <- c(
  area(t = 1, l = 1, b = 50, r = 50),
  area(t = 51, l = 1, b = 90, r = 99),
  area(t = 1, l = 51, b = 35, r = 99),
  area(t = 3, l = 73, b = 12, r = 94),
  area(t = 42, l = 60, b = 49, r = 90)  
)

x <- map_prov_no_legend + map_bull_no_legend + 
  map_region + map_country + legend_prov +
  plot_layout(design = layout)

ggsave("output/krew_paired_watershed_map.pdf", plot=x, width = 6.2, height = 10)
ggsave("output/krew_paired_watershed_map.eps", plot=x, width = 6.2, height = 10)




