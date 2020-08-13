# Map for Providence watersheds - P301, P303 and P304
# For 'Ram's Paper', aka vegetation change water balance
# Ryan Bart 
# Started 19 March 2020, 1st night of statewide Coronavirus shelter-in-place



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
  dplyr::filter(POLY_ID %in% c("P301", "P303", "P304"))
box_prov <- st_bbox(st_buffer(prov, 0))       # Establish extent of prov. Buffer can be used to add extent



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



# Sites
sites <- st_read("data/krew/KREW_Watersheds/Sites.shp")
sites_prov <- sites %>% 
  st_transform(proj_utm) %>% 
  st_crop(box_prov) %>% 
  dplyr::filter(Name %in% c("UP", "LP","P301", "P303","P304")) %>% 
  mutate(Instrument = case_when(
    Name == "UP" ~ "Meteorological\nStation",
    Name == "LP" ~ "Meteorological\nStation",
    Name == "P301" ~ "Stream\nGauge",
    Name == "P303" ~ "Stream\nGauge",
    Name == "P304" ~ "Stream\nGauge"))

# Add flux tower to sites file
flux <- data.frame(Site = "Flux Tower", Northing = 4104511, Easting=304850.2,
                   Name = "Flux Tower", Instrument = "Flux Tower")
sfc = st_sfc(st_point(c(304850.2,4104511)), crs=26711)
st_geometry(sfc)
st_geometry(flux) <- sfc
flux <- st_transform(flux, proj_utm) 
sites_prov <- rbind(sites_prov, flux)

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

location_dot <- tibble(site=c("prov"),lat=c(37.055),lon=c(-119.19))

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

dem_prov <- dem_full %>% 
  raster::crop(e_prov) %>% 
  rasterToPoints() %>% 
  as_tibble()



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Prov map

shed_labels <- tibble(
  x = c(304900, 305160, 305830),
  y = c(4104830, 4103870, 4102671),
  # x = c(-119.2,-119.19,-119.18),
  # y = c(37.05,37.06,37.07),
  name = c("P301", "P303", "P304")
)

map_prov <- ggplot() +
  geom_sf(data=prov, fill="navajowhite") + 
  stat_contour(aes(x = x, y = y, z=dem30m), data = dem_prov, 
               breaks = c(1725,1750,1775,1800,1825,1850,1875,1900,1925,1950,1975,2000,2025,2050,2075,2100,2125),
               colour="tan", size=0.4) +
  geom_text_contour(aes(x = x, y = y, z=dem30m), data = dem_prov,
                    breaks = c(1750,1775,1800,1825,1850,1875,1900,1925,1950,1975,2000,2025,2050,2075,2100,2125),
                    min.size=10,skip = 1,
                    stroke = 0.3, size=3, color = "tan",
                    check_overlap = TRUE) +
  geom_sf(data=prov, fill=NA) + 
  geom_sf(data=streams_prov, color="blue") +
  geom_point(data=sites_prov, aes(x=Easting, y=Northing, shape=Instrument),
             alpha = 1, color="black",
             fill="yellow", size = 3, stroke=1) +
  # geom_text(data=sites_prov, aes(x=Easting, y=Northing ,label=Name), fontface = "bold",
  #           size = 3, nudge_x = c(120,0,0,-140,-200,0),
  #           nudge_y = c(110,-110,-110,-110,0,0)) +
  geom_text(data=shed_labels, aes(x=x, y=y ,label=name), fontface = "bold", size = 4.6) +
  scale_shape_manual(values=c(21, 22, 24)) +
  scale_x_continuous(breaks=c(-119.2,-119.19,-119.18), expand = c(0,0)) +
  scale_y_continuous(breaks=c(37.04,37.05,37.06,37.07), expand = c(0,0)) +
  theme_bw(base_size = 11) +
  theme(axis.text.y = element_text(angle = 315, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="right") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  NULL
plot(map_prov)

map_prov_no_legend <- map_prov + theme(legend.position="none") 


# --------------------------------------------------------------------------
# Regional map

map_region <- ggplot() +
  geom_sf(data=states_region, color="black", fill="white", size=0.5) +
  geom_sf(data = sn, fill="gray70", color=NA) +
  geom_point(data = location_dot, aes(x=lon, y=lat), size=0.7) +
  geom_text(data=location_dot, aes(x=lon, y=lat), 
            label=c("Providence"),
            size = 2.5, nudge_x = c(-1.18, 1.1), nudge_y = c(-0.6, -1.0)) +
  geom_text(data=location_dot, aes(x=-120.6, y=38.1), 
            label=c("Sierra\nNevada"), size = 3.5) +
  geom_segment(aes(x=-119.43,y=36.62,xend=-119.26,yend=36.93), size=0.7) +   # Providence
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
plot(map_region)

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
plot(map_country)

# --------------------------------------------------------------------------
# Patchwork

legend_prov <- cowplot::get_legend(map_prov)


layout <- c(
  area(t = 1, l = 1, b = 100, r = 70),
  area(t = 10, l = 71, b = 60, r = 99),
  area(t = 17, l = 84, b = 27, r = 97),
  area(t = 61, l = 70, b = 93, r = 99)
)

x <- map_prov_no_legend + map_region +
  map_country + legend_prov +
  plot_layout(design = layout)

ggsave("output/krew_providence_map.pdf", plot=x, width = 7.3, height = 6)





