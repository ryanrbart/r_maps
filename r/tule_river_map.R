# Map for Tule River application
# Ryan Bart - December 2019


# Tule River
# USGS Gauge Number: 11203580 (50km2)
# USGS Gauge Number: 11204100 (250km2)


# To do list
# Make inset map


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
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
library(rgdal)

proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj_utm <- "+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs"


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Read in spatial data

# Import Gages 2 boundary data
usgs_boundary <- st_read("data/gages_2/boundaries-shapefiles-by-aggeco/bas_ref_all.shp") %>% 
  st_transform(proj_longlat) 
#gage_2_boundary$GAGE_ID

# Upper tule watershed boundary
usgs_boundary_upper <- usgs_boundary %>% 
  dplyr::filter(GAGE_ID == 11203580)

# Lower tule watershed boundary
usgs_boundary_lower <- usgs_boundary %>% 
  dplyr::filter(GAGE_ID == 11204100)
e <- st_bbox(usgs_boundary_lower)


# Import Gages 2 point data
usgs_gage <- st_read("data/gages_2/gagesII_9322_point_shapefile/gagesII_9322_sept30_2011.shp") %>% 
  st_transform(proj_longlat) 
#gage_2_point$GAGE_ID

# Upper tule watershed gage
usgs_gage_upper <- usgs_gage %>% 
  dplyr::filter(STAID == 11203580)

# Lower tule watershed gage
usgs_gage_lower <- usgs_gage %>% 
  dplyr::filter(STAID == 11204100)


# Import shapefile for Tule reservation
# https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-american-indian-alaska-native-native-hawaiian-area
tule_reservation <- st_read("data/indian_reservations/tl_2017_us_aiannh.shp") %>% 
  st_transform(proj_longlat) %>% 
  dplyr::filter(GEOID == "4300R")

# Combine large usgs watershed and tule reservation to provide largest relevant extent
full_extent <- st_union(usgs_boundary_lower, tule_reservation)

ssrwmg <- st_read("data/ssirwmp_gis/SSIRWM.shp") %>% 
  st_transform(proj_longlat)

rivers <- st_read("data/California_Streams/California_Streams.shp") %>% 
  st_crop(ssrwmg)
rivers_detail <- dplyr::filter(rivers, Source_Mea > 3000) %>% 
  st_intersection(full_extent)
rivers_broad <- dplyr::filter(rivers, Source_Mea > 20000)

dac_irwm <- st_read("data/dwr_census_dac_2016/Census_Place_Disadvantaged_Communities_2016.shp") %>% 
  st_transform(proj_longlat) %>% 
  st_simplify(dTolerance = 0.01) %>% 
  st_intersection(ssrwmg)
# Mapping DACs across larger scene produces error. Not solved by following code: https://github.com/r-spatial/sf/issues/347
# st_intersection(st_make_valid(sheds_ssirwm))
# st_intersection(st_buffer(sheds_ssirwm, 0))



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Tule Watershed Map

tule_detail <- ggplot() + 
  geom_sf(data=tule_reservation, color = "gray35", fill="lightblue", size=0.9) +
  #geom_sf(data=usgs_boundary_lower, color="black", fill="gray95", alpha = 0.5, size=0.7) +
  geom_sf(data=usgs_boundary_upper, color="black", fill="gray85", alpha = 0.5, size=0.7) +
  geom_sf(data=rivers_detail, color = "dodgerblue") +
  geom_sf(data=usgs_gage_upper, color = "blue", size=2) +
  geom_sf_text(data=tule_reservation, aes(label="Tule River\nReservation"), 
               fontface = "bold", size=3.5,
               nudge_x = c(-0.039), nudge_y = c(-0.019))  +
  geom_sf_text(data=usgs_gage_upper, aes(label="USGS\nGauge"), 
               fontface = "bold", size=2.8,
               nudge_x = c(-0.015), nudge_y = c(0.00))  +
  geom_sf_text(data=usgs_boundary_upper, aes(label="South Fork\nTule River\nUpper Watershed"), 
               fontface = "bold", size=3.5,
               nudge_x = c(0.009), nudge_y = c(0.008))  +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 315, hjust=0, vjust=0.6),
        axis.text.y = element_text(angle = 315, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  NULL
plot(tule_detail)

ggsave("output/tule_reservation_map_detail.pdf", plot=tule_detail, width = 5, height = 4)





tule_broad <- ggplot() +
  geom_sf(data=ssrwmg, color="gray35", fill=NA, size=0.9) + 
  geom_sf(data=tule_reservation, color = "gray35", fill="lightblue", size=0.9) +
  geom_sf(data=usgs_boundary_upper, color="black", fill="gray85", alpha = 0.5, size=0.7) +
  #geom_sf(data=rivers_broad, color="dodgerblue") +
  #geom_sf(data=dac_irwm, color="firebrick", aes(fill="DACs"), alpha=.8) +
  geom_sf_text(data=ssrwmg, aes(label="Southern\nSierra\nIRWM"),
               fontface = "bold", size=8,
               nudge_x = c(0.00), nudge_y = c(0.00))  +
  #geom_rect(aes(xmin=-118.85, xmax=-118.55,ymin=35.93,ymax=36.14), fill=NA, color="black", size=0.8) +
  scale_x_continuous(breaks=c(-119.5,-119,-118.5,-118), expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  # scale_fill_manual(values = c("DACs" = "navajowhite"), name = NULL,
  #                   guide = guide_legend(override.aes = list(color = "firebrick", fill = "navajowhite"))) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        # axis.text.x = element_text(angle = 315, hjust=0, vjust=0.6),
        # axis.text.y = element_text(angle = 315, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  #ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  #theme(legend.position="right") +
  NULL
plot(tule_broad)
ggsave("output/tule_reservation_map_broad.pdf", plot=tule_broad, width = 4, height = 5.8)









# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make KMLs

st_write(tule_reservation, dsn = "output/kml/tule_reservation.kml")

st_write(usgs_boundary_upper, dsn = "output/kml/sf_tule_upper_11203580.kml")

st_write(usgs_boundary_lower, dsn = "output/kml/sf_tule_lower_11204100.kml")



