#  MAke map of dendro trees ####

# clear environment ####
rm(list = ls())

# load libraries ####
library(ggplot2)
library(ggrepel)
library(sf)


# load data ####
trees <- read.csv("data/PointDendrometerTrees.csv")
census <- read.csv(paste0(dirname(getwd()), "/SCBI-ForestGEO-Data/spatial_data/UTM coordinates/scbi_stem_utm_lat_long_2018.csv"))

sp_data_path = paste0(dirname(getwd()), "/SCBI-ForestGEO-Data/spatial_data/shapefiles/")

quadrats <-  read_sf(paste0(sp_data_path, "20m_grid.shp"))
deer <-  read_sf(paste0(sp_data_path, "deer_exclosure_2011.shp"))
roads <-  read_sf(paste0(sp_data_path, "SCBI_roads_clipped_to_plot.shp"))
streams <-  read_sf(paste0(sp_data_path, "SCBI_streams_clipped_to_plot.shp"))
contour_10m <-  read_sf(paste0(sp_data_path, "contour10m_SIGEO_clipped.shp"))

# plot only trees that were censused ####

## get tree data ready
trees[, c("lat", "lon")] <- census[match(trees$tag, census$tag),  c("lat", "lon")]

trees <- st_as_sf(trees, coords =  c("lon", "lat"))

st_crs(trees) <- 4269

tags <- st_centroid(trees)


## get quadrat data ready

rows <- annotate("text", x = seq(747350, 747365, length.out = 32), y = seq(4309135, 4308515, length.out = 32), label = sprintf("%02d", 32:1), size = 3, color = "black")

cols <- annotate("text", x = seq(747394, 747780, length.out = 20), y = seq(4308495, 4308505, length.out = 20), label = sprintf("%02d", 1:20), size = 2.8, color = "black")



ggplot()  +
  geom_sf(data = quadrats) +
  geom_sf(data = deer, fill = "transparent", lwd  = 1) +
  geom_sf(data = roads, color = "#993300", linetype = 2, size = 0.8) +
  geom_sf(data = streams, color = "blue", size=0.5) +
  geom_sf(data = contour_10m, color = "grey") +
  geom_sf(data = trees[is.na(trees$dbh.2018.mm) |is.na(trees$sp), ], pch = 17) +
  geom_sf(data = trees, aes(fill = sp, size = dbh.2018.mm), pch = 21, alpha = .5) +
  ggrepel::geom_text_repel(data =trees, aes(label = tag, geometry = geometry),stat = "sf_coordinates", nudge_x = 10, nudge_y = 5, size = 3) +
  ggtitle("SCBI ForestGEO Plot") +
  rows +
  cols +
  theme(plot.title = element_text(vjust=0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_rect(fill = "white"))


ggsave("doc/AutoDendrometers_map.jpg")  

