library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dggridR)
library(viridis)
library(gsl)
library(ggplot2)
library(ggpubr)
source("lib.R")

# config

xlim <- c(-45, 51)
ylim <- c(20, 90)
datafile_name <- "taxonomy_grouped_20200518"
shapefile_name <- "ne_atlantic.shp"
res <- 9

# construct discrete global grid system
# see https://github.com/r-barnes/dggridR for grid resolutions

dggs <- dgconstruct(projection = "ISEA", topology = "HEXAGON", res = res)

# download from OBIS, read occurrence data, and filter

dir.create("temp")
url <- paste0("https://ams3.digitaloceanspaces.com/obis-datasets/exports/", datafile_name, ".zip")
zip_path <- paste0("temp/", datafile_name, ".zip")
csv_path <- paste0("temp/", datafile_name, ".csv")
if (!file.exists(zip_path)) {
  download.file(url, zip_path)
  unzip(zipfile = zip_path, exdir = "temp")
}
all <- read.csv(csv_path, stringsAsFactors = FALSE)
df <- all %>%
  filter(decimallongitude >= xlim[1] & decimallongitude <= xlim[2] & decimallatitude >= ylim[1] & decimallatitude <= ylim[2])

# add cell IDs, calculate metrics, add polygons

metrics <- df %>%
  add_cell(dggs) %>%
  calc(50) %>%
  add_polygons(dggs)

# write to shapefile

dir.create("shapefiles")
st_write(metrics, "shapefiles/ne_atlantic.shp", delete_layer = TRUE)  

# plot

es <- create_map(metrics, "es", xlim, ylim)
n <- create_map(metrics, "n", xlim, ylim, trans = "log10")
sp <- create_map(metrics, "sp", xlim, ylim, trans = "log10")
shannon <- create_map(metrics, "shannon", xlim, ylim)
simpson <- create_map(metrics, "simpson", xlim, ylim)
hill_1 <- create_map(metrics, "hill_1", xlim, ylim, trans = "log10")

figure <- ggarrange(n, sp, es, shannon, simpson, hill_1, ncol = 3, nrow = 2)
figure

ggsave(filename = "metrics.png", height = 7, width = 12, dpi = 300, scale = 1.4)

