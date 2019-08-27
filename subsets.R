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

# THIS IS CODE FOR GENERATING GROUP SPECIFIC SHAPEFILES
# RUN SCRIPT.R FIRST TO GENERATE THE FULL OCCURRENCE TABLE

subsets <- list(
  mammals = df %>% filter(classid == 1837),
  turtles = df %>% filter(orderid == 2689),
  shallow = df %>% filter(minimumdepthinmeters <= 100 | maximumdepthinmeters <= 100),
  deep = df %>% filter(minimumdepthinmeters > 100 | maximumdepthinmeters > 100)
)

for (name in names(subsets)) {
  message(name)
  metrics <- subsets[[name]] %>%
    add_cell(dggs) %>%
    calc(50) %>%
    add_polygons(dggs)
  dir.create("shapefiles")
  st_write(metrics, paste0("shapefiles/", name, ".shp"), delete_layer = TRUE)  
  es <- create_map(metrics, "es", xlim, ylim)
  n <- create_map(metrics, "n", xlim, ylim, trans= "log10")
  sp <- create_map(metrics, "sp", xlim, ylim, trans= "log10")
  shannon <- create_map(metrics, "shannon", xlim, ylim)
  simpson <- create_map(metrics, "simpson", xlim, ylim)
  hill_1 <- create_map(metrics, "hill_1", xlim, ylim, trans= "log10")
  figure <- ggarrange(n, sp, es, shannon, simpson, hill_1, ncol = 3, nrow = 2)
  figure
  dir.create("maps")
  ggsave(filename = paste0("maps/", name, ".png"), height = 7, width = 12, dpi = 300, scale = 1.4)
}
