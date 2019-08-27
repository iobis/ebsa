library(dplyr)
library(dggridR)
library(gsl)

#' Calculates all metrics from a dataframe of occurrences.
#' 
#' @param df Dataframe of occurrences. Required columns are cell, speciesid, and records.
#' @param esn Hurlbert index sample size.
calc <- function(df, esn = 50) {
  t1 <- df %>%
    group_by(cell, speciesid) %>%
    summarize(ni = sum(records))
  t2 <- t1 %>%
    group_by(cell) %>%
    mutate(n = sum(ni))
  t3 <- t2 %>%
    group_by(cell, speciesid) %>%
    mutate(
      hi = -(ni/n*log(ni/n)),
      si = (ni/n)^2,
      qi = ni/n,
      esi = case_when(
        n-ni >= esn ~ 1-exp(lngamma(n-ni+1)+lngamma(n-esn+1)-lngamma(n-ni-esn+1)-lngamma(n+1)),
        n >= esn ~ 1
      )
    )
  t4 <- t3 %>%
    group_by(cell) %>%
    summarize(
      n = sum(ni),
      sp = n(),
      shannon = sum(hi),
      simpson = sum(si),
      maxp = max(qi),
      es = sum(esi)
    )
  result <- t4 %>%
    mutate(
      hill_1 = exp(shannon),
      hill_2 = 1/simpson,
      hill_inf = 1/maxp
    )
  return(result)
}

#' Add cell discrete global grid system cell IDs to a dataframe of occurrences.
#' 
#' @param df Dataframe of occurrences. Required columns are decimallongitude and decimallatitude.
#' @param dggs Discrete global grid system object.
add_cell <- function(df, dggs){
  df$cell <- dgGEO_to_SEQNUM(dggs, df$decimallongitude, df$decimallatitude)$seqnum
  return(df)
}

#' Creates a sf object from a metrics table.
#' 
#' @param df Metrics table. Required columns are cell.
#' @param dggs Discrete global grid system object.
add_polygons <- function(df, dggs) {
  grid <- dgearthgrid(dggs, frame = FALSE, wrapcells = FALSE)
  grid_sf <- st_as_sf(grid)
  grid_wrap <- st_wrap_dateline(grid_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=230"))
  grid_wrap$cell <- names(grid)
  grid_wrap <- merge(grid_wrap, df, by.x = "cell", by.y = "cell")
  return(grid_wrap)
}

#' Create a ggplot map.
#' 
#' @param metrics Metrics dataframe.
#' @param metric Metric to be visualized.
#' @param xlim Longitude range.
#' @param ylim Latitude range.
create_map <- function(metrics, metric = "es", xlim = c(-180, 180), ylim = c(-90, 90), trans = "identity") {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  g <- ggplot() +
    geom_sf(data = metrics %>% filter(!is.na(get(metric))), aes_string(fill = metric, geometry = "geometry"), lwd = 0) +
    scale_fill_viridis(option = "inferno", trans = trans) +
    geom_sf(data = world, fill = "#dddddd", color = NA) +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) + xlab("") + ylab("") +
    coord_sf(xlim = xlim, ylim = ylim)
  return(g)
}
