listMap <- function(myeb.area = NULL,
                    projmap = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
                    admMap = "ne_110m_admin_0_countries",
                    landMap = "ne_110m_land",
                    maptitle = "t",
                    boxlim = c(90, 180, -90, -180)) {
  
  if (is.null(myeb.area) || nrow(myeb.area) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::ggtitle(paste0(maptitle, " (no data)")))
  }
  if (!all(c("longitude", "latitude") %in% names(myeb.area))) {
    stop("myeb.area must contain columns: longitude, latitude")
  }
  
  crs_ll <- 4326
  
  pts <- sf::st_as_sf(myeb.area, coords = c("longitude", "latitude"), crs = crs_ll, remove = FALSE)
  
  countries <- sf::st_read(paste0(admMap, ".shp"), quiet = TRUE)
  land      <- sf::st_read(paste0(landMap, ".shp"), quiet = TRUE)
  
  # boxlim = c(N, E, S, W)
  box_matrix <- matrix(
    c(boxlim[4], boxlim[1],
      boxlim[2], boxlim[1],
      boxlim[2], boxlim[3],
      boxlim[4], boxlim[3],
      boxlim[4], boxlim[1]),
    byrow = TRUE, ncol = 2
  )
  box <- sf::st_sfc(sf::st_polygon(list(box_matrix)), crs = crs_ll)
  
  grid <- sf::st_make_grid(box, n = c(18, 18), what = "polygons")
  grid <- sf::st_sf(geometry = grid)
  
  countries <- sf::st_transform(countries, projmap)
  land      <- sf::st_transform(land, projmap)
  grid      <- sf::st_transform(grid, projmap)
  pts       <- sf::st_transform(pts, projmap)
  
  checklistsMap <- ggplot2::ggplot() +
    ggplot2::ggtitle(maptitle) +
    ggplot2::geom_sf(data = grid, fill = "white", color = grey(0.7), linewidth = 0.2) +
    ggplot2::geom_sf(data = countries, fill = grey(0.95), color = grey(0.4), linewidth = 0.1) +
    ggplot2::geom_sf(data = land, fill = NA, color = "black", linewidth = 0.25) +
    ggplot2::geom_sf(data = pts, color = ebPal[4], alpha = 1/2, size = 1) +
    ggplot2::coord_sf() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = figTitleSz)
    )
  
  checklistsMap
}