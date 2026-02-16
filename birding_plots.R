
pal<-colorRampPalette(c("black",ebPal[5],ebPal[4],"white"),bias=2)(100)

yStart <- 2015
yEnd <- max(myeb$year)
yNum <- yEnd-yStart + 1
colorIntervals <- round(seq(from=70,to=15,length.out=yNum))

colTab <- data.frame(pal[colorIntervals])
names(colTab) <- c("color")
colTab$year <- as.integer(yStart:yEnd)
colTab <- as_tibble(colTab)

yearsTot <- myeb.sp %>%
  filter(year>=yStart) %>%
  group_by(year)%>%
  distinct(latin_binomial,.keep_all=T)%>%
  summarise(total = n(),last = max(date))%>%
  left_join(colTab,by="year") %>%
  mutate(last_julian=as.integer(format(last,"%j")))  

myeb.byyear <- myeb.sp %>% 
    arrange(date) %>%
    filter(!is.na(latin_binomial)) %>%
    filter(year >= 2015) %>%
    left_join(colTab,by="year") %>%
    group_by(year) %>%
    distinct(latin_binomial,.keep_all=T) %>%
    mutate(julian=as.integer(format(date,"%j")))%>%   
    mutate(seq = seq_along(year))

endLines <- bind_rows(yearsTot,yearsTot)
endLines[(yNum+1):(2*yNum-1),"last_julian"]<-380
endLines[2*yNum,"last_julian"] <- endLines[2*yNum,"last_julian"]+6
endLines <- arrange(endLines,year)
label_rows <- (yNum + 1):(2 * yNum)
    
ggplot() +
  ggtitle("Rate of increase in species list in each new year") +
  geom_step(
    data = myeb.byyear,
    aes(x = julian, y = seq, group = year, color = color)
  ) +
  geom_line(
    data = dplyr::arrange(endLines, year),
    aes(x = last_julian, y = total, group = year, color = color),
    linewidth = lwdDotted,
    linetype = 3
  ) +
  geom_point(
    data = yearsTot,
    aes(x = last_julian, y = total, color = color),
    size = ptSz
  ) +
  geom_label(
    data = endLines[label_rows, ],
    aes(x = last_julian, y = total, label = paste0(year, ": ", total, " sp.")),
    hjust = 0
  ) +
  scale_color_identity() +
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
    labels = c(month.abb, month.abb[1]),
    limits = c(0, 450)
  ) +
  theme_set(theme_gray())

crs_eck4      <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

myeb.spatial <- sf::st_as_sf(myeb, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
myeb.spatial <- sf::st_transform(myeb.spatial, crs_eck4)

countries   <- sf::st_read("ne_110m_admin_0_countries.shp", quiet = TRUE)
land <- sf::st_read("ne_110m_land.shp", quiet = TRUE)
box <- sf::st_sfc(
  sf::st_polygon(list(matrix(
    c(-180,  90,
      180,  90,
      180, -90,
      -180, -90,
      -180,  90),
    byrow = TRUE,
    ncol = 2
  ))),
  crs = 4326
)
grid <- sf::st_make_grid(box, n = c(18,18), what = "polygons")
grid <- sf::st_sf(geometry = grid)

land          <- sf::st_transform(land, crs_eck4)
countries     <- sf::st_transform(countries, crs_eck4)
grid          <- sf::st_transform(grid, crs_eck4)


checklistsMap <- ggplot() +
  ggtitle("eBird checklists submitted") +
  geom_sf(data = grid, fill = "white", color = grey(0.7), linewidth = 0.2) +
  geom_sf(data = countries, fill = grey(0.95), color = grey(0.4), linewidth = 0.1) +
  geom_sf(data = land, fill = NA, color = "black", linewidth = 0.25) +
  geom_sf(data = myeb.spatial, color = ebPal[4], alpha = 1/2, size = 1) +
  coord_sf() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = figTitleSz)
  )



print(checklistsMap)

