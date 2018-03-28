listMap <- function(myeb.area=NULL,
                    projmap="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
                    admMap="ne_110m_admin_0_countries",
                    landMap="ne_110m_land",
                    maptitle="t",
                    boxlim=c(90,180,-90,-180)){

myeb.spatial              <- data.frame(myeb.area)
coordinates(myeb.spatial) <-c("longitude","latitude")
proj4string(myeb.spatial) <-CRS("+proj=longlat +datum=NAD83")

#countries   <- readOGR("ne_110m_admin_0_countries.shp",layer="ne_110m_admin_0_countries")
#land        <- readOGR("ne_110m_land.shp",layer="ne_110m_land")

countries   <- readOGR(paste(admMap,".shp",sep=""),layer=admMap)
land        <- readOGR(paste(landMap,".shp",sep=""),layer=landMap)
box   <-matrix(c(boxlim[4],boxlim[1], boxlim[2],  boxlim[1], boxlim[2], boxlim[3], boxlim[4], boxlim[3], boxlim[4],  boxlim[1]), byrow = TRUE, ncol = 2) %>%
  list()%>% st_polygon() %>% st_sfc(.,crs=4326)
grid  <-st_make_grid(box,n=c(18,18),crs=4326,what="polygons")
grid <- as(grid,"Spatial")

myeb.spatial  <- spTransform(myeb.spatial, 
                             CRS(projmap))
land          <- spTransform(land,
                             CRS(projmap))
countries     <- spTransform(countries,
                             CRS(projmap))
grid          <- spTransform(grid,
                             CRS(projmap))

checklistsMap <- ggplot()+
  ggtitle(maptitle) +
  geom_polygon(data=grid,aes(x=long,y=lat,group=group),fill="white",col=grey(0.4),lwd=0.5)+
  geom_polygon(data=grid,aes(x=long,y=lat,group=group),fill="white",col=grey(0.7),lwd=0.2)+
  geom_polygon(data=countries,aes(x=long,y=lat,group=group),col=grey(0.4),fill=grey(0.95),lwd=0.1)+
  geom_polygon(data=land,aes(x=long,y=lat,group=group),col="black",fill=NA,lwd=0.25)+
  geom_point(data=data.frame(myeb.spatial),aes(x=longitude,y=latitude),shape=16,color=ebPal[4],alpha=1/2,cex=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text = element_blank(),axis.line = element_blank(),axis.title = element_blank(),axis.ticks=element_blank(),plot.title=element_text(size=figTitleSz)) +
  coord_fixed()

checklistsMap
}