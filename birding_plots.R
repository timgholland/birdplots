

``` {r yearsCummulative, echo=FALSE}
pal<-colorRampPalette(c("black",ebPal[5],ebPal[4],"white"),bias=2)(100)

yStart <- 2015
yEnd <- max(myeb$year)
yNum <- yEnd-yStart + 1
colorIntervals <- round(seq(from=70,to=15,length.out=yNum))

colTab <- data.frame(pal[colorIntervals])
names(colTab) <- c("color")
colTab$year <- as.integer(2015:2018)
colTab <- as_tibble(colTab)

yearsTot <- myeb.sp %>%
  filter(year>=2015) %>%
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
    
ggplot() +
ggtitle("Rate of increase in species list in each new year") +
geom_step(data=myeb.byyear, aes(julian,seq,group=year),color=myeb.byyear$color) +
geom_line(data=arrange(endLines,year),aes(x=last_julian,y=total,group=year),lwd=lwdDotted,lty=3,color=endLines$color) + 
geom_point(data=yearsTot,aes(x=last_julian,y=total),col=yearsTot$color,cex=ptSz) +
geom_label(data=endLines[c(2,4,6,8),],aes(x=last_julian,y=total,label=paste(year,": ",total," sp.",sep="")),hjust=0) +
scale_x_continuous(minor_breaks=NULL,breaks=c(0,31,59,90,120,151,181,212,243,273,304,334,365),labels=c(month.abb,month.abb[1]),limits=c(0,450)) +
theme_set(theme_gray())

```

``` {r checklistMaps, echo=FALSE,include=FALSE}
myeb.spatial              <- data.frame(myeb)
coordinates(myeb.spatial) <-c("longitude","latitude")
proj4string(myeb.spatial) <-CRS("+proj=longlat +datum=NAD83")

countries   <- readOGR("ne_110m_admin_0_countries.shp",layer="ne_110m_admin_0_countries")
land        <- readOGR("ne_110m_land.shp",layer="ne_110m_land")
box   <-matrix(c(-180,  90, 180,  90, 180, -90, -180, -90, -180,  90), byrow = TRUE, ncol = 2) %>%
    list()%>% st_polygon() %>% st_sfc(.,crs=4326)
grid  <-st_make_grid(box,n=c(18,18),crs=4326,what="polygons")
grid <- as(grid,"Spatial")

myeb.spatial  <- spTransform(myeb.spatial, 
                    CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
land          <- spTransform(land,
                    CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
countries     <- spTransform(countries,
                    CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
grid          <- spTransform(grid,
                    CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

checklistsMap <- ggplot()+
  ggtitle("eBird checklists submitted") +
  geom_polygon(data=grid,aes(x=long,y=lat,group=group),fill="white",col=grey(0.4),lwd=0.5)+
  geom_polygon(data=grid,aes(x=long,y=lat,group=group),fill="white",col=grey(0.7),lwd=0.2)+
  geom_polygon(data=countries,aes(x=long,y=lat,group=group),col=grey(0.4),fill=grey(0.95),lwd=0.1)+
  geom_polygon(data=land,aes(x=long,y=lat,group=group),col="black",fill=NA,lwd=0.25)+
  geom_point(data=data.frame(myeb.spatial),aes(x=longitude,y=latitude),shape=16,color=ebPal[4],alpha=1/2,cex=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text = element_blank(),axis.line = element_blank(),axis.title = element_blank(),axis.ticks=element_blank(),plot.title=element_text(size=figTitleSz)) +
  coord_fixed()
```

``` {r printmap, echo=FALSE,fig.width=18,fig.height=14}
print(checklistsMap)
```
