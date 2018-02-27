---
title: "Birding plots"
author: "Tim Holland"
date: "1/10/2018"
output: html_document
---

```{r setup, echo=FALSE,include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

### packages required ###
require(cowplot)
require(colorspace)
require(sf)
require(rgdal)
require(sp)
require(knitr)
require(rmarkdown)
require(tidyverse)

### aesthetic specs ###
figTitleSz <- 30
figAxTitleSz <- 22
figLabelSz <- 18
lwdMain <- 0.8 #primary line width for line graphs
lwdDotted <- 0.5 * lwdMain #line width for dotted lines to labels
ptSz <- lwdMain * 1.4
```

## R Markdown

```{r readin, echo=FALSE, include=FALSE, quiet=T}

curYear <- 2018
homeState <- "CA"
homeCounty <- "Alameda"

ebPal <- c("#e76f51","#f4a261","#e9c46a","#2a9d8f","#264653")

muteCol <- function(col,sVal,vVal){
  cTemp <-hsv(h=rgb2hsv(t(coords(hex2RGB(col))))[1],s=sVal,v=vVal)
  cTemp}

#muteCol <- function(col,sVal,vVal){
  #   cRamp <- colorRampPalette(c(col,"white"))(100)
  # cRamp[perc]}


orders <- read_csv("order_names.csv")

clem <- read_csv(file="eBird-Clements-integrated-checklist-v2017.csv") %>% 
  rename(common_name = `English name`, species_group = `eBird species group`, sort2017 = `sort v2017`, latin_name_incSubsp = `scientific name`) %>% 
  select(-starts_with("X"),-`eBird species code 2017`,-extinct, -`extinct year`, -range) %>%
  separate(latin_name_incSubsp,sep=" ",into=c("genus","species","subspA","subspB"),remove=F) %>%
  unite(latin_binomial, c(genus, species),sep=" ", remove=F) %>%
  unite(subspTemp, c(subspA,subspB),sep=" ",remove=T) %>%
  mutate(subspecies = gsub("NA","",subspTemp)) %>%
  left_join(select(orders, c(order,order_with_desc)),by="order") %>%
  rename(family_with_desc=family) %>%
  separate(family_with_desc,sep=" ",into=c("family","famTemp"),remove=F) %>%
  select(-subspTemp,-famTemp)

clem.sp <- filter(clem,category=="species")

clem <- clem %>% #cleaning up clem
  mutate(genus=replace(genus, !genus %in% clem.sp$genus, NA)) %>% #removes "genus" names that aren't actually genus names (i.e. the "goose" that would come from "goose sp.")
  mutate(latin_binomial=replace(latin_binomial, category %in% c("spuh"),NA)) %>%
  mutate(subspecies = replace(subspecies,subspecies==" ",NA)) %>%
  mutate(species = replace(species,species=="sp.",NA))
  
#filter(clem.sp,grepl(pattern=genuslist$genus[8],latin_name_incSubsp))

# clem.sp %>%   # use to generate list of families in each order from current Clements list (to check descriptive names)
# select(c(family,order))%>%
# distinct(family,.keep_all=T) %>%
# arrange(order) %>%
# write_csv("famOrd.csv")

iso <- read_csv(file="https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>%
  rename(iso2 = `alpha-2`, iso3 = `alpha-3`, country = name) %>%
  select(country, iso2, iso3)

calist <- read_table("CA_main_list.txt")
names(calist) <- "list"
calist <- calist %>%
  filter(str_detect(list,"\t\t")) 
calist$list <- calist$list %>%
  str_replace("\t\t","") ##this only works after doing a manual edit of the original text file to remove extra tabs at line end after the order Phaethontiformes - Tropicbirds
calist<-str_split(calist$list,pattern="\\(")
calist<-unlist(calist)[seq(from=2,to=length(unlist(calist)),by=2)]
calist<-str_split(calist,pattern="\\)")
calist<-unlist(calist)[seq(from=1,to=length(unlist(calist))-1,by=2)]
calist<-gsub("Porphyrio martinicus","Porphyrio martinica",calist)
calist<-tibble(calist)
calist<-calist %>%
  rename("latin_binomial"=calist) %>%
  left_join(.,clem,by=c("latin_binomial"="latin_name_incSubsp"))

myeb <- read_csv(file="MyEBirdData.csv") %>%
  filter(Count!=0) %>% #downloaded data has ZERO counts included (i.e. ones where I corrected an ID by zero'ing out one that I had selected by mistake)
  select(c(2:12)) %>%
  rename(common_name_incSubsp = `Common Name`, latin_name_incSubsp = `Scientific Name`, count = Count, taxon_sort = `Taxonomic Order`, state_prov_code = `State/Province`, county = County, location = Location, latitude = Latitude, longitude = Longitude, date = Date, time = Time) %>%
  mutate (iso2 = substr(state_prov_code,1,2)) %>%
  left_join(iso, by="iso2") %>%
  left_join(select(clem,-common_name), by = "latin_name_incSubsp") %>%
  mutate (state_prov_2 = substr(state_prov_code,4,5)) %>%
  mutate (date = as.Date(date, format="%m-%d-%Y")) %>%
  left_join(select(clem,common_name,latin_name_incSubsp), by = c("latin_binomial" = "latin_name_incSubsp")) %>%
  mutate(aba=ifelse(iso3=="CAN"|iso3=="USA"|iso3=="SPM",1,0)) %>%
  mutate(year=as.integer(substr(date,1,4))) %>% 
  arrange(date) %>%
  mutate(month_year=format(date,format="%b %Y")) %>%
  mutate(month_year=factor(month_year,levels=unique(month_year)))

myeb.sp <- filter(myeb, category=="species" | category=="form" | category=="group (monotypic)" | category=="group (polytypic)" | latin_binomial=="Columba livia")

g<- ggplot()+theme_set(theme_gray()) + geom_step(data=myeb,aes(date,seq_along(date))) #this line is here as a weird hack. package cowplot modifies the default plotting theme for ggplot. But when I set the theme explicitly in my plotting function later, it only works for the 2nd plot, and leaves the first plot showing the cowplot version. So I put this here because it seems to get the ggplot theme back in place for the 1st plot. 

```


``` {r area.funtion, echo=FALSE, include=FALSE}
knitr::opts_chunk$set(knitr.kable.NA = "")

areaList<-function(myeb.subset,subset.name){

  sp.list <- myeb.subset %>%
    filter(category %in% c("species","form","group (monotypic)","group (polytypic)") | latin_binomial=="Columba livia") %>%
    filter(!is.na(latin_binomial)) %>%
    arrange(date,time) %>%
    distinct(latin_binomial,.keep_all=TRUE)

  mostRecents <- slice(distinct(sp.list,latin_binomial,.keep_all=T),n()) %>%
    bind_rows(slice(distinct(sp.list,genus,.keep_all=T),n())) %>%
    bind_rows(slice(distinct(sp.list,family,.keep_all=T),n())) %>%
    bind_rows(slice(distinct(sp.list,order,.keep_all=T),n())) %>% 
    bind_cols(taxon_level=c("Species","Genus","Family","Order")) %>% 
    bind_cols(new_taxon = rep(NA,4)) %>% 
    bind_cols(today = rep(Sys.Date(),4)) %>%
    bind_cols(taxon_count = rep(NA,4)) %>% 
    mutate(taxon_count = replace(taxon_count,taxon_level=="Species",length(rownames(distinct(sp.list,latin_binomial))))) %>%
    mutate(taxon_count = replace(taxon_count,taxon_level=="Genus",length(rownames(distinct(sp.list,genus))))) %>%
    mutate(taxon_count = replace(taxon_count,taxon_level=="Family",length(rownames(distinct(sp.list,family))))) %>%
    mutate(taxon_count = replace(taxon_count,taxon_level=="Order",length(rownames(distinct(sp.list,order))))) %>%
    mutate(new_taxon = replace(new_taxon,taxon_level=="Species",latin_binomial[1])) %>%
    mutate(new_taxon = replace(new_taxon,taxon_level=="Genus",genus[2])) %>%
    mutate(new_taxon = replace(new_taxon,taxon_level=="Family",family_with_desc[3])) %>%
    mutate(new_taxon = replace(new_taxon,taxon_level=="Order",order_with_desc[4])) %>%
    mutate(county = replace(county,is.na(county),"")) 

  list(subset.name,sp.list,mostRecents)
  }
```

``` {r plot.function,echo=FALSE, include=FALSE}

areaVis<- function(areaList){ 
  subset.name <- areaList[[1]]
  sp.list <- areaList[[2]]
  mostRecents <- areaList[[3]]

  mostRecents <- mostRecents %>%
    mutate(today = today+1000) %>%
    mutate(linecol = ebPal[c(1,3:5)]) %>%
    mutate(taxon_plural = c("Species","Genera","Families","Orders"))
  
wid <-0.8
    
g<- ggplot()+
theme_set(theme_gray()) +
ggtitle(paste(subset.name,": Bird taxa observed at each taxonomic level (cummulative)",sep="")) +
geom_step(data=filter(distinct(sp.list,order,.keep_all=T),!is.na(order)),aes(date,seq_along(date)),col=ebPal[5],lwd=lwdMain) +
geom_step(data=filter(distinct(sp.list,family,.keep_all=T),!is.na(family)),aes(date,seq_along(date)),col=ebPal[4],lwd=lwdMain) +
geom_step(data=filter(distinct(sp.list,genus,.keep_all=T),!is.na(genus)),aes(date,seq_along(date)),col=ebPal[3],lwd=lwdMain) +
geom_step(data=sp.list,aes(date,seq_along(date)),col=ebPal[1],lwd=lwdMain) +
geom_line(data=gather(mostRecents,key="lineEnds","date",date,today),aes(x=date,y=taxon_count,group=taxon_level),col=c(ebPal[4],ebPal[4],ebPal[3],ebPal[3],ebPal[5],ebPal[5],ebPal[1],ebPal[1]),lwd=lwdDotted,lty=3) +
geom_point(data=mostRecents,aes(x=date,y=taxon_count),col=c(ebPal[1],ebPal[3],ebPal[4],ebPal[5]),cex=wid*1.4) +
geom_label(data=mostRecents,aes(x=today-300,y=taxon_count,label=paste(taxon_plural,": ",taxon_count,sep="")),hjust=0.5)

#scale_x_continuous(breaks=seq(from=2004,to=2018,by=2),origin = "1899-12-30")
#xlim(2015,2019,origin = "1899-12-30")
print(g)

kable(mostRecents[,c("taxon_level","new_taxon","common_name","date","county","state_prov_2","iso3")],
      col.names = c("Level","Newest Addition","Species","Date","County","Prov/State","Country"),
      caption=paste("Most recent addition to the", subset.name, "list at each taxonomic level"),
      knitr.kable.NA = "")
}
```

``` {r areaPlots,echo=FALSE}
areaVis(areaList(myeb,"World"))
areaVis(areaList(subset(myeb,state_prov_code=="US-CA"),"California"))
areaVis(areaList(subset(myeb,state_prov_code=="CA-QC"),"Quebec"))
areaVis(areaList(subset(myeb,iso3=="CAN"),"Canada"))

```

``` {r ordersPlots, echo=FALSE}
myOrders <- myeb %>%
  filter(category != "domestic" | latin_binomial=="Columba livia") %>%
  distinct(latin_binomial,.keep_all=T) %>%
  group_by(order_with_desc) %>%
  summarize(myOrders = n()) %>%
  arrange(desc(myOrders))

orderSum <- clem.sp %>%
  group_by(order_with_desc) %>%
  summarize(totOrders = n()) %>%
  arrange(desc(totOrders)) %>%
  left_join(myOrders,by="order_with_desc") %>%
  mutate(myOrders = replace(myOrders,is.na(myOrders),0)) %>%
  mutate(order_with_desc=factor(order_with_desc,levels=rev(order_with_desc))) %>%
  mutate(IDed = 100*myOrders/totOrders) %>%
  mutate(NotIDed = 100-IDed)
  
ordersPerc <-  ggplot(data=gather(orderSum,yn,percentage,IDed,NotIDed))+
  geom_col(aes(order_with_desc,percentage,fill=yn),position = position_stack(reverse = TRUE),fill=rep(c(ebPal[5],muteCol(ebPal[5],0.2,0.8)),length(rownames(orderSum)))) +
  coord_flip(ylim=c(0,100),xlim=c(1,length(rownames(orderSum)))) +
  theme(axis.text=element_text(size=figLabelSz), plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) 

ordersRawTrunc <-  ggplot(data=orderSum)+
  geom_col(aes(order_with_desc,totOrders),position = position_stack(reverse = TRUE),fill=muteCol(ebPal[4],0.2,0.8)) +
  geom_col(aes(order_with_desc,myOrders),position = position_stack(reverse = TRUE),fill=ebPal[4]) +
   coord_flip(xlim=c(1,length(rownames(orderSum)))) +
  theme(axis.text.x=element_text(size=figLabelSz),axis.text.y=element_blank(),axis.title.y = element_blank())
```

``` {r ordersLayout, echo=FALSE,fig.width=18,fig.height=18}

title <- ggdraw() + draw_label("Species ID'ed in each order", fontface='bold',size=figTitleSz,hjust=0,x=0)
ordPlots <- plot_grid(ordersPerc + 
                labs(x="Orders",y = "Percentage") + 
                theme(axis.title = element_text(size=figAxTitleSz)),
          ordersRawTrunc + 
                coord_flip(ylim=c(0,600)) + 
                labs(y = "Number of species") + 
                theme(axis.title = element_text(size=figAxTitleSz)),
          ordersRawTrunc + 
   #             scale_x_continuous(minor_breaks=NULL,breaks=c(6000,6200,6400)) +
                coord_flip(ylim=c(6000,6400)) + 
                theme(plot.margin=margin(0.1,0.1,0.1,0.5,"cm"),axis.ticks = element_blank(),axis.title=element_blank()),
          align="h",rel_widths = c(0.5,0.3,0.2),nrow=1)
plot_grid(title,ordPlots,align="v",rel_heights=c(0.05,1),ncol=1)
```

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
