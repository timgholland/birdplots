countsBars <- function(myeb.area=NULL,minDate=min(myeb.area$date),maxDate=max(myeb.area$date),y=T,m=T,w=T,d=T,lifers=F){

eb.tmp <- myeb.area

#minDate <- min(eb.tmp$date)
#maxDate <- max(eb.tmp$date)
#maxDate <- as.Date("2017-12-01")
dateRange <- as.numeric(maxDate-minDate)
allDays <- tibble("allDays"=seq(minDate,maxDate,by=1))
eb.tmp <- right_join(eb.tmp,allDays,by=c("date"="allDays"))

eb.tmp$year <- as.Date(cut(eb.tmp$date, breaks = "year"))
eb.tmp$month <- as.Date(cut(eb.tmp$date, breaks = "month"))
eb.tmp$week <- as.Date(cut(eb.tmp$date, breaks = "week"))

yTab <-eb.tmp %>%
  group_by(year) %>%
  distinct(latin_binomial) %>%
  summarise(count=sum(!is.na(latin_binomial))) %>%
  mutate(width=as.numeric(diff(c(year,maxDate)))) %>%
  filter(count!=0)

mTab <-eb.tmp %>%
  group_by(month) %>%
  distinct(latin_binomial) %>%
  summarise(count=sum(!is.na(latin_binomial))) %>%
  mutate(width=as.numeric(diff(c(month,maxDate)))) %>%
  filter(count!=0)

  #####
#####
# mutate(width=diff(c()))

wTab <- eb.tmp %>%
  group_by(week) %>%
  distinct(latin_binomial) %>%
  summarise(count=sum(!is.na(latin_binomial))) %>%
  mutate(width=as.numeric(diff(c(week,maxDate)))) %>%
  filter(count!=0)

dTab <- eb.tmp %>%
  group_by(date) %>%
  distinct(latin_binomial) %>%
  summarise(count=sum(!is.na(latin_binomial))) %>%
  mutate(width=as.numeric(diff(c(date,maxDate)))) %>%
  filter(count!=0)


### need to fix position so that columns have left edge at the beginning of each period (currently centred at beginning)
if (dateRange >= 500) biggap<-1
if (dateRange < 500) biggap<-0.5
smallgap <- 0.05
spAll<- ggplot() +
  coord_cartesian(xlim=c(minDate,maxDate)) +
  labs(x="Date",y="Number of species")

if (dateRange >= 1500){
  spAll <- spAll+scale_x_date(date_labels = ("%Y"), date_breaks="1 year",date_minor_breaks="1 year",limits=as.Date(c("2000-01-01","2100-01-01"))) }
if (dateRange < 1500 & dateRange >=500){
  spAll <- spAll+scale_x_date(date_labels = ("%b %Y"), date_breaks="6 months",date_minor_breaks="1 month") }
if (dateRange < 500 & dateRange >=150){
  spAll <- spAll+scale_x_date(date_labels = ("%b '%y"), date_breaks="3 months",date_minor_breaks="1 month") }
if (dateRange < 150){
  spAll <- spAll+scale_x_date(date_labels = ("%b %d %Y"), date_breaks="1 month",date_minor_breaks="1 day") }

 if(y) spAll <- spAll+geom_col(data=yTab,aes(year,count),fill=ebPal[5],width=yTab$width-biggap,position=position_nudge(x=yTab$width/2-biggap/2))
 if(m) spAll <- spAll+geom_col(data=mTab,aes(month,count),fill=ebPal[4],width=mTab$width-biggap,position=position_nudge(x=mTab$width/2-biggap/2))
 if(w) spAll <- spAll+geom_col(data=wTab,aes(week,count),fill=ebPal[3],width=wTab$width-smallgap,position=position_nudge(x=wTab$width/2-smallgap/2))
 if(d) spAll <- spAll+geom_col(data=dTab,aes(date,count),fill=ebPal[1],width=dTab$width-smallgap,position=position_nudge(x=dTab$width/2-smallgap/2))
  #plot(yTab$year,yTab$count,col="black",pch=19,cex=0.5,xlim=c(as.numeric(c(as.Date("2018-01-01"),max(eb.tmp$date)))))
#points(mTab$month,mTab$count,col="red",pch=19,cex=0.5)
#points(wTab$week,wTab$count,col="green",pch=19,cex=0.5)
#points(dTab$date,dTab$count,col="blue",pch=19,cex=0.5)

spAll
}