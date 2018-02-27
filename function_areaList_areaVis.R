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
  
  list(subset.name,sp.list,mostRecents)}

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
    ggtitle(paste(subset.name,": Bird taxa observed at each taxonomic level (cumulative)",sep="")) +
    geom_step(data=filter(distinct(sp.list,order,.keep_all=T),!is.na(order)),aes(date,seq_along(date)),col=ebPal[5],lwd=lwdMain) +
    geom_step(data=filter(distinct(sp.list,family,.keep_all=T),!is.na(family)),aes(date,seq_along(date)),col=ebPal[4],lwd=lwdMain) +
    geom_step(data=filter(distinct(sp.list,genus,.keep_all=T),!is.na(genus)),aes(date,seq_along(date)),col=ebPal[3],lwd=lwdMain) +
    geom_step(data=sp.list,aes(date,seq_along(date)),col=ebPal[1],lwd=lwdMain) +
    geom_line(data=gather(mostRecents,key="lineEnds","date",date,today),aes(x=date,y=taxon_count,group=taxon_level),col=c(ebPal[4],ebPal[4],ebPal[3],ebPal[3],ebPal[5],ebPal[5],ebPal[1],ebPal[1]),lwd=lwdDotted,lty=3) +
    geom_point(data=mostRecents,aes(x=date,y=taxon_count),col=c(ebPal[1],ebPal[3],ebPal[4],ebPal[5]),cex=wid*1.4) +
    geom_label(data=mostRecents,aes(x=today-300,y=taxon_count,label=paste(taxon_plural,": ",taxon_count,sep="")),hjust=0.5) 
   # scale_x_continuous(breaks=c(2005,2010,2015),minor_breaks=2004:2018)
  
  #scale_x_continuous(breaks=seq(from=2004,to=2018,by=2),origin = "1899-12-30")
  #xlim(2015,2019,origin = "1899-12-30")
  print(g)
  
  kable(mostRecents[,c("taxon_level","new_taxon","common_name","date","county","state_prov_2","iso3")],
        col.names = c("Level","Newest Addition","Species","Date","County","Prov/State","Country"),
        caption=paste("Most recent addition to the", subset.name, "list at each taxonomic level"),
        knitr.kable.NA = "")
}