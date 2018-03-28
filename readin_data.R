### file to read in data to be accessed by all pages


ebPal <- c("#e76f51","#f4a261","#e9c46a","#2a9d8f","#264653")


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
  select(-subspTemp,-famTemp) %>%
  filter(extinct!=1)

clem.sp <- filter(clem,category=="species")

clem <- clem %>% #cleaning up clem
  mutate(genus=replace(genus, !genus %in% clem.sp$genus, NA)) %>% #removes "genus" names that aren't actually genus names (i.e. the "goose" that would come from "goose sp.")
  mutate(latin_binomial=replace(latin_binomial, category %in% c("spuh"),NA)) %>%
  mutate(subspecies = replace(subspecies,subspecies==" ",NA)) %>%
  mutate(species = replace(species,species=="sp.",NA))

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

