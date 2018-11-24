BigdaysTable <- function(myeb.sp.area){

bigdays <- myeb.sp.area %>% 
  group_by(date) %>% 
  distinct(common_name,.keep_all = T) %>% 
  summarise(spnum = n(),
            country=names(which.max(table(country))),
            stateprov=names(which.max(table(state_prov_code))),
            county=names(which.max(table(county,useNA = "always")))) %>% 
  arrange(desc(spnum))

return(bigdays)
}