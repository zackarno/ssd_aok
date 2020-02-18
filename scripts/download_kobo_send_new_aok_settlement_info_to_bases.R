library(koboloadeR)
library(dply)
source("scripts/functions/keys.R")


# KOBO_datasets <- kobo_datasets(user = c(keys[1],keys[2]), api="kobohr")
aok_data<-kobo_data_downloader(formid = 489385,user =  c(keys[1],keys[2]), api="kobohr", check = FALSE)
aok_data$S.shock_arrival %>% unique()
colnames(aok_data)<-gsub("\\/", ".", colnames(aok_data))
aok_other_settlement<- aok_data %>%
  filter(!is.na(D.info_settlement_other)) %>%
  filter(D.info_settlement_other!= "n/a") %>%
  select(A.base,
         A.enumerator_id,
         D.info_county,
         D.info_settlement_other) %>% arrange(A.base)

# aok_by_base<-split(aok_other_settlement, aok_other_settlement$A.base)



