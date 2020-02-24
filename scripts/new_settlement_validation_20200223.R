library(sf)
library(dplyr)
source("scripts/functions/utils.R")
#cross_check settlements


master_settlement_gdb<- "../../gis_data/gis_base/population/reach_settlement_data"
admin_gdb<- "../../gis_data/gis_base/boundaries"
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)
colnames(master_settlement)<-paste0("mast.",colnames(master_settlement))
master_settlement_sf<- st_as_sf(master_settlement,coords=c("mast.X","mast.Y"), crs=4326)

# RUN THIS TO DOWNLOAD THE DATA IN SSD FORMAT (WITH GROUPS)
# aok_data<- download_aok_data("scripts/functions/keys.R")

aok_other_settlement<- aok_data %>%
  filter(!is.na(D.info_settlement_other)) %>%
  filter(D.info_settlement_other!= "n/a") %>%
  select(uuid="_uuid" ,
         A.base,
         A.enumerator_id,
         D.info_county,
         D.info_settlement_other) %>% arrange(A.base)


# new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)

new_sett %>% filter(is.na(X)|is.na(Y))
adm2<- st_read(admin_gdb,"ssd_admbnda_adm2_imwg_nbs_20180817" )
new_sett_sf<-st_as_sf(new_sett,coords=c("X","Y"), crs=4326)

new_sett_county_join<-new_sett_sf %>% st_join( adm2 %>% dplyr::select(ADM2_EN))
new_sett_county_join %>% View()

colnames(new_sett_county_join)[1:ncol(new_sett_county_join)-1]<-paste0("new.",colnames(new_sett_county_join)[1:ncol(new_sett_county_join)-1])


new_sett_county_join<- new_sett_county_join %>%
  mutate(
    new.settlement_county_sanitized= paste0(new.settlement.name, new.ADM2_EN) %>%
      trimws()%>%
      gsub("[[:punct:]]","",.) %>%
      gsub(" ","",.) %>%
      tolower())
new_sett_county_join %>% filter(str_detect(new.settlement_county_sanitized, "kudli$"))

master_settlement_sf<-master_settlement_sf %>%
  mutate(
    mast.settlement_county_sanitized= mast.NAMECOUNTY %>%
      trimws()%>%
      gsub("[[:punct:]]","",.) %>%
      gsub(" ","",.) %>%
      tolower())

master_settlement_sf %>% filter(str_detect(mast.settlement_county_sanitized, "kudli$"))

new_settlements_not_matched<-new_sett_county_join %>%
  butteR::st_drop_geometry_keep_coords() %>%
  anti_join( master_settlement_sf %>%
               st_drop_geometry(),
             by= c("new.settlement_county_sanitized"="mast.settlement_county_sanitized"))


new_settlements_not_matched %>% View()
new_sett_county_join %>% nrow()
new_settlements_not_matched %>% nrow()



buffered_new_settlements_not_matched<-new_settlements_not_matched %>%
  st_as_sf(coords=c("X","Y"), crs=4326) %>%
  st_transform(crs=32636) %>%
  st_buffer(dist = 500)

master_settlement_sf_not_matched<-master_settlement_sf %>%
  filter(mast.settlement_county_sanitized %in% new_sett_county_join$new.settlement_county_sanitized==FALSE)

nrow(master_settlement_sf_not_matched)
nrow(master_settlement_sf)




########################################################################################
new_with_closest_old<-butteR::closest_distance_rtree(new_settlements_not_matched %>%
                                                       st_as_sf(coords=c("X","Y"), crs=4326) ,master_settlement_sf_not_matched)

new_with_closest_old_vars<-new_with_closest_old %>%
  select(new.Base, new.Name,new.settlement_county_sanitized, county_input_enum=new.County,county_from_shp= new.ADM2_EN, mast.settlement_county_sanitized, dist_m)

new_with_closest_old_vars %>% filter(dist_m<500) %>% View()

#BEST OUTPUT FOR DOLLAR AND DUKU
check_these<-new_with_closest_old_vars %>%
  mutate(gte_50=ifelse(dist_m<500, " < 500 m",">= 500 m"),
         string_proxy=stringdist::stringdist(a =new.settlement_county_sanitized,
                                          b= mast.settlement_county_sanitized,
                                          method= "dl", useBytes = TRUE)
         ) %>%
  arrange(dist_m,desc(string_proxy))

write.csv(check_these,"outputs/20200207_new_settlements_no_exact_match_to_check.csv")



############################################################################################

library(stringdist)
new_with_closest_old_vars_gte500m %>% mutate(sd=stringdist::stringdist(a =new_with_closest_old_vars_gte500m$new.settlement_county_sanitized,
                        b= new_with_closest_old_vars_gte500m$mast.settlement_county_sanitized,
                        method= "cosine")) %>% arrange(sd) %>% View()



#other method
###########################################################################################
master_settlements_within_500m<-master_settlement_sf_not_matched %>%
  st_transform(crs=32636) %>%
  st_join(buffered_new_settlements_not_matched) %>%
  filter(!is.na(new.settlement_county_sanitized)) %>%
  select(new.settlement_county_sanitized, new.Base, new.Name , mast.settlement_county_sanitized)

master_settlements_within_500m %>% View()
