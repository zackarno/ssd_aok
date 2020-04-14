library(tidyverse)
library(sf)
library(butteR)
#cross_check settlementsdo



# master_settlement_gdb<- "../../gis_data/gis_base/population/reach_settlement_data"
aok_raw<-read.csv("inputs/2020_02/2020_02_FEB_AOK_RAW_20200301.csv", stringsAsFactors = F, na.strings=c("n/a",""," "))
# admin_gdb<- "../../gis_data/gis_base/boundaries"
admin_gdb<- "../../gis_data/gis_base/boundaries/county_shapefile"
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)
colnames(master_settlement)<-paste0("mast.",colnames(master_settlement))
master_settlement_sf<- st_as_sf(master_settlement,coords=c("mast.X","mast.Y"), crs=4326)

new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)
new_sett<-read.csv("cleaning_log/Juba_New_Settlements_Mapping_Feb_2020.csv", stringsAsFactors = FALSE)
new_sett<-new_sett %>% filter(!is.na(Long))




adm2<- st_read(admin_gdb,"ssd_admbnda_adm2_imwg_nbs_20180401" )
adm2<-st_transform(adm2,crs=4326)
new_sett_sf<-st_as_sf(new_sett,coords=c("Long","Latitude"), crs=4326)
# adm2$admin2Name
ggplot()+geom_sf(data=adm2)+
  geom_sf_label(data= new_sett_sf,aes(label =D.info_settlement_other))+
  geom_sf(data=new_sett_sf)

new_sett_sf<-new_sett_sf %>% st_join( adm2 %>% dplyr::select(adm2=admin2RefN))


new_sett_sf<-new_sett_sf %>%
  mutate(
    new.enum_sett_county=paste0(D.info_settlement_other,D.info_county) %>% tolower_rm_special(),
    new.adm2_sett_county=paste0(D.info_settlement_other,adm2) %>% tolower_rm_special()
  )
master_settlement<-master_settlement %>%
  mutate(
    mast.settlement_county_sanitized= mast.NAMECOUNTY %>% tolower_rm_special()
  )



# CHECK IF NEW SETTLEMENTS HAVE BEEN FIXED IN CL --------------------------

aok_clean1<-aok_raw
# aok_clean1[aok_clean1$X_uuid=="b4d97108-3f34-415d-9346-f22d2aa719ea","D.info_settlement_other"]<-"Bajur"
# aok_clean1[aok_clean1$X_uuid=="b4d97108-3f34-415d-9346-f22d2aa719ea","D.info_settlement"]<-NA


remove_from_new_sett<-aok_clean1 %>%
  filter(X_uuid %in% new_sett_sf$uuid  & is.na(D.info_settlement))%>%
  select(X_uuid,D.info_settlement) %>% pull(X_uuid)

new_sett_sf<- new_sett_sf %>% filter(!uuid %in% remove_from_new_sett)


# NEW SETTLEMENT DATA WHICH MATCHES MASTER SETTLEMENTS EXACTLZ ------------

exact_matches1<-new_sett_sf %>%
  mutate(matched_where= case_when(new.enum_sett_county %in% master_settlement$mast.settlement_county_sanitized~"enum", #CHECK WITH ENUMS INPUT
                                  new.adm2_sett_county %in% master_settlement$mast.settlement_county_sanitized~"shapefile_only" #CHECK WITH SHAEFILE COUNTY
                                  )) %>%
           filter(!is.na(matched_where)) #ONLY RETURN EXACT MATCHES

# WRITE EXACT MATCHES TO CLEANING LOG TO THEN IMPLEMENT ON DATA.
aok_exact_matches_cl<-exact_matches_to_cl(exact_match_data = exact_matches1,user = "Jack")

#NOW IMPLEMENT THIS CLEANING LOG!
aok_clean2<-butteR::implement_cleaning_log(df = aok_clean1,df_uuid = "X_uuid",
                               cl = aok_exact_matches_cl,
                               cl_change_type_col = "change_type",
                               cl_change_col = "indicator",
                               cl_uuid = "uuid",
                               cl_new_val = "new_value")

#EXTRACT NEW SETTLEMENTS WHICH DO NO MATCH
new_sett_sf_unmatched<- new_sett_sf %>% filter(!uuid %in% exact_matches1$uuid)



#REMOVE MATCHED SETTLEMENTS FROM MASTER
master_settlement_sf_not_matched<-master_settlement_sf %>%
  filter(mast.settlement_county_sanitized %in% new_sett_sf_unmatched$new.enum_sett_county==FALSE)




# MATCH NEW SETTLEMENT TO CLOSEST SETTLEMENT IN MASTER --------------------

new_with_closest_old<-butteR::closest_distance_rtree(new_sett_sf_unmatched %>%
                                                       st_as_sf(coords=c("X","Y"), crs=4326) ,master_settlement_sf_not_matched)
#CLEAN UP DATASET
new_with_closest_old_vars<-new_with_closest_old %>%
  mutate(new.D.info_settlement_other= D.info_settlement_other %>% gsub("-","_",.)) %>%
  select(uuid,
         new.A.base=A.base,
         new.county_enum=D.info_county,
         new.county_adm2= adm2,
         new.sett_county_enum=new.enum_sett_county,
         new.sett_county_adm2= new.adm2_sett_county,
         new.D.info_settlement_other=D.info_settlement_other,
         mast.settlement=mast.NAMEJOIN,
         mast.settlement_county_sanitized,
         dist_m)



# ADD A FEW USEFUL COLUMNS - THIS COULD BE WRITTEN TO A CSV AND WOULD BE THE BEST OUTPUT TO BE REVIEWED
settlements_best_guess<-new_with_closest_old_vars %>%
  mutate(gte_50=ifelse(dist_m<500, " < 500 m",">= 500 m"),
         string_proxy=stringdist::stringdist(a =new.sett_county_enum,
                                          b= mast.settlement_county_sanitized,
                                          method= "dl", useBytes = TRUE)
         ) %>%
  arrange(dist_m,desc(string_proxy))


# HOWEVER, TO KEEP EVERYTHING IN THE R ENVIRONMENT- HERE IS AN INTERACTIVE FUNCTION TO MODIFY THE SETTLEMENT BEST GUESS DF IN PLACE
# OUTUT WILL BE A CLEANING LOG (IF THERE ARE CHANGES TO BE MADE)
new_settlement_evaluation<-evaluate_unmatched_settlements(user= "zack",new_settlement_table = settlements_best_guess)

new_settlement_evaluation$cleaning_log; new_settlement_evaluation$checked_setlements



# ADD NEW SETTLEMENTS TO NEW ITEMSET  -------------------------------------

#put into itemset format
new_sets_to_add<-new_settlement_evaluation$checked_setlements %>%
  filter(action==2) %>%
  mutate(
    list_name="settlements",
    label= new.D.info_settlement_other %>% gsub("_","-", .)
    ) %>%
  select(list_name,name=new.D.info_settlement_other, label,admin_2=new.county_adm2)

# read in previous itemset
itemset<-read.csv("inputs/tool/REACH_SSD_AoK_V38_Febuary2020/itemsets.csv", strip.white = T, stringsAsFactors = T,na.strings = c(" ",""))

itemset_not_other<-itemset %>% filter(name!="other")
itemset_other<- itemset %>% filter(name=="other")

itemset_binded<-bind_rows(list(new_sets_to_add,itemset_not_other)) %>% arrange(admin_2)
itemset_full_binded<- bind_rows(list(itemset_binded,itemset_other))



# NEXT WE ADD THE NEW SETTLEMENTS TO THE SHAPEFILE ------------------------

month_of_assessment<-"2020-02-01"

library(lubridate)
# add to master file ------------------------------------------------------
new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)
new_sett %>% head()
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)
master_settlement$DATA_SOURC
new_setts_add_to_master<-new_settlement_evaluation$checked_setlements %>%
  filter(action==2) %>%
  mutate(
  NAME= new.settlement.name %>% gsub("_","-", .),
  NAMEJOIN= new.settlement.name,
  NAMECOUNTY=paste0(NAMEJOIN,new.county_shp),
  COUNTYJOIN= new.county_shp,
  DATE= month_of_assessment %>% ymd(),
  DATA_SOURC="AOK",
  IMG_VERIFD= 0
  ) %>% #get coordinates from field data back in
  left_join(new_sett_county_join %>%
              st_drop_geometry_keep_coords()) %>%
  filter(!is.na(X)) %>%
  select(NAME,NAMEJOIN,NAMECOUNTY,COUNTYJOIN,DATE,DATA_SOURC,IMG_VERIFD,X,Y)

master_new<-bind_rows(list(new_setts_add_to_master,master_settlement %>% mutate(DATE=mdy(DATE))))

# write to csv



































cleaning_log<- name_in_kobo_not_in_new_settlements %>% mutate(uuid= X_uuid,
                                                              spotted=user,
                                                              change_type="change_response",
                                                              Sectors="Area_of_Knowledge",
                                                              indicator="",
                                                              current_value= "",
                                                              new_value="",
                                                              issue="",
                                                              suggested_indicator= "D.info_settlement_other",
                                                              suggested_issue="Name spelled incorrectly in Kobo Form",
                                                              suggested_current_value=master_col) %>%
  select(uuid:suggested_current_value)


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




buffered_new_settlements_not_matched<-new_settlements_not_matched %>%
  st_as_sf(coords=c("X","Y"), crs=4326) %>%
  st_transform(crs=32636) %>%
  st_buffer(dist = 500)
