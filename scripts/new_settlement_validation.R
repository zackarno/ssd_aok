library(tidyverse)
library(sf)
library(dplyr)
library(butteR)
#cross_check settlements


master_settlement_gdb<- "../../gis_data/gis_base/population/reach_settlement_data"

admin_gdb<- "../../gis_data/gis_base/boundaries"
admin_gdb<- "../../gis_data/gis_base/boundaries/county_shapefile"
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)
colnames(master_settlement)<-paste0("mast.",colnames(master_settlement))
master_settlement_sf<- st_as_sf(master_settlement,coords=c("mast.X","mast.Y"), crs=4326)

new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)
new_sett<-read.csv("cleaning_log/Juba_New_Settlements_Mapping_Feb_2020.csv", stringsAsFactors = FALSE)
new_sett<-new_sett %>% filter(!is.na(Long))


new_sett %>% filter(is.na(X)|is.na(Y))
adm2<- st_read(admin_gdb,"ssd_admbnda_adm2_imwg_nbs_20180401" )
adm2<-st_transform(adm2,crs=4326)
new_sett_sf<-st_as_sf(new_sett,coords=c("X","Y"), crs=4326)
adm2$admin2Name
adm2$admin2RefN
new_sett_county_join<-new_sett_sf %>% st_join( adm2 %>% dplyr::select(shp_county=admin2RefN))


colnames(new_sett_county_join)[1:ncol(new_sett_county_join)-1]<-paste0("new.",colnames(new_sett_county_join)[1:ncol(new_sett_county_join)-1])


new_sett_county_join<- new_sett_county_join %>%
  mutate(
    new.settlement_county_sanitized= paste0(new.settlement.name, new.shp_county) %>%  tolower_rm_special()
    )


master_settlement_sf<-master_settlement_sf %>%
  mutate(
    mast.settlement_county_sanitized= mast.NAMECOUNTY %>% tolower_rm_special()
  )



#settlements without exact matches
new_settlements_not_matched<-new_sett_county_join %>%
  butteR::st_drop_geometry_keep_coords() %>%
  anti_join( master_settlement_sf %>%
               st_drop_geometry(),
             by= c("new.settlement_county_sanitized"="mast.settlement_county_sanitized"))



# remove matched settlements from master list for further matcheing
master_settlement_sf_not_matched<-master_settlement_sf %>%
  filter(mast.settlement_county_sanitized %in% new_sett_county_join$new.settlement_county_sanitized==FALSE)




#find closest
new_with_closest_old<-butteR::closest_distance_rtree(new_settlements_not_matched %>%
                                                       st_as_sf(coords=c("X","Y"), crs=4326),
                                                     master_settlement_sf_not_matched)

new_with_closest_old_vars<-new_with_closest_old %>%
  mutate(new.settlement.name= new.settlement.name %>% gsub("-","_",.)) %>%
  select(#X_uuid,
         new.Base,
         new.Name,
         new.shp_county,
         new.settlement.name,
         new.settlement_county_sanitized,
         new.county_enum=new.County,
         new.county_shp= new.shp_county,
         mast.settlement=mast.NAMEJOIN,
         mast.settlement_county_sanitized,
         dist_m)



#BEST OUTPUT FOR DOLLAR AND DUKU
settlements_best_guess<-new_with_closest_old_vars %>%
  mutate(gte_50=ifelse(dist_m<500, " < 500 m",">= 500 m"),
         string_proxy=stringdist::stringdist(a =new.settlement_county_sanitized,
                                          b= mast.settlement_county_sanitized,
                                          method= "dl", useBytes = TRUE)
         ) %>%
  arrange(dist_m,desc(string_proxy))


evaluate_unmatched_settlements<-function(user,new_settlement_table){
  output<-list()
  new_settlement_table$action<-NA

  for ( i in 1: nrow(new_settlement_table)){
    print(new_settlement_table[i,])
    choice <- menu(c("fix with master", "master is not correct"))
    new_settlement_table[i,][["action"]]<- choice
      # do other things

  }
  cleaning_log1<-new_settlement_table %>%
    filter(action==1) %>%mutate(#uuid= X_uuid,
      spotted=user,
      change_type="change_response",
      Sectors="Area_of_Knowledge",
      indicator="",
      current_value= "",
      new_value="",
      issue="",
      suggested_indicator= "D.info_settlement",
      suggested_issue="User chose other when name correct name was available",
      suggested_new_value=mast.settlement) %>%
    select(spotted:suggested_new_value) #need to add uuid into selection on real data
  cleaning_log2<-new_settlement_table %>%
    filter(action==1) %>%mutate(#uuid= X_uuid,
                                    spotted=user,
                                    change_type="change_response",
                                    Sectors="Area_of_Knowledge",
                                    indicator="",
                                    current_value= "",
                                    new_value="",
                                    issue="",
                                    suggested_indicator= "D.info_settlement_other",
                                    suggested_issue="User chose other when name correct name was available",
                                    suggested_new_value=NA) %>%
    select(spotted:suggested_new_value) #need to add uuid into selection on real data


  cleaning_log_combined<-bind_rows(list(get0("cleaning_log1"), get0("cleaning_log2")))
  output$checked_setlements<-new_settlement_table
  output$cleaning_log<-cleaning_log_combined

  return(output)
}
# debugonce(evaluate_unmatched_settlements)
new_settlement_evaluation<-evaluate_unmatched_settlements(user= "zack",new_settlement_table = settlements_best_guess)



new_sets_to_add<-new_settlement_evaluation$checked_setlements %>%
  filter(action==2) %>%
  mutate(
    list_name="settlements",
    label= new.settlement.name %>% gsub("_","-", .)
    ) %>%
  select(list_name,name=new.settlement.name, label,admin_2=new.county_shp)
itemset<-read.csv("inputs/tool/REACH_SSD_AoK_V38_Febuary2020/itemsets.csv", strip.white = T, stringsAsFactors = T,na.strings = c(" ",""))
itemset_not_other<-itemset %>% filter(name!="other")
itemset_other<- itemset %>% filter(name=="other")

itemset_binded<-bind_rows(list(new_sets_to_add,itemset_not_other)) %>% arrange(admin_2)
itemset_full_binded<- bind_rows(list(itemset_binded,itemset_other))

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
