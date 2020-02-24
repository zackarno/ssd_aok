library(sf)
library(dplyr)
#cross_check settlements


master_settlement_gdb<- "../../gis_data/gis_base/population/reach_settlement_data"
admin_gdb<- "../../gis_data/gis_base/boundaries"
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)
colnames(master_settlement)<-paste0("mast.",colnames(master_settlement))
master_settlement_sf<- st_as_sf(master_settlement,coords=c("mast.X","mast.Y"), crs=4326)

new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)

master_settlement %>% filter(str_detect(mast.NAMECOUNTY, "Kud$"))
df_settlements_fixed %>% filter( str_detect(D.info_settlement_final, "Odemo$")) %>% select(D.info_settlement_final)
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


master_settlement_sf<-master_settlement_sf %>%
  mutate(
    mast.settlement_county_sanitized= mast.NAMECOUNTY %>%
      trimws()%>%
      gsub("[[:punct:]]","",.) %>%
      gsub(" ","",.) %>%
      tolower())



new_settlements_not_matched<-new_sett_county_join %>%
  butteR::st_drop_geometry_keep_coords() %>%
  anti_join( master_settlement_sf %>%
               st_drop_geometry(),
             by= c("new.settlement_county_sanitized"="mast.settlement_county_sanitized"))


new_settlements_not_matched %>% View()
new_sett_county_join %>% nrow()
new_settlements_not_matched %>% nrow()


master_settlement_sf_not_matched<-master_settlement_sf %>%
  filter(mast.settlement_county_sanitized %in% new_sett_county_join$new.settlement_county_sanitized==FALSE)







########################################################################################
new_with_closest_old<-butteR::closest_distance_rtree(new_settlements_not_matched %>%
                                                       st_as_sf(coords=c("X","Y"), crs=4326),
                                                     master_settlement_sf_not_matched)

new_with_closest_old_vars<-new_with_closest_old %>%
  select(#X_uuid,
         new.Base,
         new.Name,
         new.settlement_county_sanitized,
         county_input_enum=new.County,
         county_from_shp= new.ADM2_EN,
         mast.settlement=mast.NAMEJOIN,
         mast.settlement_county_sanitized,
         dist_m)

new_with_closest_old_vars %>% filter(dist_m<500) %>% View()

#BEST OUTPUT FOR DOLLAR AND DUKU
check_these<-new_with_closest_old_vars %>%
  mutate(gte_50=ifelse(dist_m<500, " < 500 m",">= 500 m"),
         string_proxy=stringdist::stringdist(a =new.settlement_county_sanitized,
                                          b= mast.settlement_county_sanitized,
                                          method= "dl", useBytes = TRUE)
         ) %>%
  arrange(dist_m,desc(string_proxy))


new_settlement_table<-check_these
print(new_settlement_table[1,])
check_these$uuid<-rnorm(1,100)
debugonce()
asdf<-evaluate_unmatched_settlements(user= "zack",new_settlement_table = check_these)

asdf$cleaning_log
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
