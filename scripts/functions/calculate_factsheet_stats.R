library(forcats)
library(srvyr)


# Functions ---------------------------------------------------------------

output_table_to_datamerge<-function(output_table, sector){
  top_7_list<-list()
  df_temp<-output_table %>% select(county_label,ends_with(".yes"))
  for( i in 2: ncol(df_temp)){
    col_temp<- colnames(df_temp)[i]
    print(col_temp)
    df_temp1<- df_temp[,c(1,i)]

    df_temp2<-df_temp1 %>% arrange(desc(!!sym(col_temp)))
    df_temp3<- df_temp2[1:7,]
    df_temp3[["val_label"]]<- paste0(col_temp,"_val_",1:nrow(df_temp3))
    df_temp3[["name_label"]]<- paste0(col_temp,"_label_",1:nrow(df_temp3))
    df_wide1<-df_temp3 %>% select(!!sym(col_temp), val_label) %>%
      tidyr::pivot_wider(names_from = val_label,values_from = !!sym(col_temp))
    df_wide2<-df_temp3 %>% select(county_label, name_label) %>%
      tidyr::pivot_wider(names_from = name_label,values_from = county_label)
    df_wide<- bind_cols(list(df_wide1,df_wide2))
    top_7_list[[col_temp]]<-df_wide
  }
  data_merge_row<-bind_cols(top_7_list) %>% mutate(FS=sector) %>% select(FS, everything()) %>% return()
}

# READ IN DATA ------------------------------------------------------------



aok_ag<-read.csv("outputs/2020_02_17_reach_ssd_aok_data_analysis_basic_JAN2020_Data.csv", stringsAsFactors = FALSE, na.strings = c(""," "))

#CREATE NEW COLUMN WITH COUNTY LABELS
kobo_choices<-  readxl::read_xlsx("inputs/tool/REACH_SSD_AoK_V38_Febuary2020/REACH_SSD_AoK_V38b_Febuary2020.xlsx",sheet = "choices")
county_label_lookup_table<-kobo_choices %>%
  filter(list_name =="admin_2") %>%
  select(county_name=name, county_label=`label::english`)

aok_ag<-aok_ag %>% left_join(county_label_lookup_table, by=c("D.info_county"="county_name"))

aok_ag$T.main_info_source %>% unique() %>% dput()
aok_ag$R.info_source_who %>% unique() %>% dput()
aok_ag$H.death_cause1 %>% unique() %>% dput()

#CREATE COUNTY LABELS
aok_with_composite<- aok_ag %>%
  mutate(ind.wash.unprotected_water_yn= ifelse(K.water_now_how %in% c("river","swamp", "donkey","well"),"yes","no"),
         ind.wash.time_water_more_30_yn= ifelse( K.water_now_time %in% c("1_hour_half_day",
                                                                         "30_minutes_1_hour",
                                                                         "full_day"), "yes", "no"),
         ind.wash.water_unavailable_year_round_yn= ifelse(K.water_source_seasonal=="no", "yes","no"),
         ind.wash.water_access_unsafe_yn= K.water_safety,
         ind.wash.latrine_barrier= case_when(K.latrine_no_usage == "overcrowded"~"overcrowded.yes",
                                             K.latrine_no_usage=="none_available"~"none_available.yes",
                                             TRUE~"no"),
         ind.wash.main_death_malaria_diar= ifelse(H.death_cause1 %in% c("malaria", "diarrhoeal_diseases"),"yes","no"),
         #should standardize this in tool
         ind.wash.main_healthprob_malaria_diar= ifelse(H.death_cause1 %in% c("malaria", "water_borne"),"yes","no"),
         #NFI
         ind.nfi.mosquito_net_prim_need_yn= ifelse(J.nfi_need1 =="mosquito_net", "yes", "no"),
         ind.nfi.plastic_sheet_prim_need_yn= ifelse(J.nfi_need1 =="plastic_sheet", "yes", "no"),
         ind.nfi.blanket_prim_need_yn= ifelse(J.nfi_need1 =="blanket", "yes", "no"),
         ind.nfi.cooking_pot_prim_need_yn= ifelse(J.nfi_need1 =="cooking_pot", "yes", "no"),
         ind.prot.main_info_fam_friend_yn= ifelse(R.info_source_who =="friend_family","yes","no"),
         ind.prot.idp_leadership_absent_yn= ifelse(R.idp_leadership=="no","yes","no"),
         ind.prot.community_not_sharing_yn= ifelse(R.idp_supported=="no", "yes","no"),
         # missing - most idps staying in temp shelters
         ind.cccm.main_info_fam_friend_yn=ind.prot.main_info_fam_friend_yn,
         ind.cccm.main_info_aidworker_commmob_yn=ifelse(R.info_source_who %in%
                                                          c( "community_mobilizers","aidworker"),
                                                        "yes","no"),
         ind.cccm.unable_access_localauthorities=ifelse(R.local_authorities=="no","yes","no"),
         ind.cccm.main_info_fam_friend_yn=ind.prot.main_info_fam_friend_yn

         # missing - most idps staying in temp shelters

  )

wash_indicators<-c("ind.wash.unprotected_water_yn", "ind.wash.time_water_more_30_yn",
                   "ind.wash.water_unavailable_year_round_yn", "ind.wash.water_access_unsafe_yn"
)

aok_svy<- as_survey(aok_with_composite)

composites_to_analyze<-aok_with_composite %>% select(starts_with("ind.")) %>% colnames()

aok_fs_analysis<- butteR::mean_proportion_table(design = aok_svy,
                                              list_of_variables =composites_to_analyze,
                                              aggregation_level =c("county_label"))


aok_wash_analysis<- aok_fs_analysis %>% select(county_label, starts_with("ind.wash."))
aok_protection_analysis<- aok_fs_analysis %>% select(county_label, starts_with("ind.prot."))
aok_nfi_analysis<- aok_fs_analysis %>% select(county_label, starts_with("ind.nfi."))
aok_cccm_analysis<- aok_fs_analysis %>% select(county_label, starts_with("ind.cccm."))


sectors<-c("WASH","Protection","NFI", "CCCM")
analyses<-list(aok_wash_analysis,aok_protection_analysis,aok_nfi_analysis,aok_cccm_analysis)

data_merge_output<-list()
for(i in 1:length(sectors)){
  data_merge_output[[sectors[i]]]<-output_table_to_datamerge(analyses[[i]], sector=sectors[i])
}

data_merge_output %>% class()
data_merge_output$Protection
data_merge_output$NFI
data_merge_file_ready<-rbindlist(data_merge_output, fill=T)

data_merge_file_ready %>% write.csv("datamerge_testtests.csv")

