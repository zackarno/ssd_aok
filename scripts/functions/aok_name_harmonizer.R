library(sf)
library(dplyr)

gdb_settlements<- "../../gis_data/gis_base/population/reach_settlement_data"
new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)

aok_monthly_data<- df3
attributes(df3)
ao_new_settlements<-new_sett


st_layers(gdb_settlements)
gdb<- "../../gis_data/gis_base/boundaries/county_shapefile"
gdb<- "../../gis_data/gis_base/boundaries"
st_layers(gdb)

adm2_shapefile_name<- "ssd_admbnda_adm2_imwg_nbs_20180401"
v37<-st_read(gdb_settlements,"settlements_v37")
adm2<- st_read(gdb,"ssd_admbnda_adm2_imwg_nbs_20180817" )
adm2<- st_read(gdb,adm2_shapefile_name )

new_sett_sf<-st_as_sf(new_sett,coords=c("X","Y"), crs=4326)
new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)
new_sett_county_join<-new_sett_sf %>% st_join( adm2 %>% dplyr::select(ADM2_EN))

attributes(new_sett_sf)
butteR::check_reported_strata_against_spatial_poly(dataset = )

adm2<- sf::st_read(gdb, adm2_shapefile_name)
adm2<- sf::st_transform(adm2, crs=4326)



#'
#'check coordinates for mistakes

check_new_coordinates<- function(ao_new_settlements){
if(class(ao_new_settlements$X)!="numeric"|class(ao_new_settlements$Y)!="numeric"){
  if(class(ao_new_settlements$X)!="numeric"){print("there are mistakes in you x coordinate")}
  if(class(ao_new_settlements$Y)!="numeric"){print("there are mistakes in you y coordinate")}
  ao_new_settlements %>%
    mutate(x_as_numeric=as.numeric(X)) %>%
    mutate(y_as_numeric=as.numeric(Y)) %>%
    filter(is.na(x_as_numeric)|is.na(y_as_numeric)) %>% select(-x_as_numeric, -y_as_numeric) %>%  return()}

}

# wrapper to create clean strings
simplify_clean_string<-function(string){
  string %>% gsub("[[:punct:]]","",.) %>%
  gsub(" ","",.) %>% trimws() %>%
  tolower() %>% return()
  }


test_run<-aok_name_harmonizer1(aok_monthly_data = df3,
                     previous_settlement_list = master_settlement,
                     ao_new_settlements = new_sett,
                     gdb = "../../gis_data/gis_base/boundaries",
                     adm2_shapefile_name="ssd_admbnda_adm2_imwg_nbs_20180817",
                     user= "zack"
                     )


check_ao_table_against_kobo_data<-function(aok_monthly_data, ao_new_settlements, user="zack"){
  aok_monthly_data_with_names<-
    aok_monthly_data %>%
    filter(!is.na(D.info_settlement_other)) %>%
    mutate(
      name_clean= D.info_settlement_other %>% simplify_clean_string(),
      name_county_concat_clean= paste0(D.info_settlement_other,D.info_county) %>%
        simplify_clean_string())

  reported_new_settlement_names<-ao_new_settlements  %>%
    filter(!is.na(settlement.name)) %>%
    mutate(
      name_clean=settlement.name %>% simplify_clean_string(),
      name_county_concat_clean= paste0(settlement.name,County) %>%
        simplify_clean_string()
    )

  name_in_kobo_not_in_new_settlements<-
    aok_monthly_data_with_names %>%
    anti_join(reported_new_settlement_names, by = "name_county_concat_clean") %>%
    select(A.base, X_uuid,D.info_county, D.info_settlement_other, name_clean,name_county_concat_clean)

  name_in_new_settlements_not_in_kobo<-
    reported_new_settlement_names%>% anti_join(aok_monthly_data_with_names, by = "name_county_concat_clean") %>%
    select(Base,County,settlement.name, name_clean,name_county_concat_clean) %>%
    arrange(Base)


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
                                                            suggested_current_value=D.info_settlement_other) %>%
    select(uuid:suggested_current_value)



}



aok_name_harmonizer1<- function(aok_monthly_data,
                                previous_settlement_list,
                                ao_new_settlements,
                                gdb,
                                adm2_shapefile_name,
                                user){
  x<-list()
  coordinate_mistake_table<-check_new_coordinates(ao_new_settlements)
  if(!is.null(coordinate_mistake_table)){
    return(coordinate_mistake_table)
  }

  aok_monthly_data_with_names<-
    aok_monthly_data %>%
    filter(!is.na(D.info_settlement_other)) %>%
    mutate(
      new_data_settlement_county_sanitized= paste0(D.info_settlement_other,D.info_county) %>%
        simplify_clean_string())

  ao_new_settlements_sf <-st_as_sf(ao_new_settlements,coords=c("X","Y"),crs=4326)
  ao_new_settlements_sf_adm_joined<-ao_new_settlements_sf %>% st_join(adm2 %>% select(admin2RefN))

  reported_new_settlement_names<-ao_new_settlements_sf_adm_joined%>% filter(!is.na(settlement.name)) %>%
    mutate(
      reported_county_name= paste0(settlement.name,admin2RefN) %>%
        simplify_clean_string()
    )
previous_settlement_list<-previous_settlement_list %>%
  mutate(county_name_simple= NAMECOUNTY %>% simplify_clean_string())
# MATCH A0 NEW SETT VS KOBO DATA - SHOULD NOT BE NECESSARY SHOULD ONLY BE ONE LIST ------------
  x[["AO_exact_settlement_county_matches"]]<-reported_new_settlement_names %>% filter(reported_county_name %in% aok_monthly_data_with_names$new_data_settlement_county_sanitized)

  x[["AO_exact_settlement_county_NOT_matched"]]<-reported_new_settlement_names %>% filter(!reported_county_name %in% aok_monthly_data_with_names$new_data_settlement_county_sanitized)

  x[["AO_exact_settlement_name_only_NOT_matched"]]<-reported_new_settlement_names %>%
    filter(!settlement.name %in%
             aok_monthly_data_with_names$D.info_settlement_other)

  x[["AO_exact_settlement_name_only_matched"]]<-reported_new_settlement_names %>%
    filter(settlement.name %in%
             aok_monthly_data_with_names$D.info_settlement_other)



# MATCH NEW SETTLEMENTS AGAINST MASTER LIST -------------------------------
  # previous_settlement_list<-v37

    x[["settlements_with_direct_matches"]]<-
    x$AO_exact_settlement_county_matches %>%
    st_drop_geometry() %>%
    left_join(previous_settlement_list %>%
                select(NAME,
                       NAMEJOIN,
                       NAMECOUNTY,
                       county_name_simple),
              by=c("reported_county_name"="county_name_simple")) %>%
    filter(!is.na(NAME))

  aok_settlement_other_matched_db<-aok_monthly_data %>% filter(D.info_settlement_other %in% x$settlements_with_direct_matches$settlement.name)

  x[["cleaning_log_1_direct_matches"]]<- aok_settlement_other_matched_db %>% mutate(uuid= X_uuid,
                                                                                    spotted=user,
                                                                                    change_type="change_response",
                                                                                    Sectors="Area_of_Knowledge",
                                                                                    indicator="D.info_settlement_other",
                                                                                    current_value= D.info_settlement_other,
                                                                                    new_value=NA,
                                                                                    issue="name already exists in master list") %>%
    select(uuid:issue) %>%
    bind_rows(
    aok_settlement_other_matched_db %>% mutate(uuid= X_uuid,
                                               spotted=user,
                                               change_type="change_response",
                                               Sectors="Area_of_Knowledge",
                                               indicator="D.info_settlement",
                                               current_value= D.info_settlement,
                                               new_value=D.info_settlement_other,
                                               issue="name already exists in master list") %>%
    select(uuid:issue))


    previous_settlements_not_matched<-previous_settlement_list %>%
      filter(!county_name_simple %in% x$settlements_with_direct_matches$reported_county_name) %>%
      st_as_sf(coords=c("X","Y"), crs=4326)

   x[["new_with_closest_old"]]<-butteR::closest_distance_rtree(x$AO_exact_settlement_county_NOT_matched,
                                                        previous_settlements_not_matched)
   return(x)




}




  other_settlements<-aok_monthly_data %>%filter(!is.na(D.info_settlement_other)) %>%  pull(D.info_settlement_other)
  other_settlements[other_settlements %in% (ao_new_settlements %>% pull(settlement.name))]
  other_settlements[other_settlements %in% (ao_new_settlements %>% pull(settlement.name))==FALSE]

}


aok_name_harmonizer2<- function(aok_monthly_data,updated_settlement_list){

}
