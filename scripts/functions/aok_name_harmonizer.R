
adm2<- st_read(admin_gdb,"ssd_admbnda_adm2_imwg_nbs_20180817" )
new_sett_sf<-st_as_sf(new_sett,coords=c("X","Y"), crs=4326)
new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)
new_sett_county_join<-new_sett_sf %>% st_join( adm2 %>% dplyr::select(ADM2_EN))

aok_monthly_data<- df3
ao_new_settlements<-new_sett
ao_new_settlements$County
gdb<- "../../gis_data/gis_base/population/reach_settlement_data"
gdb<- "../../gis_data/gis_base/boundaries/county_shapefile"
st_layers(gdb)
adm2_shapefile_name<- "ssd_admbnda_adm2_imwg_nbs_20180401"

adm2<- sf::st_read(gdb, adm2_shapefile_name)
adm2<- sf::st_transform(adm2, crs=4326)
check_new_coordinates<- function(ao_new_settlements){
if(class(ao_new_settlements$X)!="numeric"|class(ao_new_settlements$Y)!="numeric"){
  if(class(ao_new_settlements$X)!="numeric"){print("there are mistakes in you x coordinate")}
  if(class(ao_new_settlements$Y)!="numeric"){print("there are mistakes in you u coordinate")}
  ao_new_settlements %>%
    mutate(x_as_numeric=as.numeric(X)) %>%
    mutate(y_as_numeric=as.numeric(Y)) %>%
    filter(is.na(x_as_numeric)|is.na(y_as_numeric)) %>% select(-x_as_numeric, -y_as_numeric) %>%  return()}

}
debugonce(aok_name_harmonizer1)

asdf<-aok_name_harmonizer1(aok_monthly_data = df3,
                     previous_settlement_list = master_settlement,
                     ao_new_settlements = new_sett,
                     gdb = "../../gis_data/gis_base/boundaries",
                     adm2_shapefile_name="ssd_admbnda_adm2_imwg_nbs_20180817"
                     )
asdf

aok_name_harmonizer1<- function(aok_monthly_data,previous_settlement_list, ao_new_settlements, gdb,adm2_shapefile_name){
  coordinate_mistake_table<-check_new_coordinates(ao_new_settlements)
  if(!is.null(coordinate_mistake_table)){
    return(coordinate_mistake_table)
  }
  aok_monthly_data_with_names<-aok_monthly_data %>% filter(!is.na(D.info_settlement_other)) %>%
    mutate(
      new_data_settlement_county_sanitized= paste0(D.info_settlement_other,D.info_county) %>%
        gsub("[[:punct:]]","",.) %>%
        gsub(" ","",.) %>% trimws() %>%
        tolower()
    )
  ao_new_settlements_sf <-st_as_sf(ao_new_settlements,coords=c("X","Y"),crs=4326)
  ao_new_settlements_sf_adm_joined<-ao_new_settlements_sf %>% st_join(adm2 %>% select(admin2RefN))

  reported_new_settlement_names<-ao_new_settlements_sf_adm_joined%>% filter(!is.na(settlement.name)) %>%
    mutate(
      reported_county_name= paste0(settlement.name,admin2RefN) %>%
        gsub("[[:punct:]]","",.) %>%
        gsub(" ","",.) %>% trimws() %>%
        tolower()
    )

  reported_name_exact_match_to_data<-reported_new_settlement_names %>% filter(reported_county_name %in% aok_monthly_data_with_names$new_data_settlement_county_sanitized)
  reported_name_no_match_to_data<-reported_new_settlement_names %>% filter(reported_county_name %in% aok_monthly_data_with_names$new_data_settlement_county_sanitized==FALSE)
  settlement_name_not_in_data<-reported_new_settlement_names %>% filter(settlement.name %in% aok_monthly_data_with_names$D.info_settlement_other==FALSE)
  reported_new_settlement_names %>% nrow()
  settlement_name_not_in_data %>% write.csv("reported_new_settlements_not_in_data_aokJAN2020.csv")
  settlement_name_not_in_data %>% data.frame() %>%
  settlement_name_in_data<-reported_new_settlement_names %>% filter(settlement.name %in% aok_monthly_data_with_names$D.info_settlement_other)






}




  other_settlements<-aok_monthly_data %>%filter(!is.na(D.info_settlement_other)) %>%  pull(D.info_settlement_other)
  other_settlements[other_settlements %in% (ao_new_settlements %>% pull(settlement.name))]
  other_settlements[other_settlements %in% (ao_new_settlements %>% pull(settlement.name))==FALSE]

}


aok_name_harmonizer2<- function(aok_monthly_data,updated_settlement_list){

}
