
library(tidyverse)
library(sf)
library(lubridate)


# LOAD IN DATA ------------------------------------------------------------
month_of_assessment<-"2020-02-01" #already in main script
# IF THE AOK PROCESS IS BUILT TO RUN SEQUENTIALLY WE DONT NECESSARILY HAVE TO LOAD THESE FROM THE DATA BECAUSE THEY WILL ALREADY BE IN MEMORY
AOK <- read.csv("outputs/long_term_settlement_compiled/2020_01_REACH_SSD_AoK_LongTermSettlementData.csv", stringsAsFactors = FALSE, na.strings=c(""," "))
aok_raw <- read.csv("inputs/2020_02/2020_02_FEB_AOK_RAW_20200304_from_KOBO.csv", stringsAsFactors = FALSE, na.strings=c(""," "))

# assume we have the data to clean it and the cleaning log has been implemented
aok_clean<- aok_raw

#pretend its an object from main script
aok_clean3<- aok_clean
aok_clean3<-aok_clean3 %>%
  mutate(date=month_of_assessment %>% ymd(),
         month=month(date),
         year=year(date),
         #if we use D.info_settlement_final the others wont match until clean
         D.settlecounty=paste0(D.info_settlement,D.info_county)) %>%
  # therefore use D.info_settlement and filter others for tutorial
  filter(D.info_settlement!="other")

#used to trouble shoot
# aok_clean3 %>%
# filter(D.info_settlement!=D.info_settlement_final) %>% select(D.info_settlement,D.info_settlement_final) %>% nrow()

#assume that this is the neweset settlement list
settlements<- read.csv("inputs/new_settlements/round_finals/2020_01_SSD_Settlements.csv", stringsAsFactors = FALSE, na.strings= c(""," "))

#DUPLICATED SETTLEMENTS
settlements %>% filter(NAMECOUNTY %in% settlements$NAMECOUNTY[which(duplicated(settlements$NAMECOUNTY))]) %>% arrange(NAMECOUNTY)

#HEX GRID HAS ALREADY BEEN CREATED
hex_grid <- st_read(dsn = "inputs/GIS",layer ="Grids_info")



# MAKE SETTLEMENTS DATA SPATIAL AND PROJECT TO THE SAME CRS AS THE GRIDS
settlements<- st_as_sf(settlements,coords= c("X", "Y"), crs=4326)
settlements<- st_transform(x = settlements, crs= 32636) # project to utm (same as grid)


hex_grid <- hex_grid %>% mutate( id_grid = as.numeric(rownames(hex_grid)))

settlements <- settlements %>% mutate(id_sett = as.numeric(rownames(settlements)))


#SPATIAL JOIN SETTLEMENT DATA TO GRIDD
sett_w_grid <- st_join(settlements, hex_grid) #%>% select(-DATE)
which(duplicated(settlements$NAMECOUNTY)) # this is an issue


#THEree will be many here if we use D.info_settlement_final to define D.settlecounty before cleaning
aok_clean3$D.settlecounty[!aok_clean3$D.settlecounty %in% sett_w_grid$NAMECOUNTY]



# sett_grid_assessed_l <-left_join(sett_w_grid, aok_clean3, by = c("NAMECOUNTY"="D.settlecounty") )
assessed_w_grid <-inner_join(sett_w_grid, aok_clean3, by = c("NAMECOUNTY"="D.settlecounty") )


assessed_w_grid$R.phone_use %>% unique() %>% dput()
assessed_w_grid<-assessed_w_grid %>%
  mutate(
    i.cellphone= ifelse(R.phone_use %in% c("yes_network_often", "yes_network_sometimes"), "yes","no")
  )


cols_to_analyze<- c("i.cellphone")
library(srvyr)
assessed_svy<- as_survey(assessed_w_grid)
assessed_w_grid$Q.main_language %>% unique()
assessed_w_grid$language
grid_analysis<- butteR::mean_proportion_table(design =assessed_svy,
                                          list_of_variables = cols_to_analyze,
                                          aggregation_level =  "id_grid",
                                          na_replace = F)
grid_poly_joined<- inner_join (hex_grid, grid_analysis)

adm2_gdb<-"../../gis_data/gis_base/boundaries/county_shapefile"
adm2<- st_read(adm2_gdb,"ssd_admbnda_adm2_imwg_nbs_20180401")
# st_write(grid_poly_joined,"qgis/aap/shp/feb_aok_aap_hex_analysis.shp")
st_layers(adm2_gdb)[1]
library(tmap)
?tm_polygons
tm_shape(grid_poly_joined)+
  # tm_fill(showNA=F )
  tm_polygons("i.cellphone.yes", border.col = NA)+

tm_shape(adm2)+
  tm_polygons(col=NA, alpha=0,border.col = "black")


