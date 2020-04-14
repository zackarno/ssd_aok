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
st_area(hex_grid)


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
# sett_grid_assessed2 <-inner_join(aok_clean3, sett_w_grid, by = c("D.settlecounty"="NAMECOUNTY") )
sett_grid_assessed$date
# sett_grid_assessed$NAMECOUNTY[!sett_grid_assessed$NAMECOUNTY%in%aok_clean3$D.settlecounty]
# sett_grid_assessed %>% nrow();sett_grid_assessed_l %>% nrow(); aok_clean3 %>% nrow();sett_grid_assessed2 %>% nrow()

# THEREFORE SETTLEMENTS ARE DUPLICATED- SHOULD NOT MATTER BECAUSE WE LATER GROUP BY YEAR AND
# MONTH






#I THINK THIS IS ACTUALLY WRONG BECAUSE THERE CAN BE DUPLICATE NAMECOUNT COLS

# error -------------------------------------------------------------------

#Make sure there are no duplicates in the unique column

distinct(sett_grid_assessed,NAMECOUNTY, .keep_all= TRUE) %>% nrow()
sett_grid_assessed %>%
  filter(NAMECOUNTY %in%
           sett_grid_assessed$NAMECOUNTY[duplicated(sett_grid_assessed$NAMECOUNTY)]) %>%
  select(date,NAMECOUNTY)
#No Null values allowed
assessed_w_grid <- sett_grid_assessed %>% filter(!is.na(NAMECOUNTY))
assessed_w_grid %>% nrow()


# can skip error section to here ------------------------------------------

 # I am not sure where D.ki_coverage comes from in the long term dataset. but i will try to create it based on assumptions here
assessed_w_grid$NAMECOUNTY
grid_summary<-assessed_w_grid %>%
  group_by(NAMECOUNTY,State_id) %>%
  summarise(D.ki_coverage=n()) %>%
  group_by(State_id) %>%
  summarise(settlement_num=n() ,ki_num=sum(D.ki_coverage) ) #%>%
  # filter(settlement_num!=ki_num)
#Filter Grids with less than 2 KIs
grid_summary_thresholded <- grid_summary %>% filter(ki_num > 1, settlement_num > 1)

  # group_by(State_id) %>%



#let us check how many settlements and KIs we have per  grid

grid_summary <- sett_grid_assessed %>%
  st_drop_geometry() %>%
  filter(!is.na(D.info_county)) %>%
  group_by(State_id,month,year,date,D.info_state, D.info_county) %>%
  summarise(settlement_num = length(NAMECOUNTY), ki_num= sum(D.ki_coverage))


#Filter Grids with less than 2 KIs
grids_threshold <- grid_summary %>% filter(ki_num > 1, settlement_num > 1)




#Proportion of assessed settlements reporting IDPs living in informal IDP sites separate from host communitys

assessed_w_grid<-assessed_w_grid %>%
  mutate(
    idp_sites= ifelse(J.j2.idp_location=="informal_sites",1,0),
    IDP_present= ifelse(F.idp_now=="yes",1,0),
    IDP_time_arrive=  ifelse(F.f2.idp_time_arrive %in% c("1_month","3_month"),1,0),
    IDP_majority=  ifelse( F.f2.idp_perc %in% c("half","more_half"),1,0),
    food_inadequate= ifelse(G.food_now == "no", 1,0),
    less_one_meal = ifelse(G.meals_number %in% c("one", "Less_than_1"),1,0),
    hunger_severe_worse = ifelse(S.shock_hunger %in% c("hunger_severe", "hunger_worst"),1,0),
    wildfood_sick_alltime = ifelse(G.food_wild_emergency=="yes"|G.food_wild_proportion=="all",1,0),
    skipping_days = ifelse(G.food_coping_comsumption.skip_days == "yes",1,0),
    flooded_shelter = ifelse(J.shelter_flooding == "yes",1,0),
    fsl_composite = (food_inadequate +less_one_meal+hunger_severe_worse+wildfood_sick_alltime+skipping_days)/5
  )


grids_cccm <- assessed_w_grid %>%
  st_drop_geometry() %>%
  select(State_id,id_grid,idp_sites, IDP_present, IDP_time_arrive, IDP_majority, food_inadequate,less_one_meal,
         hunger_severe_worse,wildfood_sick_alltime,skipping_days, flooded_shelter,month,year,date,D.info_state, D.info_county,fsl_composite )%>%
  group_by(id_grid, State_id,month,year,date,D.info_state, D.info_county)%>%
  summarise_all(funs(mean)) %>% ungroup()


#Proportion of assessed settlements reporting thalf or more  settlements population are IDPs and food access composite indicator
grids_cccm<- grids_cccm %>%
  mutate(
    fsl_composite = (food_inadequate +less_one_meal+hunger_severe_worse+wildfood_sick_alltime+skipping_days)/5
  )
grids_cccm %>%
  filter(fsl_composite!=fsl_composite2) %>%
  select(fsl_composite, fsl_composite2)
data.frame(asdf=grids_cccm$fsl_composite,asdfg=grids_cccm$fsl_composite2)
grids_cccm$fsl_composite

# I DON THINK THIS LEFT JOIN IS FILTERING... IT SHOULD HAVE BEEN REVERESED
###################################################
#Filter Grids with less than 2 KIs
grids_cccm <- left_join(grids_cccm , grids_threshold%>%
                          ungroup() %>%
                          select(-month, -year, -date) , by = c("State_id" = "State_id"))

write.csv(
  grids_cccm,
  file = "longterm_Assessed_AoK_cccm.csv",
  na = "NA",
  row.names = FALSE)










