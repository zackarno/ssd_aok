library(tidyverse)
library(sf)
library(lubridate)


# LOAD IN DATA ------------------------------------------------------------

# IF THE AOK PROCESS IS BUILT TO RUN SEQUENTIALLY WE DONT NECESSARILY HAVE TO LOAD THESE FROM THE DATA BECAUSE THEY WILL ALREADY BE IN MEMORY
AOK <- read.csv("outputs/long_term_settlement_compiled/2020_01_REACH_SSD_AoK_LongTermSettlementData.csv", stringsAsFactors = FALSE, na.strings=c(""," "))

settlements<- read.csv("inputs/new_settlements/round_finals/2020_01_SSD_Settlements.csv", stringsAsFactors = FALSE, na.strings= c(""," "))

#DUPLICATED SETTLEMENTS
settlements %>% filter(NAMECOUNTY %in% settlements$NAMECOUNTY[which(duplicated(settlements$NAMECOUNTY))]) %>% arrange(NAMECOUNTY)

Grids <- st_read(dsn = "inputs/GIS",layer ="Grids_info")

#THESE COLUMNS SHOULD PROBABLY HAVE BEEN MADE AT AN EARLIER STAGE
AOK<-AOK %>%
  mutate(
    date= ymd(month),
    month= month(date),
    year = year(date)
  )%>% filter(date == "2020-01-01")





# MAKE SETTLEMENTS DATA SPATIAL AND PROJECT TO THE SAME CRS AS THE GRIDS
settlements<- st_as_sf(settlements,coords= c("X", "Y"), crs=4326)
settlements<- st_transform(x = settlements, crs= 32636)


Grids <- mutate(Grids, id_grid = as.numeric(rownames(Grids)))
settlements <- mutate(settlements, id_sett = as.numeric(rownames(settlements)))


#SPATIAL JOIN SETTLEMENT DATA TO GRIDD
setgrids <- st_join(settlements, Grids)
which(duplicated(settlements$NAMECOUNTY))

Assessed <-left_join(setgrids, AOK, by = c("NAMECOUNTY"="D.settlecounty") )

which(duplicated(Assessed$NAMECOUNTY))


# THEREFORE SETTLEMENTS ARE DUPLICATED- SHOULD NOT MATTER BECAUSE WE LATER GROUP BY YEAR AND
# MONTH
setgrids %>% nrow();Assessed %>% nrow



Assessed_current <-Assessed %>% filter(date == "2020-01-01")



#Make sure there are no duplicates in the unique column
Assessed_current %>% nrow()
distinct(Assessed_current,NAMECOUNTY, .keep_all= TRUE) %>% nrow()
Assessed_current %>%
  filter(NAMECOUNTY %in%
           Assessed_current$NAMECOUNTY[duplicated(Assessed_current$NAMECOUNTY)]) %>%
  select(date,NAMECOUNTY)
#No Null values allowed
Assessed_current <- Assessed_current %>% filter(!is.na(NAMECOUNTY))




#let us check how many settlements and KIs we have per  grid

grid_summary <- Assessed_current %>%
  st_drop_geometry() %>%
  filter(!is.na(D.info_county)) %>%
  group_by(State_id,month,year,date,D.info_state, D.info_county) %>%
  summarise(settlement_num = length(NAMECOUNTY), ki_num= sum(D.ki_coverage))


#Filter Grids with less than 2 KIs
grids_threshold <- grid_summary %>% filter(ki_num > 1, settlement_num > 1)

Assessed_AoK_cccm <- Assessed_current %>%
  select(id_grid,id_sett,D.ki_coverage, NAME,
         STATEJOIN,	COUNTYJOIN,PAYAM,State_id,G.food_now,H.mortality_increase,K.latrine_now,K.water_now_how,R.idp_supported,R.local_authorities,
         R.community_leadership, R.idp_leadership, R.info_source_who,F.idp_now, F.f2.idp_perc,F.f2.idp_time_arrive,F.hcdisp_now, F.refugee_return_now,
         F.f1.hc_remain_perc, F.f1.hc_leave,F.f1.hc_leave_time, G.food_distr, P.malnourished_children, P.malnourished_elderly,
         L.land_available,	L.ag_seeds,	L.ag_tools, G.market_now, N.prot_now, N.prot_incidence,N.comm_relations, N.land_disputes, J.j2.idp_location,
         J.j3.returnee_location, J.shelter_open_yn, J.shelter_open_yn, J.shelter_damage, J.shelter_destroyed_percent, J.shelter_flooding, J.nfi_distribution,
         K.water_now_how,K.water_now_time,K.water_source_animals, K.water_safety, K.water_source_seasonal, K.latrine_now, M.edu_now,M.edu_girl_attendance,
         M.edu_boy_attendance, G.food_now,G.meals_number,G.food_coping_comsumption.skip_days,S.shock_hunger, G.food_wild_proportion,
         G.food_wild_emergency, J.j2.idp_location,month,year,date,D.info_state, D.info_county)


#Proportion of assessed settlements reporting IDPs living in informal IDP sites separate from host communitys

Assessed_AoK_cccm<-Assessed_AoK_cccm %>%
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
    flooded_shelter = ifelse(J.shelter_flooding == "yes",1,0)
  )


grids_cccm <- Assessed_AoK_cccm %>%
  st_drop_geometry() %>%
  select(State_id,id_grid,idp_sites, IDP_present, IDP_time_arrive, IDP_majority, food_inadequate,less_one_meal,
         hunger_severe_worse,wildfood_sick_alltime,skipping_days, flooded_shelter,month,year,date,D.info_state, D.info_county )%>%
  group_by(id_grid, State_id,month,year,date,D.info_state, D.info_county)%>%
  summarise_all(funs(mean)) %>% ungroup()


#Proportion of assessed settlements reporting thalf or more  settlements population are IDPs and food access composite indicator
grids_cccm<- grids_cccm %>%
  mutate(
    fsl_composite = (food_inadequate +less_one_meal+hunger_severe_worse+wildfood_sick_alltime+skipping_days)/5
  )





#Filter Grids with less than 2 KIs
grids_cccm <- left_join(grids_cccm , grids_threshold%>%
                          ungroup() %>%
                          select(-month, -year, -date) , by = c("State_id" = "State_id"))

write.csv(
  grids_cccm,
  file = "longterm_Assessed_AoK_cccm.csv",
  na = "NA",
  row.names = FALSE)










