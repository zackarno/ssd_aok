library(dplyr)
library(sf)
library(stringr)
# detach("packages:butteR")
# butteR::extract_sm_option_columns


source("scripts/functions/utils.R")


# read and compile all csvs -----------------------------------------------

input_csv_folder<- "inputs/48_December_2019/2_Export_to_CSV"
csv_list<- read_all_csvs(input_csv_folder = input_csv_folder)
df<- dplyr::bind_rows(csv_list)
df2<-df %>% select_if(~!all(is.na(.)))



# GET RID OF ROWS THAT ARE ALL NA
df2$all_blanks <- apply(df2, 1, function(x) all(is.na(x)))
df3<-filter(df2, all_blanks==FALSE)

#LOAD QUESTIONNAIRE
kobo_choices<-readxl::read_xlsx(path="inputs/48_December_2019/REACH_SSD_AoK_V37a_January2020.xlsx",
                                sheet = "choices")
kobo_survey<-readxl::read_xlsx(path="inputs/48_December_2019/REACH_SSD_AoK_V37a_January2020.xlsx",
                               sheet = "survey")

colnames(df3)<-butteR::remove_kobo_grouper(colnames(df3))
kobo_questionnaire<-koboquest::load_questionnaire(data = df3,questions = kobo_survey,choices = kobo_choices)

#EXTRACT SELECT  MULTIPLE
sm_cols_df<-butteR::extract_sm_option_columns(df = df3,name_vector=colnames(df3))
#SELECTING LOGICAL ONLY WILL REMOVE CONCATENATED STRING SELECT MULTIPLE
df_just_sm<-df3 %>% select(sm_cols_df$sm_options) %>% select_if(is.integer)

#REPLACE 1S WITH YES 0 WITH NO SM
df_just_sm<-df_just_sm %>%
  purrr::map_df(function(x)as.character(x) %>%
                  stringr::str_replace_all(c("0"="no", "1"="yes"))) %>% data.frame()

df3[,colnames(df_just_sm)]<-df_just_sm

settlements<-readr::read_csv("inputs/48_December_2019/SSD_Settlements_V37.csv")


df4<-df3 %>%
  mutate(
    settlement_county_sanitized= paste0(info_settlement_final, info_county) %>%
      trimws()%>%
      gsub("[[:punct:]]","",.) %>%
      tolower())
df4$settlement_county_sanitized

settlements<- settlements %>%
  mutate(settlement_county_sanitized= NAMECOUNTY %>%
           trimws() %>% gsub("[[:punct:]]","",.) %>%  tolower())

settlements$settlement_county_sanitized


aok_settlements_missing<-df4 %>% anti_join( settlements, by= "settlement_county_sanitized")
settlements$NAMEJOIN
settlements$NAMECOUNTY
settlements$NAME


aok_settlements_missing %>% nrow()
fuzzy_join<-fuzzyjoin::stringdist_join(aok_settlements_missing%>% select(X_uuid, info_settlement_final,info_county,  settlement_county_sanitized), settlements%>% select(NAME, NAMEJOIN, NAMECOUNTY,settlement_county_sanitized), by = NULL, max_dist = 2, method = c("osa","lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw","soundex"), mode = "inner", ignore_case = FALSE,distance_col = "string_dist")


fuzzy_join<-fuzzyjoin::stringdist_join(aok_settlements_missing%>% select(X_uuid, info_settlement_final,info_county,  settlement_county_sanitized), settlements%>% select(NAME, NAMEJOIN, NAMECOUNTY,settlement_county_sanitized), by = NULL, max_dist = 2, method = c("osa","lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw","soundex"), mode = "left", ignore_case = FALSE,distance_col = "string_dist")


new_settlements_ao<- read.csv(file = "inputs/48_December_2019/5_Add_Mapped_Settlements/REACH_SSD_AoK_newly_mapped_settlements_December2019.csv")
new_settlements_ao<-new_settlements_ao %>% filter( !is.na(Latitude)& !is.na(Longitude))
new_settlements_ao$Longitude
new_settlements_ao_sf<- sf::st_as_sf(new_settlements_ao, coords= c("Longitude","Latitude"),crs= 4326)
settlements_spatial_full<-settlements %>% filter(!is.na(X) & !is.na(Y))
nrow(settlements_spatial_full); nrow(settlements)
settlements_sf<- sf::st_as_sf(settlements_spatial_full, coords=c("X", "Y"), crs=4326)



debugonce(butteR::closest_distance_rtree)
asdf<-butteR::closest_distance_rtree(sf1=new_settlements_ao_sf,sf2 = settlements_sf)


asdf<-butteR::match_closest_pt_rtree(new_settlements_ao_sf,sf2 = settlements_sf)

sf::st_coordinates(settlements_sf)

butteR::check_distances_from_target(dataset =new_settlements_ao_sf,target_points =settlements_sf  )
butteR::match_closest_pt_rtree()












write.csv(fuzzy_join, "adsf.csv")
aok_settlements_missing %>% nrow()
asdf %>% nrow()
asdf

# scrap -------------------------------------------------------------------




asdf<-aok_settlements_missing %>% fuzzyjoin::fuzzy_left_join(settlements, by = c("settlement_county_sanitized"), match_fun = str_detect)




data.frame(asdf$settlement_county_sanitized.x, asdf$settlement_county_sanitized.y)

aok_settlements_missing %>% select(settlement_county_sanitized)
