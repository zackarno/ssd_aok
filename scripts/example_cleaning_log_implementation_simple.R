library(dplyr)
library(koboloadeR)
source("scripts/functions/keys.R")


aok_data<-kobo_data_downloader(formid = 489385,user =  c(keys[1],keys[2]), api="kobohr", check = FALSE)
colnames(aok_data)<-gsub("\\/", ".", colnames(aok_data))

cleaning_log<-read.csv(file = "cleaning_log/example_cleaning_log_from_kapoeta.csv", stringsAsFactors = FALSE)


cleaning_log<-cleaning_log %>% filter(!is.na(uuid))
cleaning_log$uuid %in% aok_data$`_uuid`

implement_cleaning_log <- function(ds,
                         ds_uuid,
                         cl,
                         cl_change_col,
                         cl_uuid,cl_new_val){
  cl[[cl_change_col]]<-cl[[cl_change_col]] %>% trimws()
  cl[[cl_new_val]]<-cl[[cl_new_val]] %>% trimws()

  if(all(cl[[cl_change_col]] %in% colnames(ds))==F){
    problem_question_in_cl<-cl[[cl_change_col]][cl[[cl_change_col]] %in% colnames(ds)==FALSE]
    print(paste0(problem_question_in_cl,": not in data"))
  }

  if(all(cl[[cl_uuid]] %in% ds[[ds_uuid]])==F){
    problem_uuid_in_cl<-cl[[cl_uuid]][cl[[cl_uuid]] %in% ds[[ds_uuid]]==FALSE]
    print(problem_uuid_in_cl)
    print("NOT IN DATASET")

    }

  assertthat::assert_that(all(cl[[cl_change_col]] %in% colnames(ds)),
                          msg="Error: Make sure all name in question label column in the cleaning log are in dataset")

  assertthat::assert_that(all(cl[[cl_uuid]] %in% ds[[ds_uuid]]),
                          msg="Error:Make sure all uuids in cleaing log are in data set")

  for(i in 1:nrow(cl)){
    print(cl[[cl_change_col]][i])

    ds[,cl[i,][[cl_change_col]]][ds[,cl_uuid]==cl[i,][[cl_uuid]]] <- cl[i,][[cl_new_val]]
  }

  return(ds)
}

"945f458c-1ddc-409b-9095-6025da43c5d5" %in% aok_data$`_uuid`

clean_data<-implement_cleaning_log(ds = aok_data,
                         ds_uuid ="_uuid",
                         cl=cleaning_log,
                         cl_change_col = "name",
                         cl_uuid = "uuid",
                         cl_new_val = "value")

check_cleaning_log <- function(df,
                                   df_uuid,
                                   cl,
                                   cl_change_col,
                                   cl_uuid,cl_new_val){
  cl[[cl_change_col]]<-cl[[cl_change_col]] %>% trimws()
  cl[[cl_new_val]]<-cl[[cl_new_val]] %>% trimws()

  cl_change_col_prob_df<-cl %>%
    mutate(cl_prob="question_does_not_exist") %>%
    filter(!!sym(cl_change_col) %in% colnames(df)==FALSE) %>%
    select(cl_prob,everything())

  cl_uuid_prob_df<-cl %>% filter(!!sym(cl_uuid) %in% df[[df_uuid]]==FALSE)%>%
    mutate(cl_prob="uuid_does_not_exist") %>%
    filter(!!sym(cl_uuid) %in% df[[df_uuid]]==FALSE) %>%
    select(cl_prob,everything())

  cl_problems_df<-bind_rows(get0("cl_change_col_prob_df"), get0("cl_uuid_prob_df"))

  if(nrow(cl_problems_df)>0){
    print("cleaning log has issues, see output table")
  }
  else{
    cl_problems_df<-"no issues in cleaning log found"
    print("no issues in cleaning log found")
  }
  return(cl_problems_df)

}
# debugonce(check_cleaning_log)
clean_log_probs<-check_cleaning_log(df = aok_data,
                                   df_uuid ="_uuid",
                                   cl=cleaning_log,
                                   cl_change_col = "name",
                                   cl_uuid = "uuid",
                                   cl_new_val = "value")
clean_log_probs
