library(dplyr)
libra
aok_data<-kobo_data_downloader(formid = 489385,user =  c(keys[1],keys[2]), api="kobohr", check = FALSE)
colnames(aok_data)<-gsub("\\/", ".", colnames(aok_data))
aok_data
colnames(aok_data)<-butteR::remove_kobo_grouper(colnames(aok_data))
cleaning_log/
# cleaning_log<- read.csv(aok_data




cl<-readxl::read_xlsx(path = "cleaning_log/example_cleaning_kapoeta.xlsx" ,  sheet= "cleaning")
getwd()

# start here --------------------------------------------------------------

# aok_data<-readxl::read_xlsx(path ="cleaning_log/example_cleaning_kapoeta.xlsx",sheet = "data" )
# aok_data[aok_data=="n\/a"]<-NA

kobold_cl<-kobold::read_xls_form(filepath = "cleaning_log/example_cleaning_kapoeta.xlsx" ,survey ="survey" ,choices = "choices",data = "data",cleaning = "cleaning" )
kobold_cl$cleaning$name <- butteR::remove_kobo_grouper(kobold_cl$cleaning$name)
kobold_cl$cleaning<- kobold_cl$cleaning %>% filter(!is.na(uuid))
kobold_cl$cleaning %>% filter(name=="remotely_how")
kobold_cl$survey %>% filter(name=="remotely_how")

colnames(kobold_cl$data) <- butteR::remove_kobo_grouper(colname = colnames(kobold_cl$data))

kobold_cl$survey$name

kobold_cl_implemented<-kobold::kobold_cleaner(object = kobold_cl)




data<-readxl::read_xlsx("cleaning_log/example_cleaning_kapoeta.xlsx", sheet = "data")
cleaning_log<-readxl::read_xlsx("cleaning_log/example_cleaning_kapoeta.xlsx", sheet = "cleaning")

colnames(data)
raw_to_clean <- function(ds,cl_log){
  for(i in 1:nrow(cl_log))
    print(i)
  print(cl_log$indicator[i])
  print(cl_log[i,]$indicator)
  ds[,cl_log[i,]$indicator][ds[,"_uuid"]==cl_log[i,]$_uuid] <- cl_log[i,]$new_value

}
