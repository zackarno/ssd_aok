

read_all_csvs<- function(input_csv_folder){
  filenames_long<-list.files(input_csv_folder,full.names = TRUE)
  filenames_short<-list.files(input_csv_folder,full.names = FALSE)
  all_csvs<-list()
  for (i in 1: length(filenames_long)){
    file_of_interest<- filenames_long[i]
    file_of_interest_short_name<- filenames_short[i]
    data<- read.csv(file_of_interest,
                    stringsAsFactors = FALSE,
                    row.names = NULL, na.strings = c(""," ",NA, "NA","<NA>"))
    all_csvs[[file_of_interest_short_name]]<-data
  }
  return(all_csvs)
}

download_aok_data<- function(keys_file){
  source(keys_file)
  aok_data<-kobo_data_downloader(formid = 489385,user =  c(keys[1],keys[2]), api="kobohr", check = FALSE)
  colnames(aok_data)<-gsub("\\/", ".", colnames(aok_data))
  return(aok_data)

}



























