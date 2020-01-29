

read_all_csvs<- function(input_csv_folder){
  filenames_long<-list.files(input_csv_folder,full.names = TRUE)
  filenames_short<-list.files(input_csv_folder,full.names = FALSE)
  all_csvs<-list()
  for (i in 1: length(filenames_long)){
    file_of_interest<- filenames_long[i]
    file_of_interest_short_name<- filenames_short[i]
    data<- read.csv(file_of_interest,
                    stringsAsFactors = FALSE,
                    row.names = NULL, na.strings = c(""," ",NA, "NA"))
    all_csvs[[file_of_interest_short_name]]<-data
  }
  return(all_csvs)
}




























