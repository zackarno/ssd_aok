library(dplyr)
questions<- readxl::read_xlsx(path = "inputs/48_December_2019/REACH_SSD_AoK_V37a_January2020.xlsx",sheet =  "survey")
questions$name
choices<-readxl::read_xlsx(path = "inputs/48_December_2019/REACH_SSD_AoK_V37a_January2020.xlsx",sheet =  "choices")
choices<-choices %>% select(list_name,name, label)

df<-read.csv("inputs/48_December_2019/5_Add_Mapped_Settlements/REACH_SSD_AoK_CleanedDataset_December2019.csv")

df_quick<-df[,2:391]
df_quick_list<-list()

df_quick<-df_quick %>% select_if(function(col)n_distinct(col)<100)
max_number_choices<-purrr::map(df_quick, function(x)length(unique(x))) %>% unlist() %>% max()
new_df_list<-list()
for(i in 1:ncol(df_quick)){
  print(i)
  df_quick_temp<-df_quick[,i]
  colname_temp<-colnames(df_quick)[i]
  df_temp2=data.frame(unique(df_quick_temp) %>% as.character() %>% as.vector())
  colnames(df_temp2)<-colname_temp
  number_to_fill<-max_number_choices-nrow(df_temp2)
  empty_df<-data.frame(rep(NA,number_to_fill)) %>% as_tibble()
  colnames(empty_df)<-colname_temp
  df_temp2 %>% as_tibble()
  df_temp_binded<-dplyr::bind_rows(df_temp2,empty_df) %>% mutate(index=1:nrow(.) %>% as.character()) %>% select(index, everything())
  new_df_list[[i]]<-df_temp_binded
}
new_df_list[[1]]$index
asdf<-purrr::reduce(new_df_list,left_join)
write.csv(asdf, "unique_answers_aokv37.csv",na = "")

without_group<- readxl::read_xlsx("inputs/reach_ssd_aok38_example_without_group.xlsx", sheet = "REACH_SSD_AoK_V38_Febuary2020")
with_group<- readxl::read_xlsx("inputs/reach_ssd_aok38_example_WITH_group.xlsx", sheet = "aok_example")
asdf<-butteR::auto_detect_select_multiple(df = without_group)
butteR::return_select_multiple_groups(without_group,questionnaire = assessment)


which_are_select_multiple<-which(
  sapply(names(without_group), assessment$question_is_select_multiple))
select_multiple_in_data<-names(which_are_select_multiple)}
asdf<-choices %>% filter(list_name %in% names(which_are_select_multiple))

questions_sm<-questions %>% filter(name %in% names(which_are_select_multiple))
questions_sm$type_lab <-questions_sm$type %>% stringr::str_replace_all("select_multiple","") %>% trimws()
choicessm<-choices %>%  filter(list_name  %in%  questions_sm$type_lab)

choices_sm2<-choicessm %>%
  left_join(questions_sm %>% select(type_lab,type,name), by=c("list_name"= "type_lab")) %>%
  mutate(label=paste0(name.y,".", name.x)) %>% data.frame()
choices_sm3<-choices_sm2$label %>% t() %>% data.frame()
write.csv(choices_sm3, 'select_multiple_new_formv38.csv', na = "")
# colnames(choices_sm3)<-choices_sm3[1,]
choices_sm3[1,]<- TRUE

choicessm$list_name %>% unique %>% length(),


questions$name

colname_df<- data.frame(with_group=colnames(with_group)[1:428], without_group=colnames(without_group))


#the grouper will be  a huge problem
# i need to extract select multiple columns using

col_name_df<-data.frame(group_col_names=colnames(df), non_group_col_names=butteR::remove_kobo_grouper(colnames(df)))
df_no_group<-df
colnames(df_no_group)<- butteR::remove_kobo_grouper(colnames(df))
assessment<- koboquest::load_questionnaire(data = df_no_group,questions = questions,choices = choices,choices.label.column.to.use ="label" )


sm_concat_cols<-sapply(names(df_no_group), assessment$question_is_select_multiple) %>% which()
names(sm_concat_cols)%in% questions$name
questions$type
choices$list_name
names(sm_concat_cols) %>% sort()
choices$list_name %>% unique() %>% sort()
questions$name


questions$type_lab<- questions$type %>% stringr::str_replace_all(c("select_one"="","select_multiple"= "")) %>% trimws()
choices_joined<-left_join(choices,questions, by=c("list_name"="type_lab")) %>% data.frame()
choices_joined<-choices_joined %>% select(list_name,name.x,name.y)
choices_questions_cols_joined<-left_join(choices_joined,colname_df, by=c("name.y"="without_group")) %>% data.frame()

choices_list<-split(choices,choices$list_name)
new_df_list1<-list()
new_df_list2<-list()
max_number_choices<-purrr::map(choices_list, nrow) %>%unlist() %>%  max()
debugonce(asdf)
asdf()
choices_questions_cols_joined<- choices_questions_cols_joined %>% filter(!is.na(with_group))
for (i in 1: nrow(choices_questions_cols_joined)){
  print
  choices_section1=unique(choices_questions_cols_joined$list_name)[i]
  df_temp<-choices %>% filter(list_name==choices_section1)
  # df_temp<-choices_list[[82]]

  df_temp<-df_temp %>% left_join(choices_questions_cols_joined  , by = "list_name")
  df_temp<-df_temp %>% filter(!is.na(with_group))
  df_temp$with_group<-as.character(df_temp$with_group)
  unique(df_temp$with_group) %>% length()
  if(length(unique(df_temp$with_group))>1){
    df_temp_split<-split(df_temp, df_temp$with_group)
    for(j in 1:length(df_temp_split)){
      print(paste0("j=",j))
      df_temp_split_temp<-df_temp_split[[j]]
      new_col_name<- df_temp_split_temp$with_group %>% unique()
      df_temp_column<-data.frame(df_temp_split_temp %>% pull(name) %>% unique())
      colnames(df_temp_column)<-new_col_name
      number_to_fill<-max_number_choices-nrow(df_temp_column)
      empty_df<-data.frame(rep(NA,number_to_fill)) %>% as_tibble()
      colnames(empty_df)<-new_col_name
      df_temp_binded<-dplyr::bind_rows(df_temp_column,empty_df) %>%
        mutate(index=1:nrow(.) %>% as.character()) %>% select(index, everything())
      new_df_list1[[j]]<-df_temp_binded
    }
  }
  if(length(unique(df_temp$with_group))==1){
  new_col_name<-df_temp$with_group %>% unique()
  df_temp_column<-data.frame(df_temp %>% pull(name) %>% unique())
  colnames(df_temp_column)<-new_col_name
  number_to_fill<-max_number_choices-nrow(df_temp_column)
  empty_df<-data.frame(rep(NA,number_to_fill)) %>% as_tibble()
  colnames(empty_df)<-new_col_name
  df_temp_column %>% as_tibble()
  df_temp_binded<-dplyr::bind_rows(df_temp_column,empty_df) %>% mutate(index=1:nrow(.) %>% as.character()) %>% select(index, everything())
  new_df_list2[[i]]<-df_temp_binded

  }
}
new_df_list1
new_df_list2
new_df_list3<-new_df_list2[!sapply(new_df_list2, is.null)]
# new_df_list4<-list(new_df_list1,new_df_list3)
new_df_list4 %>% unlist()



choices_data_validation1<-purrr::reduce(.x = new_df_list1,.f = left_join, by="index")
choices_data_validation2<-purrr::reduce(.x = new_df_list3,.f = left_join, by="index")
choices_data_validation3<-choices_data_validation1 %>% left_join(choices_data_validation2, by="index")
write.csv(choices_data_validation3, "choice_options_built_from_tool_v38.csv", na = "")

questions_joined<-left_join(questions,colname_df, by=c("name"="without_group")) %>% data.frame()

choices_joined %>% View()

choices_list<-split(choices,choices$list_name)
purrr::map(choices_list, nrow) %>%unlist() %>%  max()

new_df_list<-list()
max_number_choices<-purrr::map(choices_list, nrow) %>%unlist() %>%  max()
for (i in 1: length(choices_list)){
  print(i)
  df_temp<-choices_list[[i]]
  new_col_name<-df_temp$list_name %>% unique()
  df_temp_column<-df_temp %>% select(name)
  colnames(df_temp_column)<-new_col_name
  number_to_fill<-max_number_choices-nrow(df_temp_column)
  empty_df<-data.frame(rep(NA,number_to_fill)) %>% as_tibble()
  colnames(empty_df)<-new_col_name
  df_temp_binded<-dplyr::bind_rows(df_temp_column,empty_df) %>% mutate(index=1:nrow(.) %>% as.character()) %>% select(index, everything())
  new_df_list[[i]]<-df_temp_binded
}
choices_data_validation<-purrr::reduce(.x = new_df_list,.f = left_join, by="index")

data.frame(choices_names_orig=colnames(choices_data_validation))
questions$name


choices_data_validation %>% View()
questions$type


questions %>% group_by()
df$d.ki
df$D.F_startTme


col_name_df$non_group_col_names[col_name_df$non_group_col_names %in% colnames(choices_data_validation) %>% sort()]


