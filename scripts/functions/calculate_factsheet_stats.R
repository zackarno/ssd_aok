settlement$F.idp_now
settlement$I.health_now.none
asdf<-settlement %>%
  group_by(D.info_county) %>%
  summarise(perc_no_health= sum(I.health_now.none=="yes")/n()) %>%
  arrange(desc(perc_no_health))

return_top_n_choices(asdf,unique_answer_col =perc_no_health,rank = 5)
