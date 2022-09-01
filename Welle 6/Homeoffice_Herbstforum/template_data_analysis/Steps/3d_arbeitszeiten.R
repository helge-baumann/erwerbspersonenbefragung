
Wechsel <- dat %>%
  mutate(across(arbeitszeit_1_2020:arbeitszeit_6_2021, ~if_else(. == 0, NA_integer_, .))) %>%
  group_by(ID) %>% 
  mutate(
    hb_azv__0 = case_when(Welle == 1 & `2020-01-15`[1] == 1 ~ arbeitszeit_1_2020[1]),
    hb_azv__1 = case_when(Welle == 1 & `2020-01-15`[1] == 1 & `2020-04-15`[1] == 0 ~ arbeitszeit_4_2020[1]),
    hb_azv__2 = case_when(Welle == 2 & `2020-04-15`[1] == 1 & `2020-06-20`[2] == 0 ~ arbeitszeit_6_2020[2]),
    hb_azv__3 = case_when(Welle  == 3 & `2020-06-20`[2] == 1 & `2020-11-06`[3] == 0 ~ arbeitszeit_11_2020[3]),
    hb_azv__4 = case_when(Welle == 4 & `2020-11-06`[3] == 1 & `2020-12-15`[4] == 0 ~ arbeitszeit_12_2020[4]),
    hb_azv__5 = case_when(Welle == 4 & `2021-01-28`[4] == 1 & `2020-12-15`[4] == 0 ~ arbeitszeit_2_2021[4]),
    hb_azv__6 = case_when(Welle == 5 & `2021-07-01`[5] == 1 & `2021-01-28`[4] == 0 ~ arbeitszeit_6_2021[5]),
    #
    bh_azv__0 = case_when(Welle == 1 & `2020-01-15`[1] == 0 ~ arbeitszeit_1_2020[1]),
    bh_azv__1 = case_when(Welle == 1 & `2020-01-15`[1] == 0 & `2020-04-15`[1] == 1 ~  arbeitszeit_4_2020[1]),
    bh_azv__2 = case_when(Welle == 2 & `2020-04-15`[1] == 0 & `2020-06-20`[2] == 1 ~  arbeitszeit_6_2020[2]),
    bh_azv__3 = case_when(Welle  == 3 & `2020-06-20`[2] == 0 & `2020-11-06`[3] == 1 ~  arbeitszeit_11_2020[3]),
    bh_azv__4 = case_when(Welle == 4 & `2020-11-06`[3] == 0 & `2020-12-15`[4] == 1 ~  arbeitszeit_12_2020[4]),
    bh_azv__5 = case_when(Welle == 4 & `2021-01-28`[4] == 0 & `2020-12-15`[4] == 1 ~  arbeitszeit_2_2021[4]),
    bh_azv__6 = case_when(Welle == 5 & `2021-07-01`[5] == 0 & `2021-01-28`[4] == 1 ~  arbeitszeit_6_2021[5]),
    #
    hh_azv__0 = case_when(Welle == 1 & `2020-01-15`[1] == 1 ~ arbeitszeit_1_2020[1]),
    hh_azv__1 = case_when(Welle == 1 & `2020-01-15`[1] == 1 & `2020-04-15`[1] == 1 ~ arbeitszeit_4_2020[1]),
    hh_azv__2 = case_when(Welle == 2 & `2020-04-15`[1] == 1 & `2020-06-20`[2] == 1 ~ arbeitszeit_6_2020[2]),
    hh_azv__3 = case_when(Welle  == 3 & `2020-06-20`[2] == 1 & `2020-11-06`[3] == 1 ~ arbeitszeit_11_2020[3]),
    hh_azv__4 = case_when(Welle == 4 & `2020-11-06`[3] == 1 & `2020-12-15`[4] == 1 ~ arbeitszeit_12_2020[4]),
    hh_azv__5 = case_when(Welle == 4 & `2021-01-28`[4] == 1 & `2020-12-15`[4] == 1 ~ arbeitszeit_2_2021[4]),
    hh_azv__6 = case_when(Welle == 5 & `2021-07-01`[5] == 1 & `2021-01-28`[4] == 1 ~ arbeitszeit_6_2021[5]),
    #
    bb_azv__0 = case_when(Welle == 1 & `2020-01-15`[1] == 0 ~ arbeitszeit_1_2020[1]),
    bb_azv__1 = case_when(Welle == 1 & `2020-01-15`[1] == 0 & `2020-04-15`[1] == 0 ~  arbeitszeit_4_2020[1]),
    bb_azv__2 = case_when(Welle == 2 & `2020-04-15`[1] == 0 & `2020-06-20`[2] == 0 ~  arbeitszeit_6_2020[2]),
    bb_azv__3 = case_when(Welle  == 3 & `2020-06-20`[2] == 0 & `2020-11-06`[3] == 0 ~  arbeitszeit_11_2020[3]),
    bb_azv__4 = case_when(Welle == 4 & `2020-11-06`[3] == 0 & `2020-12-15`[4] == 0 ~  arbeitszeit_12_2020[4]),
    bb_azv__5 = case_when(Welle == 4 & `2021-01-28`[4] == 0 & `2020-12-15`[4] == 0 ~  arbeitszeit_2_2021[4]),
    bb_azv__6 = case_when(Welle == 5 & `2021-07-01`[5] == 0 & `2021-01-28`[4] == 0 ~  arbeitszeit_6_2021[5])
  ) %>%
  ungroup() %>%
  #select(azv__1:b_azv__6) 
  #summarise(across(azv__1:b_azv__6, ~sum(!is.na(.))))
  summarise(across(hb_azv__0:bb_azv__6, wtd.mean, Faktor_w123456, na.rm=T))

Wechsel_neu <- Wechsel %>% 
  pivot_longer(everything(), names_sep="__", names_to=c("name1", "zeitpunkt"), values_to="value")

ggplot(Wechsel_neu) + geom_line(aes(x=as.numeric(zeitpunkt), y=value, color=name1))

write.csv2(Wechsel_neu, "Output/Wechsel.csv")

# absolut
AZ <- 
  dat %>%
  mutate(across(arbeitszeit_1_2020:arbeitszeit_6_2021, ~if_else(. == 0, NA_integer_, .))) %>%
  group_by(ID) %>%
  mutate(across(arbeitszeit_1_2020:arbeitszeit_6_2021, 
                .fns = list(prozent = ~(./arbeitszeit_1_2020[1]*100)))) %>%
  ungroup() %>%
  pivot_longer(cols=c(`2020-01-15`:`2021-07-01`), names_to="homeoffice", values_to="value") %>% 
  filter(!is.na(value)) %>%
  group_by(homeoffice, value) %>%
  summarise(across(arbeitszeit_1_2020:arbeitszeit_6_2021, wtd.mean, Faktor_w123456, na.rm=T)) 

AZ_short <- tibble(homeoffice=character(nrow(AZ)), value=double(nrow(AZ)), arbeitszeit=double(nrow(AZ)))

num <- 0

for(i in 1:nrow(AZ)) {
  
  AZ_short$homeoffice[i] <- AZ$homeoffice[i]
  AZ_short$value[i] <- AZ$value[i]
  if(i %% 2 == 1) num <- num+1
  AZ_short$arbeitszeit[i] <- as.double(AZ[i, num+2])
  
}

write.csv2(AZ_short, "Output/AZ_absolut.csv")

# durchgehend? Prozentuale Veränderungen
 AZ <- 
   dat %>%
   mutate(across(arbeitszeit_1_2020:arbeitszeit_6_2021, ~if_else(. == 0, NA_integer_, .))) %>%
   group_by(ID) %>%
   mutate(durchgehend = case_when(
     ((`2020-04-15`[1] == 1) + (`2020-06-20`[2] == 1) + (`2020-11-06`[3] == 1) + (`2020-12-15`[4] == 1) + (`2021-01-28`[4] == 1) + (`2021-07-01`[5] == 1)) >= 5 ~ 1,
     ((`betrieb_2020-04-15`[1] == 1) + (`betrieb_2020-06-20`[2] == 1) + (`betrieb_2020-11-06`[3] == 1) + (`betrieb_2020-12-15`[4] == 1) + (`betrieb_2021-01-28`[4] == 1) + (`betrieb_2021-07-01`[5] == 1)) >= 5 ~ 0)
     ) %>%
   #group_by(Welle, durchgehend) %>% count()
   mutate(across(arbeitszeit_1_2020:arbeitszeit_6_2021, 
                    .fns = list(prozent = ~(./arbeitszeit_1_2020[1]*100)))) %>%
   #select(contains("prozent"))
   ungroup() %>%
   pivot_longer(cols=c(`2020-01-15`:`2021-07-01`), names_to="homeoffice", values_to="value") %>% 
   filter(!is.na(value)) %>%
   group_by(homeoffice, durchgehend) %>%
   summarise(across(arbeitszeit_1_2020_prozent:arbeitszeit_6_2021_prozent, wtd.mean, Faktor_w123456, na.rm=T), n=n()) %>%
 filter(!is.na(durchgehend))
 
 AZ_short <- tibble(homeoffice=character(nrow(AZ)), durchgehend=double(nrow(AZ)), arbeitszeit=double(nrow(AZ)))
 
 num <- 0
 
 for(i in 1:nrow(AZ)) {
   
   AZ_short$homeoffice[i] <- AZ$homeoffice[i]
   AZ_short$durchgehend[i] <- AZ$durchgehend[i]
   if(i %% 2 == 1) num <- num+1
   AZ_short$arbeitszeit[i] <- as.double(AZ[i, num+2])
   
 }
 
write.csv2(AZ_short, "Output/AZ_prozentual.csv")

 # Pendeln und Uhrzeiten
   write.csv2(dat %>%
              
   group_by(ID) %>%
   filter(((`2020-06-20`[2] == 1) + (`2020-11-06`[3] == 1) +  (`2021-07-01`[5] == 1)) == 3) %>%
   ungroup() %>%
     filter(A2a_w235 %in% 1:3) %>%
   group_by(Welle, A2a_w235) %>%
   summarise(n=sum(Faktor_w123456), N=n()) %>%
     mutate(p = n/sum(n)) ,
   "Output/Uhrzeiten_vergleichbar.csv")
   
   # Mehrarbeit / Quali
   write.csv2(dat %>%
     group_by(ID) %>%
     filter(((`2020-06-20`[2] == 1) + (`2020-11-06`[3] == 1) +  (`2021-01-28`[4] == 1)) == 3) %>%
     mutate(angestellt = case_when(Welle == 2 ~ S6c_w1235, Welle == 3 ~ S6c_w1235, Welle == 4 ~ S6c_w1235[3])) %>%
     ungroup() %>% 
     filter(A2c_11_w234 %in% 1:4 & !is.na(angestellt)) %>%
     mutate(angestellt = as_factor(case_when(angestellt %in% 1:2 ~ "einfach", angestellt %in% 3:4 ~ "quali"))) %>% 
     group_by(Welle, angestellt, A2c_11_w234) %>%
     summarise(n=sum(Faktor_w123456), N=n()) %>%
     mutate(p = n/sum(n), N=sum(N)) %>%
     summarise(p = sum(p[A2c_11_w234 %in% 1:2])),
     "Output/Mehrarbeit_quali.csv")
   
   # Pendeln
   write.csv2(dat %>%
     mutate(
       auto=case_when(
         A2a2_1_w3 == 0 ~ 0,
         !is.na(A2a2_1o_w3) ~ as.numeric(A2a2_1o_w3)),
       bahn = case_when(
         A2a2_2_w3 == 0 ~ 0,
         !is.na(A2a2_2o_w3) ~ as.numeric(A2a2_2o_w3)),
       pedes = case_when(
         A2a2_3_w3 == 0 ~ 0,
         !is.na(A2a2_3o_w3) ~ as.numeric(A2a2_3o_w3))
     ) %>%
     pivot_longer(cols=c(auto:pedes), names_to="var", values_to="val") %>%
     filter(!is.na(val)) %>%
     group_by(F2_w12345, var) %>%
     summarise(p=wtd.mean(val, Faktor_w123456), n=n()) %>%
     mutate(pendeln = sum(p)),
     "Output/Homeoffice_Pendeln.csv"
   )
   
   # Noch: Kinder im Haushalt und Work-Life-Balance
 
   # Homeoffice und Arbeitszeitreduktion
   dat %>%
     mutate(home = as_factor(F2_w12345), azr = as_factor(F24_1_w1235)) %>%
     filter(home %in% levels(home)[1:3] & !is.na(azr)) %>%
    group_by(Welle, sex, home, azr) %>%
     summarise(n = sum(Faktor_w123456, na.rm=T), N=n()) %>%
     mutate(p=round(n/sum(n)*100)) %>%
     filter(azr == "Trifft zu") %>%
     ggplot() + geom_line(aes(x=Welle, y=p, color=home)) + theme(legend.position="bottom") +facet_wrap(~sex)
   
   # Homeoffice und Arbeitszeitreduktion
  write.csv2(dat %>%
     mutate(home = as_factor(case_when(F2_w12345 %in% 2:3 ~ "home", F2_w12345 == 1 ~ "betrieb")), azr = as_factor(F24_1_w1235)) %>%
     filter(!is.na(azr) & !is.na(home)) %>%
     group_by(Welle, sex, home, azr) %>%
     summarise(n = sum(Faktor_w123456, na.rm=T), N=n()) %>%
     mutate(p=round(n/sum(n)*100)) %>%
     filter(azr == "Trifft zu"), 
     "Output/Homeoffice_Arbeitszeitreduktion.csv")
     
   
   write.csv2(dat %>%
     #mutate(home = as_factor(case_when(F2_w12345 %in% 2:3 ~ "home", F2_w12345 == 1 ~ "betrieb"))) %>%
     group_by(ID) %>%
     mutate(home = as_factor(case_when(
       ((`2020-04-15`[1] == 1) + (`2020-06-20`[2] == 1) + (`2020-11-06`[3] == 1) + (`2021-01-28`[4] == 1) + (`2021-07-01`[5] == 1)) >= 3 ~ 1,
       ((`betrieb_2020-04-15`[1] == 1) + (`betrieb_2020-06-20`[2] == 1) + (`betrieb_2020-11-06`[3] == 1)  + (`betrieb_2021-01-28`[4] == 1) + (`betrieb_2021-07-01`[5] == 1)) >= 3 ~ 0)
     )) %>%
     ungroup() %>%
     filter(!is.na(home)) %>%
     pivot_longer(cols=c(F28_1_w12345:F28_4_w12345), names_to="var", values_to="value") %>%
     mutate(value = case_when(value %in% 1:2 ~ "belastend", value == 3 ~ "etwas", value %in% 4:5 ~ "nicht belastend")) %>%
     filter(!is.na(value)) %>%
     group_by(Welle, home, var, value) %>%
     summarise(n = sum(Faktor_w123456, na.rm=T), N=n()) %>%
     mutate(p=round(n/sum(n)*100)) %>%
     filter(value == "belastend") , 
     "Output/Homeoffice_Sorgen.csv")
   
  write.csv2(dat %>%
               mutate(home = as_factor(case_when(F2_w12345 %in% 2:3 ~ "home", F2_w12345 == 1 ~ "betrieb"))) %>%
    filter(C13_w35 %in% 1:2 & !is.na(home)) %>%
    mutate(home = as_factor(F2_w12345), corona=as_factor(C13_w35)) %>%
    group_by(Welle, home, corona) %>%
   
    summarise(n=n()) %>%
    mutate(p=n/sum(n)) %>%
    filter(corona=="Ja"),
    "Output/Infektionen.csv")
  