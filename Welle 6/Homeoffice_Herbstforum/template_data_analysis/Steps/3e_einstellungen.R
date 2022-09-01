# verschiedene Dimensionen. 

# Perspektive (Darstellung: Erst undifferenziert, dann nach Potenzial)
write.csv2(dat %>%
  group_by(ID) %>%
  filter(F2_w12345[2] %in% 2:3 & F2_w12345[3] %in% 2:3 & F2_w12345[4] %in% 2:3 & F2_w12345[5] %in% 2:3) %>%
  filter(!is.na(A2d_w2345)) %>%
  mutate(perspektive=as_factor(A2d_w2345)) %>%
  group_by(Welle, perspektive) %>%
  summarise(n=sum(Faktor_w123456)) %>%
  mutate(p=n/sum(n)),
  "Output/Perspektive_Zeit.csv")

write.csv2(dat %>%
  group_by(ID) %>%
  filter(F2_w12345[2] %in% 2:3 & F2_w12345[3] %in% 2:3 & F2_w12345[4] %in% 2:3 & F2_w12345[5] %in% 2:3) %>%
  filter(!is.na(A2d_w2345)) %>%
  mutate(potenzial=as_factor(potenzial), perspektive=as_factor(A2d_w2345)) %>%
  group_by(Welle, potenzial, perspektive) %>%
  summarise(n=sum(Faktor_w123456)) %>%
  mutate(p=n/sum(n)) ,
  "Output/Potenzial_Perspektive_Zeit.csv")

write.csv2(dat %>%
  mutate(potenzial=as_factor(potenzial), perspektive=as_factor(A2d_w2345)) %>%
  filter(Welle == 5 &  !is.na(A2d_w2345) ) %>%
  group_by(potenzial, perspektive) %>%
  filter(!is.na(potenzial)) %>%
  summarise(n=sum(Faktor_w123456), N=n()) %>%
  mutate(p = round(n/sum(n)*100, 5), N=sum(N)) ,
  "Output/Potenzial_Perspektive_w5.csv")

# Pendeln (Welle 3)
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
      !is.na(A2a2_3o_w3[3]) ~ as.numeric(A2a2_3o_w3)),
    pendeln = auto+bahn+pedes,
    pendeln_kat = cut(
      pendeln, 
      c(-1, 15, 30, 45, 1000000), 
      c("15", "30", "45", "max")
    )
  ) %>%
  ungroup() %>%
  mutate(perspektive=as_factor(A2d_w2345)) %>%
  filter(Welle == 3 & !is.na(perspektive) & !is.na(pendeln_kat)) %>%
  group_by(pendeln_kat, perspektive) %>%
  summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
  mutate(p = round(n/sum(n)*100, 1), N=sum(N)),
  "Output/Pendeln_Perspektive_w3.csv")




  write.csv2(dat %>%
  mutate(regelung = A2b_w5, perspektive=as_factor(A2d_w2345)) %>%
    mutate(regelung = as_factor(regelung)) %>%
  filter(Welle == 5 & !is.na(regelung) & !is.na(A2d_w2345)) %>%
  group_by(regelung, perspektive) %>%
  summarise(n=sum(Faktor_w123456), N=n()) %>%
  mutate(p = round(n/sum(n)*100, 5), N=sum(N)) %>%
   filter(regelung %in% levels(regelung)[1:2]),
  "Output/Regelung_Perspektive.csv")
  
  write.csv2(dat %>%
              mutate(perspektive=as_factor(A2d_w2345)) %>%
               filter(Welle == 5 & !is.na(haushaltstyp) & !is.na(perspektive)) %>%
               group_by(haushaltstyp, perspektive) %>%
               summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
               mutate(p = round(n/sum(n)*100, 5), N=sum(N)) ,
             "Output/Haushaltstyp_Perspektive.csv")
  
  # mit Potenzial: egal
  write.csv2(dat %>%
    mutate(regelung = A2b_w5, perspektive=as_factor(A2d_w2345)) %>%
    mutate(regelung = as_factor(regelung)) %>%
    filter(Welle == 5 & !is.na(regelung) & !is.na(A2d_w2345) ) %>%
    group_by(potenzial, regelung, perspektive) %>%
    summarise(n=sum(Faktor_w123456), N=n()) %>%
    mutate(p = round(n/sum(n)*100, 5), N=sum(N)) %>%
    filter(regelung %in% levels(regelung)[1:2]),
    "Output/Potenzial_Regelung_Perspektive.csv")
  
  # Arbeitszeiterfassung
  write.csv2(dat %>%
               #group_by(ID) %>%
               mutate(azerf = case_when(F2_w12345 %in% 1 ~ W5_A2a_w5, F2w12345 %in% 2:3 ~ W5_A2a, perspektive=as_factor(A2d_w2345))) %>%
               mutate(azerf = as_factor(azerf)) %>%
               filter(Welle == 5 & !is.na(azerf) & !is.na(A2d_w2345) & F2_w12345 %in% 1:2) %>%
               group_by(azerf, perspektive) %>%
               summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
               mutate(p = round(n/sum(n)*100, 5), N=sum(N)) %>%
               filter(azerf %in% levels(azerf)[1:3] ),
             "Output/Azerf_Perspektive.csv")
  
  write.csv2(dat %>%
    #group_by(ID) %>%
    mutate(azerf = W5_A2b_w5, perspektive=as_factor(A2d_w2345)) %>%
    mutate(azerf = as_factor(azerf)) %>%
    filter(Welle == 5 & !is.na(azerf) & !is.na(A2d_w2345) & F2_w12345 %in% 1:2 & !is.na(potenzial)) %>%
    group_by(potenzial, azerf, perspektive) %>%
    summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
    mutate(p = round(n/sum(n)*100, 5), N=sum(N)) %>%
    filter(azerf %in% levels(azerf)[1:3] ),
    "Output/Potenzial_Azerf_Perspektive.csv")
  
  # Arbeitszeiterfassung und Wochenendarbeit
  write.csv2(dat %>%
    filter(Welle == 5 & A1j_w35 %in% 1:4 & F2_w12345 %in% 1:3 & !is.na(W5_A2b_w5)) %>%
    mutate(wochenend = as_factor(A1j_w35), home=as_factor(F2_w12345), azerf = as_factor(W5_A2b_w5)) %>%
    group_by(home, azerf, wochenend) %>%
    filter(!is.na(home) & azerf %in% levels(azerf)[1:3]) %>%
    summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
    mutate(p = round(n/sum(n)*100, 5), N=sum(N)),
    "Output/Azerf_Wochenendarbeit.csv")
  
  write.csv2(dat %>%
               #group_by(ID) %>%
               mutate(azerf = case_when(F2_w12345 %in% 1 ~ as.numeric(W5_A2a_w5), F2_w12345 %in% 2:3 ~ as.numeric(W5_A2b_w5)), perspektive=as_factor(A2d_w2345)) %>%
               mutate(azerf = as_factor(case_when(azerf == 1 ~ "Betrieb", azerf == 2 ~ "Selbst", azerf == 3 ~ "Nicht")), wochenend = as_factor(A1j_w35), home=as_factor(F2_w12345)) %>%
               group_by(home, azerf, wochenend) %>%
               filter(!is.na(home) & azerf %in% levels(azerf)[1:3]) %>%
               summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
               mutate(p = round(n/sum(n)*100, 5), N=sum(N)),
             "Output/Azerf_Wochenendarbeit.csv")
  
  # extreme Arbeitszeiten
  write.csv2(dat %>%
    filter(Welle == 5 & F2_w12345 %in% 1:3 & !is.na(arbeitszeit_6_2021)) %>%
    mutate(home=as_factor(F2_w12345), azerf = as_factor(W5_A2b_w5)) %>%
    filter(azerf %in% levels(azerf)[1:3] & D6_3_w5 == 2 & arbeitszeit_6_2021 > 0),
    "Output/Azerf_extremeArbeitszeiten.csv")
  
  # Arbeitszeiterfasung und gute Arbeit im Homeoffice
  write.csv2(dat %>%
   group_by(ID) %>%
    mutate(azerf = as_factor(W5_A2b_w5[5]), gut = as_factor(A2c_12_w4[4])) %>%
    filter(Welle == 5 & F2_w12345 %in% 2:3 & !is.na(azerf) & gut %in% levels(gut)[1:4] & azerf %in% levels(azerf)[1:3]) %>%
    group_by(azerf, gut) %>%
    summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
    mutate(p = round(n/sum(n)*100, 5), N=sum(N)),
  "Output/Azerf_guteArbeit.csv")
  
  

 
  # BR
 write.csv2( dat %>%
    mutate(perspektive=as_factor(A2d_w2345)) %>%
    filter(Welle == 5 & !is.na(betriebsrat) & !is.na(potenzial) & !is.na(perspektive)) %>%
    group_by(potenzial, betriebsrat, perspektive) %>%
    summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
    mutate(p = round(n/sum(n)*100, 5), N=sum(N)) ,
    "Output/BR.csv")
 
 # BR

  
# Tarifbindung.   
  write.csv2(dat %>%
    mutate(perspektive=as_factor(A2d_w2345)) %>%
    filter(Welle == 5 & !is.na(tarifvertrag) & !is.na(potenzial) & !is.na(perspektive)) %>%
    group_by(potenzial, tarifvertrag, perspektive) %>%
    summarise(n=sum(Faktor_w123456, na.rm=T), N=n()) %>%
    mutate(p = round(n/sum(n)*100, 5), N=sum(N)),
    "Output/Tarifbindung.csv")
  

 
  

              