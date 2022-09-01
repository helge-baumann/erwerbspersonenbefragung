rbind(
  dat_long_full %>%
    filter(!is.na(F3_w1) & Stichprobe_w5 == "Basisstichprobe") %>%
    mutate(home = F3_w1) %>%
    group_by(Welle, home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=homeoffice/sum(homeoffice)*100) %>%
    filter(home == 2) %>%
    mutate(Welle = 0) %>%
    select(Welle, everything()),
  dat_long_full %>%
    filter(!is.na(F2_w123457) & Stichprobe_w5 == "Basisstichprobe") %>%
    mutate(home = F2_w123457) %>%
    group_by(Welle, home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=homeoffice/sum(homeoffice)*100) %>%
    filter(home == 2),
  dat_long_full %>%
    filter(!is.na(A2_w8) & Stichprobe_w5 == "Basisstichprobe" & Welle == 8) %>%
    mutate(home = A2_w8) %>%
    group_by(home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=homeoffice/sum(homeoffice)*100) %>%
    filter(home == 2) %>%
    mutate(Welle = 8) %>%
    select(Welle, everything()),
  dat_long_full %>%
    filter(!is.na(W4_1_w4) & Stichprobe_w5 == "Basisstichprobe" & Welle == 4) %>%
    mutate(home = W4_1_w4) %>%
    group_by(home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=homeoffice/sum(homeoffice)*100) %>%
    filter(home == 2) %>%
    mutate(Welle = 9) %>%
    select(Welle, everything())
) %>%
  left_join(
rbind(
  dat_long_full %>%
    filter(!is.na(F3_w1) & Stichprobe_w5 == "Basisstichprobe") %>%
    mutate(home = F3_w1) %>%
    group_by(Welle, home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=sum(homeoffice[2:3])/sum(homeoffice)*100) %>%
    filter(home == 2) %>%
    mutate(Welle = 0) %>%
    select(Welle, everything()),
  dat_long_full %>%
    filter(!is.na(F2_w123457) & Stichprobe_w5 == "Basisstichprobe") %>%
    mutate(home = F2_w123457) %>%
    group_by(Welle, home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=sum(homeoffice[2:3])/sum(homeoffice)*100) %>%
    filter(home == 2),
  dat_long_full %>%
    filter(!is.na(A2_w8) & Stichprobe_w5 == "Basisstichprobe" & Welle == 8) %>%
    mutate(home = A2_w8) %>%
    group_by(home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=sum(homeoffice[2:3])/sum(homeoffice)*100) %>%
    filter(home == 2) %>%
    mutate(Welle = 8) %>%
    select(Welle, everything()) ,
  dat_long_full %>%
    filter(!is.na(W4_1_w4) & Stichprobe_w5 == "Basisstichprobe" & Welle == 4) %>%
    mutate(home = W4_1_w4) %>%
    group_by(home) %>%
    summarise(homeoffice = sum(Faktor_w12345678)) %>%
    mutate(homeoffice=sum(homeoffice[2:3])/sum(homeoffice)*100) %>%
    filter(home == 2) %>%
    mutate(Welle = 9) %>%
    select(Welle, everything()) 
) %>% 
  select(Welle, homeoffice),
by="Welle") %>%
  write.csv2("./Output/homeoffice.csv")
  
