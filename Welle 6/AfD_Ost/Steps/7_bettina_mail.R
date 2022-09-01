write.csv2(
  dat %>%
    filter(!is.na(einkommensverlust_hh) & !is.na(W6_2_w6)) %>%
    mutate(partei = as_factor(W6_2_w6)) %>%
  group_by(partei, einkommensverlust_hh) %>%
  summarise(n=sum(Faktor_w123456), N=n()) %>%
  mutate(p = round(n/sum(n)*100, digits=0), N = sum(N)) %>%
    select(-n) %>%
    pivot_wider(names_from=einkommensverlust_hh, values_from=p),
  "Output/Einkommensverluste.csv"
)

write.csv2(
dat %>%
  filter(W6_2_w6 %in% 1:7) %>%
  pivot_longer(cols=befristet:selbststaendig, names_to="Variable", values_to="Wert") %>%
  filter(!is.na(Wert)) %>%
  mutate(partei = as_factor(W6_2_w6)) %>%
  group_by(partei, Variable, Wert ) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)) %>%
  filter(Wert =="ja") %>%
  select(-n, -N, -Wert) %>%
  pivot_wider(names_from = Variable, values_from = p, 
              id_cols=c(partei)) ,
"Output/Beschäftigungsformen.csv")

# Institutionenvertrauen
write.csv2(
dat %>%
  mutate(spd = 
           case_when(
             partei_5 == "SPD" & W6_2_w6 == 3 ~ "SPD-Stammwähler",
             partei_5 %in% levels(partei_5)[c(1,3:16,18,19)] & W6_2_w6 == 3 ~ "zur SPD gewechselt",
             W6_2_w6 %in% c(1,2, 4:14) ~ "Nicht-SPD-Wähler"
           )) %>%
  mutate_at(vars(W6_5_1_w6:W6_5_8_w6), as_factor) %>%
  pivot_longer(cols=c(W6_5_1_w6:W6_5_8_w6), names_to="Variable", values_to="Antwort") %>%
  group_by(spd, Variable, Antwort) %>%
  summarise(n = sum(Faktor_w123456[Antwort != "Weiß nicht"])) %>%
  mutate(p = round(n/sum(n)*100, 1)) %>%
  summarise(p = sum(p[str_detect(Antwort, "großes")])) %>%
  pivot_wider(names_from=Variable, values_from=p),
"Output/SPD_Institutionen.csv")

# Transformation / Klimawandel
write.csv2(
  dat %>%
    mutate(spd = 
             case_when(
               partei_5 == "SPD" & W6_2_w6 == 3 ~ "SPD-Stammwähler",
               partei_5 %in% levels(partei_5)[c(1,3:16,18,19)] & W6_2_w6 == 3 ~ "zur SPD gewechselt",
               W6_2_w6 %in% c(1,2, 4:14) ~ "Nicht-SPD-Wähler"
             )) %>%
    mutate_at(vars(W6_7_1_w6:W6_7_4_w6), as_factor) %>%
    pivot_longer(cols=c(W6_7_1_w6:W6_7_4_w6), names_to="Variable", values_to="Antwort") %>%
    group_by(spd, Variable, Antwort) %>%
    summarise(n = sum(Faktor_w123456[Antwort != "Weiß nicht"])) %>%
    mutate(p = round(n/sum(n)*100, 1)) %>%
    summarise(p = sum(p[str_detect(Antwort, "eher zu|voll und g")])) %>%
    pivot_wider(names_from=Variable, values_from=p),
  "Output/SPD_Klimawandel.csv")


Ergebnis <- list()

wb <- createWorkbook()

for(b in names(dat)[
  str_detect(names(dat), "^W6_[3-8]")]) {
 

# numerisch
  if(length(attributes(dat[[b]])$labels) > 6) {
    
    Ergebnis[[b]]$table <- 
      dat %>%
      filter(W6_2_w6 %in% 1:7 & !is.na(get(b))) %>%
      mutate(spd = 
                      case_when(
                        partei_5 == "SPD" & W6_2_w6 == 3 ~ "SPD-Stammwähler",
                        partei_5 %in% levels(partei_5)[c(1,3:16,18,19)] & W6_2_w6 == 3 ~ "zur SPD gewechselt",
                        W6_2_w6 %in% c(1,2, 4:14) ~ "Nicht-SPD-Wähler"
                      ),
             var=as_factor(case_when(
               get(b) %in% 1:2 ~ "hohe Wichtigkeit",
               get(b) %in% 3:5 ~ "mittlere Wichtigkeit", 
               get(b) %in% 6:7 ~ "geringe Wichtigkeit",
               get(b) == 99 ~ "Keine Angabe"))) %>%
      filter(!is.na(spd)) %>%
      group_by(spd, var) %>%
      summarise(n = sum(Faktor_w123456)) %>%
      mutate(p=round(n/sum(n)*100)) %>%
      pivot_wider(names_from=var, values_from=p, id_cols=spd) %>%
      select(spd, "hohe Wichtigkeit",
             "mittlere Wichtigkeit", 
              "geringe Wichtigkeit", "Keine Angabe")
       
    addWorksheet(wb, b)
    writeData(wb, b, paste0(b, ": ", attributes(dat[[b]])$label))
    writeData(wb, b, t(attributes(dat[[b]])$labels), startRow=3)
    
    writeData(wb, b, Ergebnis[[b]]$table, startRow=6)
  }
  
  
  
}

saveWorkbook(wb, paste0("Output/", Sys.Date(), "/", Sys.Date(), 
                        "_SPD_politikpräferenzen_kat.xlsx"), overwrite=T)
