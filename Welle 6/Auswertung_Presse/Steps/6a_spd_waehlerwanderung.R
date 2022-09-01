# Interessant ist ja auch ein Abgleich der Parteipräferenz im Juni und November.
# Hier Könnte man insbesondere die Gruppe, die in diesem Zeitraum  noch zur 
# SPD gewechselt ist, näher untersuchen. 
# Was sind deren Politikpräferenzen und Sorgen? 
# Da könnte man vielleicht noch einen Punkt in der KOA-Verhandlungen draus machen.

# Untersuchung drei Gruppen: CDU --> SPD, Grüne/Linke --> SPD, SPD --> SPD (Stamm)
SPD_Personen <- 
  dat %>%
  mutate(spd = 
           case_when(
             partei_5 == "SPD" & W6_2_w6 == 3 ~ "SPD-Stammwähler",
             partei_5 %in% levels(partei_5)[c(1,3:16,18,19)] & W6_2_w6 == 3 ~ "zur SPD gewechselt",
             W6_2_w6 %in% c(1,2, 4:14) ~ "Nicht-SPD-Wähler"
           )) %>% 
  filter(!is.na(spd)) %>%
  pivot_longer(cols=c(gewerkschaft:erwerbsstatus, zufriedenheit_regierung:alter), 
               names_to="Variable", values_to="Wert") %>%
  filter(!is.na(Wert)) %>%
  group_by(spd, Variable, Wert) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)) %>%
  ungroup() %>%
  pivot_wider(names_from = spd, values_from = p, 
              id_cols=c(Variable, Wert)) %>%
  filter(!is.na(Wert))

# speichern
wb <- createWorkbook()

addWorksheet(wb, "SPD")
writeData(wb, "SPD", "Achtung: Alle nicht im Bundestag vertretenen Parteien wurden ausgeklammert, 
          Prozente sind bezogen auf gültige Zweitstimmen!")
writeDataTable(wb, "SPD", SPD_Personen, startRow=3)
saveWorkbook(wb, paste0("Output/", Sys.Date(), "/", Sys.Date(), 
                        "_SPD_Wählerwanderung.xlsx"), overwrite=T)
  

  