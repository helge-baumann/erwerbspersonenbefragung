# Struktur der AFD-Wähler.# Wer sind die AFD WählerInnen und vor allem auch die Gewerkschaftsmitglieder 
# unter den AfD Wähler*innen (das ist vielleicht eher was für den Vortrag)? 
# Bei der AfD Wahl ist das Institutionenvertrauen besonders interessant.

AFD <- 
  dat %>%
  mutate(afd = 
           case_when(
             W6_2_w6 == 7 ~ "AfD", 
             W6_2_w6 %in% 1:6 ~ "andere Partei im Bundestag"
           )) %>%
  filter(!is.na(afd)) %>%
  pivot_longer(cols=`(1) gesamt`:alter, names_to="Variable", values_to="Wert") %>%
  mutate(`(1) gesamt`="") %>%
  pivot_longer(
    c(`(1) gesamt`, einkommensverlust_hh, gewerkschaft), 
    names_to="Variable2", values_to="Wert2") %>%
  group_by(Variable, Wert, Variable2, Wert2, afd) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)) %>%
  pivot_wider(names_from = afd, values_from = p, 
              id_cols=c(Variable, Wert, Variable2, Wert2, N)) %>%
  filter(!is.na(Wert) & !is.na(Wert2) & Wert2 != "Keine Angabe")

# speichern
wb <- createWorkbook()

addWorksheet(wb, "AfD")
writeData(wb, "AfD", "Achtung: Alle nicht im Bundestag vertretenen Parteien wurden ausgeklammert, 
          Prozente sind bezogen auf gültige Zweitstimmen!")
writeDataTable(wb, "AfD", AFD, startRow=3)
saveWorkbook(wb, paste0("Output/", Sys.Date(), "/", Sys.Date(), 
                        "_AfD.xlsx"), overwrite=T)