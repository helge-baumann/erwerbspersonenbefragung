Ergebnis <- 
  dat %>%
  mutate(
    gesamt = "gesamt", 
    westost = case_when(bundesland %in% levels(bundesland)[1:10] ~ "West", 
                             bundesland %in% levels(bundesland)[11:16] ~ "Ost")) %>%
  mutate(spd = 
           case_when(
             partei_5 == "SPD" & zweitstimme == 3 ~ "SPD-Stammwähler",
             partei_5 %in% levels(partei_5)[c(1,3:16,18,19)] & zweitstimme == 3 ~ "zur SPD gewechselt",
             zweitstimme %in% c(1,2, 4:14) ~ "Nicht-SPD-Wähler"
           )) %>% 
  filter(!is.na(spd)) %>%
  pivot_longer(cols = c(gesamt, spd), names_to = "Variable1", values_to = "Wert1") %>%
  mutate(gesamt = "gesamt") %>%
  pivot_longer(cols=c(gesamt, westost, alter, bildung, geschlecht), names_to="Variable2", values_to="Wert2") %>%
  pivot_longer(cols=c(W8_U3_1_w8:W8_U3_5_w8), names_to="Variable3", values_to="Wert3") %>%
  filter(Wert3 %in% 1:4) %>%
  group_by(Variable1, Wert1, Variable2, Wert2, Variable3, Wert3) %>%
  summarise(n = sum(Faktor_w12345678), N=n()) %>%
  mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
  pivot_wider(names_from=Wert3, values_from=p, id_cols=c(Variable1, Wert1, Variable2, Wert2, Variable3, N)) %>%
  mutate(
    Variable3 = sapply(Variable3, function(x) attributes(dat[[x]])$label),
    Variable3 = str_remove_all(Variable3, "W8_U3.[:digit:] Auswirkungen des Krieges auf Verteilung und soziale Gerechtigkeit: ")) 

wb <- createWorkbook()

addWorksheet(wb, "SPD gesamt")
writeDataTable(wb, "SPD gesamt", Ergebnis[Ergebnis$Variable1 == "spd" & Ergebnis$Variable2 == "gesamt", ], startRow=1)

addWorksheet(wb, "Gesamt")
writeDataTable(wb, b, Ergebnis[Ergebnis$Variable1 == "gesamt" & Ergebnis$Variable2 == "gesamt", ], startRow=1)

addWorksheet(wb, "SPD differenziert")
writeDataTable(wb, b, Ergebnis[Ergebnis$Variable1 == "spd" & Ergebnis$Variable2 != "gesamt", ], startRow=1)

addWorksheet(wb, "Gesamt differenziert")
writeDataTable(wb, b, Ergebnis[Ergebnis$Variable1 == "gesamt" & Ergebnis$Variable2 != "gesamt", ], startRow=1)

saveWorkbook(wb, paste0("Output/", Sys.Date(), "/", Sys.Date(), "_", "SPD-Kohlrausch.xlsx"), overwrite=T)

             