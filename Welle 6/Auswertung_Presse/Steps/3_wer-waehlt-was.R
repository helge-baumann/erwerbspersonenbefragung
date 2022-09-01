# Welche Erfahrungen während der Pandemie Einkommensverluste etc haben 
# Menschen gemacht und wie steht das mit ihren Partei und Politikpräferenzen 
# in Zusammenhang?

# Achtung: "Ungültig" und "keine Angabe" ausgeklammert!
Erststimme <- 
 dat %>%
  filter(!is.na(W6_1_w6) & W6_1_w6 < 96) %>%
  pivot_longer(cols=`(1) gesamt`:alter, names_to="Variable", values_to="Wert") %>%
  mutate(`(1) gesamt`="") %>%
  pivot_longer(
    c(`(1) gesamt`, einkommensverlust_hh, gewerkschaft), 
    names_to="Variable2", values_to="Wert2") %>%
  group_by(Variable, Wert, Variable2, Wert2, as_factor(W6_1_w6)) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)) %>%
  pivot_wider(names_from = `as_factor(W6_1_w6)`, values_from = p, 
              id_cols=c(Variable, Wert, Variable2, Wert2, N)) %>%
  filter(!is.na(Wert) &!is.na(Wert2) & Wert2 != "Keine Angabe")

Zweitstimme <- 
  dat %>%
  filter(!is.na(W6_2_w6) & W6_2_w6 < 96) %>%
  pivot_longer(cols=`(1) gesamt`:alter, names_to="Variable", values_to="Wert") %>%
  mutate(`(1) gesamt`="") %>%
  pivot_longer(
    c(`(1) gesamt`, einkommensverlust_hh, gewerkschaft), 
    names_to="Variable2", values_to="Wert2") %>%
  group_by(Variable, Wert, Variable2, Wert2, as_factor(W6_2_w6)) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)) %>%
  pivot_wider(names_from = `as_factor(W6_2_w6)`, values_from = p, 
              id_cols=c(Variable, Wert, Variable2, Wert2, N)) %>%
  filter(!is.na(Wert) &!is.na(Wert2) & Wert2 != "Keine Angabe")

# speichern
wb <- createWorkbook()
addWorksheet(wb, "Erststimme")
writeData(wb, "Erststimme", "Achtung: 'Ungültige' und 'keine Angabe' wurden ausgeklammert, 
          Prozente sind bezogen auf gültige Stimmen!")
writeDataTable(wb, "Erststimme", Erststimme, startRow=3)
addWorksheet(wb, "Zweitstimme")
writeData(wb, "Zweitstimme", "Achtung: 'Ungültige' und 'keine Angabe' wurden ausgeklammert, 
          Prozente sind bezogen auf gültige Stimmen!")
writeDataTable(wb, "Zweitstimme", Zweitstimme, startRow=3)
saveWorkbook(wb, paste0("Output/", Sys.Date(), "/", Sys.Date(), 
                        "_Wer-wählt-was.xlsx"), overwrite=T)

