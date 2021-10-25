write.csv2(
  dat5 %>%
  mutate(impfung = case_when(
    W5_C14__5 == 1 ~ "Vollständig geimpft",
    W5_C14__5 == 2 ~ "Einmal geimpft",
    W5_C14__5 == 3 ~ "Noch nicht geimpft, aber mit Impftermin",
    W5_C14__5 == 4 & W5_C15__5 == 1 ~ "Umgeimpfte: Ja, werde mich auf jeden Fall noch impfen.",
    W5_C14__5 == 4 & W5_C15__5 == 2 ~ "Umgeimpfte: Ja, werde mich eher noch impfen.",
    W5_C14__5 == 4 & W5_C15__5 == 3 ~ "Umgeimpfte: Werde mich eher nicht impfen.",
    W5_C14__5 == 4 & W5_C15__5 == 4 ~ "Umgeimpfte: Nein, werde mich auf keinen Fall noch impfen.",
    W5_C14__5 == 99 | W5_C15__5 == 99 ~ "Keine Angabe")
  ) %>%
  filter(S1__5 >= 18 & S1__5 <= 59) %>%
  dplyr_relfreq(impfung, Faktor__5),
  "Output/impfquoten.csv"
)


