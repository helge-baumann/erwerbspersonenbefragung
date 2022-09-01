# Analyse Längsschnitt 5. Welle

Ergebnis <- list()
vars <- names(dat_long)[str_detect(names(dat_long), "_w[:digit:]{0,4}5$")]
vars <- vars[vars != "Faktor_w12345"]

as_factor_date <- function(x) {
  
  if(class(x)[1] == "Date") return(x) else return(as_factor(x))
  
}

dat_long <- dat_long %>%
  mutate(stratum_Alle = "Gesamt",
         stratum_Alter = 
           cut(S1_w12345, 
                        breaks=c(0, 30, 45, 60, 100), 
                        labels=c("1. bis 30", "2. 31 bis 45", "3. 46 bis 60", "4. 61 und älter")),
         stratum_Geschlecht = if_else(S2_w12345 == 1, "1. Männlich oder Divers", "2. Weiblich")#,
         #stratum_Bildung = as_factor(S3_w12345)
         )
levels(dat_long$stratum_Bildung) <- paste0("", 1:5, ". ", levels(dat_long$stratum_Bildung))
attributes(dat_long$stratum_Alle)$label <- "Alle"
attributes(dat_long$stratum_Alter)$label <- "Alter"
attributes(dat_long$stratum_Geschlecht)$label <- "Geschlecht"
#attributes(dat_long$stratum_Bildung)$label <- "Bildung"

for(b in vars) {
  
  if(length(unique(dat_long[[b]])) < 20) {
  Ergebnis[[b]]$table <- dat_long %>%
    select(starts_with("stratum"), b, Faktor_w12345, Welle) %>%
    pivot_longer(cols=-c(b, "Faktor_w12345", "Welle"), names_to="Variable", values_to="Wert") %>%
    filter(!is.na(get(b))) %>%
    mutate(!!b := as_factor_date(get(b))) %>%
    group_by(Variable, Wert, Welle, get(b)) %>%
    summarise(anteil = sum(Faktor_w12345), n=n()) %>%
    mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
    rename(!!b := `get(b)`) %>%
    mutate(Variable = str_remove_all(Variable, "stratum_"))
  
  Ergebnis[[b]]$label <- attributes(dat_long[[b]])$label
  Ergebnis[[b]]$name <- str_remove(b, "_w[:digit:]{1,5}$")
  
  if(str_detect(b, "_w[1][:digit:]{0,4}")) {
  name <- which(abgleich[, "n1"] == Ergebnis[[b]]$name)
  }
  if(str_detect(b, "_w[2][:digit:]{0,4}")) {
    name <- which(abgleich[, "n2"] == Ergebnis[[b]]$name)
  }
  if(str_detect(b, "_w[3][:digit:]{0,4}")) {
    name <- which(abgleich[, "n3"] == Ergebnis[[b]]$name)
  }
  if(str_detect(b, "_w[4][:digit:]{0,4}")) {
    name <- which(abgleich[, "n4"] == Ergebnis[[b]]$name)
  }
  if(str_detect(b, "_w[5][:digit:]{0,4}")) {
    name <- which(abgleich[, "n5"] == Ergebnis[[b]]$name)
  }
  Ergebnis[[b]]$namen <- paste0(abgleich[name, paste0("n", 1:5)], " (Welle ", 1:5, ")")
  Ergebnis[[b]]$namen <- Ergebnis[[b]]$namen[!str_detect(Ergebnis[[b]]$namen, "NA") ]
  Ergebnis[[b]]$namen <- paste(Ergebnis[[b]]$namen, collapse=", ")
  
  }
  
  print(b)
  
}

saveRDS(Ergebnis, "./Output/Ergebnisse_Laengsschnitt.rds")
