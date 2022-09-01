##############################################################
# Titel: Arbeitszeiten
# R-Version: 4.1.0
# Autor: Helge Emmler
# Letztes Update: 20.09.2021
##############################################################


# Präambel----

# Pakete installieren
if (!("pacman" %in% installed.packages()[, 1])) install.packages("pacman")
library(pacman)

# p_load checkt, ob Paket installiert ist; wenn ja: direkt laden
p_load(haven, dplyr, tidyr, tibble, here, openxlsx, stringr, Hmisc)

# here() setzt das Arbeitsverzeichnis an die Stelle des master-files.
# (Alternative: Projekt anlegen unter File --> New Project)
setwd(here())

# Datenanalyse----
# Daten einlesen
dat <- read_dta("Erwerbspersonenbefragung_Wellen1-5_long_v1-2.dta")

# Arbeitszeiten

# Schichtungsvariablen (bitte ergänzen oder vorher generieren mit Wellensuffix)
dat <- dat %>%
  group_by(lfdn_w1__1) %>%
  mutate(
    az_vor = A1co_w2[2],
    alle = "gesamt",
    sex = as_factor(S2_w12345),
    bildung = as_factor(S3_w12345),
    kinder = as_factor(F22_w1235),
    partner = as_factor(F23_w1[1]), # Achtung: Angabe aus Welle 1
    taetigkeit = as_factor(S6_w123[1]), # Achtung: Angabe aus Welle 1
    alter = cut(S1_w12345,
      breaks = c(0, 35, 50, 100),
      labels = c("(1) bis 35", "(2) 36 bis 50", "(3) 51 und älter")
    )
  )

# Arbeitszeitvariablen extrahieren (A1co / A1d)
az <- names(dat)[
  str_detect(names(dat), "^A1d|^A1co") & !str_detect(names(dat), "99") | 
    names(dat) == "az_vor"
]

# Nach welchen Variablen soll geschichtet werden
strata <- c("alle", "sex", "bildung", "kinder", "partner", "taetigkeit", "alter")

# Gewicht Welle 5 anpassen
dat <- dat %>% mutate(gewicht = if_else(Welle == 5, Faktor2_w5, Faktor_w12345))
dat[, az][dat[,az] > 72] <- NA
dat$az_vor[dat$az_vor > 72 | dat$az_vor == 0] <- NA

# Mittelwerte der Arbeitszeiten auswerten
Ergebnis <- dat %>%
  ungroup() %>%
  select(starts_with(az), starts_with(strata), gewicht) %>%
  pivot_longer(cols = -c(az, gewicht), names_to = "Variable", values_to = "Wert") %>%
  group_by(Variable, Wert) %>%
  summarise_at(
    vars(starts_with(az)),
    funs(A = wtd.mean(az_vor - ., w = gewicht, na.rm = T),
         P = wtd.mean((.-az_vor)/az_vor, w = gewicht, na.rm = T))
  ) %>%
  filter(!is.na(Wert)) %>%
  select(!contains("vor"))

# Spalten in Daten überführen
names(Ergebnis)[3:ncol(Ergebnis)] <-
  c(paste(
    months(seq(as.Date("2020/02/01"), by = "month", length.out = ncol(Ergebnis)/2 - 1)),
    format(seq(as.Date("2020/02/01"), by = "month", length.out = ncol(Ergebnis)/2 - 1),
      format = "%Y"
    )),
    paste(
      months(seq(as.Date("2020/02/01"), by = "month", length.out = ncol(Ergebnis)/2 - 1)),
      format(seq(as.Date("2020/02/01"), by = "month", length.out = ncol(Ergebnis)/2 - 1),
             format = "%Y"
      ))
  )

# abspeichern (Excel)----
wb <- createWorkbook()
addWorksheet(wb, "Arbeitszeiten")
writeData(wb, "Arbeitszeiten", Ergebnis)

saveWorkbook(wb, "Ergebnis.xlsx", overwrite = T)
