 
Ergebnis <- list()

# Homeoffice

b <- "F2_w1234"
Ergebnis[["F2_w1234"]]$table <- 
  dat %>%
  select(starts_with("stratum"), b, Faktor_w1234, Welle) %>%
  pivot_longer(cols=-c(b, "Faktor_w1234", "Welle"), names_to="Variable", values_to="Wert") %>%
  filter(!is.na(get(b))) %>%
  mutate(!!b := as_factor(get(b))) %>%
  group_by(Variable, Wert, Welle, get(b)) %>%
  summarise(anteil = sum(Faktor_w1234), n=n()) %>%
  mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
  rename(!!b := `get(b)`) %>%
  mutate(Variable = str_remove_all(Variable, "stratum_"))

Ergebnis[[b]]$label <- attributes(dat[[b]])$label
Ergebnis[[b]]$name <- str_remove(b, "_w[:digit:]{1,4}$")

# Belastung (als anstrengender als im Betrieb)
b <- "F21_6_w1234"
Ergebnis[["F21_6_w1234"]]$table <- 
  dat %>%
  group_by(lfdn_w1__1) %>% 
  mutate(durch = sum(F2_w1234 %in% c(2,3))) %>% 
  #select(lfdn_w1__1, durch) %>% 
  filter(durch == 4) %>%
  ungroup() %>%
  select(starts_with("stratum"), b, Faktor_w1234, Welle) %>%
  pivot_longer(cols=-c(b, "Faktor_w1234", "Welle"), names_to="Variable", values_to="Wert") %>%
  filter(!is.na(get(b)) & get(b) != 99) %>%
  mutate(!!b := as_factor(get(b))) %>%
  group_by(Variable, Wert, Welle, get(b)) %>%
  summarise(anteil = sum(Faktor_w1234), n=n()) %>%
  mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
  rename(!!b := `get(b)`) %>%
  mutate(Variable = str_remove_all(Variable, "stratum_"))

Ergebnis[[b]]$label <- attributes(dat[[b]])$label
Ergebnis[[b]]$name <- str_remove(b, "_w[:digit:]{1,4}$")

# Vereinbarkeit
b <- "A2c_10_w234"
Ergebnis[["A2c_10_w234"]]$table <- 
  dat %>%
  group_by(lfdn_w1__1) %>% 
  filter(Welle != 1) %>%
  mutate(durch = sum(F2_w1234 %in% c(2,3))) %>% 
  filter(durch == 3) %>%
  ungroup() %>%
  select(starts_with("stratum"), b, Faktor_w1234, Welle) %>%
  pivot_longer(cols=-c(b, "Faktor_w1234", "Welle"), names_to="Variable", values_to="Wert") %>%
  filter(!is.na(get(b)) & get(b) != 99) %>%
  mutate(!!b := as_factor(get(b))) %>%
  group_by(Variable, Wert, Welle, get(b)) %>%
  summarise(anteil = sum(Faktor_w1234), n=n()) %>%
  mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
  rename(!!b := `get(b)`) %>%
  mutate(Variable = str_remove_all(Variable, "stratum_"))

Ergebnis[[b]]$label <- attributes(dat[[b]])$label
Ergebnis[[b]]$name <- str_remove(b, "_w[:digit:]{1,4}$")

# mehr im Homeoffice
b <- "A2d_w234"
Ergebnis[[b]]$table <- 
  dat %>%
  group_by(lfdn_w1__1) %>% 
  filter(Welle != 1) %>%
  mutate(durch = sum(F2_w1234 %in% c(2,3))) %>% 
  filter(durch == 3) %>%
  ungroup() %>%
  select(starts_with("stratum"), b, Faktor_w1234, Welle) %>%
  pivot_longer(cols=-c(b, "Faktor_w1234", "Welle"), names_to="Variable", values_to="Wert") %>%
  filter(!is.na(get(b)) & get(b) != 99) %>%
  mutate(!!b := as_factor(get(b))) %>%
  group_by(Variable, Wert, Welle, get(b)) %>%
  summarise(anteil = sum(Faktor_w1234), n=n()) %>%
  mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
  rename(!!b := `get(b)`) %>%
  mutate(Variable = str_remove_all(Variable, "stratum_"))

Ergebnis[[b]]$label <- attributes(dat[[b]])$label
Ergebnis[[b]]$name <- str_remove(b, "_w[:digit:]{1,4}$")

# Potenzial
b <- "W4_2_w4"
Ergebnis[[b]]$table <- 
  dat %>%
  filter(Welle == 4) %>%
  select(starts_with("stratum"), b, Faktor_w1234, Welle) %>%
  pivot_longer(cols=-c(b, "Faktor_w1234", "Welle"), names_to="Variable", values_to="Wert") %>%
  filter(!is.na(get(b))) %>%
  mutate(!!b := as_factor(get(b))) %>%
  group_by(Variable, Wert, Welle, get(b)) %>%
  summarise(anteil = sum(Faktor_w1234), n=n()) %>%
  mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) %>%
  rename(!!b := `get(b)`) %>%
  mutate(Variable = str_remove_all(Variable, "stratum_"))

Ergebnis[[b]]$label <- attributes(dat[[b]])$label
Ergebnis[[b]]$name <- str_remove(b, "_w[:digit:]{1,4}$")


  