
# Themen
dat %>%
  filter(Welle == 6 & W6_2_w6 %in% 1:7) %>%
  mutate(gesamt = "gesamt", partei=as_factor(W6_2_w6)) %>%
  pivot_longer(cols=c(gesamt, partei), names_to="Variable", values_to="Wert") %>%
  pivot_longer(cols=c(W6_4a_1_w6:W6_4g_3_w6), names_to="Variable2", values_to="Wert2") %>%
  filter(Wert2 %in% 1:7) %>%
  mutate(
    Wert2 = case_when(
    Wert2 %in% 1:2 ~ "wichtig", 
    Wert2 %in% 3:5 ~ "neutral", 
    Wert2 %in% 6:7 ~ "unwichtig")) %>%
  group_by(Variable, Wert, Variable2, Wert2) %>%
  summarise(n = sum(Faktor_w123456), N=n()) %>%
  mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
  pivot_wider(names_from=Wert2, values_from=p, id_cols=c(Wert, Variable2, N)) %>%
  select(Wert, Variable2, N, wichtig, neutral, unwichtig) %>%
  mutate(
    Variable2 = sapply(Variable2, function(x) attributes(dat[[x]])$label),
    Variable2 = str_remove_all(Variable2, "W6\\_4[:lower:] ")) %>%
  write.csv2("Output/themen.csv", row.names=F)

# Plot
  dat %>%
    filter(Welle == 6 & W6_2_w6 %in% 1:7) %>%
    mutate(gesamt = "gesamt", partei=as_factor(W6_2_w6)) %>%
    pivot_longer(cols=c(gesamt, partei), names_to="Variable", values_to="Wert") %>%
    pivot_longer(cols=c(W6_4a_1_w6:W6_4g_3_w6), names_to="Variable2", values_to="Wert2") %>%
    filter(Wert2 %in% 1:7) %>%
    mutate(
      Wert2 = case_when(
        Wert2 %in% 1:2 ~ "wichtig", 
        Wert2 %in% 3:5 ~ "neutral", 
        Wert2 %in% 6:7 ~ "unwichtig")) %>%
    group_by(Variable, Wert, Variable2, Wert2) %>%
    summarise(n = sum(Faktor_w123456), N=n()) %>%
    mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
    mutate(
      Variable2 = sapply(Variable2, function(x) attributes(dat[[x]])$label),
      Variable2 = str_remove_all(Variable2, "W6\\_4[:lower:] ")) %>%
    filter(!is.na(Wert)) %>%
    ggplot(aes(x=Wert, y=p, fill=Wert2)) +
    geom_col() +
    facet_wrap(~Variable2) + coord_flip()
  
 # Institutionen
  dat %>%
    filter(Welle == 6) %>%
    mutate(gesamt = "gesamt", partei=as_factor(W6_2_w6)) %>%
    pivot_longer(cols=c(gesamt, partei), names_to="Variable", values_to="Wert") %>%
    pivot_longer(cols=c(W6_5_1_w6:W6_5_8_w6), names_to="Variable2", values_to="Wert2") %>%
    filter(Wert2 %in% 1:7) %>%
    mutate(
      Wert2 = case_when(
        Wert2 %in% 1:2 ~ "wenig oder kein Vertrauen", 
        Wert2 %in% 3 ~ "mittelmäßiges Vertrauen", 
        Wert2 %in% 4:55 ~ "großes oder sehr großes Vertrauen")) %>%
    group_by(Variable, Wert, Variable2, Wert2) %>%
    summarise(n = sum(Faktor_w123456), N=n()) %>%
    mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
    pivot_wider(names_from=Wert2, values_from=p, id_cols=c(Wert, Variable2, N)) %>%
    select(Wert, Variable2, N, `großes oder sehr großes Vertrauen`, `mittelmäßiges Vertrauen`, `wenig oder kein Vertrauen`) %>%
    mutate(
      Variable2 = sapply(Variable2, function(x) attributes(dat[[x]])$label),
      Variable2 = str_remove_all(Variable2, "W6\\_5 Institutionenvertrauen: ...")) %>%
    write.csv2("Output/institutionen.csv", row.names=F)
  
  # Plot
  dat %>%
    filter(Welle == 6 & W6_2_w6 %in% 1:7) %>%
    mutate(gesamt = "gesamt", partei=as_factor(W6_2_w6)) %>%
    pivot_longer(cols=c(gesamt, partei), names_to="Variable", values_to="Wert") %>%
    pivot_longer(cols=c(W6_5_1_w6:W6_5_8_w6), names_to="Variable2", values_to="Wert2") %>%
    filter(Wert2 %in% 1:7) %>%
    mutate(
      Wert2 = case_when(
        Wert2 %in% 1:2 ~ "wenig oder kein Vertrauen", 
        Wert2 %in% 3 ~ "mittelmäßiges Vertrauen", 
        Wert2 %in% 4:55 ~ "großes oder sehr großes Vertrauen")) %>%
    group_by(Variable, Wert, Variable2, Wert2) %>%
    summarise(n = sum(Faktor_w123456), N=n()) %>%
    mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
    mutate(
      Variable2 = sapply(Variable2, function(x) attributes(dat[[x]])$label),
      Variable2 = str_remove_all(Variable2, "W6\\_5 Institutionenvertrauen: ...")) %>%
    ggplot(aes(x=Wert, y=p, fill=Wert2)) +
    geom_col() +
    facet_wrap(~Variable2) + coord_flip()
  