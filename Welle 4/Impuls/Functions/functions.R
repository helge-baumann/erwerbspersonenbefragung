dplyr_relfreq <- function(.data, x, gew, round=T) {
  
  erg <- .data %>%
    filter(!is.na({{x}})) %>%
    mutate(x = as_factor({{x}})) %>%
    group_by(x) %>%
    summarise(anteil = sum({{gew}}), n=n()) %>%
    mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) 
  
  if(round) erg <- erg %>% mutate(anteil=round(anteil))
  
  return(erg)
}

dplyr_relfreq2 <- function(.data, x, y, gew, round=T) {
    
  erg <- .data %>%
    filter(!is.na({{x}}) & !is.na({{y}})) %>%
    mutate(x = as_factor({{x}}), y = as_factor({{y}})) %>%
    group_by(x, y) %>%
    summarise(anteil = sum({{gew}}), n=n()) %>%
    mutate(anteil=(anteil/sum(anteil)*100), n=sum(n))
  
  if(round) erg <- erg %>% mutate(anteil=round(anteil))

  return(erg)
  
}

dplyr_relfreq3 <- function(.data, x, y, z, gew, round=T) {
  
  erg <- .data %>%
    filter(!is.na({{x}}) & !is.na({{y}}) & !is.na({{z}})) %>%
    mutate(x = as_factor({{x}}), y = as_factor({{y}}), z = as_factor({{z}})) %>%
    group_by(x, y, z) %>%
    summarise(anteil = sum({{gew}}), n=n()) %>%
    mutate(anteil=(anteil/sum(anteil)*100), n=sum(n))
  
  if(round) erg <- erg %>% mutate(anteil=round(anteil))
  
  return(erg)
  
}

# Homeoffice während der Krise

ho_extract <- function(x, kat=NULL) {
  
  lab_ho_v <- "Ich habe ausschließlich / überwiegend von zu Hause aus gearbeitet."
  lab_ho_j <- "Ich arbeite ausschließlich / überwiegend von zu Hause aus."
  
  erg <- data.frame(
    vor = dat_ges %>%
      dplyr_relfreq2({{x}}, F3__1, Faktor__1) %>% 
      filter(x %in% kat, y == lab_ho_v) %>%
      pull(anteil),
    apr = dat_ges %>% 
      dplyr_relfreq2({{x}}, F2__1, Faktor__1) %>% 
      filter(x %in% kat, y == lab_ho_j) %>%
      pull(anteil),
    jun = dat_ges %>% 
      dplyr_relfreq2({{x}}, A2__2, Faktor__2) %>% 
      filter(x %in% kat, y == lab_ho_j) %>%
      pull(anteil),
    nov = dat_ges %>% 
      dplyr_relfreq2({{x}}, A2__3, Faktor__3) %>% 
      filter(x %in% kat, y == lab_ho_j) %>%
      pull(anteil),
    dec = dat_ges %>% 
      dplyr_relfreq2({{x}}, W4_1__4, Faktor__4) %>% 
      filter(x %in% kat, y == lab_ho_v) %>%
      pull(anteil),
    jan = dat_ges %>% 
      dplyr_relfreq2({{x}}, A2__4, Faktor__4) %>% 
      filter(x %in% kat, y == lab_ho_j) %>%
      pull(anteil)
  )
  
  row.names(erg) <- kat
  names(erg) <- c("Januar 2020", "April 2020", "Juni 2020",
                  "November 2020", "Dezember 2020", "Januar 2021")
  
  return(erg)
  
}

