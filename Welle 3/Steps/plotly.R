d07 <- read_dta("./Input/wsi_br befragung 2007.dta") %>%
  mutate(sa = c11a_1, sp=besch) %>%
  select(sa, sp) %>%
  filter(sa < 999995 & sp < 999995) %>%
  mutate(jahr = "2007 Hauptbefragung")
d21 <- read_dta("./Input/2020-11-24_Pretest_BR_PR_Stichprobeninfo.dta") %>%
  filter(cluster == "1_Privatwirtschaft") %>%
  mutate(sa = besch_n, sp=mitanz) %>%
  select(sa, sp) %>%
  filter(sa > 0 & sp > 0) %>%
  mutate(jahr = "2021 Pretest")
d07s <- d07[sample(1:nrow(d07), nrow(d21)),]
dat <- rbind(d07s, d21)


fig <- dat %>%
  plot_ly() %>%
  add_trace(x=~sa, y=~sp, color = ~jahr, colors="Set1", 
            type="scatter", mode="markers") %>%
  layout(xaxis=list(title="Größe Selbstauskunft", range=c(0,10000)),
         yaxis=list(title="Stichprobe", range=c(0,10000)))
fig

htmlwidgets::saveWidget(
  frameableWidget(fig),
  file = paste0(getwd(), "/Output/GK.html")
)