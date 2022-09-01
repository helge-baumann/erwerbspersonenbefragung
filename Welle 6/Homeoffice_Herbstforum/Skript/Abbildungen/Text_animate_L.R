
setwd("W:/Baumann/Dissertation/Backup/Backup_komplett_2018-03-06/Promotion/Datenanalyse")

load("WS_2018-02-05.RData")

# DGB

# schichtbeginn dritteln
cont_dgb$schichtbeginn <- cont_dgb$schichtbeginn/60
library(Hmisc)
cont_dgb$int_tag <- cut2(cont_dgb$schichtbeginn, cuts= c(0, 1))
cont_dgb$int_tag <- factor(cont_dgb$int_tag, 
                           labels=c("weniger als eine Stunde", 
                                    "mehr als eine Stunde")
)

x <- ksmooth(cont_dgb$hour[cont_dgb$int_tag == "weniger als eine Stunde"],
              cont_dgb$R[cont_dgb$int_tag == "weniger als eine Stunde"],
              "normal",
              bandwidth=3, 
              range.x=c(8,20)
)

y <- ksmooth(cont_dgb$hour[cont_dgb$int_tag == "mehr als eine Stunde"],
              cont_dgb$R[cont_dgb$int_tag == "mehr als eine Stunde"],
              "normal",
              bandwidth=3,
              range.x=c(9,20)
)

x <- data.frame(x); x <- x[seq(1, nrow(x), length.out=40),]
y <- data.frame(y); y <- y[seq(1, nrow(y), length.out=40),]
x[,1] <- (x[,1]-8)/2; x[,2] <- x[,2] * 10
y[,1] <- (y[,1]-8)/2; y[,2] <- y[,2] * 10

###

wl1 <- NA
wl2 <- NA

#num <- 0


for(i in 1:nrow(x)) {

  if(i == 1) {
    wl1[i] <- paste0(
      "(",
      x[i,1],
      ",",
      x[i,2],
      ")" 
    )
    wl2[i] <- paste0(
      "(",
      y[i,1],
      ",",
      y[i,2],
      ")" 
    )
  } else {
    wl1[i] <- paste0(
      wl1[i-1], 
      " -- ",
      "(",
      x[i-1,1],
      ",",
      x[i-1,2],
      ") -- (",
      x[i,1],
      ",",
      x[i,2],
      ") "
    )
    wl2[i] <- paste0(
      wl2[i-1], 
      " -- ",
      "(",
      y[i-1,1],
      ",",
      y[i-1,2],
      ") -- (",
      y[i,1],
      ",",
      y[i,2],
      ") "
    )
    
  }
  
}

wl1_tex <- NA
wl2_tex <- NA

for(i in 1:length(wl1)) {
  
  
  wl1_tex[i] <- paste0(
    "\\draw[-, blue]",
    wl1[i],
    "; \\newframe"
  )
  
  if(i == length(wl2)) {
    
    wl2_tex[1] <- paste0(
      "\\draw[-, dotted]",
      wl2[i],
      "; \\newframe*"
    )
  }
  
}

writeLines(wl1_tex, con=
             "C:/Users/Helge-Baumann/Desktop/Disputation/Präsentationen/Disputation/Vortrag_Disputation/Abbildungen/WL1.txt")
writeLines(wl2_tex, con=
             "C:/Users/Helge-Baumann/Desktop/Disputation/Präsentationen/Disputation/Vortrag_Disputation/Abbildungen/WL2.txt")
