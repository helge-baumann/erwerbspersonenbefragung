# Werte für tikZ

load("E:/Promotion/Datenanalyse/WS_2018-02-05.RData")

cont_dgb$ksq.s <- scale(cont_dgb$ksq, center=T, scale=T)
ksq.synt <- NA
n <- 800
a <- 0

for(i in 1:n) {
  
  if(i %in% seq(50,800, by=50)) {
    a <- a + 0.2814365
    print(a)
  }
  
  diff1 <- 0.4221547+a
  diff <- diff1/(50)

  if(i == 1) { ksq.synt[i] <- min(cont_dgb$ksq.s[cont_dgb$fullsample.fn == 1], na.rm=T) 
  } else {
  ksq.synt[i] <- 
    ksq.synt[i-1]+
    (0.4221547+a)/50
  }
 
}

pdf("J:/Dissertation_20082015/Text/Dissertation/Kontakte_DGB_V.pdf", 
    family="CM Roman", width=7.2, height=5.5, pointsize=12
)
par(mfrow=c(1,1))
par(mar = c(3, 2, 2, 1)) # default c(5.1 4.1 4.1 2.1)
par(mgp=c(0.1,0.25,0)) # default 3 1 0
#par(xpd=NA)
par(bg="white")

coefs.v <- exp(fixef(m.all.3.v.dgb))

cont_dgb$ksq.s <- scale(cont_dgb$ksq, center=T, scale=T)

re.scale <- cbind(
  seq(from = min(cont_dgb$Kontakt[cont_dgb$fullsample.fn == 1], na.rm=T), 
      to = max(cont_dgb$Kontakt[cont_dgb$fullsample.fn == 1], na.rm=T),
      length.out=800),
  seq(from = min(cont_dgb$kont.s[cont_dgb$fullsample.fn == 1], na.rm=T), 
      to = max(cont_dgb$kont.s[cont_dgb$fullsample.fn == 1], na.rm=T),
      length.out=800),
  ksq.synt
)
#re.scale <- unique(cbind(cont_dgb$Kontakt, cont_dgb$kont.s)) # sollte f?r Plot verwendbar sein
re.scale <- re.scale[order(re.scale[,1]),]

# vage Termine
probs.kont <- coefs.v[1] *
  coefs.v[4]^re.scale[,2] *
  coefs.v[5]^re.scale[,3] *
  coefs.v[6]^1 *
  coefs.v[9]^mean(scale(cont_dgb$anznsk[cont_dgb$fullsample.fn ==1], center = T, scale = T)) *
  coefs.v[11]^mean(scale(cont_dgb$schichtbeginn[cont_dgb$fullsample.fn ==1], center = T, scale = T)) *
  coefs.v[12]^mean(scale(cont_dgb$workload[cont_dgb$fullsample.fn ==1], center = T, scale = T)) * 
  coefs.v[28]^mean(scale(cont_dgb$anznsk[cont_dgb$fullsample.fn ==1], center = T, scale = T))
probs.kont <- probs.kont/(1+probs.kont)

plot( re.scale[,1], probs.kont, ylim=c(0.5, 1),
      xlim=c(0,15),  bty="l", "n", 
      cex.axis=1, col.axis=gray(0.25), mgp=c(1.6,0.6,0), 
      tck=-0.01,main="", cex.main=1,
      xlab="Anzahl Sprachkontakte", ylab="", xaxt="n", cex.lab=0.9)
rect(-8, -8, 50, 150, col=gray(0.95), border=NA)
axis(1, at=seq(0, 40, 5), labels=F, tck=1, lwd=0, lwd.ticks=0.05, col.ticks="white")
axis(1, at=seq(0, 40, 10), labels=F, tck=1, lwd=0, lwd.ticks=1.5, col.ticks="white")

axis(1, at=seq(0, 40, 5), 
     labels=T, col.axis=gray(0.25), 
     tck=-0.0075, lwd=0.000, lwd.ticks=0.01, col.ticks="black", cex.axis=1)

#axis(2, at=seq(7.5, 9.5, 0.025), labels=F, tck=-0.005, lwd=0, lwd.ticks=0.5)
axis(2, at=seq(0.0, 150, 0.1), labels=F, tck=1, lwd=0, lwd.ticks=0.05, col.ticks="white")
axis(2, at=seq(0.0, 150, 0.2), labels=F, tck=1, lwd=0, lwd.ticks=1.5, col.ticks="white")
#mtext(side = 1, text = "Uhrzeit", line = 1.5, cex=0.7)

lines(re.scale[,1], probs.kont, ylim=c(0, 0.5), col="black")

re.scale <- cbind(re.scale[,1], probs.kont)

kont <- NA
num <- 0

kont_tex <- NA

for(i in seq(from=2, to= nrow(re.scale), by=20)) {
  
  num <- num + 1
  if(num == 1) {
  kont[num] <- paste0(
    "(",
    re.scale[i-1,1]/3,
    ",",
    re.scale[i-1,2]*4,
    ") -- (",
    re.scale[i,1]/3,
    ",",
    re.scale[i,2]*4,
    ") "
  )
  } else {
    kont[num] <- paste0(
      kont[num-1], 
      " -- ",
      "(",
      re.scale[i-1,1]/3,
      ",",
      re.scale[i-1,2]*4,
      ") -- (",
      re.scale[i,1]/3,
      ",",
      re.scale[i,2]*4,
      ") "
    )
    
  }
  
}
  
for(i in 1:length(kont)) {
  
kont_tex[i] <- paste0(
  "\\draw[->] (0,0) -- (6,0); \\draw[->] (0,0) -- (0,4); ",
  "\\draw[-, blue]",
  kont[i],
  "; \\newframe"
)

}

writeLines(kont_tex, con=
             "E:/Promotion/Präsentationen/Disputation/Vortrag_Disputation/Abbildungen/Kont_DGB.txt")


dev.off()

pdf("J:/Dissertation_20082015/Text/Dissertation/Kontakte_DGB_R.pdf", 
    family="CM Roman", width=7.2, height=5.5, pointsize=12
)
par(mfrow=c(1,1))
par(mar = c(3, 2, 2, 1)) # default c(5.1 4.1 4.1 2.1)
par(mgp=c(0.1,0.25,0)) # default 3 1 0
#par(xpd=NA)
par(bg="white")

coefs.r <- exp(fixef(m.all.3.r.dgb))

cont_dgb$kont.s <- scale(cont_dgb$Kontakt, center=T, scale=T)
cont_dgb$ksq.s <- scale(cont_dgb$ksq, center=T, scale=T)

#ksq.synt <- NA



re.scale <- cbind(
  seq(from = min(cont_dgb$Kontakt[cont_dgb$fullsample.fn == 1], na.rm=T), 
      to = max(cont_dgb$Kontakt[cont_dgb$fullsample.fn == 1], na.rm=T),
      length.out=800),
  seq(from = min(cont_dgb$kont.s[cont_dgb$fullsample.fn == 1], na.rm=T), 
      to = max(cont_dgb$kont.s[cont_dgb$fullsample.fn == 1], na.rm=T),
      length.out=800),
  ksq.synt
)

#re.scale <- unique(cbind(cont_dgb$Kontakt, cont_dgb$kont.s, cont_dgb$ksq.s)) # sollte f?r Plot verwendbar sein
re.scale <- re.scale[order(re.scale[,1]),]

#re.scale2 <- re.scale[order(re.scale[,2]),]

# vage Termine
probs.kont <- coefs.r[1] *
  coefs.r[4]^re.scale[,2] *
  coefs.r[5]^re.scale[,3] *
  #coefs.r[4]^sort(unique(cont_dgb$kont.s)) *
  #coefs.r[5]^sort(unique(cont_dgb$ksq.s)) *
  coefs.r[6]^1 *
  coefs.r[9]^mean(scale(cont_dgb$anznsk[cont_dgb$fullsample.fn ==1], center = T, scale = T)) *
  coefs.r[11]^mean(scale(cont_dgb$schichtbeginn[cont_dgb$fullsample.fn ==1], center = T, scale = T)) *
  coefs.r[12]^mean(scale(cont_dgb$workload[cont_dgb$fullsample.fn ==1], center = T, scale = T)) * 
  coefs.r[28]^mean(scale(cont_dgb$anznsk[cont_dgb$fullsample.fn ==1], center = T, scale = T))
probs.kont <- probs.kont/(1+probs.kont)

plot( re.scale[,1], probs.kont, ylim=c(0, 0.5),
      xlim=c(0,15),  bty="l", "n", 
      cex.axis=1, col.axis=gray(0.25), mgp=c(1.6,0.6,0), 
      tck=-0.01,main="", cex.main=1,
      xlab="Anzahl Sprachkontakte", ylab="", xaxt="n", cex.lab=0.9)
rect(-8, -8, 50, 150, col=gray(0.95), border=NA)
axis(1, at=seq(0, 40, 5), labels=F, tck=1, lwd=0, lwd.ticks=0.05, col.ticks="white")
axis(1, at=seq(0, 40, 10), labels=F, tck=1, lwd=0, lwd.ticks=1.5, col.ticks="white")

axis(1, at=seq(0, 40, 5), 
     labels=T, col.axis=gray(0.25), 
     tck=-0.0075, lwd=0.000, lwd.ticks=0.01, col.ticks="black", cex.axis=1)

#axis(2, at=seq(7.5, 9.5, 0.025), labels=F, tck=-0.005, lwd=0, lwd.ticks=0.5)
axis(2, at=seq(0.0, 150, 0.1), labels=F, tck=1, lwd=0, lwd.ticks=0.05, col.ticks="white")
axis(2, at=seq(0.0, 150, 0.2), labels=F, tck=1, lwd=0, lwd.ticks=1.5, col.ticks="white")
#mtext(side = 1, text = "Uhrzeit", line = 1.5, cex=0.7)

lines(re.scale[,1], probs.kont, ylim=c(0, 0.5), col="black")

re.scale <- cbind(re.scale[,1], probs.kont)

kont <- NA
num <- 0

kont_tex <- NA

for(i in seq(from=2, to= nrow(re.scale), by=20)) {
  
  num <- num + 1
  if(num == 1) {
    kont[num] <- paste0(
      "(",
      re.scale[i-1,1]/3,
      ",",
      re.scale[i-1,2]*12,
      ") -- (",
      re.scale[i,1]/3,
      ",",
      re.scale[i,2]*12,
      ") "
    )
  } else {
    kont[num] <- paste0(
      kont[num-1], 
      " -- ",
      "(",
      #re.scale[i-1,1]/3,
      #",",
      #re.scale[i-1,2]*12,
      #") -- (",
      re.scale[i,1]/3,
      ",",
      re.scale[i,2]*12,
      ") "
    )
    
  }
  
}

for(i in 1:length(kont)) {
  
  if(i < 11) {
  kont_tex[i] <- paste0(
    "\\draw[-, blue]",
    kont[i],
    "; \\newframe"
  )
  } else {
    kont_tex[i] <- paste0(
      "\\draw[-, blue]",
      kont[i],
      "; \\path[->, draw] (1.6750104297038,3) -- (1.6750104297038,2.54785374653898);",
      "\\path[draw] (1.6750104297038,3.1) node[align=center, blue] {\\scriptsize \\firalight{n = 5}};",
      "\\newframe"
    )
    
  }
  
}

writeLines(kont_tex, con=
             "E:/Promotion/Präsentationen/Disputation/Vortrag_Disputation/Abbildungen/Kont_DGB-R.txt")




