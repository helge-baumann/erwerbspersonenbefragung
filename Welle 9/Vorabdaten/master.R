library(knitr)
knit2pdf("Erwerbspersonenbefragung_Verstetigung_Emmler.Rnw", compiler="xelatex")

x <- names(dat)[1:512]
n <- length(x)
mat <- tibble(Variable= character(n), Label=character(n), 
              `Welle 1` = character(n), `Welle 2` = character(n), `Welle 3` = character(n),
              `Welle 4` = character(n),
              `Welle 5` = character(n),`Welle 6` = character(n),`Welle 7` = character(n))

num <- 0

for(b in x) {
  
  if(str_detect(b, "_w[:digit:]")) {
  num <- num+1
  mat[num,1]<- str_remove_all(b, "_w[:digit:]{1,7}$")
  if(length(attributes(dat[[b]])$label)>0)mat[num,2]<- attributes(dat[[b]])$label
  w <- str_extract(b, "[:digit:]{1,7}$")
  
  for(i in 1:7) {
    
    if(str_detect(w, as.character(i))) mat[num, i+2] <- "x"
    
  }
  
  }
}

write.csv2(mat, "Matrix.csv")
