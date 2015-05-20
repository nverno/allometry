### scratch.R --- 
## Filename: scratch.R
## Description: 
## Author: Noah Peart
## Created: Tue May 19 15:22:02 2015 (-0400)
## Last-Updated: Tue May 19 15:46:04 2015 (-0400)
##           By: Noah Peart
######################################################################
dat <- read.table(text="    Name    Score1    Score2    Score3    Score4    Score5
1   John     2           2        NA        3         NA
2   Sam      1           NA        3        1          1
3   Bob      4           4         4        3          NA")

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
 }



df$Row_Mode <- 0
for (row in 1:nrow(df){
  df[row,]$Row_Mode <- as.numeric(Mode(df[row,2:6]))
}
     
     )
    
}

dat[,2:ncol(dat)] <- t(apply(dat[,2:ncol(dat)], 1, function(x) {
    enc <- rle(x)
    val <- enc$values[which.max(enc$lengths)]
    x[is.na(x)] <- val
    x
}))


