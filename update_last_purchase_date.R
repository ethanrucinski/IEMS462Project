# Read cleaned data
dat <- read.csv("Huazhen_Joel_Updated.csv")

summary(dat)
updates = data.frame('2012' = 0,'2011' =0,'2010' =0,'2009'=0)
for (i in 77744:92251) {
  # Iterate over each row and correct last purchase year
  if (dat[i,]$ordtyr2 != 0 && dat[i,]$lpuryear2 != 2012) {
    # Update year to 2012 and choose season based on probability
    dat[i,]$lpuryear2 = 2012
    dat[i,]$lp6_bin = 64+rbinom(1,1,(10607/(10607+4668)))
    updates$X2012 = 1 + updates$X2012
  } else if (dat[i,]$ordlyr2 != 0 && dat[i,]$lpuryear2 != 2011) {
    dat[i,]$lpuryear2 = 2011
    dat[i,]$lp6_bin = 62+rbinom(1,1,(12504/(12504+4874)))
    updates$X2011 = 1 + updates$X2011
  } else if (dat[i,]$ord2ago != 0 && dat[i,]$lpuryear2 != 2010) {
    dat[i,]$lpuryear2 = 2010
    dat[i,]$lp6_bin = 60+rbinom(1,1,(14368/(14368+5851)))
    updates$X2010 = 1 + updates$X2010
  } else if (dat[i,]$ord3ago != 0 && dat[i,]$lpuryear2 != 2009) {
    dat[i,]$lpuryear2 = 2009
    dat[i,]$lp6_bin = 58+rbinom(1,1,(3/(3+9197)))
    updates$X2009 = 1 + updates$X2009
  }
}
  
write.csv(dat, "Huazhen_Joel_Ethan_Updated.csv")   
  
