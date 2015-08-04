# Investigating Discriminant Analysis with more than 2 groups
# October 4, 2013

# I am using the Egyptian Skulls Dataset
# Description: Four measurements of male Egyptian skulls from 5 different time periods. 
# Thirty skulls are measured from each time period.
# Number of cases: 150
# Variable Names:
  
# MB: Maximal Breadth of Skull
# BH: Basibregmatic Height of Skull
# BL: Basialveolar Length of Skull
# NH: Nasal Height of Skull
# Year: Approximate Year of Skull Formation (negative = B.C., positive = A.D.) 
# The years are 4000BC, 3300BC, 1850BC, 200BC, 150AD

skulls <- read.table("c:/Users/Nate/Git/riemann/skulls.txt", header=T)
attach(skulls) # Attach the skulls data frame

#  Conduct an LDA analysis using the skulls data where the prior probability = the same
(skull.lda <- lda(EPOCH~MB+BH+BL+NH, prior=c(0.2,0.2,0.2,0.2,0.2) ))
#skull.lda 

# Let's predict the epoch (year) of a new skull with 
# MB = 135, BH = 144, BL = 97, NH = 53:

newobs <- rbind( c(135,144,97,53) )
dimnames(newobs) <- list(NULL,c('MB','BH', 'BL', 'NH'))
newobs <- data.frame(newobs)
(predict(skull.lda,newdata=newobs))
# As wee see, we predict that the year will be 1850BC based on the data provided