# Understanding Linear Discriminant Analysis
# October 4, 2013

# First, let us explore one-sample inference about a mean vector for a sample data set
# I will do this using the SAT scores dataset
# This data set shows SAT scores of various individuals
# The follow up dataset shows the relative status (whether the individual graduated from their program)
satmult <- read.table("c:/Users/Nate/Git/riemann/SATscoresmult.txt", 
                      header=T)
my.n <- nrow(satmult)  # How many individuals are in the dataset; There are 40 individuals
xbar.sat <- apply(satmult, 2, mean) # Sample mean vectors for the SAT scores data
my.q <- length(xbar.sat)  # number of variables; There are 3 variables in the sample mean vector

# What does the sample covariance matrix for the SAT scores data look like? 
(S.sat <- var(satmult))

# What is the correlation between the various variables?
cor(satmult)

# Let's first do a test to see whether the mean vector is equal to (500, 500, 500)'
mu.0 <- c(500, 500, 500)

# Hotelling's T-squared distribution is a univariate distribution proportional to the 
# F-distribution and arises importantly as the distribution of a set of statistics which are 
# natural generalizations of the statistics underlying Student's t-distribution. In particular, 
# the distribution arises in multivariate statistics in undertaking tests of the differences 
# between the (multivariate) means of different populations, where tests for univariate problems 
# would make use of a t-test.

# Calculate Hotelling T^2, the F-statistic, and the P-value
T2 <-  my.n * t(xbar.sat - mu.0) %*% solve(S.sat) %*% (xbar.sat - mu.0)
Fstat <- ( (my.n - my.q) / ((my.n-1)*my.q) ) * T2
(pvalue <- 1-pf(Fstat, my.q, my.n-my.q))

# Print the output from the previous calculation
print(paste("Hotelling T^2 =", round(T2,4), 
            "F=", round(Fstat,4), "P-value =", round(pvalue,4)))

# Now, let us plot a 2-D confidence ellipse for the mean vector for the math and reading scores
my.q <- 2  # since now we are just focusing on the math and reading variables

my.x1 <- runif(n=50000, min=min(satmult[,1]), max=max(satmult[,1]) )
my.x2 <- runif(n=50000, min=min(satmult[,2]), max=max(satmult[,2]) )
my.xs <- cbind(my.x1,my.x2)

my.pts<-rep(0,times=length(my.x1))

for (i in 1:(length(my.pts)) ) {
  my.pts[i] <- ( (my.n - my.q) / ((my.n-1)*my.q) ) * my.n * t(xbar.sat[c(1,2)] - my.xs[i,]) %*% solve(S.sat[c(1,2),c(1,2)]) %*% (xbar.sat[c(1,2)] - my.xs[i,])
}

my.x1.inside <- my.x1[my.pts <= qf(0.95, my.q, my.n-my.q)]
my.x2.inside <- my.x2[my.pts <= qf(0.95, my.q, my.n-my.q)]

png('c:/Users/Nate/Git/riemann/linear_discriminant_analysis_mean_vector.png')
plot(my.x1.inside, my.x2.inside, xlab="Math", ylab="Reading",
     main = "Plot of a 2D Confidence Ellipse For Mean Vector: SAT scores")
conv.hull <- chull(my.x1.inside, my.x2.inside)
polygon(my.x1.inside[conv.hull], my.x2.inside[conv.hull])
dev.off()

# Now, let us just plot the ellipse
png('c:/Users/Nate/Git/riemann/linear_discriminant_analysis_ellipse_only.png')
plot(my.x1.inside, my.x2.inside, xlab="Math", ylab="Reading", type='n',
     main = "Plot of a 2D Confidence Ellipse For Mean Vector: SAT scores")
conv.hull <- chull(my.x1.inside, my.x2.inside)
polygon(my.x1.inside[conv.hull], my.x2.inside[conv.hull])
dev.off()

# Let us review the individual t-based CIs for each of the two variables
t.test(satmult[,1])$conf.int  # 95% CI for mean math score
t.test(satmult[,2])$conf.int  # 95% CI for mean reading score

abline(v=t.test(satmult[,1])$conf.int[1], lty=2)  # Lower conf. limit of 95% CI, math
abline(v=t.test(satmult[,1])$conf.int[2], lty=2)  # Upper conf. limit of 95% CI, math

abline(h=t.test(satmult[,2])$conf.int[1], lty=2)  # Lower conf. limit of 95% CI, reading
abline(h=t.test(satmult[,2])$conf.int[2], lty=2)  # Upper conf. limit of 95% CI, reading

# ----------------------------------------------------------------------------------------  
#  Discriminant Analysis

# Linear discriminant analysis (LDA) and the related Fisher's linear discriminant are methods 
# used in statistics, pattern recognition and machine learning to find a linear combination of 
# features which characterizes or separates two or more classes of objects or events. 
# The resulting combination may be used as a linear classifier, or, more commonly, 
# for dimensionality reduction before later classification

# Here is the SAT scores data augmented with graduation information
satgradu <- read.table("c:/Users/Nate/Git/riemann/satgradu.txt", 
                      header=T)

# The first three columns are the SAT scores on the 3 tests,
# the last column (called gradu) is an indicator of whether the student successfully graduated
# (1 = graduated, 0 = did not graduate)

attach(satgradu)

# I will now use the built-in lda function in the MASS package
# for linear discriminant analysis:

library(MASS)

# Let me assume that there are equal prior probabilities of graduating or not:
(dis <- lda(gradu ~ math + reading + writing, data=satgradu, prior=c(0.5, 0.5)))
# a1, a2, a3 are given as "Coefficients of linear discriminants".

# Let's predict whether a new applicant with SAT scores of: math = 550, reading = 610, 
# writing = 480 will graduate:

newobs <- rbind( c(550,610,480) )
dimnames(newobs) <- list(NULL,c('math','reading', 'writing'))
newobs <- data.frame(newobs)
predict(dis,newdata=newobs)$class

# Let us determine the posterior probabilities of this applicant being in each group
# Note that 0 = not graduate and 1 = graduate
predict(dis,newdata=newobs)$posterior

# Now, let us try to make predictions for several new individuals at once
newobs <- rbind( c(300,420,280), c(510,480,470), c(780,760,710) )
dimnames(newobs) <- list(NULL,c('math','reading', 'writing'))
newobs <- data.frame(newobs)
predict(dis,newdata=newobs)

# I note that we could assume now that the prior probability of graduating is about 
# twice as large as probability of not graduating:
# dis <- lda(gradu ~ math + reading + writing, data=satgradu, prior=c(0.33, 0.67))

# If we do not specify any prior probabilities, it will by default use the proportions
# of the sampled individuals that are in each group as the prior probabilities.

dis2 <- lda(gradu ~ math + reading + writing, data=satgradu)
dis2

# Let us now look at the misclassification rate of our model
# Simple plug-in misclassification rate
group<-predict(dis, satgradu, method='plug-in')$class
table(group,gradu)

# The plug-in misclassification rate for LDA that we can calculate by hand here is (11+4)/40 = 0.375
# Now, let us use cross-validation to check ... 

correct<-rep(0,times=nrow(satgradu) )
for (j in 1:nrow(satgradu) ) {
  mydis<-lda(grouping=gradu[-j], x=satgradu[-j,1:3], prior=c(0.5,0.5))
  mypred<-predict(mydis,newdata=satgradu[j,1:3])$class
  correct[j] <- (mypred==gradu[j])
}
cv.misclass <- 1-mean(correct)
cv.misclass # As we note here, the cross-validation misclassification rate is 0.425

# ----------------------------------------------------------------------------------------  
# Let us change pace and look at another form of DA, Quadratic Discriminant Analysis

# Quadratic discriminant analysis (QDA) is closely related to linear discriminant analysis (LDA), 
# where it is assumed that the measurements from each class are normally distributed. 
# Unlike LDA however, in QDA there is no assumption that the covariance of each of the classes 
# is identical. When the normality assumption is true, the best possible test for the hypothesis 
# that a given measurement is from a given class is the likelihood ratio test.

# Quadratic discriminant analysis can be implemented with the qda function:
disquad <- qda(gradu ~ math + reading + writing, data=satgradu, prior=c(0.5, 0.5))

# Let us look at the misclassification rate of QDA rule:
# Simple plug-in misclassification rate:
group<-predict(disquad, satgradu, method='plug-in')$class
table(group,gradu)

# The plug-in misclassification rate for QDA here is (10+2)/40 = 0.3

# Let us cross validate the QDA rule:
correct<-rep(0,times=nrow(satgradu) )
for (j in 1:nrow(satgradu) ) {
  mydisquad<-qda(grouping=gradu[-j], x=satgradu[-j,1:3], prior=c(0.5,0.5))
  mypred<-predict(mydisquad,newdata=satgradu[j,1:3])$class
  correct[j] <- (mypred==gradu[j])
}
cv.misclass <- 1-mean(correct)
cv.misclass

# The cross-validation misclassification rate for QDA here is still 0.425.