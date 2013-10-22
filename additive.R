# Understanding Additive Models
# October 21, 2013

# Additive regression models essentially apply local
# regression to low dimensional projections of the data

# Additive models create an estimate of the regression
# surface by a combination of a collection of onedimensional
# functions

# The assumption that the contribution of each covariate
# is additive is analogous to the assumption in linear
# regression that each component is estimated separately

# We use a procedure called backfitting to find each curve,
# controlling for the effects of the others

# Let us look at the Canadian Prestige Data to fit an additive model
# Note that this dataset contains 1971 information on Canadian occupations
# The Prestige data frame has 102 rows and 6 columns. The observations are occupations. 
# Source: Canada (1971) Census of Canada. Vol. 3, Part 6. Statistics Canada [pp. 19-1-19-21]. 

# install.packages('mgcv') # Install the mgcv package
library(MASS)
library(mgcv) # Load the mgcv package
library(nlme) # Load the nlme package
library(forecast) # Load the nlme package
library(car) # To get the data

data(Prestige) # Load in the data

#Removing missing cases
Prestige2<-na.omit(Prestige)
attach(Prestige2)

# Note that one could also read in the data (I omitted this step due to the step above)
# prestige <- read.table("c:/Users/Nate/Git/riemann/prestige.txt",header=TRUE)
# names(prestige)
# attach(prestige)

# Let use a gam to model the data
prestige.gam <- gam(prestige~s(income) + s(education), data = Prestige2)
# Remember that smooths will be fit to any variable specified with the
# s(variable) argument

# Remember that the summary function returns tests for each smooth, the
# degrees of freedom for each smooth, and an adjusted Rsquare
# for the model. The deviance can be obtained from the deviance(model) command

summary(prestige.gam)

# Again, as with other nonparametric models, we have no
# slope parameters to investigate (we do have an                                  intercept, however)
# A plot of the regression surface is necessary
# Let us create a perspective plot
# Note that we can also use persp.gam() to generate perspective plots and 95% CI's'
png("c:/Users/Nate/Git/riemann/additive_perspective.png")
inc <- seq(min(income), max(income), len=25)
ed <- seq(min(education), max(education), len=25)
data <- expand.grid(income=inc, education=ed)
fit.prestige <- matrix(predict(prestige.gam, data), 25, 25)
persp(inc, ed, fit.prestige, theta=45, phi=30,
      ticktype='detailed', xlab='income', ylab='Education',
      zlab='Prestige', expand=2/3, shade=0.5,
      cex=0.5)
title(main="Perspective Plot Generated From GAM - Prestige Data")
dev.off()

# Note that we can also create perspective plots in mgcv package
png("c:/Users/Nate/Git/riemann/additive_perspective_mgcv.png")
vis.gam(prestige.gam, theta=45)
title(main="Perspective Plot Generated From GAM mgcv - Prestige Data")
dev.off()

# Looking at the graph, we can see the nonlinear relationship for both education and
# income with Prestige but there is no interaction between them-i.e., the slope
# for income is the same at every value of education. 

# Since the slices of the additive regression in the direction of one predictor 
# (holding the other constant) are parallel, we can graph each partial regression
# function separately
png("c:/Users/Nate/Git/riemann/additive_partial.png")
plot(prestige.gam, pages=1)
title(main = "Graph of Partial Regression Functions - GAM - Prestige Data")
dev.off()

# Let's interpret the results more closely
png("c:/Users/Nate/Git/riemann/additive_partial_interpret.png")
par(mfrow=c(1,2))
# Fitted value at income=10000
plot(prestige.gam, select=1, main="Income=10,000") # Generate the first plot of the income
segments(0,6,10000,6, lty=2, lwd=2, col="red") 
segments(10000, -20, 10000,6, lty=2, lwd=2, col="red")
points(10000,6, cex=1.5, pch=19)
text(6500,8.5, "(10 000,6)")

# Fitted value at education=10
plot(prestige.gam, select=2, main="Education=10") # Generate the second plot showing education
segments(10,-20,10,-5, lty=2, lwd=2, col="red") 
segments(0, -5,10,-5, lty=2, lwd=2, col="red")
points(10,-5, cex=1.5, pch=19)
text(9.2,-2.5, "(10,-5)")
dev.off() # Turn the device off

# Determine the fitted value for income=10,000 and education=10
mean(prestige)+6-5

# As was the case for smoothing splines and lowess smooths, 
# statistical inference and hypothesis testing is based on the residual sum of 
# squares (or deviance in the case of generalized additive models) and the degrees of
# freedom. The RSS for an additive model is easily defined in the usual manne.
# The approximate degrees of freedom, however, need to be adjusted from the regular 
# nonparametric case, however, because we are no longer specifying a jointly-conditional
# functional form

# What about tests for linearity
# Let us fit a regular linear model using a gam
#Fitting a regular linear model using gam
prestige.ols<-gam(prestige ~ income + education, data = Prestige2)
deviance(prestige.ols)

# Next, let us determine the residual degrees of freedom from the additive model
prestige.gam

# Now I simply calculate the difference in the deviance between the two model relative to the difference in
# degrees of freedom (difference in df=7.3-2=5). This gives a Chi-square test for linearity
deviance(prestige.ols) - deviance(prestige.gam)
1 - pchisq(deviance(prestige.ols) - deviance(prestige.gam), 5)

# As we see here, the difference between the models is highly statistically
# significant-the additive model describe the relationship
# between prestige and education and income much better

# Note that we could also use the following to test for linearity
anova(prestige.ols, prestige.gam)

# We could also do diagnostic plots
png("c:/Users/Nate/Git/riemann/additive_gam_diagnostic.png")
gam.check(prestige.gam)
dev.off()

# As a final step, let us look at the semi-parametric model with interactions
type.bc <- as.numeric(type=="bc") # Blue collar
type.prof <- as.numeric(type=="prof") # Professional
type.wc <- as.numeric(type=="wc") # Working class
inter.gam<-gam(prestige ~ type + s(income,by=type.bc) + s(income,by=type.prof) + s(income,by=type.wc),
               data = Prestige2)

png("c:/Users/Nate/Git/riemann/additive_gam_interactions.png")
split.screen(figs=c(1,3))
screen(1)
plot.gam(inter.gam, select=1)
title("Blue Collar")
screen(2)
plot.gam(inter.gam, select=2)
title("Professional")
screen(3)
plot.gam(inter.gam, select=3)
title("White Collar")
close.screen(all=TRUE)
dev.off()

# Generate a summary of the data & review the anova
summary(inter.gam) # Summary
anova(inter.gam) # Anova
