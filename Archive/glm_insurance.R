# Playing with Generalized Linear Models for insurance data
# October 16, 2013
# Nathaniel Payne

# Case 1 - Poisson Regression - Children births 

# The birth data set contains the number of children for each of 141 pregnant women. 
# The age of each mother or mother-to-be is also recorded.
# Variables: age -> mother's age; children -> number of children

# Note that Poisson regression is a form of regression analysis used to model count data 
# and contingency tables. Poisson regression assumes the response variable Y has a Poisson 
# distribution, and assumes the logarithm of its expected value can be modeled by a linear 
# combination of unknown parameters. A Poisson regression model is sometimes known as a 
# log-linear model, especially when used to model contingency tables.

birth <- read.table("c:/Users/Nate/Git/riemann/birth.txt", header=T)
attach(birth) # Attach the birth data frame

# Generate a raw plot of the data
png('c:/Users/Nate/Git/riemann/glm_insurance_birth_raw.png')
plot(children ~ age, main = "Poisson Regression - Number of Children / Women")
dev.off()

# Let us now run a poisson regression on the data
birth.log <- glm(formula = children ~ age, family = poisson(link = log),data=birth)
summary(birth.log)
anova(birth.log)

# Let us look at some plots of the output
png('c:/Users/Nate/Git/riemann/glm_insurance_birth_summary.png')
par(oma=c(4,4,4,4)) # Note that this gives the margins more space (3 units of space each)
par(mfrow=c(2,2))
plot(birth.log)
title(main="Summary Outputs - Poisson Regression: Birth Data", outer = TRUE, cex = 1.5)
dev.off()

# Note that we get an error if we try to use an identity link, as opposed to a logit link
birth.id <- glm(formula = children ~ age, family = poisson(link = identity),data=birth)
# Error: no valid set of coefficients has been found: please supply starting values

# -----------------------------------------------------------------------------------------
# Case 2 - Understanding Diabetes Deaths and Categorical Ages

# The number of deaths due to diabetes in New South Wales, Australia in 2002 were provided by the 
# Australian Institute of Health and Welfare, from their mortality database. 

# Variables: Gender - Male, Female; ge		<25, 25-34, ..., 85+; Deaths - number of deaths; 
# Popn - population size

# In order the read the data into R, diabetes.xls must be saved as diabetes.csv. Gender and
# age are both character variables in the data file, so R will treat them as categorical. The way
# that the model is specified is deaths ~ gender + age. The default base level in R is the 
# lowest level, which is female gender and age <25. In order to reproduce the SAS output, \
# we control the base level using the C function. In the case of age,
# for example, we want "45-54" to be the base level. This is the fourth level of age, so the term
# is specified in the model as C(age,base=4).

Diabetes <- read.table("diabetes_insurance.txt",header=T)
attach(Diabetes)

# Let us try a model using age as a categorical variable
# We want to understand what causes death
Model1 <- glm(deaths ~ C(gender,base=2) + C(age,base=4), 
              family = poisson(link = log), 
              offset = l_popn)
summary(Model1)

# Now, let's try using a polynomial for age instead of treating it as a category
# Remember that polynomials are specified in R using the poly function.
Model2 <- glm(deaths ~ C(gender,base=2) + poly(agemidpt,3), 
              family = poisson(link = log), 
              offset = l_popn)
summary(Model2)

# Note that this gives us different coefficients for the agemidpt polynomial to SAS. 
# That said, the SAS solution can be reproduced as
minage <- min(agemidpt)
maxage <- max(agemidpt)
agestd <- (agemidpt-0.5*(minage+maxage))/(0.5*(maxage-minage))

# Now let's try to plot the model
Model3 <- glm(deaths ~ C(gender,base=2) + agestd + I(agestd^2) + I(agestd^3), 
              family = poisson(link = log), 
              offset = l_popn)
summary(Model3)

# -----------------------------------------------------------------------------------------
# Case 3 - Understanding Third Party Claims

# In many countries, liability insurance is a compulsory form of insurance for those at 
# risk of being sued by third parties for negligence. The most usual classes of mandatory 
# policy cover the drivers of vehicles, those who offer professional services to the public, 
# those who manufacture products that may be harmful, constructors and those who offer employment. 
# The reason for such laws is that the classes of insured are deliberately engaging in activities 
# that put others at risk of injury or loss. Public policy therefore requires that such individuals 
# should carry insurance so that, if their activities do cause loss or damage to another, 
# money will be available to pay compensation. In addition, there are a further range of 
# perils that people insure against and, consequently, the number and range of liability 
# policies has increased in line with the rise of contingency fee litigation offered by 
# lawyers (sometimes on a class action basis).

# Third party insurance is a compulsory insurance for vehicle owners
# in Australia. It insures vehicle owners against injury  caused to
# other drivers, passengers or pedestrians, as a result of an
# accident.

# This data set records the number of third party claims in a
# twelve-month period between 1984-1986 in each of 176 geographical
# areas (local government areas) in New South Wales, Australia.

# Variables:
# lga - 	local government area
# sd	- 	statistical division (1, ..., 13)
# claims - 	number of third party claims
# accidents	- number of accidents
# ki -	number killed or injured
# population - population size
# pop_density	- population density

TP <- read.table("Third_Party_Claims.txt",header=T)
attach(TP)

# Let's try the first model
model1 <- glm(claims ~ log(accidents), family=poisson, offset=log(population))
summary(model1) # Do you like the model fit? Residual deviance?

# Now, let's try this using a negative binomial regression

# Refresher on Negative Binomial Regression
# Negative binomial regression is for modeling count variables, usually for over-dispersed 
# count outcome variables. Overdispersion is the presence of greater variability 
# (statistical dispersion) in a data set than would be expected based on a given simple 
# statistical model.

# Overdispersion is often encountered when fitting very simple parametric models, such as those 
# based on the Poisson distribution. The Poisson distribution has one free parameter and does 
# not allow for the variance to be adjusted independently of the mean. The choice of a 
# distribution from the Poisson family is often dictated by the nature of the empirical data. 
# For example, Poisson regression analysis is commonly used to model count data. 
# If overdispersion is a feature, an alternative model with additional free parameters may 
# provide a better fit. In the case of the count data, a Poisson mixture model like the 
# negative binomial distribution can be used instead where the mean of the Poisson 
# distribution can itself be thought of as a random variable drawn - in this case - from the 
# gamma distribution thereby introducing an additional free parameter (note the resulting 
# negative binomial distribution has two parameters).

# Examples of negative binomial regression
# Example 1.  School administrators study the attendance behavior of high school juniors at 
# two schools.  Predictors of the number of days of absence include the type of program in 
# which the student is enrolled and a standardized test in math.
# Example 2.  A health-related researcher is studying the number of hospital visits in 
# past 12 months by senior citizens in a community based on the characteristics of the 
# individuals and the types of health plans under which each one is covered.  

# Negative binomial regression is in the MASS library, which must be installed and loaded. The
# function is glm.nb.
library(MASS)
model2 <- glm.nb(claims ~ log(accidents) + offset(log(population)))
summary(model2)

# Note that the dispersion parameter is Theta=5.831. 
# On the other hand, in SAS the dispersion parameter is given as 0.1715,
# which is 1/5.831.

# Let's now look at quasi-likelihood
# Quasi-likelihood estimation is one way of allowing for overdispersion, that is, greater 
# variability in the data than would be expected from the statistical model used. 
# It is most often used with models for count data or grouped binary data, i.e. data that 
# otherwise be modelled using the Poisson or binomial distribution.

# Remember that random-effects models, and more generally mixed models (hierarchical models) 
# provide an alternative method of fitting data exhibiting overdispersion using fully 
# specified probability models. However, these methods often become complex and 
# computationally intensive to fit to binary or count data. Quasi-likelihood methods have 
# the advantage of relative computational simplicity, speed and robustness, as they can make 
# use of the more straightforward algorithms developed to fit generalized linear models.

# Let's fit the model
model3 <- glm(claims ~ log(accidents), 
              family=quasi(link="log",variance="mu"), 
              offset=log(population))

summary(model3) # Generate a summary of the model

# -----------------------------------------------------------------------------------------
# Case 4 - Swedish mortality, categorical age and year

# The Human Mortality Database (2007) provides mortality data for countries
# across periods of time. Recorded are the number of live births,
# deaths, and population at risk classified according to age and sex.
# From these are calculated such quantities as death rates and
# expectations of life. This text uses data for Sweden from 1951
# through to 2005.

# Variables:
  
# Year  	1951,..., 2005
# Age		0, 1, ..., 109
# Female_Exp	female population
# Male_Exp	male population
# q_female	female death rate = Female_death/Female_Exp
# q_male		male death rate = Male_death/Male_Exp
# Female_death	number of female deaths
# Male_death	number of male deaths
# L_female_exp	log female population
# L_male_exp	log male population

# mortality <- read.table("swedish_mortality.txt",header=T) # This one posed an issue
mortality <- read.table("swedish_mortality.csv",header=T, sep=",")
mortality <- mortality[,-c(3,5,7,9,11)]
mortality <- na.omit(mortality)
# Note that na.fail returns the object if it does not contain any missing values, 
# and signals an error otherwise. na.omit returns the object with incomplete cases removed. 
# na.pass returns the object unchanged.
attach(mortality)
library(MASS)

# Let's try to run the model now
model1 <- glm.nb(Male_death ~ factor(Age) + factor(Year) + offset(L_male_exp))
summary(model1,corr=F)

# -----------------------------------------------------------------------------------------
# Case 5 - Vehicle Insurance - Logistic Regression
# Let's look at vehicle insurance; Vehicle insurance: quadratic vehicle value

# This data set is based on  one-year vehicle insurance
# policies taken out in 2004 or 2005. There are 67856 policies, of
# which  4624 (6.8%) had at least one claim. 

# Variables found within the data set:
  
# veh_value  vehicle value, in $10,000s
# exposure	0-1
# clm		occurrence of claim (0 = no, 1 = yes)
# numclaims	number of claims
# claimcst0	claim amount (0 if no claim)
# veh_body	vehicle body, coded as
# BUS
# CONVT = convertible  
# COUPE   
# HBACK = hatchback                  
# HDTOP = hardtop
# MCARA = motorized caravan
# MIBUS = minibus
# PANVN = panel van
# RDSTR = roadster
# SEDAN    
# STNWG = station wagon
# TRUCK           
# UTE - utility
# veh_age	age of vehicle: 1 (youngest), 2, 3, 4
# gender		gender of driver: M, F
# area		driver's area of residence: A, B, C, D, E, F
# agecat		driver's age category: 1 (youngest), 2, 3, 4, 5, 6

car <- read.table("car.csv",sep=",",header=T) # Load in the data

model1 <- glm(clm ~ veh_value + I(veh_value^2), family=binomial, data=na.omit(car))
summary(model1) # Let's review the model

# Let's try creating a banded vehicle value
valuecat <- cut(car$veh_value, c(-1,2.5,5.0,7.5,10.0,12.5,100))
table(valuecat)

car <- cbind(car,valuecat) # Add the banded values back to the original data frame
model2 <- glm(clm ~ factor(valuecat), family=binomial, data=na.omit(car))
summary(model2)

# Let's look now at Vehicle insurance: full model, adjusted for exposure
source("logit-exposure-adjusted.r") # This is a custom file
attach(car)
model3 <- glm(clm ~ C(factor(agecat),base=3)+ C(factor(area),base=3) + C(factor(veh_body),base=10) + factor(valuecat), 
              family=binomial(logitexp(exposure)))
summary(model3) # Let's generate a summary of the model

# Now, let's look at ROC curves and the AUC

# In signal detection theory, a receiver operating characteristic (ROC), or simply ROC curve, 
# is a graphical plot which illustrates the performance of a binary classifier system as 
# its discrimination threshold is varied. It is created by plotting the fraction of 
# true positives out of the total actual positives (TPR = true positive rate) vs. the 
# fraction of false positives out of the total actual negatives (FPR = false positive rate), 
# at various threshold settings.

#  An ROC curve demonstrates several things:

# 1) It shows the tradeoff between sensitivity and specificity (any increase in sensitivity 
# will be accompanied by a decrease in specificity).
# 2) The closer the curve follows the left-hand border and then the top border of the 
# ROC space, the more accurate the test.
# 3) The closer the curve comes to the 45-degree diagonal of the ROC space, the less 
# accurate the test.
# 4) The slope of the tangent line at a cutpoint gives the likelihood ratio (LR) for 
# that value of the test. You can check this out on the graph above. Recall that the LR for T4 < 5 is 52. This corresponds to the far left, steep portion of the curve. The LR for T4 > 9 is 0.2. This corresponds to the far right, nearly horizontal portion of the curve.
# 5) The area under the curve is a measure of text accuracy.

# The AUC is easily computed using the somers2 function in the Hmisc package, which needs to
# be downloaded from the CRAN website. A function ROC for computing and plotting the ROC
# curve, is given on the book website in ¯le ROC-function.r.

valuecat <- cut(car$veh_value, c(-1,2.5,5.0,7.5,10.0,12.5,100))
car <- cbind(car,valuecat)
attach(car)

# install.packages("Hmisc")
library(Hmisc) # Note that we need this for somers2 function to compute AUC
source("ROC-function.r") # This is needed for plotting the ROC-curve 

model5 <- glm(clm ~ C(factor(agecat),base=3)+ C(factor(area),base=3) + C(factor(veh_body),base=10) + factor(valuecat), family=binomial)

# Compute fitted values from logistic regression and store in fittedvalues
fittedvalues <- predict(model5, type = 'response', newdata = car)
somers2(fittedvalues,clm)

png('c:/Users/Nate/Git/riemann/glm_insurance_roc_curve.png')
ROC(fittedvalues,clm) # Create the ROC curve
title(main = "ROC Curve: Vehicle Insurance (Logistic Regression)")
dev.off()
# This curve doesn't appear to fit that well .... Tragically ... Or does it?

# -----------------------------------------------------------------------------------------
# Case 6 - Ordinal Regression

# Ordinal regression is a type of regression analysis used for predicting an ordinal 
# variable, i.e. a variable whose value exists on an arbitrary scale where only the 
# relative ordering between different values is significant. The two most common types 
# of ordinal regression models are ordered logit and ordered probit.

# This data set consists of all drivers involved in a crash in 2004 in New South
# Wales, Australia. There are  a total of 82659
# drivers in the data set.   Drivers with unknown age, age less than
# 17 years, or road user class ``Other" are omitted, leaving 76341
# cases.

# Variables:
# agecat  	driver's age, coded as
# 1 = 17-20
# 2 = 21-25
# 3 = 26-29
# 10 = 30-39 (4 coded as 10 to be base level) 
# 5 = 40-49
# 6 = 50-59
# 7 = 60+

# roaduserclass	10 = car (coded to be base level)
# 2 = light truck
# 4 = bus / heavy rigid truck / articulated truck
# 6 = motorcycle

# sex		driver's sex: M, F

# degree		degree of crash, coded as
# 1 = non-casualty
# 2 = injury
# 3 = fatal

# number		frequency of crashes

# Let's start by looking at the proportional odds model
# A few functions for this model are available. I prefer vglm in the VGAM package. The VGAM
# manual is worth consulting before attempting to implement the next three models.
# install.packages("VGAM")

injury <- read.table("injury.csv",sep=",",header=T)
attach(injury)
library(VGAM)

# Note that you can change the base levels. This is not necessary, but will result in you getting
# the same solution as that calcualted by SAS
road.x <- C(factor(roaduserclass),base=4)
age.x <- C(factor(agecat),base=7)
sex.x <- C(sex,base=2)

model1 <- vglm(degree ~ road.x + age.x + sex.x + age.x*sex.x, cumulative(parallel=TRUE), 
               weights=number)
summary(model1) # Model summary

# Let's look instead at the partial proportional odds model
# We use vglm for this model. The partial proportional odds are specified via the parallel parameter.
model2 <- vglm(degree ~ road.x + age.x + sex.x + age.x*sex.x, cumulative(parallel=TRUE~age.x*sex.x-1), weights=number)
summary(model2) # Model summary

# -----------------------------------------------------------------------------------------
# Case 7 - Injury Data - Nominal Regression

# Logistic regression analysis studies the association between a categorical dependent variable 
# and a set of independent (explanatory) variables. The name logistic regression is often 
# used when the dependent variable has only two values. The name multiple-group logistic 
# regression (MGLR) is usually reserved for the case when the dependent variable has 
# three or more unique values. Multiple-group logistic regression is sometimes called 
# multinomial, polytomous, polychotomous, or nominal logistic regression. Although the 
# data structure is different from that of multiple regression, the practical use of the 
# procedure is similar.

# As the private health insurance data are not publicly available, 
# nominal regression is illustrated here on the degree of injury data. 
# The vglm function in the VGAM package is used. We use the same injury data set
injury <- read.table("injury.csv",sep=",",header=T)
attach(injury)
library(VGAM)

# Again, change the base levels so that we can match SAS output
road.x <- C(factor(roaduserclass),base=4)
age.x <- C(factor(agecat),base=7)
sex.x <- C(sex,base=2)

# Nominal regression model
model3 <- vglm(degree ~ road.x + age.x + sex.x + age.x*sex.x, multinomial, weights=number)
summary(model3)

# -----------------------------------------------------------------------------------------
# Case 8 - Gamma regression - Vehicle Insurance

# The gamma distribution is useful for positively valued integer outcomes (zero is undefined,
# however).  The degree of skew in the distribution is estimated by the scale parameter.  The
# gamma regression is parameterized differently from the others by SAS, using an inverse power
# metric.  Consequently, the directions of coefficients are the opposite of those for other models

# We choose to use the same car.csv data from above
car <- read.table("car.csv",sep=",",header=T)

# Let's look at banded vehicle values
valuecat <- cut(car$veh_value, c(-1,2.5,5.0,7.5,10.0,12.5,100))

# Again, create variables in such a way that they match SAS output
age.x <- C(factor(car$agecat),base=3) ## agecat=3 base level
area.x <- C(factor(car$area),base=3) ## area C is 3rd level
gender.x <- C(factor(car$gender),base=2) ## gender M is 2nd level
veh_body.x <- C(factor(car$veh_body),base=10) ## SEDAN is 10th level

car <- cbind(car,valuecat, age.x,area.x,gender.x,veh_body.x)

model1 <- glm(claimcst0 ~ age.x + gender.x + age.x*gender.x + area.x + veh_body.x, 
              family=Gamma(link="log"),data=subset(car,clm==1))
summary(model1)

# Let's now look at personal injury insurance, with no adjustment for quickly settled claims
# This data set contains information on  22036 settled personal
# injury insurance claims. These claims arose from accidents occurring
# from July 1989 through to  January 1999. Claims settled with zero
# payment are not included. 

# Variables:
  
# total  	settled amount
# inj1, ..., inj5	injury 1,..., injury 5 coded as
# 1 = no injury
# 2, 3, 4, 5 = injury severities
# 6 = fatal injury
# 9 = not recorded
# legrep		legal representation (0 = no, 1 = yes)
# accmonth	accident month (1 = July 1989, ..., 120 = June 1999)
# repmonth	reporting month (as above)
# finmonth	finalization month (as above)
# op_time		operational time

persinj <- read.table("persinj.csv",sep=",",header=T)

model3 <- glm(total ~ op_time + factor(legrep) + op_time*factor(legrep), 
              family=Gamma(link="log"), data=persinj)
summary(model3) # Generate a summary of the model

# -----------------------------------------------------------------------------------------
# Case 9 - Runoff-Triangles (Gamma regression continued)

# Problem: Claims occurring in a year (of insurance cover) may not be settled or even reported 
# in that year. Some examples of this include: a) Employer's Liability insurance { Asbestosis }
# b) Motor insurance { Personal injuries } c) Medical Malpractice { MRSA }

# Solution: At the end of each year of insurance cover (realistically {financial year) the
# insurer should estimate the amount to be paid as claims in the future in respect of incidents
# in the past covered by the insurer. Run-off triangles are used in general insurance to
# forecast future claim numbers and amounts

# Run-off triangles usually arise particularly in non-life insurance where it may take some time 
# after a loss until the full extent of the claims which have to be paid are known.
# It is important that the claims are attributed to the year in which the accident occurred.
# The insurance company needs to know how much it is liable to pay in claims so that it can 
# calculate how much surplus it has made.

# Variables:
# y  	total payouts
# accyear		accident year
# devyear		development year

# Let us create a runoff triangle
runoff <- read.table("runoff_triangle.csv",sep=",",header=T)
runoff$Y[runoff$Y<0] <- 1 ### replace negative value by 1

model4 <- glm(Y ~ factor(devyear) + factor(accyear), family=Gamma(link="log"), data=runoff)
summary(model4) # Let's interpret this one ... 

# -----------------------------------------------------------------------------------------
# Case 10 - Logistic regression GLMM
# Note that the software in this area is developing very rapidly. 
# I am using glmmPQL here, which is in the MASS package.

# Note that this is a simulated data set, based on the car insurance data set
# There are assumed to be 40,000 policies over 3 years, giving 120,000 records.

# Variables:  
# poicyID  ID number of policy
# agecat	driver's age category: 1 (youngest), 2, 3, 4, 5, 6
# valuecat	vehicle value, in categories 1,...,6. (Category 1 has been recoded as 9.)
# period	1, 2, 3
# numclaims	number of claims

claimslong <- read.table("claimslong.csv",header=T,sep=",")

# Let us create a binary variable for claim/no claim
claimslong <- cbind(claimslong,clm=1*(claimslong$numclaims>0))

# Create variables with some custom base levels, for comparability
age.x <- C(factor(claimslong$agecat),base=6)
value.x <- C(factor(claimslong$valuecat),base=6)
period.x <- C(factor(claimslong$period),base=3)
claimslong <- cbind(claimslong,age.x,value.x,period.x)

library(MASS) # Use the MASS library
# Note that, due to iterations, this next step takes a little while to run
model1 <- glmmPQL(clm ~ age.x + value.x + period.x, 
                  random=~1|policyID, 
                  family=binomial, 
                  data=claimslong)

summary(model1) # Let's look at a summary of the model

# Note that the parameter estimates are similar to those produced by SAS. 
# That said, they are not identical because proc nlmixed and glmmPQL use 
# different methods for ¯nding the maximum likelihood solution.

# -----------------------------------------------------------------------------------------
# Case 11 - Logistic regression using GAM's ...
# GAMs can be fitted using either the special purpose gam package, or the more general gamlss
# package. I illustrate the use of both.

# Let's look at the vehicle insurance data
car <- read.table("car.csv",sep=",",header=T)

# Now, let's look at the banded vehicle value
valuecat <- cut(car$veh_value, c(-1,2.5,5.0,7.5,10.0,12.5,100))

# Now, let us create variables with same base levels as in the text
age.x <- C(factor(car$agecat),base=3) ## agecat=3 base level
area.x <- C(factor(car$area),base=3) ## area C is 3rd level
gender.x <- C(factor(car$gender),base=2) ## gender M is 2nd level
veh_body.x <- C(factor(car$veh_body),base=10) ## SEDAN is 10th level

car <- cbind(car,valuecat, age.x,area.x,gender.x,veh_body.x)

# Now, use use gam in gam package:
# install.packages("gam")
library(gam)
model1 <- gam(clm ~ age.x + area.x + veh_body.x + s(veh_value), family=binomial, data=car)
summary(model1)

# Generate the plots for the model output
png('c:/Users/Nate/Git/riemann/glm_insurance_gam_output.png')
par(mfrow=c(2,2))
plot(model1)
dev.off()

# Note the highly nonlinear effect of vehicle value, with a peak around 4 ($40,000),
# can be seen clearly. The gamlss implementation gives parameter estimates for the parametric 
# explanatory variables, which are similar to those given by proc gam.

# On the other hand, I can use gamlss
# install.packages("gamlss")
library(gamlss)

model2 <- gamlss(clm ~ age.x + area.x + veh_body.x + cs(veh_value), family=BI, data=car)
# Note that this step can take some time also
summary(model2)