# Experimenting with logistic regression - one numerical predictor
# October 10, 2013

# In the "MASS" library there is a data set called "menarche" (Milicer, H. and Szczotka, F., 
# 1966, Age at Menarche in Warsaw girls in 1965, Human Biology, 38, 199-203), in which there are 
# three variables: "Age" (average age of age homogeneous groups of girls), "Total" 
# (number of girls in each group), and "Menarche" (number of girls in the group who have 
# reached menarche).

library("MASS") # Let's look at the MASS library
data(menarche) # Grab the data menarche
str(menarche)
summary(menarche) # Generate a summary of the data

# Let's generate a plot of the data
png('c:/Users/Nate/Git/riemann/logistic_regression_menarche_plot.png')
plot(Menarche/Total ~ Age, 
     data=menarche,
     main = "Reviewing the Menarche Data Set: MASS Pkg")
dev.off()

# From the graph at right, it appears a logistic fit is called for here. 
# The fit would be done this way...
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)

# Some commentary. Firstly, glm( ) is the function used to do generalized linear models.
# With "family=" set to "binomial" with a "logit" link, glm( ) produces a logistic regression. 
# Because we are using glm( ) with binomial errors in the response variable, the ordinary 
# assumptions of least squares linear regression (normality and homoscedasticity) don't apply. 
# Second, our data frame does not contain a row for every case (i.e., every girl upon whom data 
# were collected). Therefore, we do not have a binary (0,1) coded response variable. This is not a problem! 
# If we feed glm( ) a table (or matrix) in which the first column is number of successes 
# and the second column is number of failures, R will take care of the coding for us. 
# In the above analysis, we made that table on the fly inside the model formula by binding "Menarche" 
# and "Total ??? Menarche" into the columns of a table using cbind( ).

# Lets look at how closely the fitted values from our logistic regression match the observed values...

png('c:/Users/Nate/Git/riemann/logistic_regression_menarche_plot_fitted.png')
plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")
dev.off()

# Let's look more closely at the results
summary(glm.out)

# The following requests also produce useful results
glm.out$coef # Review the coefficients
glm.out$fitted # Review the fitted variables
glm.out$resid # Review the residuals
glm.out$effects # Review the effects
anova(glm.out) # Generate the analysis of variance output

# Recall that the response variable is log odds, so the coefficient of "Age" can be interpreted as 
# "for every one year increase in age the odds of having reached menarche 
# increase by ...

exp(1.632) # This is equal to 5.11

# To evaluate the overall performance of the model, look at the null deviance and residual 
# deviance near the bottom of the print out. Null deviance shows how well the response 
# is predicted by a model with nothing but an intercept (grand mean). This is essentially a 
# chi square value on 24 degrees of freedom, and indicates very little fit (a highly significant 
# difference between fitted values and observed values). Adding in our predictors--just "Age" 
# in this case--decreased the deviance by 3667 points on 1 degree of freedom. Again, this is 
# interpreted as a chi square value and indicates a highly significant decrease in deviance. 
# The residual deviance is 26.7 on 23 degrees of freedom. We use this to test the overall fit 
# of the model by once again treating this as a chi square value. A chi square of 26.7 on 23 
# degrees of freedom yields a p-value of 0.269. The null hypothesis (i.e., the model) is not 
# rejected. The fitted values are not significantly different from the observed values.