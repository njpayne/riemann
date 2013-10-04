# Investigating MANOVA
# October 4, 2013

# Let us investigate Multivariate Analysis of Variance (MANOVA) using the 'iris data'
# Note that the iris data set is built in with R

attach(iris) # Attaching the data frame
iris.manova <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species)

summary(iris.manova, test="Wilks") # Let us do the MANOVA test using Wilks' Lambda

# Wilks' lambda distribution (named for Samuel S. Wilks), is a probability distribution used in 
# multivariate hypothesis testing, especially with regard to the likelihood-ratio test and 
# Multivariate analysis of variance. It is a multivariate generalization of the univariate 
# F-distribution, and generalizes the F-distribution in the same way that the Hotelling's 
# T-squared distribution generalizes Student's t-distribution.
# ... Wilks' lambda distribution is related to two independent Wishart distributed variables

# Note that we could use other test statistics
summary(iris.manova, test="Roy")
summary(iris.manova, test="Hotelling-Lawley")
summary(iris.manova, test="Pillai")
# All of these seem to show that we have strong evidence that the mean vectors differ across the 3 species.

# Let us now check & model the assumption of normality:
chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  ### determine dimensions
  n <- nrow(x)
  p <- ncol(x)
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n)/(n+1)
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", lwd=2,pch=1)
}

png('c:/Users/Nate/Git/riemann/MANOVA_iris_chisplot.png')
chisplot(residuals(iris.manova))
title(main = "Checking For Evidence Against Normality: MANOVA")
# No strong evidence against normality -- we are safe.
dev.off()

# Examining the sample covariance matrices for each group:
by(iris[,-5], Species, var)

# Is there evidence that the covariance matrices are significantly different across the 3 species?
# Differences in Means for doing Bonferroni multiple comparisons
means.by.grps <- cbind(tapply(Sepal.Length,Species,mean), tapply(Sepal.Width,Species,mean), tapply(Petal.Length,Species,mean), tapply(Petal.Width,Species,mean) )

# The matrix E
my.n <- nrow(iris[,-5])
(E <- (my.n - 1)*var(residuals(iris.manova))) # Print the matrix E

