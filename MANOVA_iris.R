# ---------------------------------------------------------------------------------------- 
# Let us now try do do a Multivariate Analysis of Variance (MANOVA)
#
#####################################################################
#####################################################################


# Consider the built-in "iris" data set in R:

# Attaching the data frame:

attach(iris)

iris.manova <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species)

# Doing the MANOVA test using Wilks' Lambda:

summary(iris.manova, test="Wilks")

# Using the other test statistics:

summary(iris.manova, test="Roy")
summary(iris.manova, test="Hotelling-Lawley")
summary(iris.manova, test="Pillai")

# We have strong evidence that the mean vectors differ across the 3 species.

# Checking model assumption of normality:

###############
###
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
###
###############

chisplot(residuals(iris.manova))

# No strong evidence against normality -- we are safe.

# Examining the sample covariance matrices for each group:

by(iris[,-5], Species, var)

# Is there evidence that the covariance matrices are significantly different across the 3 species?

##### Differences in Means for doing Bonferroni multiple comparisons:

means.by.grps <- cbind(tapply(Sepal.Length,Species,mean), tapply(Sepal.Width,Species,mean), tapply(Petal.Length,Species,mean), tapply(Petal.Width,Species,mean) )

## The matrix E:

my.n <- nrow(iris[,-5])

E <- (my.n - 1)*var(residuals(iris.manova))
E
