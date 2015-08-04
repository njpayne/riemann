# Example - South African Heart Disease
# Elements of Statistical Learning - Page 149
# Reinforcing learning for Basis expansions and regularizations

# First, refresh things ... 
# Loading the South African heart disease data and 
# mak an ordinary logistic regression estimation of 
# the response (chd)

SA <- read.table("c:/Users/Nate/Git/riemann/SAheart.txt",header=TRUE,sep=",")
SA.glm <- glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family=binomial,data=SA)
summary(SA.glm)

# To obtain the estimate where we expand the effects in terms of natural
# cubic splines, we can use the spline library and ns (natural splines) to 
# build a the required X-matrix. We use four degrees of freedom 

# Note that, as in the book, I am using four natural spline bases for each term 
# in the model. For example, with X1 representing sbp, h1(X1) is a basis consisting 
# of four basis functions. This actually implies three rather than two interior 
# knots (chosen at uniform quantiles of sbp), plus two boundary knots at the 
# extremes of the data, since we exclude the constant term from each of the hj .
# Since famhist is a two-level factor, it is coded by a simple binary or
# dummy variable, and is associated with a single coefficient in the fit of the
# model.

library(splines)
degf  <- 4 # Use 4 degrees of freedom
X <- cbind(ns(SA$sbp,df=degf),
           ns(SA$tobacco,df=degf),
           ns(SA$ldl,df=degf),
           as.numeric(SA$famhist)-1,
           ns(SA$obesity,df=degf),
           ns(SA$age,df=degf))

# To obtain exactly the same graph as in Figure 5.4 in the elements of statistical learning, 
# one needs to center the columns of X. The intercept column (column of ones) is 
# of course not centered. This simply produces another basis for the column space. 
X <- cbind(rep(1,dim(SA)[1]),scale(X,scale=FALSE))

# We obtain the coefficients, we use glm.fit (which is usually called implicitly via glm). 
SA.glm2 <- glm.fit(X,SA$chd,family=binomial())
coeff <- SA.glm2$coefficients

# The (asymptotic) variance-covariance matrix for the 22 parameters 
# is obtained from the inverse of the second derivative of the 
# minus-log-likelihood function. We construct functions for the estimated
# expected value as well as the point-wise standard error. Then we produce 
# a plot which compares with Figure 5.4 in the textbook

# Note that , as stated in the textbook, fitted natural-spline functions for 
# each of the terms in the final model selected by the stepwise procedure. 
# Included are pointwise standard-error bands. The rug plot at the base of 
# each figure indicates the location of each of the
# sample values for that variable (jittered to break ties).

S <-  solve(t(X) %*% diag(SA.glm2$weights) %*% X)
h1 <- X[,2:5] %*% coeff[2:5]
se1 <- sqrt(diag(X[,2:5] %*% S[2:5,2:5] %*% t(X[,2:5])))

h2 <- X[,6:9] %*% coeff[6:9]
se2 <- sqrt(diag(X[,6:9] %*% S[6:9,6:9] %*% t(X[,6:9])))

h3 <- X[,10:13] %*%  coeff[10:13]
se3 <- sqrt(diag(X[,10:13] %*% S[10:13,10:13] %*% t(X[,10:13])))

h4 <- X[,14] *  coeff[14]
se4 <- sqrt(X[,14] * S[14,14] * X[,14])

h5 <- X[,15:18] %*%  coeff[15:18]
se5 <- sqrt(diag(X[,15:18] %*% S[15:18,15:18] %*% t(X[,15:18])))

h6 <- X[,19:22] %*%  coeff[19:22]
se6 <- sqrt(diag(X[,19:22] %*% S[19:22,19:22] %*% t(X[,19:22])))

# Generate the resulting plot which mirrors that in figure 5.4 in the text
png("c:/Users/Nate/Git/riemann/South_African_Spline.png") # Start the PNG

par(mfrow=c(3,2), mar=c(4, 4, 2, 1), oma=c(0, 0, 2, 0))

plot(sort(SA$sbp),
     h1[order(SA$sbp)],
     type="l",
     ylim=c(-1,4),
     xlab="sbp",
     ylab="f(sbp)", 
     col=3)
lines(sort(SA$sbp),h1[order(SA$sbp)]+2*se1[order(SA$sbp)])
lines(sort(SA$sbp),h1[order(SA$sbp)]-2*se1[order(SA$sbp)])
rug(jitter(SA$sbp), col=2)
polygon(c(sort(SA$sbp), rev(sort(SA$sbp))), 
        c(h1[order(SA$sbp)]+2*se1[order(SA$sbp)], 
          rev(h1[order(SA$sbp)]-2*se1[order(SA$sbp)])), col = "bisque", border = NA)
lines(sort(SA$sbp),h1[order(SA$sbp)], col=3) # Plot the main line again

plot(sort(SA$tobacco),
     h2[order(SA$tobacco)],
     type="l",
     ylim=c(-1,8),
     xlab="tobacco",
     ylab="f(tobacco)", 
     col=3)
lines(sort(SA$tobacco),h2[order(SA$tobacco)]+2*se2[order(SA$tobacco)])
lines(sort(SA$tobacco),h2[order(SA$tobacco)]-2*se2[order(SA$tobacco)])
rug(jitter(SA$tobacco), col=2)
polygon(c(sort(SA$tobacco), rev(sort(SA$tobacco))), 
        c(h2[order(SA$tobacco)]+2*se2[order(SA$tobacco)], 
          rev(h2[order(SA$tobacco)]-2*se2[order(SA$tobacco)])), col = "bisque", border = NA)
lines(sort(SA$tobacco),h2[order(SA$tobacco)], col=3) # Plot the main line again

plot(sort(SA$ldl),
     h3[order(SA$ldl)],
     type="l",
     ylim=c(-4,4),
     xlab="ldl",
     ylab="f(ldl)",
     col=3)
lines(sort(SA$ldl),h3[order(SA$ldl)]+2*se3[order(SA$ldl)])
lines(sort(SA$ldl),h3[order(SA$ldl)]-2*se3[order(SA$ldl)])
rug(jitter(SA$ldl), col=2)
polygon(c(sort(SA$ldl), rev(sort(SA$ldl))), 
        c(h3[order(SA$ldl)]+2*se3[order(SA$ldl)], 
          rev(h3[order(SA$ldl)]-2*se3[order(SA$ldl)])), col = "bisque", border = NA)
lines(sort(SA$ldl),h3[order(SA$ldl)], col=3) # Plot the main line again

plot(sort(SA$famhist),
     h4[order(SA$famhist)],
     type="l",
     ylim=c(-1,1),
     xlab="famhist",
     ylab="f(famhist)",
     col=3)
points(sort(SA$famhist),h4[order(SA$famhist)]+2*se4[order(SA$famhist)])
points(sort(SA$famhist),h4[order(SA$famhist)]-2*se4[order(SA$famhist)])

plot(sort(SA$obesity),
     h5[order(SA$obesity)],
     type="l",
     ylim=c(-2,6),
     xlab="obesity",
     ylab="f(obesity)",
     col=3)
lines(sort(SA$obesity),h5[order(SA$obesity)]+2*se5[order(SA$obesity)])
lines(sort(SA$obesity),h5[order(SA$obesity)]-2*se5[order(SA$obesity)])
rug(jitter(SA$obesity), col=2)
polygon(c(sort(SA$obesity), rev(sort(SA$obesity))), 
        c(h5[order(SA$obesity)]+2*se5[order(SA$obesity)], 
          rev(h5[order(SA$obesity)]-2*se5[order(SA$obesity)])), col = "bisque", border = NA)
lines(sort(SA$obesity),h5[order(SA$obesity)], col=3) # Plot the main line again

plot(sort(SA$age),
     h6[order(SA$age)],
     type="l",
     ylim=c(-6,2),
     xlab="age",
     ylab="f(age)",
     col=3)
lines(sort(SA$age),h6[order(SA$age)]+2*se6[order(SA$age)])
lines(sort(SA$age),h6[order(SA$age)]-2*se6[order(SA$age)])
rug(jitter(SA$age), col=2)
polygon(c(sort(SA$age), rev(sort(SA$age))), 
        c(h6[order(SA$age)]+2*se6[order(SA$age)], 
          rev(h6[order(SA$age)]-2*se6[order(SA$age)])), col = "bisque", border = NA)
lines(sort(SA$age),h6[order(SA$age)], col=3) # Plot the main line again

title("Fitted Natural Spline Functions (South African Heart Data)", outer = TRUE, cex = 1.5)
dev.off()

#Using gam (generalized additive models) is an alternative to a fixed 
#spline basis, which is sometimes also called regression splines, and with gam we use 
#instead smoothing splines meaning that the smootheness is controlled by 
#a penalty parameter. This parameter is specified through the df parameter below. 
#The resulting plot is comprable with Figure 5.4.
