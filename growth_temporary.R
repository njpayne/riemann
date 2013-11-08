# Looking at growth curves
# October 8, 2013
# Nathaniel Payne

library(fda) #Load the data from Ramsay's book
str(growth) # Note that the file we are interested in is the growth file
# The growth data set includes height measurements of 54 girls over 31 different time points

# Let us understand the data
# A list containing the heights of 39 boys and 54 girls from age 1 to 18 and 
# the ages at which they were collected.
# This list contains the following components:
# - hgtm a 31 by 39 numeric matrix giving the heights in centimeters of 39 boys at 31 ages.
# - hgtf a 31 by 54 numeric matrix giving the heights in centimeters of 54 girls at 31 ages.
# - age a numeric vector of length 31 giving the ages at which the heights were measured.

# Let us look at the original data
png('c:/Users/Nate/Git/riemann/growth_summary.png')
op <- par(cex=1.1)
with(growth, matplot(age, hgtf, type="l", 
                     ylab="Height (cm.)"))
title(main="Plot Showing 54 Female Growth Curves (Ages:1-18)")
dev.off()

# If you would like smaller subsets, please use this command (example subset 1:10)
#with(growth, matplot(age, hgtf[, 1:10], type="b", pch="o", 
#                     ylab="Height (cm.)") )

# --------------------------------------------------------------------------------------
#  Generate a phase-plane diagram for the growth data
gr.basis = create.bspline.basis(norder=6, breaks=growth$age)
children = 1:54 # Include all children
ncasef   = length(children)
cvecf           = matrix(0, gr.basis$nbasis, ncasef)
dimnames(cvecf) = list(gr.basis$names,
                       dimnames(growth$hgtf)[[2]][children])

gr.fd0      = fd(cvecf, gr.basis)
gr.fdPar1.5 = fdPar(gr.fd0, Lfdobj=3, lambda=10^(-1.5))
hgtfmonfd   = with(growth, smooth.monotone(age, hgtf[,children],
                                           gr.fdPar1.5) )
agefine = seq(1,18,len=101)
(i11.7  = which(abs(agefine-11.7) == min(abs(agefine-11.7)))[1])

velffine = predict(hgtfmonfd, agefine, 1);
accffine = predict(hgtfmonfd, agefine, 2);

png('c:/Users/Nate/Git/riemann/growth_phase_plane_diag.png')
plot(velffine, accffine, type='n', xlim=c(0, 12), ylim=c(-5, 2),
     xlab='Velocity (cm/yr)', ylab=expression(Acceleration (cm/yr^2)),
     las=1,
     main = "Phase Plane Diagram For Growth Data")
for(i in 1:10){
  lines(velffine[, i], accffine[, i])
  points(velffine[i11.7, i], accffine[i11.7, i])
}
abline(h=0, lty='dotted')
dev.off()

# What we see is that girls with early pubertal growth spurts, having marker circles
# near the end of their trajectories, have intense spurts, indicated by the size of their
# loops. Late-spurt girls have tiny loops. The net effect is that the adult height of girls
# is not much affected by the timing of the growth spurt, since girls with late spurts
# have the advantage of a couple of extra years of growth, but the disadvantage of a
# weak spurt. A hint of the complexity of growth dynamics in infancy is given by the
# two girls whose curves come from the right rather than from the bottom of the plot.

# --------------------------------------------------------------------------------------
# Regression Splines
# Define the range of the ages and set up a fine mesh of ages
ageRng  = c(1,18)
age     = growth$age
agefine = seq(1,18,len=501)

#  Set up order 6 spline basis with 12 basis functions for
#  fitting the growth data so as to estimate acceleration
nbasis = 12;
norder =  6;
heightbasis12 = create.bspline.basis(ageRng, nbasis, norder)

#  Fit the data by least squares
basismat   = eval.basis(age, heightbasis12)
heightmat  = growth$hgtf
heightcoef = lsfit(basismat, heightmat, intercept=FALSE)$coef

# Fit the data using function smooth_basis, which does the same thing.
# Note that The R function smooth.basis returns an object heightlist of the list class
heightList = smooth.basis(age, heightmat, heightbasis12)
heightfd   = heightList$fd
height.df  = heightList$df
height.gcv = heightList$gcv

# The three most important returned objects are the following, where the
# names in bold type are used in each language to retrieve the objects:
# - fd: An object of class fd containing the curves that fit the data.
# - df: The degrees of freedom used to define the fitted curves.
# - gcv: The value of the generalized cross-validation criterion: a measure of lack
# of fit discounted for degrees of freedom. If there are multiple curves, a vector
# is returned containing gcv values for each curve.

# Here is the corresponding R code for computing this matrix for the growth data
age = growth$age
heightbasismat = eval.basis(age, heightbasis12)
y2cMap = solve(crossprod(heightbasismat),
               t(heightbasismat))

# The regression approach to smoothing data only works if the number K of basis
# functions is substantially smaller than the number n of sampling points. With
# the growth data, it seems that roughly K = 12 spline basis functions are needed to
# adequately smooth the growth data. Larger values of K will tend to undersmooth
# or overfit the data. Interestingly, after over a century of development of parametric
# growth curve models, the best of these also use about 12 parameters in this example.

# Let us plot the graph showing the fitted data
png('c:/Users/Nate/Git/riemann/growth_regression_spline.png')
op <- par(cex=1.1)
with(growth, matplot(age, hgtf, type="l", 
                     ylab="Height (cm.)"))
title(main="Regression Spline Showing 54 Female Growth Curves")
lines(heightfd)
dev.off()

# Generate a plot of the first derivative (growth rate)
png('c:/Users/Nate/Git/riemann/growth_regression_spline_growth.png')
op <- par(cex=1.1)
plot(deriv(heightfd), xlab="Age", ylab="Growth rate (cm / year)",
     main="Growth Rate: Regression Splines")
dev.off()

# Generate a plot of the second derivative (acceleration rate)
png('c:/Users/Nate/Git/riemann/growth_regression_spline_acceleration.png')
op <- par(cex=1.1)
plot(deriv(heightfd, 2), xlab="Age",
     ylab="Growth Acceleration (cm / year^2)",
     main="Acceleration Rate: Regression Splines")
dev.off()

# --------------------------------------------------------------------------------------
# Smoothing splines
# Data Smoothing with Roughness Penalties
# Smoothing the growth data with a roughness penalty

# A roughness penalty is defined by constructing a functional parameter object consisting of:
# - a basis object,
# - a derivative order m or a differential operator L to be penalized and
# - a smoothing parameter l

# We put these elements together by using the fdPar class in either language and the
# function fdPar to construct an object of that class.
# The following R commands do two things: First they set up an order six B-spline
# basis for smoothing the growth data using a knot at each age. Then they define a
# functional parameter object that penalizes the roughness of growth acceleration by
# using the fourth derivative in the roughness penalty. The smoothing parameter value
# that I have found works well here is l = 0:01:

#  Set up a basis for the growth data 
#  with knots at ages of height measurement
norder      = 6
nbasis      = length(age) + norder - 2
heightbasis = create.bspline.basis(ageRng, nbasis, norder, age)

#  Define a functional parameter object for smoothing
heightLfd    = 4
heightlambda = 0.01
heightfdPar  = fdPar(heightbasis, 4, 0.01)

#  Smooth the data
heightfdSmooth = smooth.basis(age, heightmat, heightfdPar)
heightfd       = heightfdSmooth$fd
# Note that the data are in array heightmat

# --------------------------------------------------------------------------------------
# Let us now choose the smoothing Parameter lambda
# The generalized cross-validation measure GCV developed by Craven and Wahba
# (1979) is designed to locate a best value for smoothing parameter lambda

loglam         = seq(-6, 0, 0.25)
Gcvsave        = rep(NA, length(loglam))
names(Gcvsave) = loglam
Dfsave         = Gcvsave
for(i in 1:length(loglam)){
  hgtfdPari  = fdPar(heightbasis, Lfdobj=4, 10^loglam[i])
  hgtSm.i    = smooth.basis(age, heightmat, hgtfdPari)
  Gcvsave[i] = sum(hgtSm.i$gcv)
  Dfsave[i]  = hgtSm.i$df
}

# GCV values often change slowly with log10l near the minimizing value, so that
# a fairly wide range of l values may give roughly the same GCV value. This is a sign
# that the data are not especially informative about the "true" value of l : If so, it is not
# worth investing a great deal of effort in precisely locating the minimizing value, and
# simply plotting GCV over a mesh of log10l might be sufficient. Plotting the function
# GCV(l ) in any case will inform us about the curvature of near its minimum. If
# the data are not telling us all that much about l , then it is surely reasonable to use
# your judgment in working with values which seem to provide more useful results
# than the minimizing value does. Indeed, Chaudhuri and Marron (1999) argue persuasively
# for inspecting data smooths over a range of l values in order to see what
# is revealed at each level of smoothing. However, if a more precise value seems important,
# the function lambda2gcv can be used as an argument in an optimization
# function that will return the minimizing value.

# Let us now plot the smoothing parameter lambda
png('c:/Users/Nate/Git/riemann/growth_smoothing_spline_lambda.png')
plot(loglam, Gcvsave, 'o', las=1, xlab=expression(log[10](lambda)),
     ylab=expression(GCV(lambda)), lwd=2,
     main = "Plot Showing The Parameter Lambda - Smoothing Splines")
dev.off()

#  Compute the monotone smoothing of the Berkeley female growth data.
#  set up ages of measurement and an age mesh
age     = growth$age
nage    = length(age)
ageRng  = range(age)
nfine   = 101
agefine = seq(ageRng[1], ageRng[2], length=nfine)

#  Set the data
hgtf   = growth$hgtf
ncasef = dim(hgtf)[2]

# Note that We set up an order 6 spline basis with knots at ages of observations for their functions
# w, along with a roughness penalty on their third derivatives and a smoothing
# parameter of 1=sqrt(10), using these commands:
#wbasis = create.bspline.basis(c(1,18), 35, 6, age)
#growfdPar = fdPar(wbasis, 3, 10^(-0.5))

#  An order 6 bspline basis with knots at ages of measurement
norder = 6
nbasis = nage + norder - 2
wbasis = create.bspline.basis(ageRng, nbasis, norder, age)

#  Define the roughness penalty for function W
Lfdobj    = 3          #  penalize curvature of acceleration
lambda    = 10^(-0.5)  #  smoothing parameter
cvecf     = matrix(0, nbasis, ncasef)
Wfd0      = fd(cvecf, wbasis)
growfdPar = fdPar(Wfd0, Lfdobj, lambda)

# Now, note that the monotone smoothing of the data in the 31 by 54 matrix hgtf, and the extraction
# of the the functional data object Wfd for the wi functions, the coefficients b0i;b1i
# and the functional data object hgtfhatfd for the functions fitting the data are
# achieved by the following commands
# growthMon = smooth.monotone(age, hgtf, growfdPar)
# Wfd = growthMon$Wfd
# betaf = growthMon$beta
# hgtfhatfd = growthMon$yhatfd

#  Perform monotone smoothing (This takes time!!!)
growthMon = smooth.monotone(age, hgtf, growfdPar)
# Note that this step takes a while for all 54 girls

Wfd        = growthMon$Wfd
betaf      = growthMon$beta
hgtfhatfd  = growthMon$yhatfd # Holds the fitted data ... y_hat

# Determine the first derivative of the data ... for growth
accelfdUN_first     = deriv.fd(hgtfhatfd)
accelmeanfdUN_first = mean(accelfdUN_first)

# Determine the second derivative of the data ... for acceleration
accelfdUN_second     = deriv.fd(hgtfhatfd, 2)
accelmeanfdUN_second = mean(accelfdUN_second)

# Let us plot the graph showing the fitted data
png('c:/Users/Nate/Git/riemann/smoothing_spline_fitted.png')
op <- par(cex=1.1)
with(growth, matplot(age, hgtf, type="l", 
                     ylab="Height (cm.)"))
title(main="Smoothing Spline Showing 54 Female Growth Curves")
lines(hgtfhatfd)
dev.off()

# Generate a plot of the first derivative (growth rate)
png('c:/Users/Nate/Git/riemann/smoothing_spline_growth.png')
op <- par(cex=1.1)
plot(accelfdUN_first, xlab="Age", ylab="Growth Rate (cm / year)",
     main="Growth Rate: Smoothing Splines")
dev.off()

# Generate a plot of the second derivative (acceleration rate)
png('c:/Users/Nate/Git/riemann/smoothing_spline_acceleration.png')
op <- par(cex=1.1)
plot(accelfdUN_second, xlab="Age",
     ylab="Acceleration (cm / year^2)",
     main="Acceleration Rate: Smoothing Splines")
dev.off()

# --------------------------------------------------------------------------------------
# Comparing boys vs girls ...
# In the package FDA, I note that there are actually 2 related data sets
# 54 girls and 39 boys
# As a final step, I will construct 2 charts which compare the growth of girls vs boys
# Note that I only include the first 10 data points in each plots to keep the graphs readable

png('c:/Users/Nate/Git/riemann/smoothing_spline_compare_boys_girls.png')
ylim = with(growth, range(hgtm, hgtf))
with(growth, matplot(age, hgtm[, 1:10], type='l',
                     lty='dashed', ylab='Height (cm)'))
with(growth, matlines(age, hgtf[, 1:10], lty='solid'))
legend('topleft', legend=c('girls', 'boys'),
       lty=c('solid', 'dashed'))
title(main = "Comparing Boy vs Girl Height's (Simple Smoothing)")
dev.off()

png('c:/Users/Nate/Git/riemann/smoothing_spline_critical_value.png')
growthbasis = create.bspline.basis(breaks=growth$age, norder=6)
growfdPar = fdPar(growthbasis, 3, 10^(-0.5))

hgtffd = with(growth, smooth.basis(age,hgtf,growfdPar))
hgtmfd = with(growth, smooth.basis(age,hgtm,growfdPar))

# Note that tperm.df() is a permutation t-test for two groups of functional data objects
# tperm.fd creates a null distribution for a test of no difference between two groups of 
# functional data objects.
tres = tperm.fd(hgtffd$fd,hgtmfd$fd)
title(main = "Are Boy Growth Rates Significantly Different From Girls?")
dev.off()