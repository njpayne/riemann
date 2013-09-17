# Below is an example of a CART with tree - Single Explanatory Variable
# Code compiled September 16, 2013

# ---------------------------------------------------------------------------
# Some background ...

# Tree models are computationally intensive techniques for recursively 
# partitioning response variables into subsets based on their relationship to 
# one or more (usually many) predictor variables.

# When would you use a CART model rather than a GLM or GAM?  
# The recursive structure of CART models is ideal for uncovering complex 
# dependencies among predictor variables.  If the effect of, say, soil moisture 
# content depends strongly on soil texture in nonlinear fashion, CART models of 
# species occurrences have a better shot at detecting this than interaction terms 
# in GLMs or even GAMs.  When you have good reason to suspect non-additive 
# interactions among variables, or have many variables and don't know what to expect, 
# try a tree model.  If you only have a few variables and generally expect 
# simple linear or curvilinear functions with relatively simple (or no) interactions, 
# tree models will only return approximations of the actual relationship 
# (bumpy as opposed to smooth) using too many parameters.  Tree models also have a 
# tendency to overfit (i.e., error is fitted along with the data), and thus lead 
# to over-interpretation.  Finally, because they do not involve fitting a 
# smooth function to data, tree model output can be overly sensitive to small changes 
# in the input variables.  

# ---------------------------------------------------------------------------
# Meta-data for the tree dataset that I am using
# The dataset includes tree abundances from a subset of a vegetation database of Great Smoky Mountains National Park (TN, NC).
# plotID: unique code for each spatial unit (note some sampled more than once)
# date: when species occurrence recorded
# plotsize: size of quadrat in m2
# spcode: unique 7-letter code for each species
# species: species name
# cover: local abundance measured as estimated horizontal cover (ie, relative area of shadow if sun is directly above) classes 1-10 are: 1=trace, 2=0-1%, 3=1-2%, 4=2-5%, 5=5-10%, 6=10-25%, 7=25-50%, 8=50-75%, 9=75-95%, 10=95-100%
# utme: plot UTM Easting, zone 17 (NAD27 Datum)
# utmn: plot UTM Northing, zone 17 (NAD27 Datum)
# elev: elevation in meters from a digital elevation model (10 m res)
# tci: topographic convergence index, or site "water potential"; measured as the upslope contributing area divided by the tangent of the slope angle (Beven and Kirkby 1979)
# streamdist: distance of plot from the nearest permanent stream (meters)
# disturb: plot disturbance history (from a Park report); CORPLOG=corporate logging; SETTLE=concentrated settlement, VIRGIN="high in virgin attributes", LT-SEL=light or selective logging
# beers: transformed slope aspect ('heat load index'); 0 is SW (hottest), 2 is NE (coolest)
# ---------------------------------------------------------------------------

# Start with the tree data set and focus on the distribution of red oak
# Construct a dataset that includes absences
dat = read.csv("c:/Users/Nate/Git/riemann/treedata.csv",header=TRUE)     #grab the treedata.csv file
dat2 = subset(dat,dat$species=="Quercus rubra") # Select a red oak subset
ro.plots = unique(as.character(dat2$plotID))    # Select all plots with red oak
u.plots = unique(as.character(dat$plotID)) # Select all plots
nob.plots = u.plots[is.element(u.plots,ro.plots)==F] # Select all plots without red oak
dat3 = subset(dat,is.element(as.character(dat$plotID),nob.plots)) # Create a dataset with no red oak
dat4 = subset(dat3,duplicated(dat3$plotID)==F)  # Select one row per plot
dat4$cover = 0     #cover of red oak is zero in these plots
rodat = rbind(dat2,dat4) # Create a new dataframe of presences and absences

# Use one predictor variable to model red oak cover class
rt1 = tree(cover~elev,data=rodat) 
rt1 # Look at the model
summary(rt1) # Generate a summary of the regression tree
# Note that the root level shows the total number of observations

# Generate the response deviance or the responce sum of squares
sum(sapply(rodat$cover,function(x)(x-mean(rodat$cover))^2))
# Generated value as 7405.374

# Generate the total residual deviace (which is the residual sum of squares)
sum(sapply(resid(rt1),function(x)(x-mean(resid(rt1)))^2))
# Generated value was 6521.413

# Plot the tree and review the output
plot(rt1) # Plot the tree
text(rt1) # Add text to the tree
# I note that there are three terminal nodes produced by two splits.  
# The first partitioned the data into two sets of above and below 1509.5 m elevation.  
# The second took the latter group and partitioned it again into above and below 678m 
# elevation.

# Plot comparing our predictions against our observations
png('c:/Users/Nate/Git/riemann/Elev_vs_Cover.png')
with(rodat,plot(elev,cover, 
                main="Plot Comparing Predictions vs Observations",
                xlab="Elevation",
                ylab="Cover"))
x = seq(0,2000)
lines(x,predict(rt1,newdata=list(elev=x)),col="lightblue",lwd=3)
dev.off()
# The plot shows that the highest elevation set has virtually no variance 
# because these are (almost) all absences: the residual deviance of this 
# terminal node is low (87.51). The other two terminal nodes do a poor job of 
# predicting red oak abundance, with residual deviances of 5379 (middle elevations) 
# and 1055 (lowest elevations).

# Our tree above only stopped with three leaves. Run the model again using
# the prune.tree function and reduce the minimum residual deviance
rt2 = tree(cover~elev,data=rodat,control=tree.control(1039,mindev=.003))
plot(prune.tree(rt2),
     main="Plot of Residual Deviance vs Tree Size")
abline(v=3,col="red")
# The output shows that deviance continues to decrease as we add splits 
# (we can do this until each node equals one datum), but with generally 
# decreasing returns.  

#To use a data-based criterion of when to stop the splitting, tree models use a 
# cross-validation technique that splits the data into 'training' set for model 
# fitting and a validation set to evaluate goodness of fit.  The default is a 
# '10-fold' cross-validation, meaning that 10% of the data is left out of each 
# training run.  Use cv.tree to automate the process.
plot(cv.tree(rt2))

# Let us now compare the tree model to a simple second-order GLM
png('c:/Users/Nate/Git/riemann/Elev_vs_Cover_GLM_CV.png')
glm1 = glm(cover~elev+I(elev^2),data=rodat,family=poisson)
with(rodat,plot(elev,cover,
                main="Plot comparing GLM with CV Tree Model",
                xlab="Elevation",
                ylab="Cover"))
x = seq(0,2000)
lines(x,predict(rt1,newdata=list(elev=x)),col="lightblue",lwd=3)
lines(x,predict(glm1,newdata=list(elev=x),type="response"),col="orange",lwd=3)
dev.off() # Turn the device off

# Compare their pseudo-R2s
1-(deviance(rt1)/7405)
# Pseudo-R^2 = 0.119323
1-(glm1$dev/glm1$null)
# Pseudo-R^2 = 0.107073

# As is shown above, the tree model is slightly more predictive, mostly due to its 
# ability to match the zero abundances above 1500 m.
