# As was noted online, there are  two common packages for CART models in R: tree and rpart.
# Tree was argued to be easier to use because it was based on the familiar deviance 
# statistics; on the other hand, rpart output is more difficult to compare to GLM and 
# GAM alternatives. It is also argued to produce different results. Thus, I wanted to 
# test this hypothesis and see whether these routines did  produce different results 
# for the same dataset

# Start with the same tree data set and focus on the distribution of red oak
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
library(rpart) # Use the rpart library
rt3 = rpart(cover~elev,data=rodat, method="anova") 
# Note that for the rpart model, "class" is a classification tree
# On the other hand "anova" refers to a regression tree
rt3 # Look at the model
summary(rt3) # Generate a summary of the regression tree
# Note that the root level shows the total number of observations

# The format of the output table is similar as that for tree, 
# but note we have very different results from the tree model; 
# And we have lost information about deviance (SSE); 
# rpart uses instead a measure of statistical inequality called the Gini 
# coefficient (default) or a metric based on information theory 
# (parms=list(split="information")). Clearly the choice of splitting depends upon 
# the metric used.  The cost of adding another split to the tree is assessed 
# with the 'complexity' parameter (cp).  The minimum cp has a default value of 0.01 
# and can be accessed in a similar way to the 'mindev' parameter of tree. 
  
rp4 = rpart(cover~elev,data=rodat, method="anova", control=rpart.control(cp=0.005))
# Note that a call to 'rp2' generates a very large tree. 
# I will plot it with the same plot and text functions
plot(rp4); text(rp4, cex=0.3)

#Pruning the tree is accomplished with cross-validation as before, where the 
# relative error rate of 'dropping' the out-of-bag samples through the existing 
# tree is plotted with standard errors for each split:
plotcp(rp4)

# Here it looks as though relative error is minimized for a tree of 5 nodes.  
# Here, I return the pruned tree with prune and the cp associated with our desired 
# tree size:
printcp(rp4)

# Plot comparing our predictions against our observations
png('c:/Users/Nate/Git/riemann/Elev_vs_Cover_Rpart.png')
with(rodat,plot(elev,cover, 
                main="Plot Comparing Predictions vs Observations",
                xlab="Elevation",
                ylab="Cover"))
x = seq(0,2000)
lines(x,predict(rt1,newdata=list(elev=x)),col=2,lwd=3)
lines(x,predict(rp4,newdata=list(elev=x)),col=3,lwd=3)
lines(x,predict(glm1,newdata=list(elev=x),type="response"),col=4,lwd=3)
legend(210,9.25 # places a legend at the appropriate place 
c("tree()","rpart()", "glm()"), # puts text in the legend 
lty=c(1,1), # gives the legend appropriate symbols (lines)     
lwd=c(2.5,2.5, 2.5),col=c(2,3,4)) # Gives the legend lines correct colors and width
dev.off()

# Determine an approximate R^2 for the model
rsq.rpart(rp4)
tmp <- printcp(rp4)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),] # 0.2778699
# Note that this R^2 is significantly higher than the tree model ...