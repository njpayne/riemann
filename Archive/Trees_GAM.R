# Stat 852 
# Assignment 3

library(gam) # GAM models
library(rpart) # Regression trees
library(ggplot2) # GGplot2
library(grid) # For layout
library(mgcv) #For use with GAM's
library(car)

# -------------------------------------------------------------------------------
# Question 1

# Read In The Code
diamonds <- read.table("c:/Users/Nate/Git/riemann/diamonds.txt",header=TRUE)
head(diamonds) # Review the code to see what the data looks like
summary(diamonds) # Generate a summary of the data
str(diamonds) # Let's evaluate the structure of the diamonds object
names(diamonds) # What are the names in the data set

# Important to remember that Cert, Clarity, and Color are factors here. Thus
diamonds$Cert <- as.factor(diamonds$Cert)
diamonds$Clarity <- as.factor(diamonds$Clarity)
diamonds$Color <- as.factor(diamonds$Color)

# Generate a pairs plot of the data
png("c:/Users/Nate/Git/riemann/Trees_GAM_Diamonds_Pairs.png") # Start the PNG
pairs(diamonds, pch=21, bg = c("red", "green"))
dev.off()

# Build and prune a regression tree
# Calculate mean squared prediction errors (MSPE) using the 
# 10-folder cross-validation for both trees.

library(rpart) # Load the rpart library

main_tree <- rpart(Price/1000 ~ ., # Create the rpart tree using all variables 
                   data = diamonds, # Data set that we are using 
                   method = "anova") # Possible methods: "anova", "poisson", "class", "exp"

# Plot the full tree
png("c:/Users/Nate/Git/riemann/Trees_GAM_Diamonds_Full_Tree.png")
plot(main_tree)
text(main_tree)
title(main = "Full Regression Tree - Diamond Data (Price / 1000)")
dev.off()

# Calculate the mean squared prediction error for the full tree
# In this example MSPE = (actual - predicted)^2 / n
(main_tree_mspe <- mean((diamonds$Price - predict(main_tree)*1000)^2))

# Let us now prune the regression tree
# To do this, we will try to find the optimal relationship between the error and the complexity
main_tree_prune <- update(main_tree, cp=0.001)
printcp(main_tree_prune)
# we are pruning here using 10-fold cross validation

png("c:/Users/Nate/Git/riemann/Trees_GAM_Diamonds_Pruned_Tree_CP.png")
plotcp(main_tree_prune,
       minline = TRUE,
       col = "blue")
dev.off()

# Generate the object for the pruned tree
main_tree_prune_act <- prune(main_tree_prune, 
                             cp=main_tree_prune$cptable[6, 1])

# I can also use the following code to automatically find the complexity error
# Look at http://www.statmethods.net/advstats/cart.html
main_tree_prune_act$cptable[which.min(main_tree_prune_act$cptable[,"xerror"]),"CP"]

# Plot the pruned tree tree
png("c:/Users/Nate/Git/riemann/Trees_GAM_Diamonds_Pruned_Tree.png")
plot(main_tree_prune_act)
text(main_tree_prune_act)
title(main = "Pruned Regression Tree - Diamond Data (Price / 1000)")
dev.off()

# In genearal, I note that rpart uses 10-fold cross validation
# http://stat.ethz.ch/education/semesters/ss2012/ams/slides/v10.1.pdf

# Calculate the mean squared prediction error for the pruned tree
# In this example MSPE = (actual - predicted)^2 / n
(pruned_tree_mspe <- mean((diamonds$Price - predict(main_tree_prune_act)*1000)^2))

# Fit a generalized additive model
# Calculate the mean squared prediction error
diamonds_gam <- gam(Price/1000 ~ s(Carats) + factor(Cert) + factor(Clarity) + factor(Color),
                    data=diamonds)
summary(diamonds_gam)

png("c:/Users/Nate/Git/riemann/Trees_GAM_Diamonds_GAM_Resid.png")
par(mfrow=c(2,2))
plot(diamonds_gam, 
     residuals=TRUE, 
     pch=16, 
     cex=0.6, 
     pages = 1)
# title(main = "Plot Of The Output From the GAM Model - Diamonds Data",
#       outer = TRUE)
dev.off()

# Calculate the mean squared prediction error for generalized additive model
# In this example MSPE = (actual - predicted)^2 / n
(gam_diamond_mspe <- mean((diamonds$Price - predict(diamonds_gam)*1000)^2))

# Compare the difference between the two prediction errors
pruned_tree_mspe # Pruned regression tree
gam_diamond_mspe # Generalized additive model

# -------------------------------------------------------------------------------
# Question 2

# Load in the relevant libraries
library(car) # Library car
library(mgcv) # Library MGCV ... for use with GAM's

# Read in the code & review the data
data(trees) # Create an object that is called trees which holds the tree data
str(trees) # Look at the structure of the object
names(trees) # Look at the names in the data

# Create a pairs plot to review the structure of the data
png("c:/Users/Nate/Git/riemann/Trees_GAM_Trees_Pairs.png") # Start the PNG
pairs(trees, pch=21, bg = c("red", "green"))
dev.off()

# Build and prune a regression tree
# Calculate mean squared prediction errors (MSPE) using the 
# 10-folder cross-validation for both trees.

# library(rpart) # Load the rpart library

main_tree_trees <- rpart(Volume ~ ., # Create the rpart tree using Volume as the response
                   data = trees, # Data set that we are using 
                   method = "anova") # Possible methods: "anova", "poisson", "class", "exp"

# Plot the full tree
png("c:/Users/Nate/Git/riemann/Trees_GAM_Tree_Full_Tree.png")
plot(main_tree_trees)
text(main_tree_trees)
title(main = "Full Regression Tree - Tree Data (Response = Volume)")
dev.off()

# Calculate the mean squared prediction error for the full tree
# In this example MSPE = (actual - predicted)^2 / n
(main_tree_trees_mspe <- mean((trees$Volume - predict(main_tree_trees))^2))

# Let us now prune the regression tree
# To do this, we will try to find the optimal relationship between the error and the complexity
main_tree_trees_prune <- update(main_tree_trees, cp=0.001)
printcp(main_tree_trees_prune)
# we are pruning here using 10-fold cross validation

# Generate the object for the pruned tree
main_tree_trees_prune_act <- prune(main_tree_trees_prune, 
                             cp=main_tree_trees_prune$cptable[2, 1])

# Find the optimal cp for the regression tree
png("c:/Users/Nate/Git/riemann/Trees_GAM_Trees_Pruned_Tree_CP.png")
plotcp(main_tree_trees_prune,
       minline = TRUE,
       col = "blue")
dev.off()

# Plot the pruned regression tree
# Plot the full tree
png("c:/Users/Nate/Git/riemann/Trees_GAM_Tree_Pruned_Tree.png")
plot(main_tree_trees_prune_act)
text(main_tree_trees_prune_act)
title(main = "Pruned Regression Tree - Tree Data (Response = Volume)")
dev.off()

# Calculate the mean squared prediction error for the pruned tree
# In this example MSPE = (actual - predicted)^2 / n
(main_tree_trees_prune_mspe <- mean((trees$Volume - predict(main_tree_trees_prune_act))^2))

# Let's switch to GAM models now ...
tree_gam <- gam(Volume ~ s(Height) + s(Girth), data=trees)
summary(tree_gam)

# Let's try to plot a summary of the model fit
png("c:/Users/Nate/Git/riemann/Trees_GAM_Trees_GAM_Resid.png")
plot(tree_gam, 
     residuals=TRUE, 
     pch=16, 
     cex=0.6, 
     pages = 1)
title(main = "Plot Of The Output From the GAM Model")
dev.off()

# What about trying a semi-parametric fit here?
tree_gam_semi_para <- gam(Volume ~ Height + s(Girth), data=trees) 
summary(tree_gam_semi_para)

# Calculate the mean squared prediction error for the revised & more improved GAM model
# In this example MSPE = (actual - predicted)^2 / n
(main_tree_trees_gam <- mean((trees$Volume - predict(tree_gam_semi_para))^2))

# -------------------------------------------------------------------------------
# Question 3

# Read In The Code
SA <- read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", 
                 sep=",",head=T,row.names=1)
names(SA) # Review the names in the code
str(SA) # Look at the structure of the object

SA$famhist <- as.factor(SA$famhist) # Ensure that famhist is a factor
SA$chd <- as.factor(SA$chd) # Ensure that chd is a factor

str(SA) #Let's review the structure of the data

# Generate a pairs plot of the data
png("c:/Users/Nate/Git/riemann/Trees_GAM_SA_Pairs.png") # Start the PNG
pairs(SA, pch=21, bg = c("red", "green"))
dev.off()

# Let's switch to GAM models now ...
SA_gam <- gam(chd ~ s(sbp) + s(tobacco) + s(ldl) + s(adiposity) + factor(famhist) + s(typea) + s(obesity) + s(alcohol) + s(age), data=SA)

summary(SA_gam) # Review a summary of the model

# Let's try to plot a summary of the model fit
png("c:/Users/Nate/Git/riemann/Trees_GAM_Trees_GAM_Resid.png")
plot(tree_gam, 
     residuals=TRUE, 
     pch=16, 
     cex=0.6, 
     pages = 1)
title(main = "Plot Of The Output From the GAM Model")
dev.off()

# I will attempt to do somewhat of a stepwise elimination here to see if we can improve it
# Let's switch to GAM models now ...

# Model 1 without alcohol
SA_gam_1 <- gam(chd ~ s(sbp) + s(tobacco) + s(ldl) + s(adiposity) + factor(famhist) + s(typea) + s(obesity) + s(age), 
              data=SA) # Without alcohol

# Model 1 without alcohol & adiposity
SA_gam_2 <- gam(chd ~ s(sbp) + s(tobacco) + s(ldl) + factor(famhist) + s(typea) + s(obesity) + s(age), 
                data=SA) # Without alcohol & adiposity

# Model 1 without alcohol & adiposity & obsesity
SA_gam_3 <- gam(chd ~ s(sbp) + s(tobacco) + s(ldl) + factor(famhist) + s(typea) + s(age), 
                data=SA) # Without alcohol & adiposity & obesity

# Model 1 without alcohol & adiposity & obsesity & sbp
SA_gam_4 <- gam(chd ~ s(tobacco) + s(ldl) + factor(famhist) + s(typea) + s(age), 
                data=SA) # Without alcohol & adiposity & obesity

summary(SA_gam_1) # Generate a summary of the model
summary(SA_gam_2) # Generate a summary of the model
summary(SA_gam_3) # Generate a summary of the model
summary(SA_gam_4) # Generate a summary of the model

# Let's compare the models together
anova(SA_gam_1, SA_gam_4, test="F")

# Let's look at the AIC of various models as a check
AIC(SA_gam_1, SA_gam_2, SA_gam_3, SA_gam_4)



