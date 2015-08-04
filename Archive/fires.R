# Project - Fighting forest fires with machine learning ...
# November 12, 2013
# Nathaniel Payne

# install.packages("mda")

library(rpart) # Load the rpart library
library(mda) # Loads the MARS package
library(MASS) # Loads the MASS package for ridge regression

# --------------------------------------------------------------------------------------

# Read In the Data
fires <- read.table("c:/Users/Nate/Git/riemann/forestfires.csv",header=TRUE, sep = ",") # Read in the data
str(fires) # Let's review the structure of the fires data
head(fires) # Review the initial fires data

fires$month <- as.factor(fires$month) # Ensure that month is a factor in the model
fires$day <- as.factor(fires$day) # Ensure that day is a factor in the model

# Preliminary Analysis
colnames(fires) # Review the column names

# Initial Pairs Analysis - Generate a pairs plot of the data
png("c:/Users/Nate/Git/riemann/fires_pairs.png") # Start the PNG
pairs(fires, pch=21, bg = c("red", "green"))
dev.off()

# Let's review some summary analysis
nrow(fires) # Number of rows
nrow(fires[fires$area>0, ]) # Number of fires occurring in the data set

# Let's generate a subset within the data for only days when there are fires
fires_subset <- subset(fires, fires$area>0) # Number of fires occurring in the data set

# Histogram of the burned area
png("c:/Users/Nate/Git/riemann/fires_hist.png")
hist(fires$area, breaks = 20, main = "Histogram of Total Burned Area")
dev.off()

# Histogram of log + 1 of the fires data
# The article initially recommends this ...
png("c:/Users/Nate/Git/riemann/fires_log_hist.png")
hist(log(fires$area + 1))
dev.off()

# Create the log of the burned area
log_area = log(fires$area + 1)

# Bind the log of the burned area to the data frame
fires = cbind(fires, log_area) # This adds a column to the data frame

# --------------------------------------------------------------------------------------
# Linear Regression
# Note that I am using the log of the area as the response variable
fires_lm <- lm(log_area ~ X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH
                         + wind + rain, # Create the rpart tree using all variables 
                         data = fires)
summary(fires_lm) # Review a summary of the model

# Test 1 using all backwards stepwise regression
fires_lm_test_1 <- lm(log_area ~ month + day + FFMC + DMC + DC + ISI + temp + RH
               + wind + rain, # Create the rpart tree using all variables 
               data = fires)
summary(fires_lm_test_1) # Review a summary of the model

# Test 2 using all backwards stepwise regression
fires_lm_test_2 <- lm(log_area ~ month + day + FFMC + DMC + DC + ISI + temp + 
                        + wind + rain, # Create the rpart tree using all variables 
                      data = fires)
summary(fires_lm_test_2) # Review a summary of the model

# Test 3 using a hypothetical reduced model
fires_lm_test_3 <- lm(log_area ~ temp, # Create the rpart tree using all variables 
                      data = fires)
summary(fires_lm_test_3) # Review a summary of the model

# Test 4 using all backwards stepwise regression
fires_lm_test_4 <- lm(log_area ~ temp + wind, # Create the rpart tree using all variables 
                      data = fires)
summary(fires_lm_test_4) # Review a summary of the model

# Try with the subset data - Conditional probability
# Hypothesis here is that the exclusion of non-fire days would most effectively predict fires
fires_lm_test_5 <- lm(area ~ temp + RH + wind + rain,
                      data = fires_subset)
summary(fires_lm_test_5) # Review a summary of the model

# --------------------------------------------------------------------------------------
# Ridge Regression

# Problem getting things in seemed to be the month and the day ... Thus, I removed them
select(lm.ridge(log_area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH 
                + wind + rain, data = fires_subset, lambda = seq(0,1,0.001)))

# The generalized cross-validation (GCV) criterion says the optimal biasing constant is 1
# What does an optimal biasing constant of 1 mean?

fires_ridge_reg <- lm.ridge(log_area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH 
                            + wind + rain, data = fires_subset, lambda = 1)

# Printing the ridge-regression coefficient estimates for this problem:
summary(fires_ridge_reg) # An error is generated in the fit

# What is the mean squared error
ridge_mspe <- mean((fires_subset$log_area - fitted(fires_ridge_reg))^2)

# --------------------------------------------------------------------------------------
# Regression Trees

# Note that I am using the log of the area as the response variable
fires_main_tree <- rpart(log_area ~ X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH
                         + wind + rain, # Create the rpart tree using all variables 
                   data = fires, # We are using the firest data set
                   method = "anova") # We are using a continuous variable. Thus, anova works ..

# Plot the full tree
png("c:/Users/Nate/Git/riemann/fires_full_tree.png")
par(mar=c(1,1,1,1)+0.1)
par(oma=c(0.5,0.5,0.5,0.5)) 
plot(fires_main_tree, cex = 1)
text(fires_main_tree)
title(main = "Full Regression Tree - Forest Fires In Portugal")
dev.off()

# Calculate the mean squared prediction error for the full tree
# In this example MSPE = (actual - predicted)^2 / n
(fires_main_tree_mspe <- mean((fires$log_area - predict(fires_main_tree))^2))

# Generate a table showing the complexity parameter for the main tree
fires_main_tree$cptable

# Let us now prune the regression tree
# Note that CP here refes to the complexity parameter
png("c:/Users/Nate/Git/riemann/fires_main_tree_cp.png")
plotcp(fires_main_tree,
       minline = TRUE,
       col = "blue")
dev.off()

# Let us select the optimal complexity parameter
# Note that I am using these guidelines - https://onlinecourses.science.psu.edu/stat857/node/133
# Look at http://www.statmethods.net/advstats/cart.html
opt <- fires_main_tree$cptable[which.min(fires_main_tree$cptable[,"xerror"]),"CP"]

# Generate the object for the pruned tree
fires_pruned_tree <- prune(fires_main_tree, 
                             cp=opt) # Set the complexity parameter here to the optimal value

# Plot the full tree
png("c:/Users/Nate/Git/riemann/fires_pruned_tree.png")
par(mar=c(1,1,1,1)+0.1)
par(oma=c(0.5,0.5,0.5,0.5)) 
plot(fires_pruned_tree, cex = 1)
text(fires_pruned_tree)
title(main = "Pruned Regression Tree - Forest Fires In Portugal")
dev.off()

# Weird ... the regression tree fails here ... Why is the CP error increasing ...
# I believe that this indicates overfitting. If y is independent of the x(s) the 
# best prediction is the overall mean, and that happens with 0 splits. 

# xerror (and xstd) estimates are more realistic estimates of the performance of the tree 
# on new samples of data. They are obtained by the rpart function by an internal cross 
# validation process - Louis Torgo ... http://tolstoy.newcastle.edu.au/R/help/05/03/1541.html
# Louis thesis covering regression trees http://www.dcc.fc.up.pt/~ltorgo/PhD/th4.pdf

# Calculate the mean squared prediction error for the pruned tree
# In this example MSPE = (actual - predicted)^2 / n
(pruned_tree_mspe <- mean((fires$log_area - predict(fires_pruned_tree))^2))

# Based on data issues, other methods were ignored for the data set ....
# Confirming and interesting observations include potential overfitting within
# the regression tree, highly correlated data, and a highly skewed distribution ....
# These factors created significant problems for the data set analysis

