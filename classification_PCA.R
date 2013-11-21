# Stat 852, Assignment 4; Classification Trees & PCA
# November 19, 2013
# Nathaniel Payne

# install.packages("rpart") # Use this if necessary to install the package from scratch
library(rpart) # Turn on the rpart library
library(rattle) # Used for making fancy classification & regression trees
library(gam) # GAM models
library(mgcv) # For use with GAM's
library(MASS) # Used for LDA's
library(glmpath) # For The LASSO
library(lars)  # Used for the LASSO

# Loading the South African heart disease data from my own machine
# SA <- read.table("c:/Users/Nate/Git/riemann/SAheart.txt",header=TRUE,sep=",")

# We can also use the following
SA <- read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", 
                 sep=",",head=T,row.names=1)

# Let's look at a scatterplot matrix of the data
png("c:/Users/Nate/Git/riemann/SAheart_pairs.png") # Start the PNG
pairs(SA[1:9],pch=21,bg=c("red","green")[factor(SA$chd)])
# pairs(SA) # Versus using this pairs plot, I went with a more sophisticated plot
# Could also use the following # pairs(SA, col=c("blue", "red"), cex=0.6)
# This is the plot that I originally made. I also made some other examples
dev.off()

str(SA) # Review the characteristics of the data set

# -----------------------------------------------------------------------------
# For use later, I will scale the data and then generate a random train / test split
# Scale data and create a random train / test split
num_rows <- nrow(SA) # n = the number of rows
p_dim <- ncol(SA)-1 # p = the number of dimensions
test.ratio <- .2 # ratio of test/train samples
n.test <- ceiling(num_rows*test.ratio) # Generate the ceiling of the test values
testing <- sample(1:num_rows,num_rows.test) # Generate the number of n's in the testing data
training <- setdiff(1:num_rows,testi) # Generate the number of values in thh training data

training_data <- SA[training,] # Generate the training data
testing_data <- SA[testing,] # Generate the testing data

# -----------------------------------------------------------------------------
# Classification Tree
# 1. Do the classification using a classification tree. Compare the misclassification error 
# with the methods used in Assignment 2 and 3. 

# After coding, I went back and referenced the following resource which provided some hints
# on the classification tree models:
# www.webpages.uidaho.edu/~stevel/519/R/R16.CT.doc

# Create a classification tree
SA_class<-rpart(chd~., data=SA, method="class") # Create the classification tree object

# Plot the classification tree
png("c:/Users/Nate/Git/riemann/classification_PCA_tree.png") # Place the png
plot(SA_class, # Using the generic package
     main = "Classification Tree  - South African Heart Data") # Plot the tree
text(SA_class) # Fill in the text
dev.off() # Turn the device off

# Plot a fancy classification tree
png("c:/Users/Nate/Git/riemann/classification_PCA_tree_fancy.png") # Place the png
fancyRpartPlot(SA_class) # Plot the tree using the rattle package
text(SA_class) # Fill in the text
dev.off() # Turn the device off

# Let us now calculate the misclassification rate
(SA_class_predict <- predict(SA_class, SA, type="class")) # Make predictions for each of the patients
misclass_table <- table(Actual=SA$chd, Classified=SA_class_predict) # Generate a table using the predicted values

misclass_table <- as.matrix(misclass_table) #Turn the misclassification table into a matrix
(rate <- (misclass_table[1,2] + misclass_table[2,1]) / sum(misclass_table) # Calculate the misclassification rate)

# -----------------------------------------------------------------------------
# Principal Components Analysis
# A key resource for this analysis was: http://www.stat.cmu.edu/~cshalizi/490/pca/pca-handout.pdf
# Described some of the general code and background of the technique

# Do the principal component analysis on the 8  covariates - "sbp" "tobacco" "ldl" 
# "adiposity" "typea" "obesity" "alcohol" "age"

# Note that I am using prcomp as this is generally considered more robust than other methods
 
# Review the numerical values of the correlations for the 8 explanatory variables 
# excluding chd, which is the response variable
cor(SA[,c(1:4,6:9)], SA[,c(1:4,6:9)])
 
# Generate a box-plot of the data being used in this problem - Unscaled
png("c:/Users/Nate/Git/riemann/classification_PCA_boxplot_unscaled.png") # Place the png
boxplot(SA[,c(1:4,6:9)], main="Boxplot of South African Heart Data, Unscaled")
dev.off()
 
# How to scale the data
SA_scaled <- data.frame(apply(SA[,c(1:4,6:9)],2,scale))

# Generate a box-plot of the data being used in this problem - Unscaled
png("c:/Users/Nate/Git/riemann/classification_PCA_boxplot_scaled.png") # Place the png
boxplot(SA_scaled, main="Boxplot of South African Heart Data, Scaled")
dev.off()

# Generate a comparative box-plot of the data being used in this problem - Scaled vs Unscaled
png("c:/Users/Nate/Git/riemann/classification_PCA_boxplot_scaled_compare.png") # Place the png
par(mfrow = c(1,2)) # Put in 1 row and 2 columns
boxplot(SA[,c(1:4,6:9)], main="Unscaled") # Scaled boxplot
boxplot(SA_scaled, main="Scaled") # Unscaled boxplot
dev.off()

# (SA_PCA_test <- prcomp(SA_scaled, scale=FALSE)) Note that you can use this approach if you want
# to scale your data first manually before hand. The following approach enables you to scale your
# data automatically
 
# Conduct the principal components analysis
# Note that head(SA[,c(1:4,6:9)]) generates a subset of the data that is needed
(SA_PCA <- prcomp(SA[,c(1:4,6:9)], scale=TRUE)) # Do this on only a selection of the covariates
# Note that scale.=TRUE The second argument to tells the program to first scale all the variables 
# to have variance 1, i.e., to standardize them.

# Generate a summary of the PCA output
summary(SA_PCA)
 
# Review the linear combination for the first principal component
(PCA_first <- SA_PCA$rotation[,1])
 
# Extract the loadings or weight matrix from the PCA object (for the first 2 components ...)
round(SA_PCA$rotation[,1:2],2)

#             PC1   PC2
# sbp       -0.33  0.24
# tobacco   -0.31  0.46
# ldl       -0.34 -0.36
# adiposity -0.53 -0.19
# typea      0.02 -0.28
# obesity   -0.41 -0.39
# alcohol   -0.12  0.54
# age       -0.46  0.19 

# This shows that all variables, except the typea, have a negative projection onto the first component
# This means that there is a negative correlation between chd and everything else. This basically tells
# us whether we are getting a patient with or without type A behavior

# The second component is a little more interesting. In this component, we hate a contrast between 
# the sbp, tobacco use, alcohol consumption, and age of the patient (positive projection) vs the 
# ldl, adoposity, typeA, and obesity of the patients.
 
# Let's look at all components of the PCA analysis
round(SA_PCA$rotation,2)
 
# To check this interpretation, we can use a useful tool called a biplot, which plots the data, 
# along with the projections of the original features, on to the first two components

# Red arrows show the projections of the original features on to the principal components
png("c:/Users/Nate/Git/riemann/classification_PCA_SA_biplot.png") # Place the png
biplot(SA_PCA, cex=0.75) # Create the biplot
dev.off()
 
# Let us look at the predictions using the principal components
predict(SA_PCA)[,1]

# Let us also plot the different components to see which component accounts for the most variance
png("c:/Users/Nate/Git/riemann/classification_PCA_SA_variance_account.png") # Place the png
plot(SA_PCA, main = "Amount of Variance Explained per Component - PCA",
     xlab = "Number Of Components") # Plot the graph
dev.off() # Turn the device off

SA_PCA$x # This calculates the principal components for all the values ...
 
SA_PCA$sdev # Find the square root of the eigenvalues for the PCA output
# "The eigenvectors of V are the principal components of the data. We
# know that they are all orthogonal top each other from the previous paragraph,
# so together they span the whole p-dimensional feature space. The first principal
# component, i.e. the eigenvector which goes the largest value of , is the direction
# along which the data have the most variance. The second principal component,
# i.e. the second eigenvector, is the direction orthogonal to the first component
# with the most variance."
# http://www.stat.cmu.edu/~cshalizi/490/pca/pca-handout.pdf

# We can also generate a plot which helps us determine how many principal components to use
png("c:/Users/Nate/Git/riemann/classification_PCA_how_many_comp.png")
screeplot(SA_PCA, 
          type= "lines",
          main = "Number of Principal Components To Use For The Health Data")
dev.off() # Turn the device off
 
# Check that the previous plot simply captures the variances of the principal components
apply(SA_PCA$x, 2, var)
 
# The next script shows how much variance is explained by each of the principal components
# In particular, it shows the correlation between original variables and principal components
(corr <- cor(SA_scaled, SA_PCA$x))
round(corr, 2)
 
# correlation^2: This shows the amount of variation of each of the variables that
# is explained by the principal components
round(corr^2, 2)
 
# Do a final check to see whether the rows sum to 1
apply(corr^2, 1, sum) # They do indeed sum to 1 ...
 
summary(SA_PCA) # Based on the summary, we see that we need to use 5 principal components
# in order to explain at least 80% of the variance

# The information from the summary() command you have attached to the question allows you to 
# see, e.g., the proportion of the variance each principal component captures (Proportion of 
# variance). In addition, the cumulative proportion is computed to output. 
# This certainly is not the information you typically use as input to further analyses. 
# Rather, what you usually need is the rotated data, which is saved as 'x' in the object 
# created by prcomp.
 

newdata <- SA_PCA$x[,1:3] # Let this object hold the new principal components analysis data
# newdata <- SA_PCA$x[,1:8] # The code had we chosen to use all 8 of the generated components
# Note that we will only be using 3 principal components in the analysis going forward
# This is opposed to using the full 8 components in the analysis
# Note that we would prefer to use only 1 or 2 components. That said, the variance that each of the
# components explains is not significant enough to justify using only 1 principal component

# -----------------------------------------------------------------------------
# Redo the analysis in Problem 1  in Assignment 4 (South African Heart Data) 
# [the classification tree], by using the obtained principal components as the new covariates, 
# together with the categorical variable "famhist", and treating "chd" as the response variable. 
# In particular, do classification using a classification tree.

# Create the new data set with famhist and chd, along with all 8 principal components
SA[,c("chd", "famhist")] # Review the SA data

SA_newdata_post_PCA <- cbind(SA[,c("chd", "famhist")], newdata) # Bind the data together
head(SA_newdata_post_PCA) # Review the data
 
# Create a classification tree
SA_class_PCA<-rpart(chd~., data=SA_newdata_post_PCA, method="class") # The new classification tree
 
# Plot the classification tree
png("c:/Users/Nate/Git/riemann/classification_PCA_tree_post.png") # Place the png
plot(SA_class_PCA, # Using the generic package
    main = "Post PCA Classification Tree  - South African Heart Data") # Plot the tree
text(SA_class_PCA) # Fill in the text
dev.off() # Turn the device off
 
# Plot a fancy classification tree
png("c:/Users/Nate/Git/riemann/classification_PCA_tree_post_fancy.png") # Place the png
fancyRpartPlot(SA_class_PCA) # Plot the tree using the rattle package
text(SA_class_PCA) # Fill in the text
dev.off() # Turn the device off
 
 # Let us now calculate the misclassification rate
(SA_class_PCA_predict <- predict(SA_class_PCA, SA_newdata_post_PCA, type="class")) # Make predictions for each of the patients
misclass_table_cart <- table(Actual=SA_newdata_post_PCA$chd, Classified=SA_class_PCA_predict) # Generate a table using the predicted values
 
(misclass_table_cart <- as.matrix(misclass_table_cart)) #Turn the misclassification table into a matrix
(rate_PCA <- (misclass_table_cart[1,2] + misclass_table_cart[2,1]) / sum(misclass_table_cart)) # Calculate the misclassification rate)

# -----------------------------------------------------------------------------
# Redo the analysis in Problem 3  in Assignment 3  (South African Heart Data), using the 
# obtained principal components as the new covariates, together with the categorical 
# variable "famhist", and treating "chd" as the response variable.
 
# To complete this question, I will build off the analysis that was done in assignment
# 3 as well as all relevant code.

# I note here that the new dataset is called SA_newdata_post_PCA
 
SA_newdata_post_PCA$famhist <- as.numeric(SA_newdata_post_PCA$famhist) # Ensure that famhist is a factor
SA_newdata_post_PCA$chd <- as.numeric(SA_newdata_post_PCA$chd) # Ensure that chd is a factor
 
# Ensure that the principal components inputs are numeric
 SA_newdata_post_PCA$PC1 <- as.numeric(SA_newdata_post_PCA$PC1) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC2 <- as.numeric(SA_newdata_post_PCA$PC2) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC3 <- as.numeric(SA_newdata_post_PCA$PC3) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC4 <- as.numeric(SA_newdata_post_PCA$PC4) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC5 <- as.numeric(SA_newdata_post_PCA$PC5) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC6 <- as.numeric(SA_newdata_post_PCA$PC6) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC7 <- as.numeric(SA_newdata_post_PCA$PC7) # Ensure that this is a numeric variable
 SA_newdata_post_PCA$PC8 <- as.numeric(SA_newdata_post_PCA$PC8) # Ensure that this is a numeric variable
 
str(SA_newdata_post_PCA) #Let's review the structure of the data

SA_newdata_post_PCA <- data.frame(SA_newdata_post_PCA) # Ensure that the data is in matrix form
# dimnames(SA_newdata_post_PCA)

# Generate a pairs plot of the data before using it in the GAM
png("c:/Users/Nate/Git/riemann/classification_PCA_tree_post_PCA_Pairs.png") # Start the PNG
pairs(SA_newdata_post_PCA, pch=21, bg = c("red", "green"))
dev.off()

# Let's switch to GAM models now ...
# Note that, in order to make the models work, I needed to make all the inputs numeric & continuous
# The first are some puerly parametric models
SA_class_GAM <- gam(chd ~ PC1 + PC2 + PC3 + famhist, data=SA_newdata_post_PCA)
summary(SA_class_GAM)

# Gather together the predicted fitted values
(SA_GAM_predict_fit <- as.matrix(SA_class_GAM$fitted.values))
 
# Convert the predictions into either 1 or 2
class.vec_GAM <- ifelse(SA_GAM_predict_fit[,1]>(0.5), 1, 0) # Note that the previous chd values
# were converted to numeric variables between the values of 1 and 2 ...

# Create a table comparing the predicted values vs the actual
misclass_table_GAM <- table(Actual=SA_newdata_post_PCA$chd, Classified=class.vec_GAM) # Generate a table using the predicted values
 
(misclass_table_GAM <- as.matrix(misclass_table_GAM)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_GAM[1,2] + misclass_table_GAM[2,1]) / sum(misclass_table_GAM)) # Calculate the misclassification rate)

# Let's try some other models ... # Note that in this reduced model I use only certain
# components
SA_class_GAM_reduced <- gam(chd ~ PC1 + famhist, data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced)
 
# Let's try famhist as a factor in the model
SA_newdata_post_PCA$famhist <- as.factor(SA_newdata_post_PCA$famhist) # Ensure that famhist is a factor
str(SA_newdata_post_PCA)

# Try some other models
SA_class_GAM_reduced_1 <- gam(chd ~ PC1 + PC2 + PC4 + factor(famhist), data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced_1)

SA_class_GAM_reduced_2 <- gam(chd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + factor(famhist), data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced_2)

# Let's change the chd model into a factor
SA_newdata_post_PCA$chd <- as.factor(SA_newdata_post_PCA$chd) # Ensure that famhist is a factor

# Let's try another model .... # Note that this produces an error which is perplexing
SA_class_GAM_reduced_3 <- gam(chd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + factor(famhist), data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced_3)

# Let's change the chd model back into a numeric observation
SA_newdata_post_PCA$chd <- as.numeric(SA_newdata_post_PCA$chd) # Ensure that famhist is numeric
 
# Let's try another model .... # Note that this produces an error which is perplexing
SA_class_GAM_reduced_3 <- gam(chd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + factor(famhist), data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced_3)

# Let's try some smoothing coefficients
SA_class_GAM_reduced_4 <- gam(chd ~ s(PC1) + s(PC2) + factor(famhist), data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced_4)

# Let us reduce the model down now removing non-significant components
SA_class_GAM_reduced_5 <- gam(chd ~ s(PC1) + s(PC4) + factor(famhist), data=SA_newdata_post_PCA)
summary(SA_class_GAM_reduced_5)

# Gather together the predicted fitted values
(SA_GAM_predict_fit_reduced <- as.matrix(SA_class_GAM_reduced_4$fitted.values))
 
# Convert the predictions into either 1 or 2
class.vec_GAM_reduced <- ifelse(SA_GAM_predict_fit_reduced[,1]>(0.5), 1, 0) # Note that the previous chd values
# were converted to numeric variables between the values of 1 and 2 ...
 
# Create a table comparing the predicted values vs the actual
misclass_table_GAM_reduced <- table(Actual=SA_newdata_post_PCA$chd, Classified=class.vec_GAM_reduced) # Generate a table using the predicted values
 
(misclass_table_GAM_reduced <- as.matrix(misclass_table_GAM_reduced)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_GAM_reduced[1,2] + misclass_table_GAM_reduced[2,1]) / sum(misclass_table_GAM_reduced)) # Calculate the misclassification rate)
  
# -----------------------------------------------------------------------------
# Logistic Regression
# Use all predictor variables to do classification with the logistic regression model. 
# What 's the classification error?
SA_class_GLM <- glm(chd ~ PC1 + PC2 + PC3 + famhist,
                   family = binomial (logit), 
                   data=SA_newdata_post_PCA)
 
# Generate a plot of the residuals for the model
png("c:/Users/Nate/Git/riemann/classification_PCA_glm_plot.png")
par(mfrow=c(2,2)) # Put 2 rows and 2 columns
plot(SA_class_GLM) # Plot the residuals for the model
dev.off() # Turn the device off
 
summary(SA_class_GLM) # Generate a summary of the GLM model
 
# Create a reduced model which eliminates the non-relevant coefficients & factors
SA_class_GLM_revised <- glm(chd ~ PC1 + famhist,
                   family = binomial (logit), 
                   data=SA_newdata_post_PCA)

summary(SA_class_GLM_revised) # Generate a summary of the GLM model
 
# Let us now calculate the misclassification rate
# First, we must calculate the fitted values
SA_class_GLM_revised$fitted.values

# Convert the fitted GLM approximations into either 1 or 0's
class.vec <- ifelse(SA_class_GLM_revised$fitted.values>0.5, 1, 0)
misclass_table_GLM <- table(Actual=SA_newdata_post_PCA$chd, Classified=class.vec) # Generate a table using the predicted values
 
(misclass_table_GLM <- as.matrix(misclass_table_GLM)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_GLM[1,2] + misclass_table_GLM[2,1]) / sum(misclass_table_GLM)) # Calculate the misclassification rate)

# ------------------------------------------------------------------------------
# Linear Discriminant Analysis 
# Use all predictor variables to do classification with the linear discriminant 
# function (LDA). What's the classification error? 
 
# Create the initial LDA model
SA_class_LDA <- lda(chd ~ PC1 + PC2 + PC3 + famhist,
                    data=SA_newdata_post_PCA)
                     
SA_class_LDA # Review the output of the LDA Analysis
summary(SA_class_LDA) # Generate a summary of the data
 
SA_class_LDA.predicted <- predict(SA_class_LDA) # Review the fitted values from the model
SA_class_LDA.predicted$class # Review the predicte classifications from the model
 
# Generate the table summarizing the predicted values
misclass_table_LDA <- table(Actual=SA_newdata_post_PCA$chd, Classified=SA_class_LDA.predicted$class) # Generate a table using the predicted values

(misclass_table_LDA <- as.matrix(misclass_table_LDA)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_LDA[1,2] + misclass_table_LDA[2,1]) / sum(misclass_table_LDA)) # Calculate the misclassification rate)
 
# ------------------------------------------------------------------------------
# Quadratic Discriminant Analysis 
# Use all predictor variables to do classification with the linear discriminant 
# function (QDA). What's the classification error? 
 
# Create the initial LDA model
SA_class_QDA <- qda(chd ~ PC1 + PC2 + PC3 + famhist,
                     data=SA_newdata_post_PCA)
 
SA_class_QDA # Review the output of the QDA Analysis
summary(SA_class_QDA) # Generate a summary of the data
 
SA_class_QDA.predicted <- predict(SA_class_QDA) # Review the fitted values from the model
SA_class_QDA.predicted$class # Review the predicted classifications from the model
 
# Generate the table summarizing the predicted values
misclass_table_QDA <- table(Actual=SA_newdata_post_PCA$chd, Classified=SA_class_QDA.predicted$class) # Generate a table using the predicted values
 
(misclass_table_QDA <- as.matrix(misclass_table_QDA)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_QDA[1,2] + misclass_table_QDA[2,1]) / sum(misclass_table_QDA)) # Calculate the misclassification rate)

# ------------------------------------------------------------------------------
# Logistic Regression With LASSO
# Use logistic regression model to do classification, while choose predictor 
# variables using LASSO. Does the classification error decrease? 
 
# For this case, I am going to use L1 Regularized Regression.
# L1 regularized logistic regression is now a workhorse of
# machine learning: it is widely used for many classification
# problems, particularly ones with many features.

head(SA_newdata_post_PCA) # Review the previous data set

 # To begin, we must define x and y in order to use glmpath
x <- SA_newdata_post_PCA[,c(2:5)] # Define the appropriate values of x (exclude chd)
 x$PC1 <- as.numeric(x$PC1)
 x$PC2 <- as.numeric(x$PC2)
 x$PC3 <- as.numeric(x$PC3)
 x$famhist <- as.numeric(x$famhist) # Make sure that family history is numeric
x <- as.matrix(x) # Ensure that the explanatory variables are in a matrix form
 
 y <- SA_newdata_post_PCA[,1] # Let y by the first column of the matrix, the chd column
y <- as.numeric(y) # Ensure that the y value is numeric
y <- as.matrix(y) # Ensure that the respones variable is in matrix form
 
# Now, we train the model
SA_LARS = lars(x, y, type = "lasso")
 
# Next, we visualize the model
png("c:/Users/Nate/Git/riemann/classification_PCA_lasso.png")
par(oma=c(1,1,1,2)) # Change the outer margins to accomodate the error
plot(SA_LARS) # Plot the output of the graphs
dev.off() # Turn the device off

# Plot the complexity parameter for the LASSO
png("c:/Users/Nate/Git/riemann/classification_PCA_lasso_cp.png")
plot(SA_LARS, plottype="Cp")
dev.off()

# By using cross-validation with the lasso, an optimal value for the "fraction" can be determined.
SA_LARS_cv <- cv.lars(x, y, type="lasso") # Note that a graph is generated here
SA_LARS_cv # Review the final object

SA_LARS_predict <- predict.lars(SA_LARS, newx=x, type="fit", mode="fraction", s=0.61)

# GAther together the predicted fitted values
SA_LARS_predict_fit <- as.matrix(SA_LARS_predict$fit)
SA_LARS_predict_fit # Review the created object

class.vec_LASSO <- ifelse(SA_LARS_predict_fit[,1]>(1 + .61), 1, 0) #Use the Cp parameter to
# convert the output into either ones or zeros ...
misclass_table_LASSO <- table(Actual=SA_newdata_post_PCA$chd, Classified=class.vec) # Generate a table using the predicted values

(misclass_table_LASSO <- as.matrix(misclass_table_LASSO)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_LASSO[1,2] + misclass_table_LASSO[2,1]) / sum(misclass_table_LASSO)) # Calculate the misclassification rate)