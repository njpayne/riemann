# In this code code set, I will explore regression trees
# For a prediction tree, the goal is simple. I want to make a prediction
# for variable Y, using explanatory variables X1, X2, X3.
# To do this I will grow a binary tree.

# Trees work like the following. At each node, I apply a test to one of the
# inputs and go either to the left or the right. Eventually, I come to a leaf
# node which makes a prediction. The prediction averages or aggregates all
# training data points which read the leaf

# Why do we do this ... Predictors like linear or polynomial regression are global
# trees, where the single predictive formula is supposed to hold over the whole space

# An alternative approach to nonlinear regression is to sub-divide, or
# partition, the space into smaller regions, where the interactions are more manageable. 
# We then partition the sub-divisions again. This is recursive partitioning
# as in hierarchical clustering -- until finally we get to chunks of the space
# which are so tame that we can fit simple models to them. The global model thus
# has two parts: one is just the recursive partition, the other is a simple model
# for each cell of the partition.

# Prediction trees use the tree to represent the recursive partition. Each of the
# terminal nodes, or leaves, of the tree represents a cell of the partition, 
# and has attached to it a simple model which applies in that cell only. A point
# x belongs to a leaf if x falls in the corresponding cell of the partition. 
# To figure out which cell we are in, we start at the root node
# of the tree, and ask a sequence of ques  tions about the features. 
# The interior nodes are labeled with questions, and the edges or branches 
# between them labeled by the answers. Which question we ask
# next depends on the answers to previous questions.

# Example code: California Real-Estate Data

# First, fit the model using the tree package # 20640 total observations
calif = read.table("c:/Users/Nate/Git/riemann/cadata.dat",header=TRUE)
require(tree)
treefit = tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)

# Generate a plot of the tree
png('c:/Users/Nate/Git/riemann/simple_tree.png')
plot(treefit) # Plot the basic tree model
text(treefit,cex=0.75) # Add the text
dev.off() # Turn the device off

# Generate a map showing a plot of all the data point
png('c:/Users/Nate/Git/riemann/simple_tree_california.png')
price.deciles = quantile(calif$MedianHouseValue,0:10/10)
cut.prices = cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude",
     main="Map of actual median house prices in California",
     sub="(Darker areas are more expensive)")
partition.tree(treefit,ordvars=c("Longitude","Latitude"),add=TRUE)
dev.off() # Turn the device off

# Let's generate a summary of the tree fit
summary(treefit) # Note thate deviance here is just the MSE under tree()
# Note that the tree fitting function has a number of controls settings which 
# limit how much it will grow. Each node has to contain a certain number of points, 
# and adding a node has to reduce the error by at least a certain amount

# Regression tree fitting
# Once we x the tree, the local models are completely determined, and easy to
# find (we just average), so all the effort should go into finding a good tree

# Generate a new model with a small deviation
#  The default for the latter, min.dev, is 0.01. We turn it down ...
treefit2 <- tree(log(MedianHouseValue) ~ Longitude + Latitude,
                 data=calif,
                 mindev=.001)

png('c:/Users/Nate/Git/riemann/simple_treefit2_california.png')
plot(treefit2, 
     main="Tree Generated Using Reduced Deviance, 68 Nodes") # Plot the basic tree model
text(treefit2,cex=0.05) # Add the text
dev.off() # Turn the device off

# Create a partition for treefit2. Note the high level of detail around the cities,
# as compared to the much coarser cells covering rural areas where variations in
# prices are less extreme (when looking at the plot)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude")
partition.tree(treefit2,ordvars=c("Longitude","Latitude"),add=TRUE,cex=0.3)

# In clustering, remember, what we would ideally do was maximizing
# I[CX], the information the cluster gave us about the features X. 
# With regression trees, what we want to do is maximize I[CY]
# where Y is now the response variable, and C the variable saying which leaf 
# of the tree we end up at. Once again, we can't do a direct maximization, 
# so we again do a greedy search. 

# Every recursive algorithm needs to know when it's done, a stopping criterion. 
# Here this means when to stop trying to split nodes. 

# --------------------------------------------------------------------------------
# Cross Validation and tree fitting

# A  successful approach to finding regression trees uses the idea of cross-
# validation from last time. We randomly divide our data into a training set and
# a testing set (say, 50% training and 50% testing). We then apply the basic
# tree-growing algorithm to the training data only

# At each pair of leaf nodes with a common parent, we evaluate the error on the
# testing data, and see whether the testing sum of squares would shrink if we removed
# those two nodes and made their parent a leaf. If so, we prune; if not, not. This
# is repeated until pruning no longer improves the error on the testing data. The
# reason this is superior to arbitrary stopping criteria, or to rewarding parsimony
# as such, is that it directly checks whether the extra capacity (nodes in the tree)
# pays for itself by improving generalization error.

# The tree package contains functions prune.tree and cv.tree for pruning trees 
# by cross-validation. The function prune.tree takes a tree you fit by
# tree and evaluates the error of the tree and various prunings of the tree,
# all the way down to the stump.

# Plot size (horizontal axis) versus cross-validated sum of squared errors
# (vertical axis) for successive prunings of the treefit2 model. 
png('c:/Users/Nate/Git/riemann/size_vs_sse_treefit2_california.png')
treefit2.cv <- cv.tree(treefit2)
plot(treefit2.cv,
     main="Size vs CV SSE for Treefit2 model")
dev.off() # Turn the device off

# treefit2 after being pruned by ten-fold cross-validation
opt.trees = which(treefit2.cv$dev == min(treefit2.cv$dev))
best.leaves = min(treefit2.cv$size[opt.trees])
treefit2.pruned = prune.tree(treefit2,best=best.leaves)
plot(treefit2.pruned,
     main = "Treefit2: Pruned by ten-fold cross-validation")
text(treefit2.pruned,cex=0.5) # Add the text

# treefit2.pruned's partition of California after cross validation
png('c:/Users/Nate/Git/riemann/treefit2_california_after_CV.png')
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude",
     main="Map of Predicted California's Median Housing Prices After CV")
partition.tree(treefit2.pruned,ordvars=c("Longitude","Latitude"),
               add=TRUE,cex=0.3)
dev.off()
