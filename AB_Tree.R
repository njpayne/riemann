# Investigating Multi-variate regression trees
# October 23, 2013
# Nathaniel Payne

# Multivariate regression trees is an extension of CART. 
# It works exactly the same way, except that you have multiple response variables 
# instead of one. As in CART, the response variables can be numeric or class
# variables, and the same applies for the predictor variables. 
# However the objectives are very different: a constrained gradient analysis rather 
# than a prediction. 

# For this investigation I will use the "AB_Tree_Plots_Normalized.csv" data
# This dataset contains data of % forest cover for 13 species of trees in different
# sample plots along with eight climate variables measured at these sites.
# In addition the ecosystem and biome is given for each sample plot. 
# The goal will be to use MRT to describe the relationship between the
# % cover data for multiple tree species (response variable) and
# climate variables (predictor variables) simultaneously

# Attach the data and check the names of the variables.
tree_clim = read.csv("c:/Users/Nate/Git/riemann/AB_Tree_Plots_Normalized.csv")
fix(tree_clim) # This function enables you to interactively edit the data frame in R
attach(tree_clim)

# Review the data & generate relevant summaries
head(tree_clim) # Look at the head of the data
summary(tree_clim) # Generate a summary of the data
names(tree_clim) # Review the names of the data

# Subdivide the dataset into different sets of variables. 
# This will make it easier to code in the tree species separate to the other columns 
# in the original dataset.
pct_cover = tree_clim[,11:23] # normalized data of % cover
climate = tree_clim[,3:10] # climate variables

fix(pct_cover) # If necessary
fix(climate) # If necessary

# To run a multivariate regression tree, we will install and run the "mvpart" package.
# install.packages("mvpart")
library(mvpart) # Load the library

# Generate a graph of the output
png("c:/Users/Nate/Git/riemann/AB_Tree_Graph.png")
mvpart(data.matrix(pct_cover)~ MAT + MWMT + MCMT + TD + lnMAP + lnMSP + lnAHM + lnSHM,
       pct_cover, margin=0)
dev.off()

# Alternatively, you can select your own size of tree, guided by an interactive graph 
# produced by R which shows the cross-validation error at each split in the MV tree. 
# The default (preferred) tree size is the one having the smallest cross-validated 
# relative error. R indicates the preferred pruning size on the graph but you can choose 
# your own preferred size of tree by clicking on the point that corresponds to your 
# desired number of splits.

# The command is the same as above, but you add xv="p" to have the control of the pruning 
# process

# Generate a revised graph
png("c:/Users/Nate/Git/riemann/AB_Tree_Graph_CV.png")
mvpart(data.matrix(pct_cover) ~ MAT + MWMT + MCMT + TD + lnMAP + lnMSP + lnAHM + lnSHM,
       pct_cover,xv="p")
dev.off()

# Looking at the graphs, we see that each split in the MV tree is indicated by the
# relevant independent variable and its value at the point of splitting. Also, 
# each leaf on the tree has a barplot associated with it. Because the data is
# normalized, all of the bars above the line are the ones driving the split. 
# Also, at the bottom of the tree is the residual error for the overall tree 
# i.e., how much variation is not explained by the tree, the cross- validation error
# and standard error for this size of tree.

# Note that instead of climate variables, you can also use class variables for classifications.
# To test the tree species data against the ecosystem classifications, simply do the
# same analysis using ECOSYS instead of the climate variables. The structure of the tree 
# is very similar, as you would expect, but it can be helpful to check how your tree species 
# groupings have fallen out relative to the different ecosystem types.
png("c:/Users/Nate/Git/riemann/AB_Tree_Graph_ECOSYS.png")
mvpart(data.matrix(pct_cover) ~ ECOSYS, pct_cover,xv="p")
dev.off()