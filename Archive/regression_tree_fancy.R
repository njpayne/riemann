# October 24, 2013
# This was an implementation exercise based on a revolution analytics blog
# Please see the following link for the link to the original code and commentary
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# Initial code was created and implemented by Joseph Rickert; All code credited to that source
# Code implemented with tweaks by Nathaniel Payne
# Note that I removed the RevoScalar links as they were not appropriate
# Implementation by Nathaniel Payne

# Plotting Classification Trees with the plot.rpart and rattle pckages

# install.packages("rpart")    		    # Popular decision tree algorithm
# install.packages("rattle")					# Fancy tree plot
# install.packages("rpart.plot")			# Enhanced tree plots
# install.packages("RColorBrewer")		# Color selection for fancy tree plot
# install.packages("party")					  # Alternative decision tree algorithm
# install.packages("partykit")				# Convert rpart object to BinaryTree
# install.packages("caret")				 	  # Just a data source for this script

library(rpart)  			  # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script

data(segmentationData)				      # Get some data
data <- segmentationData[,-c(1,2)]  # Note that the data here is really non-sense

# Make a giant tree using the rpart tree method
form <- as.formula(Class ~ .)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))

# Plot the tree (Note that this tree is a complete mess and is unusable)
png("c:/Users/Nate/Git/riemann/regression_tree_fancy_mess.png")
plot(tree.1) # Plot the tree				
text(tree.1) # Put the text on the tree
title(main = "This Tree Is A Mess")
dev.off()

# Plot the tree with shortened variable names
# Note that we also use the prp() function which makes the tree cleaner
png("c:/Users/Nate/Git/riemann/regression_tree_fancy_mess_short.png")
prp(tree.1)					    # Will plot the tree
prp(tree.1,varlen=3)		# Shorten variable names
title(main = "Notice The Significant Improvement Here")
dev.off()

# Let us now try and interatively prune the tree
# This is a super cool function that allows you to interactively prune a tree
# More of a visual aid here than anything logical
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # Interactively trim the tree
prp(new.tree.1) # display the new tree

#-------------------------------------------------------------------
# Let's try something else!
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                     # A fast plot													

png("c:/Users/Nate/Git/riemann/regression_tree_fancy_rpart.png")
fancyRpartPlot(tree.2)				  # A fancy plot from rattle
title(main = "An Example Using A Fancy Tree")
dev.off()
# This line ... Fancy plot ... provides mode insights about what is on the tree ...
