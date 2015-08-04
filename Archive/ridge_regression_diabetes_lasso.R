# R Code used to analyze & understand the LASSO
# Referencing & credit to work done by Hastie & Efron for their LARS package
# September 30, 2013

# The objective function in ridge regression uses sums of squares for both the error 
# term and the regularizing term. Lasso keeps the sum of squares for the error, but 
# looks at the absolute values of the b_j's. I note that both ridge and lasso can 
# equally well be thought of as constrained estimation problems

# It is important to note that there are many ways to calculate the lasso minimizer.
# The estimate is not a simple linear function of y as before.
# Because of this, one can use convex programming methods, because we are trying to 
# minimize a convex function subject to linear constraints.
# I note that Efron & Tibshirani have presented least angle regression as one option 
# which yields a very efficient method for calculating the lasso estimates for all ??

# Also, importantly, I note that lasso incorporates both subset selection and shrinkage

# In LASSO, figuring out an exact expression for the in-sample error appears difficult, 
# mainly because we do not have an explicit expression for the estimate or prediction

# In order to calculate the best lambda for the LASSO, I am going to use the
# lars routine in R, which was written by Bradley Efron and Trevor Hastie.
# As for ridge, the data is normalized. The program calculates the estimates for 
# all possible ?? in one fell swoop.

library(lars) # Load the lars program

# The lars program is in the lars package, so you must load it, or maybe install it and 
# then load it. Using the normalized x and y, fitting all the lasso predictors and
# plotting the coefficients is accomplished easily: 

# Load the in data that was used previously in the ridge regression dataset
diab <- read.table(file = "c:/Users/Nate/Git/riemann/diabetes.data",header=T)

diab.lasso <- lars(x,y) #Aggregate the correct data (automatically done)

# Plot the graph showing the estimates of all LASSO coefficients
png("c:/Users/Nate/Git/riemann/lasso_coefficients.png")
plot(diab.lasso,
     font.sub = 4, # Bold the font for the subtitle
     sub = 'Graph Shows Estimates of all the LASSO coefficients (Diabetes data)')
dev.off() # Turn the device off

# Note that the horizontal axis on the graph is not ??, but is the ratio of the sum of 
# magnitudes of the lasso estimates to that of the full least squares estimates. For
# ?? = 0, this ratio is 1 (since the lasso = least squares), as ?? increases to
# infinity, the ratio decreases to 0. Starting at the right, where the ratio is 1, 
# we have the least squares estimates of the coefficients. As ?? increases, we move left. 
# The coefficients generally shrink, until at some point one hits 0. That one stays 
# at zero for a while, then goes negative. Continuing, the coefficients shrink, 
# every so often one hits zero and stays the re. Finally, at the far left, all
# the coefficients are 0.

# At each stage, represented by a vertical line on the plot, there is a set of 
# coefficient estimates and the residual sum of squares. This data has 13 stages. 
# The matrix diab.lasso$beta is a 13 × 10 matrix of coefficients, each row 
# corresponding to the estimates for a given stage.

#To figure out p???, the degrees of freedom, you have to count the number of nonzero 
# coefficients in that row:
(pstar <- apply(diab.lasso$beta,1,function(z) sum(z!=0)))

# The sigma squared error is the same as before, thus:
(errhat <- diab.lasso$RSS/N+2*sigma2*(pstar+1)/N)

# The smallest occurs at the eighth stage (note the numbering starts at 0). 
# Plotting the p??? + 1 versus the estimated errors:

png("c:/Users/Nate/Git/riemann/lasso_coefficients_prediction_error.png")
plot(pstar+1,errhat,xlab="p*+1",
     ylab= "Estimated Error", 
     main = "p??? + 1 Versus the Estimated Prediction Error")
dev.off()

# Zoom in to get a better idea of the prediction errro
png("c:/Users/Nate/Git/riemann/lasso_coefficients_prediction_error_zoom.png")
plot(pstar+1,errhat,xlab="p*+1",
     ylab= "Estimated Error", 
     main = "p??? + 1 Versus the Estimated Prediction Error (Zoomed in)", 
     ylim=c(2990,3005))
dev.off()

# The smallest error occurs at the eighth stage.
# The estimates for the best predictor are then:
diab.lasso$beta[8,]