# R Code used to analyze body fat data
# Goal is to understand ridge regression
# September 25, 2013

bodyfat.data <- read.table(file = "c:/Users/Nate/Git/riemann/bodyfatdata.txt", 
                           header=FALSE, col.names = c('triceps', 'thigh', 'midarm', 'bodyfat'))
attach(bodyfat.data) # Attach the data frame
library(MASS) # Load the MASS package

# For this code, I will used R's automatic selection methods to select the biasing constant:
# R calls this constant "lambda"; Be sure to do this after loading the MASS package
select(lm.ridge(bodyfat ~ triceps + thigh + midarm, lambda = seq(0,1,0.001)))
# The generalized cross-validation (GCV) criterion is the smallest at 0.19
# Thus, we conclude that the optimal biasing constant occurs at .019
bodyfat.ridge.reg <- lm.ridge(bodyfat ~ triceps + thigh + midarm, lambda = .019)

# Print the ridge-regression coefficient estimates for this problem
summary(bodyfat.ridge.reg)

#-------------------------------------------------------------------------------
# For learning, let us compare the ridge-regression fit to the original least-squares fit

# The X matrix for this case can be represented as
X.matrix <- cbind(rep(1,length=length(bodyfat)),triceps, thigh, midarm)

# The fitted valuees for the ridge-regression fit can be generated using the following
fitted.vals <- X.matrix %*% c(43.840113, 2.117493, -0.959731, -1.018061)

# The SSE for the ridge-regression fit can be generated using the following code:
sse.ridge <- sum((bodyfat-fitted.vals)^2); sse.ridge

# On the other hand, the original least-squares fit
bodyfat.reg <- lm(bodyfat ~ triceps + thigh + midarm)

# The SSE for the original least-squares fit can be generated using the following code:
sum(resid(bodyfat.reg)^2)

# As is shown, the SSE for the ridge-regression fit is not much higher, which is good.
# SSE Ridge = 101.7287 vs SSE OLS = 98.40489
