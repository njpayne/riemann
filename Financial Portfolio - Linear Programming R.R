# Example Linear programming solution - Financial Portfolio
# Created by: Nathaniel Payne
# September 20, 2015

# The business problem:
# For this problem, select a portfolio package from a set of alternative investments. Then, either maximize the 
# expected return or minimize the risk. To solve this problem, be sure to consider the available capital, the company's policies on 
# how to treat constraints, the duration of investments' economic life, potential growth rate, danger, liquidity.

# The following are the business constraints for this problem:
# 1) Total amount that you have available to invest is equal to 90000
# 2) Amount in shares of a sector can be no larger than 50% of total amount available
# 3) Amount in shares with the larger return of a sector must be less than or equal to 80% of sector's total amount
# 4) The amount in manufacturing company ?? must be less or equal to 10% of the whole share amount
# 5) The amount in mutual funds must be less than or equal to 25% of the amount in manufacturing shares
# Note that the return on investment proposed for each investment is:
# 15.4%, 19.2%, 18.7%, 13.5%, 17.8%, 16.3%

# The mathematical problem:
# maximize  P = 0.154a + 0.192b + 0.187c + 0.135d + 0.178e + 0.163f
# subject to (constraints noted above)
# Default lower bounds of zero on all variables are represented by: x >= 0, y >= 0
# Important note - the problem constraints that conditionally represent a field are not properly represented
# In particular, constraints 3, 4, and 5 are all hardcoded on the rhs (this needs to be reviewed and fixed for the next round)

# We use the lpsolve package here:
# install.packages("lpSolveAPI") # Install the lpsolve package
library("lpSolveAPI") # Load the lpsolve package

# Make a matrix to represent our problem
lprec <- make.lp(0, 6) # This allows us to create an empty model which has two variables
lp.control(lprec, sense="max") # This is a minimization problem
set.objfn(lprec, c(0.154, 0.192, 0.187, 0.135, 0.178, 0.163)) # Set the coefficients on the objective function
add.constraint(lprec, c(1, 1, 1, 1, 1, 1), "<=", 90000)
add.constraint(lprec, c(1, 1, 0, 0, 0, 0), "<=", 45000)
add.constraint(lprec, c(0, 0, 1, 1, 0, 0), "<=", 45000)
add.constraint(lprec, c(0, 1, 0, 0, 0, 0), "<=", 32000) # Note that this needs to be reviewed and not hardcoded
add.constraint(lprec, c(0, 0, 1, 0, 0, 0), "<=", 36000) # Note that this needs to be reviewed and not hardcoded
add.constraint(lprec, c(0, 1, 0, 0, 0, 0), "<=", 8500) # Note that this needs to be reviewed and not hardcoded
add.constraint(lprec, c(0, 0, 0, 0, 1, 1), "<=", 10000) # Note that this needs to be reviewed and not hardcoded

lprec # Display the final lpsolve matrix

# The result here is:
# C1     C2     C3     C4     C5     C6           
# Maximize  0.154  0.192  0.187  0.135  0.178  0.163           
# R1            1      1      1      1      1      1  <=  90000
# R2            1      1      0      0      0      0  <=  45000
# R3            0      0      1      1      0      0  <=  45000
# R4            0      1      0      0      0      0  <=  32000
# R5            0      0      1      0      0      0  <=  36000
# R6            0      1      0      0      0      0  <=   8500
# R7            0      0      0      0      1      1  <=  10000
# Kind        Std    Std    Std    Std    Std    Std           
# Type       Real   Real   Real   Real   Real   Real           
# Upper       Inf    Inf    Inf    Inf    Inf    Inf           
# Lower         0      0      0      0      0      0

solve(lprec) # solve
# [1] 0

get.objective(lprec) # Get maximum profit
# [1] 15611

get.variables(lprec) # Get the solution
# [1] 35500  8500 36000     0 10000     0

# Based on this, the maximum profit available, based on the imposed constraints, is $15,611
