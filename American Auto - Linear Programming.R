# Example Linear programming solution - American Auto
# Created by: Nathaniel Payne
# September 20, 2015

# The business problem:
# American Auto is evaluating their marketing plan for the sedans, SUVs, and trucks they produce. A TV ad featuring this SUV has 
# been developed. The company estimates that each showing of this commercial will cost $500,000 and increase sales of SUVs by 3%, 
# but reduce sales of trucks by 1%, and have no effect of the sales of sedans. The company also has a print ad campaign 
# developed that it can run in various nationally distributed magazines at a cost of $750,000 per title. It is estimated that 
# each magazine title the ad runs in will increase the sales of sedans, SUVs, and trucks by 2%, 1%, and 4%, respectively. 
# The company desires to increase sales of sedans, SUVs, and trucks by at least 3%, 14%, and 4%, respectively, in the least 
# costly manner. Formulate an LP model for this problem.

# The mathematical problem:
# minimize  P = 500000x + 750000y, subject to 0.02y >= 0.03, 0.03x + 0.01y >= 0.14, -0.01x + 0.04y >= 0.04, x >= 0, y >= 0
# In this problem, note that the equation to be optimized is: P = 500000x + 750000y
# Default lower bounds of zero on all variables are represented by: x >= 0, y >= 0
# Note that lpsolve by default includes the last condition (i.e. all variables non-negative)
# Problem constraints of the following form: 0.02y >= 0.03, 0.03x + 0.01y >= 0.14, -0.01x + 0.04y >= 0.04

# We use the lpsolve package here:
# install.packages("lpSolveAPI") # Install the lpsolve package
library("lpSolveAPI") # Load the lpsolve package

# Make a matrix to represent our problem
lprec <- make.lp(0, 2) # This allows us to create an empty model which has two variables
lp.control(lprec, sense="min") # This is a minimization problem
set.objfn(lprec, c(500000, 750000)) # Set the coefficients on the objective function
add.constraint(lprec, c(0, 0.02), ">=", 0.03)
add.constraint(lprec, c(0.03, 0.01), ">=", 0.14)
add.constraint(lprec, c(-0.01, 0.04), ">=", 0.04)

lprec # Display the final lpsolve matrix

# The result here is: 
#               C1      C2          
# Minimize   5e+05  750000          
# R1             0    0.02  >=  0.03
# R2          0.03    0.01  >=  0.14
# R3         -0.01    0.04  >=  0.04
# Kind         Std     Std          
# Type        Real    Real          
# Upper        Inf     Inf          
# Lower          0       0       

solve(lprec) # solve
# [1] 0

get.objective(lprec) # Get maximum profit
# [1] 3500000

get.variables(lprec) # Get the solution
# [1] 4 2

# Based on this, the cost is minimized if they run 4 tv ads and 2 print ads (providing a minimum cost of $3,500,000)!
