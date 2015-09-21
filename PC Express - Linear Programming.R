# Example Linear programming solution - PC Express
# Created by: Nathaniel Payne
# September 20, 2015

# The business problem:
# PC-Express is a computer retail store that sells two kinds of microcomputers: 
# desktops and laptops. The company earns $600 on each desktop computer it sells and $900 on each laptop. 
# The microcomputers PC-Express sells are manufactured by another company. This manufacturer has a special order to fill for 
# another customer and cannot ship more than 80 desktop computers and 75 laptops to PC-Express next month. 
# The employees at PC-Express must spend about 2 hours installing software and checking each desktop computer they sell. 
# They spend roughly 3 hours to complete this process for laptop computers. They expect to have about 300 hours available 
# for this purpose during the next month. The store's management is fairly certain that they can sell all the computers they order, 
# but are unsure how many desktops and laptops they should order to maximize profits.

# The mathematical problem:
# maximize P = 600x + 200y, subject to x <= 80, y <= 75, 2x + 3y <= 300, x >= 0, y >= 0
# In this problem, note that the equation to be optimized is: P = 600x + 200y
# Default lower bounds of zero on all variables are represented by: x >= 0, y >= 0
# Note that lpsolve by default includes the last condition (i.e. all variables non-negative)
# Problem constraints of the following form: x <= 80, y <= 75, 2x + 3y <= 300

# We use the lpsolve package here:

install.packages("lpSolveAPI") # Install the lpsolve package
library("lpSolveAPI") # Load the lpsolve package

# Make a matrix to represent our problem
lprec <- make.lp(0, 2) # This allows us to create an empty model which has two variables
lp.control(lprec, sense="max")
set.objfn(lprec, c(600, 900)) # Set the coefficients on the objective function
add.constraint(lprec, c(1, 0), "<=", 80)
add.constraint(lprec, c(0, 1), "<=", 75)
add.constraint(lprec, c(2, 3), "<=", 300)

lprec # Display the final lpsolve matrix

# The result here is: 
#   C1    C2         
# Maximize   600   900         
# R1           1     0  <=   80
# R2           0     1  <=   75
# R3           2     3  <=  300
# Kind       Std   Std         
# Type      Real  Real         
# Upper      Inf   Inf         
# Lower        0     0      

solve(lprec) # solve
# [1] 0

get.objective(lprec) # Get maximum profit
# [1] 90000

get.variables(lprec) # Get the solution
# [1] 37.5 75.0

# Based on this, the profit is maximized if they order 37.5 desktops and 75 laptops (providing a maximum profit of $90,000)!
