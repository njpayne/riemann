# Solving a problem on stackoverflow
# http://stackoverflow.com/questions/22890517/evaluating-one-variable-with-a-list-in-r/22891039#22891039
# Evaluating one variable with a list in R

# Define the initial variables that might be changed here
var_1 <- 3.14159265358979 # This referred to pi in your initial expression
var_2 <- 1 # This referred to p in your initial expression
var_3 <- 1.26424111790395 # This refers to a0 in your initial expression
var_4 <- 0.251688909862584 # This refers to a in your initial expression
var_5 <- -1.03501509824516e-16 # This refers to b in your initial expression

n <- 3 # This is the number of equations that will be run through

# Create an empty dataframe to hold the outputted expressions
finished = c() # Empty data frame

# Create an array holding values from 1 to the number of n's that will be run through
cycle <- c(1:n)

# Convert cycle to a matrix
cycle <- as.matrix(cycle)

# The variable we will be changing is i ... Create the initial loop
for (i in 1:3 ) {
  nth <- expression(((1/p)*a0/2)+sum(((1/p)*a*cos(i*pi*x/p)))+sum((1/p)*b*sin(i*pi*x/p))) # Write the expression to be changed
  
  # Substitute in all the relevant values. Note that this is made to be more explicity
  nth <- as.expression(gsub('pi',var_1,nth))  
  nth <- as.expression(gsub('p',var_2,nth))
  nth <- as.expression(gsub('a0',var_3,nth))
  nth <- as.expression(gsub('a',var_4,nth))
  nth <- as.expression(gsub('b',var_5,nth))
  
  # I will also, for each value, substitue in relevant value from the cycle array
  # This will change the i values for you
  i_index <- cycle[i,1]
  i_index <- as.character(i_index)
  
  nth <- as.expression(gsub('i',i_index,nth)) # Append the nth equation
  
  # I will then bind this solution into the finished data frame to hold all solutions
  finished[i] = nth
}


