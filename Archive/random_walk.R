# Example Illustrating Various Random Walks
# September 8, 2013

#Random walk, w = 1, with drift  = :2 (upper jagged line), without
#drift,  = 0 (lower jagged line), and a straight line with slope .2 (dashed line).
# Note that when the constant is zero, the model is simply a random walk
set.seed(154) # so you can reproduce the results
png('c:/Users/Nate/Git/riemann/random_walk.png')
par(mfrow=c(1,1)) # Ensure that the graph prints in a 1 by 1 view
w = rnorm(200,0,1); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="Plot Illustrating Random Walk With Drift")
lines(x); lines(.2*(1:200), lty="dashed")
dev.off()

# ------------------------------------------------------------------------------
#Many realistic models for generating time series assume an underlying signal
#with some consistent periodic variation, contaminated by adding a random
#noise.

#Cosine wave with period 50 points (top panel) compared with the cosine
#wave contaminated with additive white Gaussian noise, w = 1 (middle panel) and
#w = 5 (bottom panel); see (1.5).

cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
png('c:/Users/Nate/Git/riemann/cosine_wave.png')
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))
dev.off()