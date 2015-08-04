library(fda)

# R code used to generate spline examples

tobs = seq(0,1,0.01)
# true curve
ytru = exp(2*tobs) + sin(tobs*10*pi)

nobs = length(tobs)
noise = 0.1*rnorm(nobs)
yobs = ytru + noise

par(mfrow=c(1,1)) # Ensure we are using 1 row and 1 column
plot(tobs,yobs,type="p")


knots    = c(seq(0,1,0.1));
nknots   = length(knots);
norder   = 4; # cubic B-spline basis function
nbasis   = length(knots) + norder - 2;
basis = create.bspline.basis(c(min(tobs),max(tobs)),nbasis,norder,knots);


# basis values at the observed time points
basismat   = eval.basis(tobs, basis);
plot(tobs,basismat[,1],type="l")  
lines(tobs,basismat[,2],type="l",col="red")
for (k in 1:nknots)
{
  lines(rep(knots[k],100),seq(0,1,length.out=100),type="l",col="black")
}

matplot(tobs,basismat,type = "l")
for (k in 1:nknots)
{
  lines(rep(knots[k],100),seq(0,1,length.out=100),type="l",col="black")
}

Amat = solve(t(basismat)%*%basismat)%*%t(basismat)
chat = Amat%*%yobs

# fitted curve
yhat = basismat%*%chat;
plot(tobs,ytru,type = "l")
points(tobs,yobs)
lines(tobs,yhat,type = "l",col="red")

# Use quadrature to get integral - Composite Simpson's Rule

delta <- 0.001
quadpts <- seq(0,1,delta)
nquadpts <- length(quadpts)
quadwts <- c(1,rep(c(4,2),(nquadpts-1)/2))
quadwts[nquadpts] <- 1
quadwts <- quadwts*delta/3


# Second derivative of basis functions at quadrature points

Q2basismat   = eval.basis(quadpts, basis,2);

# estimates for basis coefficients
Amat = t(Q2basismat)%*%(Q2basismat*(quadwts%*%t(rep(1,nbasis))))
basismat2 = t(basismat)%*%basismat;
lambda = 0.005   # smoothing parameter
Bmat                      = basismat2 + lambda*Amat;
chat = ginv(Bmat)%*%t(basismat)%*%yobs;

# fitted value
yhat = basismat%*%chat;
plot(tobs,ytru,type = "l")
points(tobs,yobs)
lines(tobs,yhat,type = "l",col="red")
