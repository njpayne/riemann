# The purpose with this program is a) to learn about the behaviour of the
# ordinary least squares estimator, b) to set up a simulation experiment that
# can be used for comparisons between several methods

library(MASS) # Call the mass library

# In this example, I am going to make the following assumptions:
#p = 15 ... There are 15 different parameters in the model
# X = X1; : : : ;Xp
# B_0 = 3 .... The intercept has a value of 3
#B_j = 2 for j = 1; : : : ; 5,
#B_j = 1 for j = 6; : : : ; 10,
#B_j = 0 for j = 11; : : : ; 15, and
#Var(e) = 25

# I am also going to assume that each X is distributed multivariate normal with zero 
# mean and covariance matrix sigma, where all the diagonal elements of 
# sigma are 1 and all non-diagonal elements are 0:8. The sigma is also
# the correlation matrix with all correlations equal to 0.8.

# Set up the original model
p <- 15 # Number of parameters = 15
beta0 < -3 # Intercept = 3
beta<-c(rep(2,5),rep(1,5),rep(0,5)) # B_j have the corresponding values
Sigma.x<-matrix(0.8,nrow=p,ncol=p) # This is the covariance matrix 
diag(Sigma.x)<-1 # This is the diagonal of the covaciarnce matrix 
sd.eps<-5 # This is the standard deviation of the epsilons

# Create some simulation functions used for the problem
# This function constructs the matrix X in the test set
sim.X<-function(n,p,Sigma.x) {
  X<-mvrnorm(n = n, mu=rep(0,p), Sigma=Sigma.x)
  X
}

sim.y.given.X<-function(X,beta0,beta,sd.eps) {
  n<-nrow(X)
  eps<-rnorm(n,mean=0,sd=sd.eps)
  y<-beta0+X%*%beta+eps
  y
}

calc.Ey.given.X<-function(X,beta0,beta) {
  n<-nrow(X)
  Ey<-beta0+X%*%beta
  Ey
}

# Determine the number of observations to be put in the various training sets
n.train1 <- 20 # Training set 1
n.train2 <- 100 # Training set 2
n.train3 <- 1000 # Training set 3
n.test <- 1000 # Test set 1

# construct the matrix X in the test set
X.test<-sim.X(n.test,p,Sigma.x)

# Copy the matrix X into a data frame
# The I function is used to get correct variable names in the prediction
X.test.df<-data.frame(X=I(X.test))

# construction of E(Y|X) in the test set
Ey.test<-calc.Ey.given.X(X.test,beta0,beta)

# Specify the number of simulations to occur
n.sim<-1000

### The variance of betahat calculated from formular will be put in this object
var.betahat<-matrix(NA,p+1,3)

### The simulation results will be put in this object
res.tot<-NULL


### Varies the number of observations in the training data
for (j in 1:3) {
  if (j==1) {
    n.train<-n.train1
  }
  if (j==2) {
    n.train<-n.train2
  }
  if (j==3) {
    n.train<-n.train3
  }

### simulate X in the traininga data
  X.train<-sim.X(n.train,p,Sigma.x)

###Calculate variance of betahat from theoretical formula
###  (X'X)^{-1} sigma^2
  Xone.train<-cbind(rep(1,n.train),X.train)
  tmp<-solve(t(Xone.train)%*%Xone.train)*sd.eps^2
  var.betahat[,j]<-diag(tmp)

  
### Construct matrices where we will store errors
  beta.err<-matrix(NA,n.sim,p+1)
  pred.err<-matrix(NA,n.sim,n.test)
  f.err<-matrix(NA,n.sim,n.test)

### loop over the simulations
  for (i in 1:n.sim) {

### Simulate y in the traing set and in the test set
    y.train<-sim.y.given.X(X.train,beta0,beta,sd.eps)
    y.test<-sim.y.given.X(X.test,beta0,beta,sd.eps)

### construct the training data
    training.data<-data.frame(y=y.train,X=I(X.train))

### Estimate the model by OLS
    obj.lm<-lm(y~X,data=training.data)
    
    beta0.hat<-obj.lm$coef[1]
    beta.hat<-obj.lm$coef[-1]
    
    y.pred<-predict(obj.lm,newdata=X.test.df)
### equivalent to
###  y.pred.manual<-beta0.hat+X.test%*%beta.hat


### calculate errors for estimation or prediction of beta, y and f
    beta.err[i,]<-c(beta0.hat,beta.hat)-c(beta0,beta)
    pred.err[i,]<-y.test-y.pred
    f.err[i,]<-Ey.test-y.pred
    
  }
  
### calculate bias, squared bias, variance and MSE for betahat
  beta.bias<-apply(beta.err,2,mean)
  beta.bias.2<-beta.bias^2
  beta.var<-apply(beta.err,2,var)
  beta.MSE<-apply(beta.err^2,2,mean)
  
  beta.res<-cbind(beta.bias,beta.bias.2,beta.var,beta.MSE)
  
### converts matrices into vectors  
  pred.err<-as.vector(pred.err)
  f.err<-as.vector(f.err)
  
### calculate bias, squared bias, variance and MSE for the prediction,
### i.e. for Xbetahat used as a predicton of y
  pred.bias<-mean(pred.err)
  pred.bias.2<-pred.bias^2
  pred.var<-var(pred.err)
  pred.MSE<-mean(pred.err^2)
  
### calculate bias, squared bias, variance and MSE for fhat,
### i.e. for fhat = Xbetahat used as an estimate of f
  
  f.bias<-mean(f.err)
  f.bias.2<-f.bias^2
  f.var<-var(f.err)
  f.MSE<-mean(f.err^2)
  
  pred.res<-c(pred.bias,pred.bias.2,pred.var,pred.MSE)
  f.res<-c(f.bias,f.bias.2,f.var,f.MSE)
  res<-rbind(beta.res,pred.res,f.res)
  colnames(res)<-c("bias","bias2","var","MSE")

### put all results into res.tot 
  res.tot<-cbind(res.tot,res)
}


### makes rownames
tmp<-rep(NA,p+1)
for (i in 1:(p+1)) {
  tmp[i]<-paste("beta",i-1,sep="")
}
rownames(res.tot)<-c(tmp,"pred","f")

### print out simulation results with 1 desimal
round(res.tot,1)

### print out theoretical results for Var(betahat) with 1 desimal
round(var.betahat,1)


############
### RESULTS
############
> round(res.tot,1)
       bias bias2   var   MSE bias bias2  var  MSE bias bias2  var  MSE
beta0  -0.1   0.0   2.9   2.9  0.0     0  0.3  0.3    0     0  0.0  0.0
beta1  -0.2   0.0  18.2  18.2  0.0     0  1.6  1.6    0     0  0.1  0.1
beta2  -0.3   0.1  28.6  28.7  0.0     0  1.1  1.1    0     0  0.1  0.1
beta3  -0.1   0.0  22.3  22.3  0.0     0  1.4  1.4    0     0  0.1  0.1
beta4   0.2   0.0  22.3  22.3  0.0     0  1.8  1.8    0     0  0.1  0.1
beta5   0.0   0.0  22.7  22.6  0.0     0  1.4  1.4    0     0  0.1  0.1
beta6   0.1   0.0   8.1   8.1 -0.1     0  1.5  1.5    0     0  0.1  0.1
beta7   0.0   0.0  30.0  30.0  0.0     0  1.6  1.6    0     0  0.1  0.1
beta8  -0.2   0.0  16.8  16.8 -0.1     0  1.1  1.1    0     0  0.1  0.1
beta9  -0.3   0.1  25.3  25.4  0.0     0  1.2  1.2    0     0  0.1  0.1
beta10  0.0   0.0  26.0  26.0  0.0     0  1.1  1.1    0     0  0.1  0.1
beta11  0.0   0.0  14.1  14.1  0.1     0  1.7  1.7    0     0  0.1  0.1
beta12  0.3   0.1  59.4  59.5  0.0     0  1.4  1.4    0     0  0.1  0.1
beta13 -0.1   0.0  12.4  12.4  0.0     0  1.7  1.7    0     0  0.1  0.1
beta14  0.6   0.3  88.0  88.2  0.0     0  1.3  1.3    0     0  0.1  0.1
beta15 -0.2   0.0  18.0  18.0  0.1     0  1.4  1.4    0     0  0.1  0.1
pred    0.1   0.0 117.2 117.2  0.0     0 29.7 29.7    0     0 25.4 25.4
f       0.1   0.0  92.1  92.1  0.0     0  4.7  4.7    0     0  0.4  0.4


> round(var.betahat,1)
      [,1] [,2] [,3]
 [1,]  2.8  0.3  0.0
 [2,] 17.3  1.6  0.1
 [3,] 29.1  1.1  0.1
 [4,] 22.2  1.4  0.1
 [5,] 21.2  1.7  0.1
 [6,] 21.7  1.4  0.1
 [7,]  7.9  1.5  0.1
 [8,] 33.0  1.6  0.1
 [9,] 16.6  1.1  0.1
[10,] 25.1  1.3  0.1
[11,] 24.0  1.1  0.1
[12,] 14.0  1.6  0.1
[13,] 63.0  1.3  0.1
[14,] 12.1  1.7  0.1
[15,] 92.5  1.2  0.1
[16,] 20.1  1.4  0.1

