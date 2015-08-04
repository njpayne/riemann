# Logit-exposure-adjusted function

logitexp <- function(exposure = 1) 
{ linkfun <- function(mu) {            
  eta <- if (any(exposure-mu <= 0)) log((mu)/abs(mu-exposure)) else log((mu)/(exposure-mu))
  eta 
}
  
  linkinv <- function(eta) {
    thresh <- -log(.Machine$double.eps)
    eta <- pmin(thresh, pmax(eta, -thresh))
    exposure*(exp(eta)/(1 + exp(eta)))
  }
  
  mu.eta <- function(eta) 
  {
    thresh <- -log(.Machine$double.eps)
    res <- rep(.Machine$double.eps, length(eta))
    res[abs(eta) < thresh] <- ((exposure*exp(eta))/(1 + exp(eta))^2)[abs(eta) < thresh]
    res
  }
  
  valideta <- function(eta) TRUE 
  link <- paste("logexp(", exposure, ")", sep="") 
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
                 valideta = valideta, name = link), class = "link-glm") } 
