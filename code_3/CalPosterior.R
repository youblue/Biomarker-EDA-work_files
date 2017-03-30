#' 
#' A function for calculating Posterior, Log Posterior, First and Second Derivative of Log-Posterior
#' 

## Posterior of beta1 when beta2 is known
PostBeta1 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    s <- sum( exp(2*mu*beta2) * ( (z1-mu)^2 + (z2-mu)^2) )
    return( beta1^(-2*n-0.8) * exp(-0.001*beta1) * exp(-0.5*s/beta1^2) )
}

## Posterior of beta2 when beta1 is known
PostBeta2 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    s <- sum( exp(2*mu*beta2) * ( (z1-mu)^2 + (z2-mu)^2) )
    return( beta2^(-0.8) * exp(-0.001*beta2) * exp(-0.5*s/beta1^2) )
}

## Log-posterior of beta1 when beta2 is known
LogPostBeta1 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    s <- sum( exp(2*mu*beta2) * ( (z1-mu)^2 + (z2-mu)^2) )
    return( (-2*n-0.8)*log(beta1) - 0.001*beta1 -0.5*s/beta1^2 )
}

## Log-posterior of beta2 when beta1 is known
LogPostBeta2 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    s <- sum( exp(2*mu*beta2) * ( (z1-mu)^2 + (z2-mu)^2) )
    return( -0.8*log(beta2) + (2*sum(mu)-0.001)*beta2 -0.5*s/beta1^2)
}

## First derivative of log-posterior of beta1 when beta2 is known
dLogPostBeta1 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    s <- sum( exp(2*mu*beta2) * ( (z1-mu)^2 + (z2-mu)^2) )
    return( (-2*n-0.8)/beta1 - 0.001 + s/beta1^3 )
}

## First derivative of log-posterior of beta2 when beta1 is known
dLogPostBeta2 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    ds <- sum( exp(2*mu*beta2) * mu * ( (z1-mu)^2 + (z2-mu)^2) )
    return( -0.8/beta2 + (2*sum(mu)-0.001) - ds/beta1^2 )
}

## Secondary derivative of log-posterior of beta1 when beta2 is known
ddLogPostBeta1 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    s <- sum( exp(2*mu*beta2) * ( (z1-mu)^2 + (z2-mu)^2) )
    return( (2*n+0.8)/beta1^2 - 3*s/beta1^4 )
}

## Secondary derivative of log-posterior of beta2 when beta1 is known
ddLogPostBeta2 <- function(z1, z2, n, mu, beta1 = 0.15, beta2 = 0.65){
    dds <- sum( exp(2*mu*beta2) * mu^2 * ( (z1-mu)^2 + (z2-mu)^2) )
    return( 0.8/beta2^2 - 2*dds/beta1^2 )
}

