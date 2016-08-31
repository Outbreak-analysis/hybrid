##' Basic chain beta-binomial simulator
## Reed-Frost 
## e.g. see http://depts.washington.edu/sismid09/software/Module_7/reedfrost.R
## or the somewhat lame Wikipedia page

##' @param beta prob. of adequate contact per infective
##' @param population size
##' @param effprop initial effective proportion of population
##' @param i0 initial infected
##' @param t0 initial time (unused)
##' @param numobs ending time
##' @param seed random number seed
##' @param reporting observation probability (1 by default)
##' @return a data frame with columns (time, S, I, R) 

rbbinom <- function(n, prob, k, size){
  mtilde <- rbeta(n, k/(1-prob), k/prob)
  return(rbinom(n, prob=mtilde, size=size))
}

simm <- function(R0 = 2, N=10000, effprop=0.9, i0=1,
                  t0=1, numobs=20, repMean=0.5, repSize=10, seed=NULL){
  
  ## *all* infecteds recover in the next time step
  
  if (!is.null(seed)) set.seed(seed)
  tvec <- seq(1,numobs)
  n <- length(tvec)
  I <- Iobs <- S <- R <- pSI <- numeric(n)
  
  ##Initial conditions
  N0 <- round(effprop*N)
  I[1] <- i0
  S[1] <- N0 - i0
  R[1] <- N-N0
  beta <- exp(-R0/N0)
  pSI[1] <- 1 - (beta)^I[1]
  Iobs[1] <- rbbinom(1, prob=repMean, k=repSize, size=I[1])
  ## Generate the Unobserved process I, and observables:
  
  for (t in 2:n){
    I[t] <- rbbinom(1,prob=pSI[t-1],k=repSize,size=S[t-1])
    S[t] <- S[t-1] - I[t]
    R[t] <- R[t-1] + I[t-1]
    pSI[t] <- 1 - (beta)^I[t]
    Iobs[t] <- rbbinom(1, prob=repMean, k=repSize, size=I[t])
  }
  
  data.frame(time=tvec, S, I, R, Iobs,pSI)
  
}

system.time(sim <- simm(R0=R0
                        , N=N
                        , effprop=effprop
                        , i0=i0
                        , repMean=repprop
                        , repSize=repSize
                        , numobs=numobs,
                        seed=seed
)
)

print(sim)
# rdsave(sim)
