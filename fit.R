library(methods)
library(coda)
library(nimble)

# iterations=2000
##options(mc.cores = parallel::detectCores())
nimbleOptions(verifyConjugatePosteriors=TRUE)
nimdata <- lme4:::namedList(obs=sim$Iobs)
nimcon <- lme4:::namedList(numobs
                           , N
                           , i0
)

niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop, 
                             initDis=initDis, N0)

if(observation == "nb"){
  niminits <- lme4:::namedList(I=sim$I,obsMean=sim$I,effprop,R0,repprop, 
                               initDis=initDis,N0, repShape=0.1)
}

params <- c("R0","effprop","repprop")

# nimmod <- nimbleModel(code=nimcode,constants=nimcon, data=nimdata,
#                       inits=niminits)
# aa <- configureMCMC(nimmod,print=TRUE)
# 
# nimble is not picking up the conjugate beta priors for nimble

source(paste("templates",type,process,observation,seed,iterations,"nimcode",sep="."))
mcmcs <- c("jags","nimble","nimble_slice") 
stanmod <- ""
if(type=="hyb"){
  mcmcs <- c("jags","nimble","nimble_slice","stan")
  stanmod <- paste(process,observation,seed,iterations,"stan",sep=".")
}


FitModel <- MCMCsuite(code=nimcode,
                      data=nimdata,
                      inits=niminits,
                      constants=nimcon,
                      MCMCs=mcmcs,
                      stan_model=stanmod,
                      monitors=params,
                      calculateEfficiency=TRUE,
                      niter=iterations,
                      makePlot=FALSE,
                      savePlot=FALSE)

print(FitModel$summary)


