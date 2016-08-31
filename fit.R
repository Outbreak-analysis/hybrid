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

niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop,N0, 
                             initDis=initDis)


params <- c("R0","effprop","repprop")

# nimmod <- nimbleModel(code=nimcode,constants=nimcon, data=nimdata,
#                       inits=niminits)
# aa <- configureMCMC(nimmod,print=TRUE)
# 
# nimble is not picking up the conjugate beta priors for nimble

source(paste(rtargetname,".nimcode",sep=""))

FitModel <- MCMCsuite(code=nimcode,
                      data=nimdata,
                      inits=niminits,
                      constants=nimcon,
                      MCMCs=c("jags","nimble","nimble_slice"),
                      monitors=params,
                      calculateEfficiency=TRUE,
                      niter=iterations,
                      makePlot=FALSE,
                      savePlot=FALSE)

print(FitModel$summary)


