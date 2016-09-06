library(methods)
library(coda)
library(nimble)

nimbleOptions(verifyConjugatePosteriors=TRUE)
nimdata <- lme4:::namedList(obs=sim$Iobs)
nimcon <- lme4:::namedList(numobs ,N ,i0)

niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop, N0)

if(observation == "nb"){
  niminits <- lme4:::namedList(I=sim$I,obsMean=sim$I,effprop,R0,repprop,N0, repShape=0.1)
}

if(observation == "bb"){
  niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop,N0,repobsa=0.1,repobsb=0.1)
  nimcon <- lme4:::namedList(numobs ,N ,i0,repobsSize=10)
  }

params <- c("R0","effprop","repprop")

source(paste("templates",type,process,observation,seed,iterations,"nimcode",sep="."))
mcmcs <- c("jags","nimble","nimble_slice") 
stanmod <- ""
if(type=="hyb"){
  niminits <- lme4:::namedList(I=sim$I*repprop,effprop,R0,repprop, N0,repShape=0.1)
  if(observation == "nb"){
    niminits <- lme4:::namedList(I=sim$I*repprop,obsMean=sim$I*repprop,effprop,R0,repprop,N0, repShape=0.1)
  }
  mcmcs <- c("jags"
               ,"nimble"
               ,"nimble_slice"
               ,"stan"
             )
  stanmod <- paste(process,observation,seed,iterations,"stan",sep=".")
}

aa <- nimbleModel(code=nimcode,constants=nimcon,data=nimdata,inits=niminits)
bb <- aa$getGraph()

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


